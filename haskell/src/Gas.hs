{-# Language DeriveFoldable #-}
{-# Language DeriveTraversable #-}
{-# Language GADTs #-}
{-# Language LambdaCase #-}
{-# Language MultiParamTypeClasses #-}
{-# Language TemplateHaskell #-}
{-# Language ViewPatterns #-}

module Gas where

import Control.Lens hiding (op)
import Control.Monad.State.Strict
import Data.List (intercalate)
import Data.Map.Strict (Map)
import Data.Foldable (toList)
import qualified Data.Map.Strict as Map

import Kast (Kast)

-- a formatted K formula with typed variables
data FormulaicString = FormulaicString
  { _formula :: String,
    _types   :: Map String String
  }
  deriving (Eq, Ord, Show)
makeLenses ''FormulaicString

data UnOp = SixtyFourth
  deriving (Eq, Ord, Show)
data BinOp = Add | Sub | Mul
  deriving (Eq, Ord, Show)
data Cond = Cond FormulaicString
  deriving (Eq, Ord, Show)

-- grammar for K gas expressions
-- r is the "base ring"
data GasExpr r where
  Value  :: r -> GasExpr r
  Unary  :: UnOp -> GasExpr r -> GasExpr r
  Binary :: BinOp -> GasExpr r -> GasExpr r -> GasExpr r
  ITE    :: Cond -> GasExpr r -> GasExpr r -> GasExpr r
  deriving (Eq, Ord, Show, Foldable, Traversable)

data IntOrStartGas where
  Literal  :: Int -> IntOrStartGas
  StartGas :: IntOrStartGas
  deriving (Eq, Ord)

data IntOrStartGasOrBlob where
  Basic :: IntOrStartGas -> IntOrStartGasOrBlob
  Blob :: Kast -> IntOrStartGasOrBlob
  deriving (Eq)

instance Show IntOrStartGas where
  show StartGas = "VGas"
  show (Literal n) = show n

-- instance Ord IntOrStartGas where
--   StartGas <= _ = True
--   _ <= StartGas =


class Convertible a b where
  convert :: a -> Maybe b

coerce :: (Show a, Convertible a b) => a -> b
coerce x = case convert x of
  Just y  -> y
  Nothing -> error $ "coercion error: couldn't convert " ++ show x

instance (Convertible a b) => Convertible (GasExpr a) (GasExpr b) where
  convert e = sequenceA (fmap convert e)

instance Functor GasExpr where
  fmap phi = \case
    Value r -> Value $ phi r
    Unary op e -> Unary op (phi <$> e)
    Binary op e f -> Binary op (phi <$> e) (phi <$> f)
    ITE c e f -> ITE c (phi <$> e) (phi <$> f)

type ConstantGasExpr = GasExpr Int
type BasicGasExpr = GasExpr IntOrStartGas
type BlobfulGasExpr = GasExpr IntOrStartGasOrBlob

instance Convertible Int IntOrStartGas where
  convert n = Just $ Literal n

instance Convertible IntOrStartGas Int where
  convert (Literal n) = Just n
  convert _ = Nothing

getLeafValues :: GasExpr r -> [r]
getLeafValues = toList

isBlobFree :: BlobfulGasExpr -> Bool
isBlobFree = all $ \case
  Blob _ -> False
  _ -> True

-- independent means "independent of VGas"
isIndependent :: BasicGasExpr -> Bool
isIndependent = all $ \case
  StartGas -> False
  _ -> True

data StratificationMap f = StratificationMap
 { _stratMap   :: Map (GasExpr f) Int,
   _nextIndex  :: Int,
   _stratLabel :: String,
   _stratTypes :: Map String String
 }
 deriving (Eq, Show)

makeLenses ''StratificationMap

type Stratification f a = State (StratificationMap f) a

bracket :: String -> String
bracket s = "( " ++ s ++ " )"

unparse :: (Show f, Ord f)
  => (Maybe (StratificationMap f)) -> (GasExpr f) -> String
unparse msm expr =
  let (sm, tag, ts) = case msm of
        Just x  -> (x ^. stratMap,
                    x ^. stratLabel,
                    x ^. stratTypes)
        Nothing -> (mempty, "", mempty)
  in case Map.lookup expr sm of
    Just i -> tag ++ show i ++ formatKArgs (Map.toList ts)
    Nothing -> case expr of
      -- (Nullary (Value StartGas)) -> "VGas"
      (Value x) -> show x
      (Unary SixtyFourth e) -> (bracket $ unparse msm e) ++ " /Int 64"
      (Binary op e f) -> bracket (s ++ opstr ++ t)
        where s = unparse msm e
              t = unparse msm f
              opstr = case op of
                        Add -> " +Int "
                        Sub -> " -Int "
                        Mul -> " *Int "
      (ITE (Cond c) e f) ->
        "(" ++ "#if " ++ (c ^. formula) ++
               " #then " ++ s ++
               " #else " ++ t ++
               " #fi" ++ ")"
        where s = unparse msm e
              t = unparse msm f

stratifier :: (Ord f)
  => (GasExpr f) -> Stratification f ()
stratifier expr = do
  smap <- get
  -- insertSoft means we deduplicate the labels
  let insertSoft = Map.insertWith (flip const)
      i = smap ^. nextIndex
      smap' = stratMap %~ (insertSoft expr i)
              $ nextIndex %~ (+1)
              $ smap
  put smap'
  case expr of
    (Unary _ e) -> do
      stratifier e
      return ()
    (Binary _ e f) -> do
      stratifier e
      stratifier f
      return ()
    (ITE (Cond c) e f) -> do
      put $
        stratTypes %~ (Map.union (c ^. types)) $
        smap'
      stratifier e
      stratifier f
      return ()
    (Value _) ->
      return ()

stratify :: (Ord f)
  => String -> (GasExpr f) -> (StratificationMap f)
stratify s e = execState (stratifier e)
  (StratificationMap
    { _stratMap   = mempty,
      _nextIndex  = 0,
      _stratLabel = s,
      _stratTypes = mempty
    })

formatStratifiedSyntax :: (Show f, Ord f)
  => (StratificationMap f) -> String
formatStratifiedSyntax sm =
  Map.foldlWithKey (formatStratifiedLeaf sm) "" (view stratMap sm)

formatStratifiedLeaf :: (Show f, Ord f)
  => (StratificationMap f) -> String -> (GasExpr f) -> Int -> String
formatStratifiedLeaf sm acc expr i =
  let args = Map.toList $ sm ^. stratTypes in acc
  ++ "syntax Int ::= \"" ++ tag ++ show i
  ++ "\" " ++ (formatAbstractKArgs args) ++ "\n"
  ++ "rule " ++ tag ++ show i ++ (formatKArgs args)
  ++ " => "
  ++ (unparse (Just sm') expr) ++ " [macro]"
  ++ "\n" ++ "\n"
  where tag = sm ^. stratLabel
        sm' = stratMap %~ (Map.delete expr) $ sm

formatAbstractKArgs :: [(String, String)] -> String
formatAbstractKArgs [] = ""
formatAbstractKArgs ts =
  "\"(\" "
  ++ (intercalate " \",\" " (snd <$> ts))
  ++ " \")\""

formatKArgs :: [(String, String)] -> String
formatKArgs [] = ""
formatKArgs ts =
  "("
  ++ (intercalate ", " (fst <$> ts))
  ++ ")"
