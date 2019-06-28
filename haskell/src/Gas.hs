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
import Data.List (elemIndex,
                  intercalate,
                  nub)
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

gasDepth :: GasExpr f -> Int
gasDepth (Value _) = 0
gasDepth (Unary _ e) = 1 + gasDepth e
gasDepth (Binary _ e f) = 1 + max (gasDepth e) (gasDepth f)
gasDepth (ITE _ e f) = 1 + max (gasDepth e) (gasDepth f)

class Convertible a b where
  convert :: a -> Maybe b

class TypedVariables r where
  getTypes :: r -> Map String String

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

instance TypedVariables Int where
  getTypes _ = mempty

instance TypedVariables IntOrStartGas where
  getTypes (Literal _) = mempty
  getTypes StartGas = Map.fromList [("VGas", "K")]

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

type VariableName = String
type KType = String

data StratificationMap f = StratificationMap
 { _stratList  :: [GasExpr f],
   _stratTypes :: Map VariableName KType
 }
 deriving (Eq, Show)

mkStratMap :: (Eq f)
  => [GasExpr f]
  -> Map VariableName KType -> StratificationMap f
mkStratMap es ts = StratificationMap (nub es) ts

makeLenses ''StratificationMap

instance (Ord f) => Semigroup (StratificationMap f) where
  sm1 <> sm2 = mkStratMap (_stratList sm1 <> _stratList sm2)
    (Map.union (_stratTypes sm1) (_stratTypes sm2))
instance (Ord f) => Monoid (StratificationMap f) where
  mempty = mkStratMap mempty mempty

type StratificationLabel = String
type Stratification f a = State (StratificationMap f) a

bracket :: String -> String
bracket s = "( " ++ s ++ " )"

showStratified :: (Show f, Ord f)
  => (Maybe (StratificationMap f, StratificationLabel))
  -> (GasExpr f) -> String
showStratified msml expr =
  let (sm, ts, tag) = case msml of
        Just (x, tag') -> (x ^. stratList,
                           x ^. stratTypes,
                           tag')
        Nothing -> (mempty, mempty, "")
  in case elemIndex expr sm of
    Just i -> tag ++ show i ++ formatKArgs (Map.toList ts)
    Nothing -> unparse (showStratified msml) expr

unparse :: (Show f)
  => (GasExpr f -> String) -> (GasExpr f) -> String
unparse _ (Value x) = show x
unparse recShow (Unary SixtyFourth e) =
  (bracket $ recShow e) ++ " /Int 64"
unparse recShow (Binary op e f) =
  bracket (s ++ opstr ++ t)
  where s = recShow e
        t = recShow f
        opstr = case op of
                  Add -> " +Int "
                  Sub -> " -Int "
                  Mul -> " *Int "
unparse recShow (ITE (Cond c) e f) =
  "(" ++ "#if " ++ (c ^. formula)
     ++ " #then " ++ s
     ++ " #else " ++ t
     ++ " #fi" ++ ")"
  where s = recShow e
        t = recShow f

stratifier :: (Ord f, TypedVariables f)
  => Int -> (GasExpr f) -> Stratification f ()
stratifier depth expr = if gasDepth expr < depth then return () else do
  smap <- get
  -- we deduplicate the labels
  let addSoft = (\x -> if elem expr x
                       then x
                       else x ++ [expr])
      smap' = stratList %~ addSoft $ smap
  put smap'
  case expr of
    (Unary _ e) -> do
      stratifier depth e
      return ()
    (Binary _ e f) -> do
      stratifier depth e
      stratifier depth f
      return ()
    (ITE (Cond c) e f) -> do
      put $
        stratTypes %~ (Map.union (c ^. types)) $
        smap'
      stratifier depth e
      stratifier depth f
      return ()
    (Value x) -> do
      put $
       stratTypes %~ (Map.union (getTypes x)) $
         smap'
      return ()

stratify :: (Ord f, TypedVariables f)
  => Int -> (GasExpr f) -> (StratificationMap f)
stratify depth e = execState (stratifier depth e) mempty

formatStratifiedSyntax :: (Show f, Ord f)
  => (StratificationMap f) -> StratificationLabel -> String
formatStratifiedSyntax sm tag =
  fst $ foldl (formatStratifiedLeaf sm tag) ("", 0) (sm ^. stratList)

formatStratifiedLeaf :: (Show f, Ord f)
  => (StratificationMap f) -> StratificationLabel
  -> (String, Int) -> (GasExpr f) -> (String, Int)
formatStratifiedLeaf sm tag (acc, i) expr =
  let args = Map.toList $ sm ^. stratTypes
      lhs  = tag ++ show i ++ (formatKArgs args)
      rhs  = (unparse (showStratified $ Just (sm, tag)) expr)
  in (acc
  ++ "syntax Int ::= \"" ++ tag ++ show i
  ++ "\" " ++ (formatAbstractKArgs args) ++ "\n"
  ++ "rule " ++ lhs ++ " => " ++ rhs ++ " [macro]"
  ++ "\n" ++ "\n", i+1)

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
