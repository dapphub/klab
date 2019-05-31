{-# Language TemplateHaskell #-}
{-# Language ViewPatterns #-}

module Gas where

import Control.Lens
import Control.Monad.State.Strict
import Data.List (intercalate)
import Data.Map.Strict as Map

-- grammar for K gas expressions
data GasExpr = Nullary NullOp
  | Unary UnOp GasExpr
  | Binary BinOp GasExpr GasExpr
  | ITE Cond GasExpr GasExpr
  deriving (Eq, Ord, Show)

data NullOp = StartGas | Literal Int
  deriving (Eq, Show)
data UnOp = SixtyFourth
  deriving (Eq, Ord, Show)
data BinOp = Add | Sub | Mul
  deriving (Eq, Ord, Show)
data Cond = Cond FormulaicString
  deriving (Eq, Ord, Show)

-- a formatted K formula with typed variables
data FormulaicString = FormulaicString
  { _formula :: String,
    _types   :: Map String String
  }
  deriving (Eq, Ord, Show)

instance Ord NullOp where
  (Literal m) `compare` (Literal n)
    = m `compare` n

data StratificationMap = StratificationMap
 { _stratMap   :: Map GasExpr Int,
   _nextIndex  :: Int,
   _stratLabel :: String,
   _stratTypes :: Map String String
 }

makeLenses ''StratificationMap
makeLenses ''FormulaicString

type Stratification a = State StratificationMap a

bracket :: String -> String
bracket s = "( " ++ s ++ " )"

unparse :: (Maybe StratificationMap) -> GasExpr -> String
unparse msm expr =
  let (sm, tag, ts) = case msm of
        Just x  -> (x ^. stratMap,
                    x ^. stratLabel,
                    x ^. stratTypes)
        Nothing -> (mempty, "", mempty)
  in case Map.lookup expr sm of
    Just i -> tag ++ show i ++ formatKArgs (Map.toList ts)
    Nothing -> case expr of
      (Nullary StartGas) -> "VGas"
      (Nullary (Literal x)) -> show x
      (Unary SixtyFourth e) -> (bracket $ unparse msm e) ++ " / 64"
      (Binary op e f) -> bracket (s ++ opstr ++ t)
        where s = unparse msm e
              t = unparse msm f
              opstr = case op of
                        Add -> " + "
                        Sub -> " - "
      (ITE (Cond c) e f) ->
        "#if " ++ (c ^. formula) ++
        " #then " ++ s ++
        " #else " ++ t ++
        " #fi"
        where s = unparse msm e
              t = unparse msm f

stratifier :: GasExpr -> Stratification ()
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
    -- should only stratify unconditional exprs
    (Binary op e f) -> do
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
    (Nullary (Literal x)) ->
      return ()

stratify :: String -> GasExpr -> StratificationMap
stratify s e = execState (stratifier e)
  (StratificationMap
    { _stratMap   = mempty,
      _nextIndex  = 0,
      _stratLabel = s,
      _stratTypes = mempty
    })

formatStratifiedSyntax :: StratificationMap -> String
formatStratifiedSyntax sm =
  Map.foldlWithKey (formatStratifiedLeaf sm) "" (view stratMap sm)

formatStratifiedLeaf :: StratificationMap -> String -> GasExpr -> Int -> String
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
formatAbstractKargs [] = ""
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
