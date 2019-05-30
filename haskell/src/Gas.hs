{-# Language TemplateHaskell #-}
{-# Language ViewPatterns #-}

module Gas where

import Control.Lens
import Control.Monad.State.Strict
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
data Cond = Cond String
  deriving (Eq, Ord, Show)

instance Ord NullOp where
  (Literal m) `compare` (Literal n)
    = m `compare` n

data StratificationMap = StratificationMap
 { _stratMap   :: Map GasExpr Int,
   _nextIndex  :: Int,
   _stratLabel :: String
 }

makeLenses ''StratificationMap

type Stratification a = State StratificationMap a

bracket :: String -> String
bracket s = "( " ++ s ++ " )"

unparse :: (Maybe StratificationMap) -> GasExpr -> String
unparse msm expr =
  let (sm, tag) = case msm of
        Just x  -> (x ^. stratMap,
                    x ^. stratLabel)
        Nothing -> (mempty, "")
  in case Map.lookup expr sm of
    Just i -> tag ++ show i
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
      (ITE (Cond c) e f) -> "#if "
                ++ c ++ " #then "
                ++ s ++ " #else " ++ t
                ++ " #fi"
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
    -- should only stratify stuff with ITEs and Nullaries
    (ITE c e f) -> do
      se <- stratifier e
      sf <- stratifier f
      return ()
    (Nullary (Literal x)) ->
      return ()

stratify :: String -> GasExpr -> StratificationMap
stratify s e = execState (stratifier e)
  (StratificationMap
    { _stratMap   = mempty,
      _nextIndex  = 0,
      _stratLabel = s
    })

formatStratifiedSyntax :: StratificationMap -> String
formatStratifiedSyntax sm =
  Map.foldlWithKey (formatStratifiedLeaf sm) "" (view stratMap sm)

formatStratifiedLeaf :: StratificationMap -> String -> GasExpr -> Int -> String
formatStratifiedLeaf sm acc expr i = acc
  ++ "syntax Int ::= \"" ++ tag ++ show i
  ++ "\" [function]" ++ "\n"
  ++ "rule " ++ tag ++ show i ++ " => " ++ (unparse (Just sm') expr)
  ++ "\n" ++ "\n"
  where tag = sm ^. stratLabel
        sm' = stratMap %~ (Map.delete expr) $ sm
