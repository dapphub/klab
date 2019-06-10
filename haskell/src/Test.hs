module Test where

import Data.Map.Strict as Map

-- grammar for K gas expressions
-- a is the "base field"
data (Ord a) => GasExpr a =
  Nullary (NullOp a)
  | Unary UnOp (GasExpr a)
  | Binary BinOp (GasExpr a) (GasExpr a)
  | ITE Cond (GasExpr a) (GasExpr a)
  deriving (Eq, Ord, Show)

type ConstantGasExpr = GasExpr Int

data (Ord a) => NullOp a = Value a
  deriving (Eq, Ord, Show)
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
