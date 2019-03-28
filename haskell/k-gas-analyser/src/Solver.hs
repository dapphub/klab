module Solver where

import Data.List  (nub, findIndex)
import Data.Maybe (catMaybes)
import Safe       (maximumMay)

import Gas

-- percolates ITEs to the top
normaliseStep :: GasExpr -> GasExpr
normaliseStep (Nullary n) = Nullary n
normaliseStep (Unary op (ITE c e f)) = normaliseStep (ITE c ope opf)
  where ope = Unary op e
        opf = Unary op f
normaliseStep (Unary op e) = Unary op e'
  where e' = normaliseStep e
normaliseStep (Binary op (ITE c e f) g) = normaliseStep (ITE c eg fg)
  where eg = Binary op e g
        fg = Binary op f g
normaliseStep (Binary op f (ITE c e g)) = normaliseStep (ITE c fe fg)        
  where fe = Binary op f e
        fg = Binary op f g
normaliseStep (Binary op e f) = Binary op e' f'
  where e' = normaliseStep e
        f' = normaliseStep f
normaliseStep (ITE c e f) = ITE c e' f'
  where e' = normaliseStep e
        f' = normaliseStep f

iteratedFix :: (Eq a) => a -> (a -> a) -> a
iteratedFix x f = let x' = f x in
                    if x' == x then x else iteratedFix x' f

normalise :: GasExpr -> GasExpr
normalise e = iteratedFix e normaliseStep

solve :: Int -> GasExpr -> GasExpr
solve maxGas = (solveLeaves maxGas) . normalise

-- only works for normalised GasExpr
solveLeaves :: Int -> GasExpr -> GasExpr
solveLeaves maxGas (ITE c e f) = ITE c e' f'
  where e' = solveLeaves maxGas e
        f' = solveLeaves maxGas f
solveLeaves maxGas e = Nullary (Literal maxOfMins)
  where Just maxOfMins = maximumMay $ [minG]
                         ++ catMaybes (map (minimiseG maxGas) (findCallSubexprs e))
        Just minG = minimiseG maxGas e

evalUnOp :: UnOp -> Int -> Int
evalUnOp SixtyFourth x = quot x 64

evalBinOp :: BinOp -> Int -> Int -> Int
evalBinOp Add x y = x + y
evalBinOp Sub x y = x - y

-- only works for unconditional GasExpr
eval :: GasExpr -> Int -> Int
eval (Nullary StartGas) vg = vg
eval (Nullary (Literal n)) _ = n
eval (Unary op e) vg = (evalUnOp op) (eval e vg)
eval (Binary op e f) vg = (evalBinOp op) (eval e vg) (eval f vg)

-- only works for unconditional GasExpr
minimiseG :: Int -> GasExpr -> Maybe Int
minimiseG maxGas e = findInput (eval e) (>=0) [1..maxGas]

findInput :: (a -> b) -> (b -> Bool) -> [a] -> Maybe a
findInput f p [] = Nothing
findInput f p (x:xs) = if p (f x) then Just x else findInput f p xs

-- only works for unconditional GasExpr
findCallSubexprs :: GasExpr -> [GasExpr]
findCallSubexprs (Binary Sub (Binary Sub a (Unary SixtyFourth b)) c)
  = if a == b then [Binary Sub (Binary Sub a (Unary SixtyFourth b)) c]
                   ++ findCallSubexprs a
                   ++ findCallSubexprs c
    else (findCallSubexprs a
           ++ findCallSubexprs b
           ++ findCallSubexprs c)
findCallSubexprs (Nullary _) = []
findCallSubexprs (Unary op a) = findCallSubexprs a
findCallSubexprs (Binary op a b) = findCallSubexprs a
                                   ++ findCallSubexprs b

