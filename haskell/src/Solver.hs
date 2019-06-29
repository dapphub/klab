module Solver where

import Data.List  (drop, take)

import Gas

-- percolates ITEs to the top
normaliseStep :: GasExpr f -> GasExpr f
normaliseStep (Value n) = Value n
normaliseStep (Unary op (ITE c e f)) =
  normaliseStep (ITE c ope opf)
  where ope = Unary op e
        opf = Unary op f
normaliseStep (Unary op e) = Unary op e'
  where e' = normaliseStep e
normaliseStep (Binary op (ITE c e f) g) =
  normaliseStep (ITE c eg fg)
  where eg = Binary op e g
        fg = Binary op f g
normaliseStep (Binary op f (ITE c e g)) =
  normaliseStep (ITE c fe fg)
  where fe = Binary op f e
        fg = Binary op f g
normaliseStep (Binary op e f) = Binary op e' f'
  where e' = normaliseStep e
        f' = normaliseStep f
normaliseStep (ITE c e f) = ITE c e' f'
  where e' = normaliseStep e
        f' = normaliseStep f

unnormaliseStep :: (Show f, Ord f) => GasExpr f -> GasExpr f
unnormaliseStep (Value n) = Value n
unnormaliseStep (Unary op e) = Unary op e'
  where e' = unnormaliseStep e
unnormaliseStep (Binary op e f) = Binary op e' f'
  where e' = unnormaliseStep e
        f' = unnormaliseStep f
unnormaliseStep (ITE c (Binary op e f) (Binary op' g h))
  | op == op' && f == h
    = Binary op (ITE c e' g') f'
  | op == op' && e == g
    = Binary op g' (ITE c f' h')
  | otherwise
    = (ITE c (Binary op e' f') (Binary op' g' h'))
  where e' = unnormaliseStep e
        f' = unnormaliseStep f
        g' = unnormaliseStep g
        h' = unnormaliseStep h
unnormaliseStep (ITE c e f) = ITE c e' f'
  where e' = unnormaliseStep e
        f' = unnormaliseStep f

cosolveStep :: ConstantGasExpr -> ConstantGasExpr
cosolveStep (Value n) = Value n
cosolveStep (Unary op e) = Unary op e'
  where e' = cosolveStep e
cosolveStep (Binary op e f) = Binary op e' f'
  where e' = cosolveStep e
        f' = cosolveStep f
cosolveStep (ITE p
             (ITE p'
              (Value a)
              (Value b))
             (ITE p''
              (Value c)
              (Value d)))
  | p' == p'' && a - b == c - d  && a > b =
    let e = a - b in
      (Binary Add
       (ITE p'
        (Value e)
        (Value 0))
       (ITE p
        (Value b)
        (Value d)))
  | p' == p'' && b - a == d - c && a < b =
    let e = b - a in
      (Binary Add
       (ITE p'
        (Value 0)
        (Value e))
       (ITE p
        (Value a)
        (Value c)))
  | otherwise = (ITE p
                 (ITE p'
                  (Value a)
                  (Value b))
                 (ITE p''
                  (Value c)
                  (Value d)))
cosolveStep (ITE p e f) = ITE p e' f'
  where e' = cosolveStep e
        f' = cosolveStep f

-- extirpation aims to transform the expression
-- to have the form Sub a b where b is independent
-- and maximal
extirpateStep :: BasicGasExpr -> BasicGasExpr
-- -- push independents to the right
-- extirpateStep (Binary Add e f)
--   | (isIndependent e) && (not $ isIndependent f) = Binary Add f e
--   | otherwise = Binary Add e' f'
--     where e' = extirpateStep e
--           f' = extirpateStep f
-- -- reassociate to the left
-- extirpateStep (Binary Add
--                e
--                (Binary Add f g))
--   | (not $ isIndependent g) = Binary Add (Binary Add e f) g
--   | otherwise = Binary Add (Binary Add e' f') g
--     where e' = extirpateStep e
--           f' = extirpateStep f
-- -- peel off independent chunks to the right
-- extirpatestep (Binary Add
--                (Binary Add e f)
--                g)
--   | (isIndependent f) && (isIndependent g)
--     && (not $ isIndependent e) = Binary Add e (Binary Add f g)
--   | (isIndependent f) && (not $ isIndependent g)
--     && (not $ isIndependent e) = Binary Add (Binary Add e g) f
--   | otherwise = Binary Add (Binary Add e' f') g'
--     where e' = extirpateStep e
--           f' = extirpateStep f
--           g' = extirpateStep g
-- reassociate to the left
extirpateStep (Binary Add
               (Binary Add e f)
               g)
  = Binary Add e (Binary Add f g)
extirpateStep (Binary Add
               (Binary Sub e f)
               g)
  = Binary Sub e (Binary Sub f g)
extirpateStep (Binary Sub
               (Binary Add e f)
               g)
  = Binary Add e (Binary Sub f g)
extirpateStep (Binary Sub
               (Binary Sub e f)
               g)
  = Binary Sub e (Binary Add f g)
-- ignore everything else
extirpateStep e = e

extirpate :: BasicGasExpr -> BasicGasExpr
extirpate = iteratedFix extirpateStep

iteratedFix :: (Eq a) => (a -> a) -> a -> a
iteratedFix f x = let x' = f x in
                    if x' == x
                    then x
                    else iteratedFix f x'

normalise :: BasicGasExpr -> BasicGasExpr
normalise = iteratedFix normaliseStep

unnormalise :: BasicGasExpr -> BasicGasExpr
unnormalise = iteratedFix unnormaliseStep

cosolve :: ConstantGasExpr -> ConstantGasExpr
cosolve = iteratedFix (unnormaliseStep . cosolveStep)

solveNumerically :: Int -> BasicGasExpr -> ConstantGasExpr
solveNumerically maxGas = (solveLeaves maxGas) . normalise

solveAlgebraically :: BasicGasExpr -> Maybe ConstantGasExpr
solveAlgebraically expr = case extirpate expr of
  (Binary Sub (Value StartGas) e) -> convert e
  _ -> Nothing

solve :: Int -> BasicGasExpr -> ConstantGasExpr
solve maxGas expr = case solveAlgebraically expr of
  Just s -> s
  Nothing -> solveNumerically maxGas expr

newSolve :: Int -> BasicGasExpr -> ConstantGasExpr
newSolve maxGas (Binary Sub (Value StartGas) e)
  | isIndependent e = coerce e
  | otherwise = case extirpate e of
      Binary Add f g | isIndependent g
                       -> (Binary Add
                           (solveNumerically maxGas (Binary Sub (Value StartGas) f))
                           (coerce g))
      _ -> solveNumerically maxGas (Binary Sub (Value StartGas) e)
newSolve _ _ = error "Can only solve expressions in the form VGas - e(VGas)."


-- only works for normalised GasExpr
solveLeaves :: Int -> BasicGasExpr -> ConstantGasExpr
solveLeaves maxGas (ITE c e f) = ITE c e' f'
  where e' = solveLeaves maxGas e
        f' = solveLeaves maxGas f
solveLeaves maxGas e = Value minG
  where Just minG = minimiseG maxGas e

evalUnOp :: UnOp -> Int -> Int
evalUnOp SixtyFourth x = quot x 64

evalBinOp :: BinOp -> Int -> Int -> Int
evalBinOp Add x y = x + y
evalBinOp Sub x y = x - y
evalBinOp Mul x y = x * y

-- only works for unconditional GasExpr
eval :: BasicGasExpr -> Int -> Int
eval (Value StartGas ) vg = vg
eval (Value (Literal n)) _ = n
eval (Unary op e) vg = (evalUnOp op) (eval e vg)
eval (Binary op e f) vg = (evalBinOp op) (eval e vg) (eval f vg)
eval (ITE _ _ _ ) _ = error "only works for unconditional GasExpr"

-- only works for unconditional GasExpr
minimiseG :: Int -> BasicGasExpr -> Maybe Int
-- findFirst is more efficient than find
minimiseG maxGas e = findFirst ((>=0) . eval e) [1..maxGas]

-- assuming p monotone, finds
-- the first x such that p x
-- in logarithmic time
findFirst :: (a -> Bool) -> [a] -> Maybe a
findFirst _ [] = Nothing
findFirst p (x:[]) = if p x
                     then Just x
                     else Nothing
findFirst p xs =
  let mid = div (length xs) 2
      top = take mid xs
      bot = drop mid xs
  in case top of
    [] -> findFirst p bot
    _  -> if p (last top)
          then findFirst p top
          else findFirst p bot

-- only works for unconditional GasExpr
findCallSubexprs :: BasicGasExpr -> [BasicGasExpr]
findCallSubexprs (Binary Sub
                  (Binary Sub
                   a (Unary SixtyFourth b))
                  c) =
  if a == b then [Binary Sub
                  (Binary Sub
                   a (Unary SixtyFourth b))
                  c]
  ++ findCallSubexprs a
  ++ findCallSubexprs c
  else findCallSubexprs a
  ++ findCallSubexprs b
  ++ findCallSubexprs c
findCallSubexprs (Value _) = []
findCallSubexprs (Unary _ a) =
  findCallSubexprs a
findCallSubexprs (Binary _ a b) =
  findCallSubexprs a
  ++ findCallSubexprs b
findCallSubexprs (ITE _ _ _) =
  error "only works for unconditional GasExpr"

maxLeaf :: ConstantGasExpr -> ConstantGasExpr
maxLeaf (Value n) = (Value n)
maxLeaf (ITE _ e f) = max (maxLeaf e) (maxLeaf f)
maxLeaf _ = error "maxLeaf applied to nontrivial algebraic expression."
