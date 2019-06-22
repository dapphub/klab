{-# Language TemplateHaskell #-}
module KastParse where

import Control.Lens
import Data.List                 (intercalate, isPrefixOf, isSuffixOf)
import qualified Data.Map.Strict as Map

import Gas
import Kast

-- instance has to live here because we need formatKast
instance Show IntOrStartGasOrBlob where
  show (Basic n) = show n
  show (Blob kast) = (formatKast kast) ^. formula

kastToGasExpr :: Kast -> BasicGasExpr
kastToGasExpr kast = case node kast of
  "KVariable" -> case originalName kast of
    Nothing -> error "KVariable missing originalName."
    Just somevar ->  if "VGas" `isPrefixOf` somevar
                     then Value StartGas
                     else error $ "Can't have variables in gas expressions, found: " ++ somevar

  "KToken" -> case sort kast of
    Nothing -> error "KToken missing sort."
    Just "Int" -> Value $ Literal n
      where n = read t
            Just t = token kast
    Just somesort -> error $ "Can't have sorts other than Int, found: " ++ somesort

  "KApply" -> case stripModuleTag <$> (label kast) of
    Nothing -> error "KApply missing label."
    Just "_+Int_" -> let Just [arg1, arg2] = args kast
                         e = kastToGasExpr arg1
                         f = kastToGasExpr arg2
                     in Binary Add e f
    Just "_-Int_" -> let Just [arg1, arg2] = args kast
                         e = kastToGasExpr arg1
                         f = kastToGasExpr arg2
                     in Binary Sub e f
    Just "_*Int_" -> let Just [arg1, arg2] = args kast
                         e = kastToGasExpr arg1
                         f = kastToGasExpr arg2
                     in Binary Mul e f

    Just "_/Int_" -> let Just [arg1, arg2] = args kast
                         e = kastToGasExpr arg1
      in case kastToGasExpr arg2 of
           Value (Literal 64) -> Unary SixtyFourth e
           n -> error $ "Gas expressions should have /64 only, found: /" ++ (show n)
    Just "#if_#then_#else_#fi" ->
      let Just [argc, arg1, arg2] = args kast
          c = formatKast argc
          e = kastToGasExpr arg1
          f = kastToGasExpr arg2
      in ITE (Cond c) e f
    Just somelabel -> error $ "Unknown KApply in gas expression: " ++ somelabel

  _ -> error "Unrecognised \"node\" in KAST."

formatKast :: Kast -> FormulaicString
formatKast kast = case node kast of
  "KVariable" -> let Just var_name = originalName kast
                     -- K is the only supported type
                     Just var_type = Just "K"
                 in FormulaicString var_name (at var_name ?~ var_type $ Map.empty)

  "KToken" -> let Just n = token kast
      in FormulaicString n Map.empty

  "KApply" -> let Just func       = stripModuleTag <$> label kast
                  Just apply_args = args kast
                  fargs    = formatKast <$> apply_args
                  fargs_forms = (^. formula) <$> fargs
                  fargs_types = Map.unions $ (^. types) <$> fargs
              in FormulaicString
                  (formatKApply func fargs_forms)
                  fargs_types
  _ -> error "Unrecognised \"node\" in KAST."

formatKApply :: String -> [String] -> String
formatKApply func fargs = let bracketed s = "( " ++ s ++ " )" in
  case (head func, last func, fargs) of
    -- binary infix
    ('_', '_', (farg1:(farg2:[]))) -> bracketed $ farg1 ++ " " ++ func_trim ++ " " ++ farg2
      where func_trim = tail $ init $ func
    -- unsupported mixfix
    ('_', '_', _) -> error $ "Couldn't parse mixfix operator: " ++ func
    -- unary prefix
    (_, '_', (farg:[])) -> bracketed $ func_trim ++ farg
      where func_trim = init $ func
    -- unary postfix
    ('_', _, (farg:[])) -> bracketed $ farg ++ func_trim
      where func_trim = tail $ func
    -- n-ary prefix
    (_, _, _) -> bracketed $ func_trim ++ (bracketed (intercalate " , " fargs))
      where func_trim = "`" ++ func ++ "`"

-- the module names will be stripped from the labels
modules :: [String]
modules = [ "_INT",
            "_INT-COMMON",
            "_K-EQUAL"
          ]

stripSuffix :: (Eq a) => [a] -> [a] -> [a]
stripSuffix a s = if a `isSuffixOf` s
                  then take (length s - length a) s
                  else s

stripModuleTag :: String -> String
stripModuleTag = foldl (.) id (stripSuffix <$> modules)
