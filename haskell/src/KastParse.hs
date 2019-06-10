{-# Language TemplateHaskell #-}
module KastParse where

import Control.Lens
import Data.Either               (lefts, rights)
import Data.List                 (intercalate, isPrefixOf, isSuffixOf)
import qualified Data.Map.Strict as Map

import Gas
import Kast

-- instance has to live here because we need formatKast
instance Show IntOrStartGasOrBlob where
  show (Basic n) = show n
  show (Blob kast) = case formatKast kast of
    Left e -> error e
    Right fs -> view formula fs




kastToGasExpr :: Kast -> Either String (BasicGasExpr)
kastToGasExpr kast = case node kast of
  "KVariable" -> case originalName kast of
    Nothing -> Left $ "KVariable missing originalName."
    Just somevar ->  if "VGas" `isPrefixOf` somevar
                     then Right $ Value StartGas
                     else Left $ "Can't have variables in gas expressions, found: " ++ somevar

  "KToken" -> case sort kast of
    Nothing -> Left $ "KToken missing sort."
    Just "Int" -> Right $ Value $ Literal n
      where n = read t
            Just t = token kast
    Just somesort -> Left $ "Can't have sorts other than Int, found: " ++ somesort

  "KApply" -> case stripModuleTag <$> (label kast) of
    Nothing -> Left "KApply missing label."
    Just "_+Int_" -> let Just [arg1, arg2] = args kast in
      case kastToGasExpr arg1 of
        Left err -> Left err
        Right e -> case kastToGasExpr arg2 of
          Left err -> Left err
          Right f -> Right $ Binary Add e f

    Just "_-Int_" -> let Just [arg1, arg2] = args kast in
      case kastToGasExpr arg1 of
        Left err -> Left err
        Right e -> case kastToGasExpr arg2 of
          Left err -> Left err
          Right f -> Right $ Binary Sub e f

    Just "_*Int_" -> let Just [arg1, arg2] = args kast in
      case kastToGasExpr arg1 of
        Left err -> Left err
        Right e -> case kastToGasExpr arg2 of
          Left err -> Left err
          Right f -> Right $ Binary Mul e f

    Just "_/Int_" -> let Just [arg1, arg2] = args kast in
      case kastToGasExpr arg1 of
        Left err -> Left err
        Right e -> case kastToGasExpr arg2 of
          Left err -> Left err
          Right (Value (Literal 64)) -> Right $ Unary SixtyFourth e
          Right n -> Left $ "Gas expressions should have /64 only, found: /" ++ (show n)

    Just "#if_#then_#else_#fi" -> let Just [arg_c, arg1, arg2] = args kast in
      case kastToGasExpr arg1 of
        Left err -> Left err
        Right e -> case kastToGasExpr arg2 of
          Left err -> Left err
          Right f -> case formatKast arg_c of
            Left err -> Left err
            Right c -> Right $ ITE (Cond c) e f
    Just somelabel -> Left $ "Unknown KApply in gas expression: " ++ somelabel
  _ -> Left $ "Unrecognised \"node\" in KAST."

formatKast :: Kast -> Either String FormulaicString
formatKast kast = case node kast of
  "KVariable" -> let Just var_name = originalName kast
                     -- K is the only supported type
                     Just var_type = Just "K"
                 in Right $ FormulaicString var_name (at var_name ?~ var_type $ Map.empty)

  "KToken" -> let Just n = token kast
      in Right $ FormulaicString n Map.empty

  "KApply" -> let Just func       = stripModuleTag <$> label kast
                  Just apply_args = args kast
                  lr_fargs    = map formatKast apply_args
                  fargs       = (^. formula) <$> rights lr_fargs
                  fargs_types = Map.unions $ (^. types) <$> rights lr_fargs
              in case lefts lr_fargs of
                   err:_ -> Left err
                   [] -> case formatKApply func fargs of
                     Left err -> Left err
                     Right x    -> Right $ FormulaicString x fargs_types
  _ -> Left $ "Unrecognised \"node\" in KAST."

formatKApply :: String -> [String] -> Either String String
formatKApply func fargs = let bracketed s = "( " ++ s ++ " )" in
  case (head func, last func, fargs) of
    -- binary infix
    ('_', '_', (farg1:(farg2:[]))) -> Right $ bracketed $ farg1 ++ " " ++ func_trim ++ " " ++ farg2
      where func_trim = tail $ init $ func
    -- unsupported mixfix
    ('_', '_', _) -> Left $ "Couldn't parse mixfix operator: " ++ func
    -- unary prefix
    (_, '_', (farg:[])) -> Right $ bracketed $ func_trim ++ farg
      where func_trim = init $ func
    -- unary postfix
    ('_', _, (farg:[])) -> Right $ bracketed $ farg ++ func_trim
      where func_trim = tail $ func
    -- n-ary prefix
    (_, _, _) -> Right $ bracketed $ func_trim ++ (bracketed (intercalate " , " fargs))
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
