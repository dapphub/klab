{-# LANGUAGE DeriveGeneric #-}
module KastParse where

import GHC.Generics
import Control.Lens
import Data.ByteString.Lazy.UTF8 (fromString)
import Data.Aeson                (FromJSON, decode)
import Data.Either               (lefts, rights)
import Data.List                 (intercalate, isPrefixOf, isSuffixOf)

import qualified Data.Map.Strict as Map

import Gas

data Kast = Kast {
  node         :: String,
  sort         :: Maybe String,
  originalName :: Maybe String,
  token        :: Maybe String,
  label        :: Maybe String,
  variable     :: Maybe Bool,
  arity        :: Maybe Int,
  args         :: Maybe [Kast]
  } deriving (Generic, Show)

instance FromJSON Kast

kastToGasExpr :: Kast -> Either String GasExpr
kastToGasExpr kast = case node kast of
  "KVariable" -> let Just somevar = originalName kast in
    if "VGas" `isPrefixOf` somevar
    then Right $ Nullary StartGas
    else Left $ "Can't have variables in gas expressions, found: " ++ somevar

  "KToken" -> case sort kast of
    Just "Int" -> Right $ Nullary $ Literal n
      where n = read t
            Just t = token kast
    Just somesort -> Left $ "Can't have sorts other than Int, found: " ++ somesort

  "KApply" -> case stripModuleTag <$> (label kast) of
    Just "_+Int_" -> let Just [arg1, arg2] = args kast in
      case kastToGasExpr arg1 of
        Left error -> Left error
        Right e -> case kastToGasExpr arg2 of
          Left error -> Left error
          Right f -> Right $ Binary Add e f

    Just "_-Int_" -> let Just [arg1, arg2] = args kast in
      case kastToGasExpr arg1 of
        Left error -> Left error
        Right e -> case kastToGasExpr arg2 of
          Left error -> Left error
          Right f -> Right $ Binary Sub e f

    Just "_*Int_" -> let Just [arg1, arg2] = args kast in
      case kastToGasExpr arg1 of
        Left error -> Left error
        Right e -> case kastToGasExpr arg2 of
          Left error -> Left error
          Right f -> Right $ Binary Mul e f

    Just "_/Int_" -> let Just [arg1, arg2] = args kast in
      case kastToGasExpr arg1 of
        Left error -> Left error
        Right e -> case kastToGasExpr arg2 of
          Left error -> Left error
          Right (Nullary (Literal 64)) -> Right $ Unary SixtyFourth e
          Right div -> Left $ "Gas expressions should have /64 only, found: /" ++ (show div)

    Just "#if_#then_#else_#fi" -> let Just [arg_c, arg1, arg2] = args kast in
      case kastToGasExpr arg1 of
        Left error -> Left error
        Right e -> case kastToGasExpr arg2 of
          Left error -> Left error
          Right f -> case formatKast arg_c of
            Left error -> Left error
            Right c -> Right $ ITE (Cond c) e f
    Just somelabel -> Left $ "Unknown KApply in gas expression: " ++ somelabel

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
                   error:_ -> Left error
                   [] -> case formatKApply func fargs of
                     Left error -> Left error
                     Right x    -> Right $ FormulaicString x fargs_types

formatKApply :: String -> [String] -> Either String String
formatKApply func fargs = let bracket s = "( " ++ s ++ " )" in
  case (head func, last func, fargs) of
    -- binary infix
    ('_', '_', (farg1:(farg2:[]))) -> Right $ bracket $ farg1 ++ " " ++ func_trim ++ " " ++ farg2
      where func_trim = tail $ init $ func
    -- unsupported mixfix
    ('_', '_', _) -> Left $ "Couldn't parse mixfix operator: " ++ func
    -- unary prefix
    (_, '_', (farg:[])) -> Right $ bracket $ func_trim ++ farg
      where func_trim = init $ func
    -- unary postfix
    ('_', _, (farg:[])) -> Right $ bracket $ farg ++ func_trim
      where func_trim = tail $ func
    -- n-ary prefix
    (_, _, _) -> Right $ bracket $ func_trim ++ (bracket (intercalate " , " fargs))
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
