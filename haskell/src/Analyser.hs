{-# LANGUAGE DeriveGeneric #-}
module Main where

import GHC.Generics
import Data.ByteString.Lazy.UTF8  (fromString)
import Data.Aeson                 (ToJSON,
                                   encode,
                                   eitherDecode)
import System.Exit                (exitWith,
                                   ExitCode(ExitSuccess),
                                   ExitCode(ExitFailure))
import System.IO                  (stderr, hPutStrLn)
import Data.Semigroup             ((<>))
import Options.Applicative

import qualified Data.ByteString.Lazy.Char8 as C8

import Gas       (coerce,
                  unparse,
                  stratify,
                  formatStratifiedSyntax)
import Kast      (Kast)
import KastParse (kastToGasExpr)
import Solver    (solve,
                  normalise,
                  cosolve,
                  maxLeaf)

-- input argument is either a path or stdin flag
data Input
  = FileInput FilePath
  | StdInput

fileInput :: Parser Input
fileInput = FileInput <$> strOption
  (  long "input"
  <> short 'i'
  <> metavar "FILENAME"
  <> help "Input file" )

stdInput :: Parser Input
stdInput = flag' StdInput
  (  long "stdin"
  <> help "Read from stdin" )

input :: Parser Input
input = fileInput <|> stdInput

inputToString :: Input -> IO String
inputToString (FileInput path) = readFile path
inputToString StdInput = getContents

data AnalyserArgs = AnalyserArgs {
  gasInput            :: Input,
  sufficientGas       :: Int,
  stratificationLabel :: String,
  laxMode             :: Bool,
  noCosolveMode       :: Bool,
  noStratifyMode      :: Bool,
  noSolveMode         :: Bool
  }

analyserParser :: Parser AnalyserArgs
analyserParser = AnalyserArgs
                 <$> input
                 <*> option auto
                 (long "sufficient-gas"
                 <> help "Amount of gas known to be sufficient for all executions"
                 <> showDefault
                 <> value 300000
                 <> metavar "GAS")
                 <*> strOption
                 (long "stratification-label"
                 <> help "Label to use for stratified gas terms in output."
                 <> showDefault
                 <> value "#G"
                 <> metavar "LABEL")
                 <*> switch
                 (long "lax"
                 <> help "Returns maximum leaf in the gas tree")
                 <*> switch
                 (long "no-cosolve"
                 <> help "Disable cosolving (cosolving decreases the maximum nesting depth of the expression).")
                 <*> switch
                 (long "no-stratify"
                 <> help "Disable stratification, output a K expression instead of K syntax declarations.")
                 <*> switch
                 (long "no-solve"
                 <> help "Disable solving and cosolving.")

data StratifiedResult = StratifiedResult
  { constructor    :: String
  , stratification :: String
  } deriving (Generic, Show)

exit :: IO()
exit = exitWith ExitSuccess

die :: IO()
die = exitWith (ExitFailure 1)

instance ToJSON StratifiedResult where

main :: IO ()
main = do
  -- parse flags and load input
  let opts = info (analyserParser <**> helper)
        (fullDesc
         <> progDesc "Analyses final gas expressions to deduce exact minimum gas requirements."
         <> header "K gas analyser")
  args <- execParser opts
  s    <- inputToString $ gasInput args
  let maxG       = sufficientGas args
      stratLabel = stratificationLabel args
      laxOn      = laxMode args
      cosolveOn  = not $ noCosolveMode args
      stratifyOn = not $ noStratifyMode args
      solveOn    = not $ noSolveMode args
  -- parse JSON as GasExpr
  case (eitherDecode (fromString s)) :: Either String Kast of
    Left err -> (hPutStrLn stderr $ "Failed in parsing JSON: " ++ err) >> die
    Right gaskast -> case kastToGasExpr gaskast of
      Left err -> (hPutStrLn stderr $ "Failed in parsing AST: " ++ err) >> die
      -- solve GasExpr, stratify, and print the K syntax declarations
      Right g -> let solved = case (solveOn, laxOn, cosolveOn)  of
                       (False, False, _)    -> normalise g
                       (True,  True, _)     -> coerce $ maxLeaf $ solve maxG g
                       (True, False, False) -> coerce $ solve maxG g
                       (True, False, True)  -> coerce $ cosolve $ solve maxG g
                       _ -> error "error: illegal combination of flags."
                     sm = stratify stratLabel solved
                     sm_result = encode $ StratifiedResult
                       (unparse (Just sm) solved)
                       (formatStratifiedSyntax sm)
                     result = if stratifyOn
                              then sm_result
                              else C8.pack $ unparse Nothing solved
                 in (C8.putStrLn result) >> exit
