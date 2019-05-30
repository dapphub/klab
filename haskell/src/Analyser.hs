module Main where

import Data.ByteString.Lazy.UTF8 (fromString)
import Data.Aeson                (eitherDecode)
import System.Environment        (getArgs)
import System.Exit               (exitWith,
                                  ExitCode(ExitSuccess),
                                  ExitCode(ExitFailure))
import System.IO                 (stderr, hPutStrLn)
import Data.Semigroup            ((<>))
import Options.Applicative

import Solver    (solve,
                  cosolve,
                  maxLeaf)
import Gas       (unparse,
                  stratify,
                  formatStratifiedSyntax)
import Parser    (readGasExpr)
import KastParse (Kast, kastToGasExpr)

exit    = exitWith ExitSuccess
die     = exitWith (ExitFailure 1)

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
  noStratifyMode      :: Bool
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
  -- parse JSON as GasExpr
  case (eitherDecode (fromString s)) :: Either String Kast of
    Left err -> (hPutStrLn stderr $ "Failed in parsing JSON: " ++ err) >> die
    Right gaskast -> case kastToGasExpr gaskast of
      Left err -> (hPutStrLn stderr $ "Failed in parsing AST: " ++ err) >> die
      -- solve GasExpr, stratify, and print the K syntax declarations
      Right g -> let solved = case (laxOn, cosolveOn) of
                       (True, _)      -> maxLeaf $ solve maxG g
                       (False, False) -> solve maxG g
                       (False, True)  -> cosolve $ solve maxG g
                     sm = stratify stratLabel solved
                     syntax = formatStratifiedSyntax sm
                     result = if stratifyOn then syntax else unparse Nothing solved
        in (putStrLn result) >> exit
