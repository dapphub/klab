module Main where

import Data.ByteString.Lazy.UTF8 (fromString)
import Data.Aeson                (decode)
import System.Environment        (getArgs)
import System.Exit               (exitWith,
                                  ExitCode(ExitSuccess),
                                  ExitCode(ExitFailure))
import Data.Semigroup            ((<>))
import Options.Applicative

import Solver    (solve)
import Gas       (unparse)
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
  gasInput :: Input,
  sufficientGas :: Int
  }

analyserParser :: Parser AnalyserArgs
analyserParser = AnalyserArgs
                 <$> input
                 <*> option auto
                 ( long "sufficient-gas"
                 <> help "Amount of gas known to be sufficient for all executions"
                 <> showDefault
                 <> value 300000
                 <> metavar "GAS" )

main :: IO ()
main = do
  -- parse flags and load input
  let opts = info (analyserParser <**> helper)
        (fullDesc
         <> progDesc "Analyses final gas expressions to deduce exact minimum gas requirements."
         <> header "K gas analyser")
  args <- execParser opts
  s    <- inputToString $ gasInput args
  let maxG = sufficientGas args
  -- parse JSON as GasExpr
  let Just gaskast = (decode (fromString s)) :: Maybe Kast
      g_parsed     = kastToGasExpr gaskast
  case g_parsed of
    Left err -> (putStrLn $ "Failed in parsing AST: " ++ err) >> die
    -- solve GasExpr, unparse, and print
    Right g -> let solution = unparse $ (solve maxG) $ g
      in (putStrLn solution) >> exit
