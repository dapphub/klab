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
import Data.Semigroup             ((<>))
import Options.Applicative

import qualified Data.ByteString.Lazy.Char8 as C8

import Gas       (coerce,
                  gasDepth,
                  showStratified,
                  stratify,
                  formatStratifiedSyntax)
-- import Kast      (Kast)
import KastParse (kastToGasExpr)
import Solver    (solve,
                  normalise,
                  cosolve,
                  maxLeaf,
                  reduceGas)

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
  noSolveMode         :: Bool,
  stratifyDepth       :: Int,
  reduce              :: String
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
                 <*> option auto
                 (long "stratify-depth"
                 <> help "Minimum nesting depth of subexpression to stratify."
                 <> showDefault
                 <> value 2
                 <> metavar "DEPTH")
                 <*> strOption
                 (long "reduce"
                 <> help "Type of reduction to perform on the solved gas tree."
                 <> showDefault
                 <> value ""
                 <> metavar "REDUCTION")

data StratifiedResult = StratifiedResult
  { constructors   :: [String]
  , stratification :: String
  } deriving (Generic, Show)

data UnstratifiedResult = UnstratifiedResult [String]
  deriving (Generic, Show)

instance ToJSON StratifiedResult
instance ToJSON UnstratifiedResult

exit :: IO()
exit = exitWith ExitSuccess

die :: IO()
die = exitWith (ExitFailure 1)

main :: IO ()
main = do
  -- parse flags and load input
  let opts = info (analyserParser <**> helper)
        (fullDesc
         <> progDesc "Analyses final gas expressions to deduce exact minimum gas requirements."
         <> header "K gas analyser")
  args <- execParser opts
  s    <- inputToString $ gasInput args
      -- extract flags
  let maxG       = sufficientGas args
      tag        = stratificationLabel args
      laxOn      = laxMode args
      cosolveOn  = not $ noCosolveMode args
      stratifyOn = not $ noStratifyMode args
      solveOn    = not $ noSolveMode args
      stratDepth = stratifyDepth args
      -- parse JSON input (array of kasts)
      gaskasts = either
        (\err -> (error $ "Failed in parsing JSON: " ++ err))
        id
        (eitherDecode (fromString s))
      -- parse into BasicGasExpr
      gs = kastToGasExpr <$> gaskasts
      reduceOn = case (reduce args) of
        "" -> False
        _ -> True
      reducer = case (reduce args) of
        "max" -> max
        "min" -> min
        "" -> (+)
      -- solve (or not), etc.
      solved = (case (solveOn, laxOn, cosolveOn, reduceOn) of
        (False, False, _, _)       -> id
        (True,  True,  _, _)       -> coerce . maxLeaf . (solve maxG)
        (True, False, False, _)    -> coerce . (solve maxG)
        (True, False, True, False) -> coerce . cosolve . (solve maxG)
        (True, False, True, True)  -> coerce . (reduceGas reducer) . (solve maxG)
        _ -> error "error: illegal combination of flags.") <$> gs
      -- stratify and merge the stratification maps
      -- min ensures we stratify each expr at least once
      depths = min stratDepth <$> gasDepth <$> solved
      sm = mconcat $ (stratify <$> depths) <*> solved
      -- encode result with JSON
      result = if stratifyOn
               then encode $ StratifiedResult
                    (showStratified (Just (sm, tag)) <$> solved)
                    (formatStratifiedSyntax sm tag)
               else encode $ UnstratifiedResult
                    ((showStratified Nothing) <$> solved)
    in (C8.putStrLn result) >> exit
