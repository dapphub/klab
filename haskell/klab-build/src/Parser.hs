module Main where

import Data.Char
import Text.Parsec
import Text.ParserCombinators.Parsec
import qualified Options.Applicative as Options
import System.Exit               (exitWith,
                                  ExitCode(ExitSuccess),
                                  ExitCode(ExitFailure))
import System.IO                 (stderr, hPutStrLn)

data Act = Act
  { contract  :: String
  , behaviour :: String
  , interface :: String
  , inputs    :: [TypedVar]
  , forall    :: [TypedVar]
  , storage   :: [Mapping]
  , iffconds  :: [KExp]
  , ifconds   :: [KExp]
  , ranges    :: [(Type, [KExp])]
  }
  deriving Show

type TypedVar = (String, Type)

data Type = Address
          | Int256
          | UInt256
  deriving (Show, Enum, Bounded)

type KExp = String

data Mapping = Const KExp KExp
             | Rewrite KExp KExp KExp
  deriving Show

withSpaces = between spaceOrTab spaceOrTab

spaceOrTab = many (oneOf " \t")

bracketed = between (char '(') (char ')')

pAct :: Parser Act
pAct = do
    string "behaviour"
    behaviour <- withSpaces $ many1 alphaNum
    string "of"
    contract <- withSpaces $ many1 alphaNum
    many1 endOfLine

    string "interface"
    spaces
    interface <- many alphaNum
    inputs <- bracketed $ sepBy input (char ',')
    many1 endOfLine

    string "forall" <|> string "types"
    many1 endOfLine
    forall <- withSpaces $ sepBy decl spaces

    string "storage"
    many1 endOfLine
    storage <- withSpaces $ sepBy mapping spaces

    string "iff"
    many1 endOfLine
    iffcond <- withSpaces $ sepBy kexp spaces

    return $ Act
      { contract = contract
      , behaviour = behaviour
      , interface = interface
      , inputs    = inputs
      , forall    = forall
      , storage   = storage
      , iffconds  = undefined
      , ifconds   = undefined
      , ranges    = undefined
      }


kexp = undefined

mapping :: Parser Mapping
mapping = do
    address <- kexp
    withSpaces $ string "|->"
    datum <- kexp
    choice [ do withSpaces $ string "=>"
                datum' <- kexp
                return $ Rewrite address datum datum'
           , return $ Const address datum ]


input :: Parser TypedVar
input = do
    type' <- withSpaces acttype
    name <- withSpaces $ many1 alphaNum -- should also allow underscores etc
    return (name, type')

acttype :: Parser Type
acttype = choice
    [ (string . map toLower . show) t *> pure t | t <- [minBound .. maxBound] ]


decl :: Parser TypedVar
decl = do
    name <- withSpaces $ many1 alphaNum
    char ':'
    type' <- withSpaces $ acttype
    return (name, type')

exit    = exitWith ExitSuccess
die     = exitWith (ExitFailure 1)
-- input argument is either a path or stdin flag
data Input
  = FileInput FilePath
  | StdInput

fileInput :: Options.Parser Input
fileInput = FileInput <$> Options.strOption
  (  Options.long "input"
  <> Options.short 'i'
  <> Options.metavar "FILENAME"
  <> Options.help "Input file" )

stdInput :: Options.Parser Input
stdInput = Options.flag' StdInput
  (  Options.long "stdin"
  <> Options.help "Read from stdin" )

cliInput :: Options.Parser Input
cliInput = fileInput Options.<|> stdInput

inputToString :: Input -> IO String
inputToString (FileInput path) = readFile path
inputToString StdInput = getContents

data ActArgs = ActArgs {
    act :: Input
  }

actParser :: Options.Parser ActArgs
actParser = ActArgs <$> cliInput

main :: IO ()
main = do
  let opts = Options.info (actParser Options.<**> Options.helper)
        (Options.fullDesc
         <> Options.progDesc ""
         <> Options.header "")
  args <- Options.execParser opts
  s    <- inputToString $ act args
  case parse pAct "" s of
    Left e -> putStrLn $ show e
    Right ast -> putStrLn $ show ast
