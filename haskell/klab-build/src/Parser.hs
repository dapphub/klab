module Main where

import Data.Char
import Text.Parsec
import Text.ParserCombinators.Parsec
import qualified Text.Show.Pretty as Pr
import qualified Options.Applicative as Options
import System.Exit               (exitWith,
                                  ExitCode(ExitSuccess),
                                  ExitCode(ExitFailure))
import System.IO                 (stderr, hPutStrLn)

data Act = Act
  { contract  :: String
  , behaviour :: String
  , interface :: String
  , abiVals   :: [TypedVar]
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
          | Bytes32
  deriving (Show, Enum, Bounded)

type KExp = String

data Mapping = Const StorageKey KExp
             | Rewrite StorageKey KExp KExp
  deriving Show

data StorageKey = StorageNum Int
                | StorageExp String [String] [String]
  deriving Show

withSpaces = between spaceOrTab spaceOrTab

spaceOrTab = many (oneOf " \t")

bracketed = between (char '(') (char ')')
sracketed = between (char '[') (char ']')
indented :: Parser x -> Parser x
indented p = do
    count 4 (char ' ')
    e <- p
    many newline
    return e
unindented = notFollowedBy (string "    ")

blockM s f = do
    string s
    many1 endOfLine
    r <- manyTill (indented f) unindented
    return r

blockBehaviour = do
    string "behaviour"
    behaviour <- withSpaces identifier
    string "of"
    contract <- withSpaces identifier
    many1 endOfLine
    return (behaviour, contract)

blockInterface = do
    string "interface"
    spaces
    interface <- many alphaNum
    abiVals <- bracketed $ sepBy abiVal (char ',')
    many1 endOfLine
    return (interface, abiVals)

pAct :: Parser Act
pAct = do
    (behaviour, contract) <- blockBehaviour
    (interface, abiVals)  <- blockInterface
    forall  <- blockM "for all" declaration
    storage <- blockM "storage" mapping
    iffcond <- blockM "iff"     kexp

    return $ Act
      { contract  = contract
      , behaviour = behaviour
      , interface = interface
      , abiVals   = abiVals
      , forall    = forall
      , storage   = storage
      , iffconds  = iffcond
      , ifconds   = []
      , ranges    = []
      }


kexp :: Parser KExp
kexp = expression

identifier = many1 (alphaNum <|> oneOf "_-")
expression = many1 (alphaNum <|> oneOf "_-+/*=() ")

accessor = do
  char '.'
  id <- identifier
  return id

storageKey :: Parser StorageKey
storageKey =
  StorageNum <$> read <$> many1 digit <|> do
    name <- identifier
    keys <- many $ sracketed identifier
    dots <- many accessor
    return $ StorageExp name keys dots


mapping :: Parser Mapping
mapping = do
    key <- storageKey
    withSpaces $ string "|->"
    datum <- identifier
    choice [ do withSpaces $ string "=>"
                datum' <- kexp
                return $ Rewrite key datum datum'
           , return $ Const key datum ]


abiVal :: Parser TypedVar
abiVal = do
    type' <- withSpaces acttype
    name <- withSpaces identifier
    return (name, type')

acttype :: Parser Type
acttype = choice
    [ (string . map toLower . show) t *> pure t | t <- [minBound .. maxBound] ]


declaration :: Parser TypedVar
declaration = do
    name <- withSpaces identifier
    char ':'
    type' <- withSpaces acttype
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
    Right ast -> putStrLn $ Pr.ppShow ast

test :: IO ()
test = do
  p <- parseFromFile pAct "./Vat_debt.act"
  case p of
    Left e -> putStrLn $ show e
    Right ast -> putStrLn $ Pr.ppShow ast
