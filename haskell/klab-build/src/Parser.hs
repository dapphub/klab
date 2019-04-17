module Parser where

import Data.Char
import Text.Parsec
import Text.ParserCombinators.Parsec

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
    endBy spaces endOfLine

    string "forall" <|> string "types"
    forall <- withSpaces $ sepBy decl spaces

    string "storage"
    storage <- withSpaces $ sepBy mapping spaces

    string "iff"
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
