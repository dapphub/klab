-- This module is for parsing string-formatted K Gas Expressions
module Parser where

import Text.ParserCombinators.Parsec

import Gas

parseStartGas :: Parser GasExpr
parseStartGas = do
  string "VGas"
  return $ Nullary StartGas

parseInt :: Parser GasExpr
parseInt = do
  s <- many1 digit
  let i = read s
  return $ Nullary (Literal i)

parseNullary :: Parser GasExpr
parseNullary = do
  x <- (parseStartGas <|> parseInt)
  spaces
  return x

parseSixtyFourth :: Parser GasExpr
parseSixtyFourth = do
  x <- (parseNullary <|> parseBracketed)
  char '/'
  spaces
  string "64"
  spaces
  return $ Unary SixtyFourth x

parseUnary :: Parser GasExpr
parseUnary = parseSixtyFourth

parseBinOp :: Parser BinOp
parseBinOp = do
  (char '+' >> return Add)
    <|> (char '-' >> return Sub)

-- default associativity should be
-- x.y.z -> (x.y).z
-- but because we use recursive descent we have
-- x.y.z -> x.(y.z) :(
parseBinary :: Parser GasExpr
parseBinary = do
  x <- (parseNullary <|> parseBracketed)
  op <- parseBinOp
  spaces
  y <- parseGasExpr
  return $ Binary op x y

-- like manyTill but the last parser doesn't consume input
manyTillEnd :: Parser a -> Parser b -> Parser [a]
manyTillEnd p q = do
  ((lookAhead $ try q) >> return [])
    <|> do {x <- p; xs <- manyTillEnd p q; return (x:xs)}

-- this is an arbitrary Bool-valued K term
-- the contents of which we don't care about
-- so we cheat and just count the #ifs
parseCond :: Parser Cond
parseCond = parseCondCounting 0
-- simple naive way which breaks if there are #ifs in the condition:
-- parseCond = do
--   s <- manyTillEnd anyChar (string "#then ")
--   return $ Cond s

-- TODO: find a better way to do this?
parseCondCounting :: Int -> Parser Cond
parseCondCounting depth = do
   s <- manyTillEnd anyChar (try (string "#if ")
                          <|> try (string "#fi ")
                          <|> try (string "#then "))
   try (do t <- string "#if"
           Cond u <- parseCondCounting (depth+1)
           return $ Cond $ s++t++u)
     <|> try (do t <- string "#fi"
                 Cond u <- parseCondCounting (depth-1)
                 return $ Cond $ s++t++u)
     <|> try (if depth == 0
              then return $ Cond s
              else do t <- string "#then"
                      Cond u <- parseCondCounting depth
                      return $ Cond $ s++t++u)

parseITE :: Parser GasExpr
parseITE = do
  string "#if "
  spaces
  c <- parseCond
  string "#then "
  spaces
  x <- parseGasExpr
  string "#else "
  spaces
  y <- parseGasExpr
  string "#fi "
  return $ ITE c x y

parseBracketed :: Parser GasExpr
parseBracketed = do
  char '('
  spaces
  x <- parseGasExpr
  char ')'
  spaces
  return x

-- this must consume all trailing spaces
parseGasExpr :: Parser GasExpr
parseGasExpr = (try parseITE)
  <|> (try parseBinary)
  <|> (try parseUnary)
  <|> (try parseNullary)
  <|> (try parseBracketed)

parseShow :: (Show a) => Parser a -> String -> String
parseShow p s = case parse p "kgas" s of
  Left error -> "Couldn't parse: " ++ show error
  Right result -> show result

parsePrint :: (Show a) => Parser a -> String -> IO ()
parsePrint p s = putStrLn $ parseShow p s

readGasExpr :: String -> Either String GasExpr
readGasExpr s = case parse parseGasExpr "K Gas Expression" s of
  Left error -> Left $ "Parse error: " ++ show error
  Right result -> Right result
 
