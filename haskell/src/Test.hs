module Test where

import Data.ByteString.Lazy.UTF8  (fromString)
import Data.Aeson (eitherDecode)

import Gas
import Kast
import KastParse
import Solver

loadExample :: String -> IO BasicGasExpr
loadExample filename = do
  f <- readFile filename
  let Right gaskast = eitherDecode (fromString f) :: Either String Kast
      g = kastToGasExpr gaskast
  return g

-- easyExample <- loadExample "easy_example.json"
-- crashExample <- loadExample "brokenexamples/gas2.json"
-- crashExample2 <- loadExample "denisgas.raw.json"
-- tinyExample <- loadExample "brokenexamples/tinygas.json"
