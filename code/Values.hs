module Values where

import Formbuild

data Value =
  IntValue Int
  | StringValue String
  | SymbolValue String
  | ConsValue Value Value
  | NilValue
  deriving Show

-- del singolo token, niente cons
tokenToValue "nil" = NilValue

tokenToValue s | allNumeric s = IntValue (read s :: Int)
               | startsWith '"' s = StringValue $ removeExtremities s
               | otherwise = SymbolValue s

  where
    allNumeric = all (`elem` ['0'..'9'])
    startsWith a [] = False
    startsWith a (b:_) = (a == b)
    removeExtremities = init . tail

       



           
