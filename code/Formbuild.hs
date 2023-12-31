module Formbuild where

import Tokenizer(tokens, Token, TokenList)
import Data.List(intercalate)

data Form = Nil | Single Token | Cons Form Form deriving(Show)

data Value =
  IntValue Int
  | StringValue String
  | SymbolValue String
  | ConsValue Value Value
  | NilValue
  deriving Show

{-
list of all the toplevel forms represented by the tokenlist
be it bare values or statements/definitions/whatever made up of conses
-}
forms :: TokenList -> [Value]
forms [] = []
forms ts = let (form, rest) = breakFirstForm ts
           in form:(forms rest)

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


breakFirstForm :: TokenList -> (Value, TokenList)
breakFirstForm [] = (NilValue,[])
breakFirstForm (")":xs) = error
  "you reduced the tokenlist wrong, this was not supposed to happen"
breakFirstForm l@("(":xs) = let (firstToks, restToks) = breakFirstFormTokens l
                            in (singleForm firstToks, restToks)
  where singleForm = consValueUpList
                     . map (fst . breakFirstForm)
                     . separateSubForms

breakFirstForm (normalToken:ts) = (tokenToValue normalToken, ts)

{-
the given list is a one dimensional list of "toplevel subforms", if the term makes sense

the last Nil is going to be part of the input, as it was extracted as a final subform.
this means we sould not consider [] as Nil here, as that would add an extra Nil 
to the output list, we will instead consider [] as an error

this means that, despite this being almost exactly a foldr, we shan't make it one
it could be one but we'd have to purposefully ignore all trailing Nils in
the construction of the forms, or somehow filter them out before giving the form to this
approaches of which neither is much cleaner than this one
-}
consValueUpList :: [Value] -> Value
consValueUpList [] = error "no forms"
consValueUpList [NilValue] = NilValue
consValueUpList (a:b) = ConsValue a (consValueUpList b)

-- we often ignore first paren of tokenlist, and mind the last one
-- this, although mildly badly hacky, works somewhat well
breakFirstFormTokens :: TokenList -> (TokenList, TokenList)
breakFirstFormTokens ("(":xs) = let (firstKinda, rest) = parenBreak xs 1
                                  in (("(":firstKinda), rest)
  where
    parenBreak l@(n:xs) 0 = ([],l)
    parenBreak (")":xs) opens = let (a,b) = parenBreak xs (opens - 1) in (")":a,b)
    parenBreak ("(":xs) opens = let (a,b) = parenBreak xs (opens + 1) in ("(":a,b)
    parenBreak (n:xs) opens = let (a,b) = parenBreak xs opens in (n:a,b)
    parenBreak [] _ = ([],[])

breakFirstFormTokens (")":xs) = error
  "breakFirstFormTokens -- some error in reduction, don't know what though"
breakFirstFormTokens (nonParen:xs) = (nonParen:[],xs)

{-
expects TokenList describing ONE composite form
and returns a list of TokenLists, each describing one subform of original
composite form
-}
separateSubForms :: TokenList -> [TokenList]
separateSubForms ("(":xs) = separateSubForms1 xs
separateSubForms _ = error
  "separateSubForms -- should have recieved tokenlist starting with '('"

separateSubForms1 :: TokenList -> [TokenList]
-- first we handle the parenthesis
-- open '(' case:
separateSubForms1 l@("(":xs) = let (a,b) = breakFirstFormTokens l
                               in a:(separateSubForms1 b)
{-
close ')' case:
having a trailing [")"] is caused by having ignored the first "(",
this is expedted behaviour
-}
separateSubForms1 [")"] = [[]]
-- not having it though, now that might be a problema
separateSubForms1 [] = error
  "spearateSubForms -- either the expression was not properly parenthesized, or this code is stupid, or both"
{-
recieving a tokenlist starting in ")" outiside of the "last ignored one" case
is not expected, in normal conditions we should always be recieving something
with a complete form's tokens as its prefix,
so either a single token, or a parenthesis starting a subform
this can be caused by the input being more forms, or being badly parenthesized
-}
separateSubForms1 (")":_) = error
  "separateSubForms -- expected tokens for ONE form, you might have given it a tokenlist describing more forms, or have messed up the parens"

-- non parenthesis case
separateSubForms1 (nonParen:xs) = (nonParen:[]):(separateSubForms1 xs)

--- printing utilities
-- dotted notation is much easier to print, and still readable
printFormDotted :: Value -> IO ()
printFormDotted NilValue = putStr "nil"
printFormDotted (StringValue s) = putStr $ "Str:<" ++ s ++ ">"
printFormDotted (SymbolValue i) = putStr $ "Sym:<" ++ (show i) ++ ">"
printFormDotted (IntValue i) = putStr $ "Int:<" ++ (show i) ++ ">"
printFormDotted (ConsValue v1 v2) = do putStr "("
                                       printFormDotted v1
                                       putStr "."
                                       printFormDotted v2
                                       putStr ")"


printlnFormDotted :: Value -> IO ()
printlnFormDotted x = do printFormDotted x
                         putStrLn ""
