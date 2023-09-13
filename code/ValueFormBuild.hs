module ValueFormBuild where

-- temporary file that will replace Formbuild.hs once I port all the stuff
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

-- dotted notation is much easier to print, and still readable
printFormDotted :: Form -> IO ()
printFormDotted Nil = putStr "nil"
printFormDotted (Single token) = putStr token
printFormDotted (Cons f1 f2) = do putStr "("
                                  printFormDotted f1
                                  putStr "."
                                  printFormDotted f2
                                  putStr ")"

tokenForms :: TokenList -> [Form]
tokenForms [] = []
tokenForms ts = let (form, rest) = breakFirstForm ts
                in form:(tokenForms rest)

breakFirstForm :: TokenList -> (Form, TokenList)
breakFirstForm [] = (Nil,[])
breakFirstForm (")":xs) = error
  "you reduced the tokenlist wrong, this was not supposed to happen"
breakFirstForm l@("(":xs) = let (firstToks, restToks) = breakFirstFormTokens l
                       in (singleForm firstToks, restToks)
                          where singleForm = listToCons
                                  . map (fst . breakFirstForm)
                                  . separateSubForms
breakFirstForm (normal:xs) = (Single normal, xs)

{-
the last Nil is going to be part of the input, as it was extracted as a final subform.
this means we sould not consider [] as Nil here, as that would add an extra Nil 
to the output list, we will instead consider [] as an error

this means that, despite this being almost exactly a foldr, we shan't make it one
it could be one but we'd have to purposefully ignore all trailing Nils in
the construction of the forms, or somehow filter them out before giving the form to this
approaches of which neither is much cleaner than this one
-}
listToCons :: [Form] -> Form
listToCons [] = error "no forms"
listToCons [Nil] = Nil -- the last nil of a sublist, this is just one nil, not two
listToCons (a:b) = Cons a (listToCons b)

  
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
