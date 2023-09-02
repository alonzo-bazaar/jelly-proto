-- temporary file that will replace Formbuild.hs once I port all the stuff
import Tokenizer(tokens, Token, TokenList)
import Data.List(intercalate)

data Form = Nil | Single Token | Cons Form Form deriving(Show)

printForm :: Form -> IO ()
printForm Nil = putStr "nil"
printForm (Single a) = putStr a
printForm (Cons a b) = do
  putStr "( "
  printForm a
  putStr " "
  printForm b
  putStr " ) "

coolerPrintForm :: Form -> IO ()
coolerPrintForm Nil = putStr ""
coolerPrintForm (Single a) = putStr (a ++ " ")
coolerPrintForm (Cons a b) = case a of
                               Nil -> do putStr "( "
                                         coolerPrintForm b
                                         putStr ") "
                               Single x -> do coolerPrintForm a
                                              coolerPrintForm b
                               Cons x y -> do putStr "( "
                                              coolerPrintForm a
                                              putStr " "
                                              coolerPrintForm b
                                              putStr ") "
                             

form :: TokenList -> Form
form ts = fst (formIter Nil ts)

formIter :: Form -> TokenList -> (Form, TokenList)
formIter acc [] = (acc, [])
formIter acc (")":xs) = (acc, xs)
formIter acc ("(":xs) = let (child, rest) = formIter Nil xs
                            withChild = formAppend child acc
                        in formIter withChild rest

formIter acc (normalToken:xs) = formIter (formAppend (Single normalToken) acc) xs

formAppend :: Form -> Form -> Form
formAppend Nil x = x
formAppend x (Cons Nil Nil) = Cons x Nil
formAppend x (Cons car Nil) = Cons car (Cons x Nil)
formAppend x (Cons car cdr) = Cons car (formAppend x cdr)
-- ?
formAppend x Nil = Cons x Nil
formAppend x (Single _) = error "cannot append to a singleton, it just don't consp"

{-
ok, last one does not the fuck work, time to rethink some apis
a function like this would probably be helpful, more explicit, and shit
making the fucking up of this all just a bit harder
-}
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

listToCons :: [Form] -> Form
listToCons = foldr Cons Nil

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
  "breakFirstFormTokens -- some fuck up in reduction"
breakFirstFormTokens (nonParen:xs) = (nonParen:[],xs)


{-
expects TokenList describing ONE composite form
and returns a list of TokenLists, each describing one subform of original
composite form
-}
-- TBD: would this work with a fold?
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
  "spearateSubForms -- either the expression was not properly parenthesized or this code is stupid, or both"

{-
recieving a tokenlist starting in ")" outiside of the "last ignored one" case
is not expected, in normal conditions we should always be recieving something
with a complete form's tokens as its prefix,
so either a single token, or a parenthesis starting a subform
this can be caused by incorrect input
-}
separateSubForms1 (")":_) = error
  "separateSubForms -- expected tokens for ONE form, you might have given it a tokenlist describing more forms"

-- non parenthesis case
separateSubForms1 (nonParen:xs) = (nonParen:[]):(separateSubForms1 xs)
