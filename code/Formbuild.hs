import Tokenizer

import Data.List(intercalate)

-- prototype for the prototype
-- I think I'm losing track of the word's meaning
data Tok = Open | Close | Inner Char deriving(Show)
data Form = Single Tok | More [Form] deriving(Show)
-- data Form = Single Tok | Cons Form Form deriving(Show)
-- once the normal version works well enough we rewrite it in cons
-- and once the cons work, then we write the form executor

str2toks :: String -> [Tok]
str2toks = map toktok

toktok :: Char -> Tok
toktok '(' = Open
toktok ')' = Close
toktok c = Inner c

toks2str :: [Tok] -> String
toks2str = map chacha

chacha :: Tok -> Char
chacha Open = '('
chacha Close = ')'
chacha (Inner x) = x

tree2str :: Form -> String
tree2str (Single (Inner s)) = (s:[])
tree2str (More xs) = "(" ++ (intercalate " " (map tree2str xs)) ++ ")"

-- this one takes the [Tok] to be a whole file and returns a More[xs] where xs is the list of all toplevel forms in the file
tree :: [Tok] -> Form
tree s = fst(treeIter (More []) s)

{- whatever the mechanism we use now must
it be able to comunicate to its next iteration what point of the string are we on
we could return an index, but it's better to return the substring -}

treeIter :: Form -> [Tok] -> (Form, [Tok])
-- adds the FIRST form in the string to the given form

treeIter acc [] = (acc, [])
treeIter acc (Close:xs) = (acc, xs)
treeIter acc (Open:xs) = let (child, rst) = treeIter (More []) xs
                             withChild = formAppend child acc
                         in treeIter withChild rst

treeIter acc (ti@(Inner i):xs) = treeIter (formAppend (Single ti) acc) xs

formAppend :: Form -> Form -> Form
formAppend x (More xs) = More (xs ++ (x:[]))
formAppend _ (Single x) = error "motherfucker appending to a singleton"
