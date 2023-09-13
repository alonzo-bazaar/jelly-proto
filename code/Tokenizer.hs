module Tokenizer where
-- simple lisp tokenizer
-- intermediate representation (lisp form) construction, and interpreter to come
-- prototyping for jelly

import Data.Char (isSpace)
import Data.Function (on)
import Data.List (groupBy, isPrefixOf)

type Token = String
type TokenList = [Token]

data Ast = Single | Tree [Ast]

-- brk :  break, separa il primo token dal resto della stringa
-- ignr : ignore, ignora eventuali caratteri inutili all'inizio del resto
-- parecchio ad hoc
collectBreakIgnore :: ([a] -> ([a],[a])) -> ([a] -> [a]) -> [a] -> [[a]]
collectBreakIgnore _ _ [] = []
collectBreakIgnore brk ignr lst = let (a,b) = (brk lst)
                                  in a : (collectBreakIgnore brk ignr (ignr b))

ignoreWhite :: String -> String 
ignoreWhite = dropWhile isSpace

-- tokenizza una stringa già preprocessata
{-
non è ancora incluso nessun meccanismo di parsing delle stringhe
o di considerazione delle stringhe come unico carattere
quello va visto meglio
-}
tokens :: String -> TokenList
tokens = collectBreakIgnore tokenBreak ignoreWhite . ignoreWhite

specialTokenList = ["(", ")", "#|", "|#", "\"", "`", "\'", ",", ";", ";;", ";;;", "#\\"]

special tok = elem tok specialTokenList

specialPrefix tok = any (\s -> tok `isPrefixOf` s) specialTokenList
startsWithSpecial str = any (\s -> s `isPrefixOf` str) specialTokenList  

-- se if you can get more haskelly about this and fold it like there's no tomorrow
-- until then, this semi imperative prolog looking shit will have to do
tokenBreak :: String -> (String, String)
tokenBreak [] = ("","")
tokenBreak ('"':xs) = let (firstKinda, rest) = quoteBreak xs
                      in ('"':firstKinda, rest)
tokenBreak xs | startsWithSpecial xs = breakOnBiggestSpecial xs
              | otherwise = breakOnNormal xs
  where breakOnNormal xs = tokenBreakIter [] xs

-- ignored first quote, now get to the second quote
-- all these badly specialized breaking functions scream for a fucking hof
quoteBreak ('\\':'"':xs) = let (thing, rest) = quoteBreak xs
                           in ('\\':'"':thing, rest)
quoteBreak ('"':xs) = ("\"",xs)
quoteBreak (x:xs) = let(firstKinda, rest) = quoteBreak xs
                    in (x:firstKinda, rest)

breakOnBiggestSpecial :: String -> (String, String)
breakOnBiggestSpecial xs = helper [] xs
  where -- helper [] (x:xs) = helper (x:[]) xs
        helper acc [] = (acc,[])
        helper acc lst@(x:xs) = let candidate = pushChar x acc
                                in if specialPrefix candidate
                                   then helper candidate xs
                                   else (acc,lst)

tokenBreakIter :: String -> String -> (String, String)
tokenBreakIter acc [] = (acc,"")
tokenBreakIter acc lst = breakingLoop someCond [] (ignoreWhite lst)
  where breakingLoop :: (String -> String -> Bool) -> String -> String -> (String,String)
        breakingLoop _ acc [] = (acc,[])
        breakingLoop someCond acc lst@(x:xs) = if someCond acc lst
                                               then (acc , lst)
                                               else breakingLoop someCond
                                                    (pushChar x acc) xs
        someCond acc lst = any id [(not (null acc)) && startsWithSpecial lst,
                                    isSpace (head lst)] -- other possible conditions tbd

pushChar :: Char -> String -> String
pushChar c s = s ++ (c:[])

formtree :: TokenList -> Ast
formtree = undefined

