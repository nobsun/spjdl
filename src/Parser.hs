module Parser
  (
  ) where

import Language

type Token = (Int, String)

clex :: Int -> String -> [Token]
clex i ccs@(c : cs)
  | isNewline c    = clex (i+1) cs
  | isWhiteSpace c = clex i cs
  | isDigit c      = (i, numToken) : clex i restCs
  | isAlpha c      = (i, varTok) : clex i restCs'
  | c == '-' && head cs == '-'
                   = clex (i+1) (dropWhile (/= '\n') (tail cs))
  | cs2 `elem` twoCharOps
                   = (i, cs2) : clex i cs
  | otherwise      = (i, [c]) : clex i cs
      where
        (numToken, restCs) = span isDigit ccs
        (varTok, restCs')  = span isIdChar ccs
        (cs2,restCs'')     = splitAt 2 ccs
clex _ [] = []

isAlpha, isDigit, isIdChar, isWhiteSpace, isNewline :: Char -> Bool
isAlpha = flip elem (['A'..'Z'] ++ ['a'..'z'])
isDigit = flip elem ['0'..'9']
isIdChar c = isAlpha c || isDigit c || c == '_'
isWhiteSpace = flip elem " \t"
isNewline = ('\n' ==)

twoCharOps :: [String]
twoCharOps = ["==", "/=", ">=", "<=", "->"]

syntax :: [Token] -> CoreProgram
syntax = undefined

parse :: String -> CoreProgram
parse = syntax . clex 1
