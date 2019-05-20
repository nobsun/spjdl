module Parser
  ( Token
  , isNewline
  , isWhiteSpace
  , isDigit
  , isAlpha
  , isIdChar
  , isIdString
  , twoCharOps
  , Parser
  , pLit
  , pNum
  , pSat
  , pVar
  , pAlt
  , pThen
  , pApply
  , pAp
  , pZeroOrMore
  , pOneOrMore
  , pEmpty
  , pMunch
  , pMunch1
  ) where

type Token = (Int, String)

isAlpha, isDigit, isIdChar, isWhiteSpace, isNewline :: Char -> Bool
isAlpha = flip elem (['A'..'Z'] ++ ['a'..'z'])
isDigit = flip elem ['0'..'9']
isIdChar c = isAlpha c || isDigit c || c == '_'
isWhiteSpace = flip elem " \t"
isNewline = ('\n' ==)

twoCharOps :: [String]
twoCharOps = ["==", "/=", ">=", "<=", "->"]

type Parser a = [Token] -> [(a, [Token])]

{- |
  Literal parser
>>> pLit "Hello" $ clex 1 "Hello John!" 
[("Hello",[(1,"John"),(1,"!")])]
-}
pLit :: String -> Parser String
pLit s = pSat (s ==)

pVar :: Parser String
pVar = pSat isIdString

isIdString :: String -> Bool
isIdString ccs@(c:cs) = isAlpha c && all isIdChar cs && notElem ccs keywords

keywords :: [String]
keywords = ["let", "letrec", "in", "case", "of", "Pack"] 

{- $setup
>>> let pHelloOrGoodbye = pLit "Hello" `pAlt` pLit "Goodbye" :: Parser String
-}
{- |
  Alternative parser combinator
>>> pHelloOrGoodbye $ clex 1 "Goodbye"
[("Goodbye",[])]
-}
pAlt :: Parser a -> Parser a -> Parser a
pAlt p1 p2 toks = p1 toks ++ p2 toks

{- |
  Combine parsers
>>> pGreeting = pThen (,) pHelloOrGoodbye pVar :: Parser (String, String)
>>> pGreeting $ clex 1 "Goodbye James!"
[(("Goodbye","James"),[(1,"!")])]
-}
pThen :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
pThen combine p1 p2 toks
  = [ (combine v1 v2, toks2) | (v1, toks1) <- p1 toks
                             , (v2, toks2) <- p2 toks1 ]

{- |
  repetition
-}
pZeroOrMore :: Parser a -> Parser [a]
pZeroOrMore p = pOneOrMore p `pAlt` pEmpty []

{- |
  one or more
>>> pOneOrMore pHelloOrGoodbye $ clex 1 "Hello Goodbye!"
[(["Hello","Goodbye"],[(1,"!")]),(["Hello"],[(1,"Goodbye"),(1,"!")])]
-}
pOneOrMore :: Parser a -> Parser [a]
pOneOrMore p toks = pThen (:) p (pZeroOrMore p) toks

pEmpty :: a -> Parser a
pEmpty x toks = [(x, toks)]

{- |
  原本では，pApply :: Parser a -> (a -> b) -> Parser b
-}
pApply :: (a -> b) -> Parser a -> Parser b
pApply f p toks = [ (f x, toks') | (x, toks') <- p toks ]

pAp :: Parser (a -> b) -> Parser a -> Parser b
pAp pf px toks = [ (f x, toks2) | (f, toks1) <- pf toks
                                , (x, toks2) <- px toks1 ]

{- |
  原文は pOneOrMoreWithSep :: Parser a -> Parser b -> Parser [a]
  のように区切り子パーザが第2引数
-}
pOneOrMoreWithSep :: Parser a -> Parser b -> Parser [b]
pOneOrMoreWithSep sep p toks = case p toks of
  [] -> []
  rs -> [ (hd:tl, toks2)
        | (hd, toks1) <- rs
        , (tl, toks2) <- pZeroOrMore (pThen (flip const) sep p) toks1
        ]

pSat :: (String -> Bool) -> Parser String
pSat p (tok:toks) | p s = [(s, toks)]
  where
    s = snd tok
pSat _ _ = []

pNum :: Parser Int
pNum = pApply read (pSat (all isDigit))

pMunch :: Parser a -> Parser [a]
pMunch p = pMunch1 p `pAlt` pEmpty []

pMunch1 :: Parser a -> Parser [a]
pMunch1 p = take 1 . pThen (:) p (pMunch p)
