module Parser
  ( isNewline
  , isWhiteSpace
  , isDigit
  , isAlpha
  , isIdChar
  , isIdString
  , Parser
  , pLit
  , pNum
  , pSat
  , pVar
  , pAlt
  , pThen
  , pApply
  , pAp
  , pLeft
  , pRight
  , pZeroOrMore
  , pOneOrMore
  , pMunch
  , pMunch1
  , pEmpty
  , pOneOrMoreWithSep
  , pBetween
  ) where

-- 
type Token = (Int, String)
tok2str :: Token -> String
tok2str = snd

clex :: Int -> String -> [Token]
clex n ccs@(c : cs)
  | isNewline c           = clex (n+1) cs
  | isWhiteSpace c        = clex n cs
  | isDigit c             = (n, numToken) : clex n restCs
  | isAlpha c || c == '_' = (n, varToken) : clex n restCs'
  | cs2 == "--"           = clex n (dropWhile (/= '\n') restCs'')
  | cs2 `elem` twoCharOps = (n, cs2) : clex n cs
  | otherwise             = (n, [c]) : clex n cs
      where
        (numToken, restCs)  = span isDigit ccs
        (varToken, restCs') = span isIdChar ccs
        (cs2,restCs'')      = splitAt 2 ccs
clex _ [] = []

twoCharOps :: [String]
twoCharOps = ["&&", "||", "==", "/=", ">=", "<=", "->"]
-- 

isAlpha, isDigit, isIdChar, isWhiteSpace, isNewline :: Char -> Bool
isAlpha = flip elem (['A'..'Z'] ++ ['a'..'z'])
isDigit = flip elem ['0'..'9']
isIdChar c = isAlpha c || isDigit c || c == '_'
isWhiteSpace = flip elem " \t"
isNewline = ('\n' ==)

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
isIdString ccs@(c:cs) = (isAlpha c || c == '_') && all isIdChar cs && notElem ccs keywords

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

infixr 6 `pAlt`

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
pOneOrMore p = pThen (:) p (pZeroOrMore p)

pEmpty :: a -> Parser a
pEmpty x toks = [(x, toks)]

pMunch :: Parser a -> Parser [a]
pMunch p toks = take 1 $ pMunch1 p `pAlt` pEmpty [] $ toks

pMunch1 :: Parser a -> Parser [a]
pMunch1 p = pThen (:) p (pMunch p)

{- |
  原本では，pApply :: Parser a -> (a -> b) -> Parser b
-}
pApply :: (a -> b) -> Parser a -> Parser b
pApply f p toks = [ (f x, toks') | (x, toks') <- p toks ]

pAp :: Parser (a -> b) -> Parser a -> Parser b
pAp pf px toks = [ (f x, toks2) | (f, toks1) <- pf toks
                                , (x, toks2) <- px toks1 ]

pLeft :: Parser a -> Parser b -> Parser a
pLeft p q toks = [ (x, toks2) | (x, toks1) <- p toks
                              , (_, toks2) <- q toks1 ]

pRight :: Parser a -> Parser b -> Parser b
pRight p q toks = [ (y, toks2) | (_, toks1) <- p toks
                               , (y, toks2) <- q toks1 ]

pBetween :: Parser a -> Parser b -> Parser c -> Parser c
pBetween p q r = pLeft (pRight p r) q 

{- |
  one or more with separator
>>> pOneOrMoreWithSep pVar (pLit ",") $ clex 1 "foo,bar,baz"
[(["foo","bar","baz"],[])]
-}
pOneOrMoreWithSep :: Parser a -> Parser b -> Parser [a]
pOneOrMoreWithSep p sep = pThen (:) p (pMunch (pThen (flip const) sep p))

pSat :: (String -> Bool) -> Parser String
pSat p (tok:toks) | p s = [(s, toks)]
  where
    s = tok2str tok
pSat _ _ = []

pNum :: Parser Int
pNum = pApply read (pSat (all isDigit))
