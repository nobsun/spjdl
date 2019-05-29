module Language
  ( Expr (..)
  , Name
  , CoreExpr
  , CoreAlt
  , CoreScDefn
  , CoreBinding
  , CoreProgram
  , preludeDefs
  , parse
  , clex
  ) where

import Utils
import Iseq
import Parser

data Expr a 
  = EVar a                   -- ^ Variables
  | ENum Int                 -- ^ Numbers
  | EConstr Int Int          -- ^ Constructor tag arity
  | EAp (Expr a) (Expr a)    -- ^ Applications
  | ELet                     -- ^ Let(rec) expressions
      IsRec                  -- ^    boolean with True = recursive
      [Binding a]            -- ^    Bindings
      (Expr a)               -- ^    Body of let(rec)
  | ECase                    -- ^ Case expression
      (Expr a)               -- ^   Expression to scrutinise
      [Alter a]              -- ^   Alternatives
  | ELam [a] (Expr a)        -- ^ Lambda expression
  deriving Show

type Binding a = (a, Expr a)
type CoreBinding = Binding Name

type CoreExpr = Expr Name
type Name = String
type IsRec = Bool

recursive, nonRecursive :: IsRec
recursive    = True
nonRecursive = False

bindersOf :: [(a, b)] -> [a]
bindersOf defns = [name | (name, _) <- defns]

rhssOf :: [(a, b)] -> [b]
rhssOf defns = [rhs | (_, rhs) <- defns]

type Alter a = (Int, [a], Expr a)
type CoreAlt = Alter Name

isAtomicExpr :: Expr a -> Bool
isAtomicExpr (EVar _) = True
isAtomicExpr (ENum _) = True
isAtomicExpr e        = False

type Program a = [ScDefn a]
type CoreProgram = Program Name

type ScDefn a = (Name, [a], Expr a)
type CoreScDefn = ScDefn Name

twoCharOps :: [String]
twoCharOps = ["&&", "||", "==", "/=", ">=", "<=", "->"]

binOps :: [(Name, Int)]
binOps = [ (".", 9)
         , ("^", 8)
         , ("*", 7)
         , ("/", 7)
         , ("+", 6)
         , ("-", 6)
         , ("==", 4)
         , ("/=", 4)
         , ("<", 4)
         , ("<=", 4)
         , (">", 4)
         , (">=", 4)
         , ("&&", 3)
         , ("||", 2)
         , ("$", 0)
         ]

binOpExpr :: CoreExpr
binOpExpr = EAp (EAp (EVar ">")
                     (EAp (EAp (EVar "+") (EVar "x")) (EVar "y")))
                (EAp (EAp (EVar "*") (EVar "p"))
                     (EAp (EVar "length") (EVar "xs")))

binOpExpr1 :: CoreExpr
binOpExpr1 = EAp (EAp (EVar "*") (EAp (EAp (EVar "+") (ENum 1)) (ENum 2)))
                 (EAp (EAp (EVar "+") (ENum 5)) (ENum 2))
-- Prelude

preludeDefs :: CoreProgram
preludeDefs = parse preludeCodes

preludeCodes :: String
preludeCodes =
  "I x = x;\n\
  \K x y = x;\n\
  \K1 x y = y;\n\
  \S f g x = f x (g x);\n\
  \B f g x = f (g x);\n\
  \C f x y = f y x;\n\
  \twice f = B f f"

-- let expression

someExpr :: CoreExpr
someExpr = ELet nonRecursive
  [("p", ENum 3)
  ,("square", ELam ["x"] (EAp (EAp (EVar "mul") (EVar "x")) (EVar "x")))
  ] (EAp (EAp (EVar "mul") (EVar "p")) (EAp (EVar "square") (ENum 5)))

-- pretty printer

pprExpr :: Int -> CoreExpr -> Iseq
pprExpr _ (EVar v) = iStr v
pprExpr _ (ENum n) = iStr (show n)
pprExpr d (EAp e1 e2) = case e1 of
  EAp (EVar o) e3 -> case lookup o binOps of
    Just d' -> if d <= d' then iseq else iConcat [iStr "(", iseq, iStr ")"]
               where
                 iseq = iConcat [pprExpr d' e3, iStr (" "++o++" "), pprExpr d' e2]
    _       -> iInterleave (iStr " ")  [iStr o, pprExpr 0 e3, pprExpr 0 e2]
  _ -> iConcat [pprExpr 0 e1, iStr " ",  pprExpr 11 e2]
pprExpr _ (ELet isrec defns expr)
  = iConcat [ iStr keyword, iNewline
            , iStr "  ", iIndent (pprDefns defns), iNewline
            , iStr "in ", pprExpr 0 expr
            ]
    where
      keyword | isrec     = "letrec"
              | otherwise = "let"
pprExpr _ (ECase expr alts)
  = iConcat [ iInterleave (iStr " ") [iStr "case", pprExpr 0 expr, iStr "of"], iNewline
            , iStr "  ", iIndent (pprAlts alts) ]
pprExpr _ (ELam xs expr)
  = iConcat [iStr "\\ ", iInterleave (iStr "") (map iStr xs), iStr " -> ", pprExpr 0 expr]

pprDefns :: [(Name, CoreExpr)] -> Iseq
pprDefns defns = iInterleave sep (map pprDefn defns)
  where
    sep = iConcat [ iStr ";", iNewline]

pprDefn :: (Name, CoreExpr) -> Iseq
pprDefn (name, expr)
  = iConcat [ iStr name, iStr " = ", iIndent (pprExpr 0 expr)]

pprAlts :: [CoreAlt] -> Iseq
pprAlts alts = iInterleave sep (map pprAlt alts)
  where
    sep = iConcat [iStr ";", iNewline]

pprAlt :: CoreAlt -> Iseq
pprAlt (i, as, expr)
  = iConcat [iStr ("<" ++ show i ++ ">"), iInterleave (iStr " ") (map iStr as), iStr " -> ", pprExpr 0 expr]

pprProgram :: CoreProgram -> Iseq
pprProgram prog
  = iInterleave sep (map pprScDefn prog)
    where
      sep = iConcat [iStr ";", iNewline]

pprScDefn :: CoreScDefn -> Iseq
pprScDefn (f, xs, expr)
  = iConcat [iInterleave (iStr " ") (map iStr (f:xs)), iStr " = ", pprExpr 0 expr]

pprint :: CoreProgram -> String
pprint prog = iDisplay (pprProgram prog)

-- lexer

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

-- parser

parse :: String -> CoreProgram
parse = syntax . clex 1

syntax :: [Token] -> CoreProgram
syntax = takeFirstParse . pProgram
  where
    takeFirstParse ((prog, []) : others) = prog
    takeFirstParse (_ : others)          = takeFirstParse others
    takeFirstParse []                    = error "Syntax error"

pProgram :: Parser CoreProgram
pProgram = pOneOrMoreWithSep pSc (pLit ";")

pSc :: Parser CoreScDefn
pSc = pApply mkSc pVar `pAp` pZeroOrMore pVar `pAp` pLit "=" `pAp` pExpr

mkSc :: Name -> [Name] -> a -> CoreExpr -> CoreScDefn
mkSc f as _ expr = (f, as, expr)

pExpr :: Parser CoreExpr
pExpr = pELet `pAlt` pELetrec `pAlt` pECase `pAlt` pELam `pAlt` pExpr1

pELet :: Parser CoreExpr
pELet = pRight (pLit "let") (pThen (ELet False) pDefns (pRight (pLit "in") pExpr))

pELetrec :: Parser CoreExpr
pELetrec = pRight (pLit "letrec") (pThen (ELet True) pDefns (pRight (pLit "in") pExpr))

pDefns :: Parser [CoreBinding]
pDefns = pOneOrMoreWithSep  pDefn (pLit ";")

pDefn :: Parser CoreBinding
pDefn = pThen (,) pVar (pRight (pLit "=") pExpr)

pECase :: Parser CoreExpr
pECase = pThen ECase (pRight (pLit "case") pExpr) (pRight (pLit "of") pCoreAlts)

pCoreAlts :: Parser [CoreAlt]
pCoreAlts = pOneOrMoreWithSep pCoreAlt (pLit ";")

pCoreAlt :: Parser CoreAlt
pCoreAlt = pApply (,,) pConstrTag `pAp` pMunch pVar `pAp` (pRight (pLit "->") pExpr)

pConstrTag :: Parser Int
pConstrTag = pRight (pLit "<") (pLeft pNum (pLit ">"))

pELam :: Parser CoreExpr
pELam = pThen ELam (pRight (pLit "\\") (pMunch1 pVar)) (pRight (pLit "->") pExpr)

mkApChain :: [CoreExpr] -> CoreExpr
mkApChain = foldl1 EAp

pAexpr :: Parser CoreExpr
pAexpr = pEVar `pAlt` pENum `pAlt` pEConstr `pAlt` pBetween (pLit "(") (pLit ")") pExpr

pEVar :: Parser CoreExpr
pEVar = pApply EVar pVar

pENum :: Parser CoreExpr
pENum = pApply ENum pNum

pEConstr :: Parser CoreExpr
pEConstr = pApply mkEConstr (pLit "Pack{") `pAp` pNum `pAp` pLit "," `pAp` pNum `pAp` pLit "}"

mkEConstr :: a -> Int -> b -> Int -> c -> CoreExpr
mkEConstr _ m _ n _ = EConstr m n

-- infix operators

data PartialExpr = NoOp
                 | FoundOp Name CoreExpr

assembleOp :: CoreExpr -> PartialExpr -> CoreExpr
assembleOp e1 NoOp = e1
assembleOp e1 (FoundOp op e2) = EAp (EAp (EVar op) e1) e2

pExpr0 :: Parser CoreExpr
pExpr0 = pThen assembleOp pExpr1 pExpr0c

pExpr0c :: Parser PartialExpr
pExpr0c = pThen FoundOp (pLit "$") pExpr0 `pAlt` pEmpty NoOp

pExpr1 :: Parser CoreExpr
pExpr1 = pThen assembleOp pExpr2 pExpr1c

pExpr1c :: Parser PartialExpr
pExpr1c = pEmpty NoOp

pExpr2 :: Parser CoreExpr
pExpr2 = pThen assembleOp pExpr3 pExpr2c

pExpr2c :: Parser PartialExpr
pExpr2c = pThen FoundOp (pLit "||") pExpr2 `pAlt` pEmpty NoOp

pExpr3 :: Parser CoreExpr
pExpr3 = pThen assembleOp pExpr4 pExpr3c

pExpr3c :: Parser PartialExpr
pExpr3c = pThen FoundOp (pLit "&&") pExpr3 `pAlt` pEmpty NoOp

pExpr4 :: Parser CoreExpr
pExpr4 = pThen assembleOp pExpr5 pExpr4c

pExpr4c :: Parser PartialExpr
pExpr4c = pThen FoundOp pRelOp pExpr5 `pAlt` pEmpty NoOp

pRelOp :: Parser Name
pRelOp = foldr1 pAlt $ map pLit ["==", "/=", "<", "<=", ">", ">="]

pExpr5 :: Parser CoreExpr
pExpr5 = pThen assembleOp pExpr6 pExpr5c

pExpr5c :: Parser PartialExpr
pExpr5c = pEmpty NoOp

pExpr6 :: Parser CoreExpr
pExpr6 = pThen assembleOp pExpr7 pExpr6c

pExpr6c :: Parser PartialExpr
pExpr6c = pThen FoundOp (pLit "+") pExpr6
  `pAlt`  pThen FoundOp (pLit "-") pExpr7
  `pAlt`  pEmpty NoOp

pExpr7 :: Parser CoreExpr
pExpr7 = pThen assembleOp pExpr8 pExpr7c

pExpr7c :: Parser PartialExpr
pExpr7c = pThen FoundOp (pLit "*") pExpr7
  `pAlt`  pThen FoundOp (pLit "/") pExpr8
  `pAlt`  pEmpty NoOp

pExpr8 :: Parser CoreExpr
pExpr8 = pThen assembleOp pExpr9 pExpr8c

pExpr8c :: Parser PartialExpr
pExpr8c = pThen FoundOp (pLit "^") pExpr8 `pAlt` pEmpty NoOp

pExpr9 :: Parser CoreExpr
pExpr9 = pThen assembleOp pExpr10 pExpr9c

pExpr9c :: Parser PartialExpr
pExpr9c = pThen FoundOp (pLit ".") pExpr9 `pAlt` pEmpty NoOp

pExpr10 :: Parser CoreExpr
pExpr10 = pApply (foldl1 EAp) (pMunch1 pAexpr)
