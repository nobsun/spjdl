module Language
  ( Expr (..)
  , Name
  , CoreExpr
  , CoreAlt
  , CoreScDefn
  , CoreProgram
  , isAtomicExpr
  , preludeDefs
  , someExpr
  , binOpExpr
  , binOpExpr1
  , binOps
  , parse
  , syntax
  ) where

import Utils
import Parser

data Expr a 
  = EVar a                   -- ^ Variables
  | ENum Int                 -- ^ Numbers
  | EConstr Int Int          -- ^ Constructor tag arity
  | EAp (Expr a) (Expr a)    -- ^ Applications
  | ELet                     -- ^ Let(rec) expressions
      IsRec                  -- ^    boolean with True = recursive
      [(a, Expr a)]          -- ^    Definitions
      (Expr a)               -- ^    Body of let(rec)
  | ECase                    -- ^ Case expression
      (Expr a)               -- ^   Expression to scrutinise
      [Alter a]              -- ^   Alternatives
  | ELam [a] (Expr a)        -- ^ Lambda expression
  deriving Show
  
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

binOps :: [(Name, Int)]
binOps = [ ("^", 8)
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
preludeDefs
  = [ ("_I", ["x"], EVar "x")
    , ("_K", ["x", "_"], EVar "x")
    , ("_K1", ["_", "y"], EVar "y")
    , ("_S", ["f", "g", "x"], EAp (EAp (EVar "f") (EVar "x"))
                                  (EAp (EVar "g") (EVar "x")))
    , ("_B", ["f", "g", "x"], EAp (EVar "f")
                                  (EAp (EVar "g") (EVar "x")))
    , ("twice", ["f"], EAp (EAp (EVar "_B") (EVar "f")) (EVar "f"))
    ]

-- let expression

someExpr :: CoreExpr
someExpr = ELet nonRecursive
  [("p", ENum 3)
  ,("square", ELam ["x"] (EAp (EAp (EVar "mul") (EVar "x")) (EVar "x")))
  ] (EAp (EAp (EVar "mul") (EVar "p")) (EAp (EVar "square") (ENum 5)))

-- lexer and parser

clex :: Int -> String -> [Token]
clex n ccs@(c : cs)
  | isNewline c    = clex (n+1) cs
  | isWhiteSpace c = clex n cs
  | isDigit c      = (n, numToken) : clex n restCs
  | isAlpha c      = (n, varToken) : clex n restCs'
  | cs2 == "--"    = clex n (dropWhile (/= '\n') restCs'')
  | cs2 `elem` twoCharOps
                   = (n, cs2) : clex n cs
  | otherwise      = (n, [c]) : clex n cs
      where
        (numToken, restCs)  = span isDigit ccs
        (varToken, restCs') = span isIdChar ccs
        (cs2,restCs'')      = splitAt 2 ccs
clex _ [] = []

syntax :: [Token] -> CoreProgram
syntax = undefined

parse :: String -> CoreProgram
parse = syntax . clex 1
