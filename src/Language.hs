module Language
  ( Expr (..)
  , Name
  , CoreExpr
  , CoreAlt
  , CoreScDefn
  , CoreProgram
  , isAtomicExpr
  , preludeDefs
  ) where

import Utils

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
  deriving (Eq, Show)

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

-- Pretty Printer

{-
pprExpr :: CoreExpr -> String
pprExpr (ENum n ) = show n
pprExpr (Evar v)  = v
pprExpr (EAp e1 e2) = pprExpr e1 ++ " " ++ pprAExpr e2

pprAExpr :: CoreExpr -> String
pprAExpr e 
  | isAtomicExpr e = pprExpr e
  | otherwise      = "(" ++ pprExpr e ++ ")"
-}

