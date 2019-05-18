module PPrint where

import Language
import Iseq

pprExpr :: CoreExpr -> Iseq
pprExpr (EVar v) = iStr v
pprExpr (ENum n) = iStr (show n)
pprExpr (EAp e1 e2) = pprExpr e1 `iAppend ` iStr " " `iAppend` pprAExpr e2
pprExpr (ELet isrec defns expr)
  = iConcat [ iStr keyword, iNewline
            , iStr "  ", iIndent (pprDefns defns), iNewline
            , iStr "in ", pprExpr expr
            ]
    where
      keyword | isrec     = "letrec"
              | otherwise = "let"
pprExpr (ECase expr alts)
  = iConcat [ iInterleave (iStr " ") [iStr "case", pprExpr expr, iStr "of"], iNewline
            , iStr "  ", iIndent (pprAlts alts) ]
pprExpr (ELam xs expr)
  = iConcat [iStr "\\ ", iInterleave (iStr "") (map iStr xs), iStr " -> ", pprExpr expr]

pprDefns :: [(Name, CoreExpr)] -> Iseq
pprDefns defns = iInterleave sep (map pprDefn defns)
  where
    sep = iConcat [ iStr ";", iNewline]

pprDefn :: (Name, CoreExpr) -> Iseq
pprDefn (name, expr)
  = iConcat [ iStr name, iStr " = ", iIndent (pprExpr expr)]

pprAlts :: [CoreAlt] -> Iseq
pprAlts alts = iInterleave sep (map pprAlt alts)
  where
    sep = iConcat [iStr ";", iNewline]

pprAlt :: CoreAlt -> Iseq
pprAlt (i, as, expr)
  = iConcat [iStr ("<" ++ show i ++ ">"), iInterleave (iStr " ") (map iStr as), iStr " -> ", pprExpr expr]

pprAExpr :: CoreExpr -> Iseq
pprAExpr expr 
  | isAtomicExpr expr = pprExpr expr
  | otherwise         = iConcat [iStr "(", pprExpr expr, iStr ")"]

pprProgram :: CoreProgram -> Iseq
pprProgram prog
  = iInterleave sep (map pprScDefn prog)
    where
      sep = iConcat [iStr ";", iNewline]

pprScDefn :: CoreScDefn -> Iseq
pprScDefn (f, xs, expr)
  = iConcat [iInterleave (iStr " ") (map iStr (f:xs)), iStr " = ", pprExpr expr]

pprint :: CoreProgram -> String
pprint prog = iDisplay (pprProgram prog)
