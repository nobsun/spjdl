module Template5 where

import Utils
import Heap
import Iseq
import Language

-- Mark 5 : Structured data

--- Structure of the implementation

runProg :: String -> String
runProg = showResults . eval . compile . parse

type TiState = (TiStack, TiDump, TiHeap, TiGlobals, TiStats)

type TiStack = [Addr]

type TiDump = [TiStack]

initialTiDump :: TiDump
initialTiDump = []

type TiHeap = Heap Node
data Node = NAp Addr Addr                   -- ^ Application
          | NSupercomb Name [Name] CoreExpr -- ^ Supercombinator
          | NNum Int                        -- ^ Number
          | NInd Addr                       -- ^ Indirection
          | NPrim Name Primitive            -- ^ Primitive
          | NData Int [Addr]                -- ^ Tag, list of components
          deriving Show

data Primitive = Negate | Add | Subtract | Multiply | Divide
               | Greater  | GreaterEq  | Less  | LessEq
               | Eq | NotEq
               | PrimConstr Int Int         -- ^ Tag and arity
               | If
               | PrimCasePair
               | PrimCaseList
               | Abort
  deriving Show

type TiGlobals = Assoc Name Addr

type TiStats
  = ( Int  -- total reductions
    , Int  -- supercombinator reductions
    , Int  -- primitive reductions
    )

tiStatInitial :: TiStats
tiStatInitial  = (0, 0, 0)

tiStatIncTotalSteps :: TiStats -> TiStats
tiStatIncTotalSteps stats = case stats of
  (total, sc, pm) -> (total + 1 , sc, pm)

tiStatIncScSteps :: TiStats -> TiStats
tiStatIncScSteps stats = case stats of
  (total, sc, pm) -> (total, sc + 1, pm)

tiStatIncPmSteps :: TiStats -> TiStats
tiStatIncPmSteps stats = case stats of
  (total, sc, pm) -> (total, sc, pm + 1)

tiStatGetTotalSteps :: TiStats -> Int
tiStatGetTotalSteps stats = case stats of
  (total, _, _) -> total

tiStatGetScSteps :: TiStats -> Int
tiStatGetScSteps stats = case stats of
  (_, sc, _) -> sc

tiStatGetPmSteps :: TiStats -> Int
tiStatGetPmSteps stats = case stats of
  (_, _, pc) -> pc

applyToStats :: (TiStats -> TiStats) -> TiState -> TiState
applyToStats f state = case state of
  (stack, dump, heap, scDefs, stats) -> (stack, dump, heap, scDefs, f stats)

-- compiler

compile :: CoreProgram -> TiState
compile prog
  = (initialStack, initialTiDump, initialHeap, globals, tiStatInitial)
    where
      scDefs = prog ++ preludeDefs ++ extraPreludeDefs
      (initialHeap, globals) = buildInitialHeap scDefs
      initialStack = [addressOfMain]
      addressOfMain = aLookup globals "main" (error "main is not defined")

extraPreludeDefs :: CoreProgram
extraPreludeDefs
  = [ ("False"  , [],       EConstr 1 0)
    , ("True"   , [],       EConstr 2 0)
    , ("not"    , ["x"],    EAp (EAp (EAp (EVar "if") (EVar "x"))
                                     (EVar "False"))
                                (EVar "True"))
    , ("and"   , ["x","y"], EAp (EAp (EAp (EVar "if") (EVar "x"))
                                     (EVar "y"))
                                (EVar "False"))
    , ("or"    , ["x","y"], EAp (EAp (EAp (EVar "if") (EVar "x"))
                                     (EVar "y"))
                                (EVar "True"))
    , ("xor"   , ["x","y"], EAp (EAp (EAp (EVar "if") (EVar "x"))
                                     (EAp (EVar "not") (EVar "y")))
                                (EVar "y"))
    , ("MkPair", []       , EConstr 1 2)
    , ("fst"   , ["p"]    , EAp (EAp (EVar "casePair") (EVar "p"))
                                (EVar "K"))
    , ("snd"   , ["p"]    , EAp (EAp (EVar "casePair") (EVar "p"))
                                (EVar "K1"))
    , ("LCons" , []       , EConstr 2 2)
    , ("Nil",    []       , EConstr 1 0)
    , ("head",   ["xs"]   , EAp (EAp (EAp (EVar "caseList") (EVar "xs"))
                                     (EVar "abort"))
                                (EVar "K"))
    , ("tail",   ["xs"]   , EAp (EAp (EAp (EVar "caseList") (EVar "xs"))
                                     (EVar "abort"))
                                (EVar "K1"))
    ]

buildInitialHeap :: [CoreScDefn] -> (TiHeap, TiGlobals)
buildInitialHeap scDefs = (heap2, scAddrs ++ primAddrs)
  where
    (heap1, scAddrs)   = mapAccuml allocateSc hInitial scDefs
    (heap2, primAddrs) = mapAccuml allocatePrim heap1 primitives

primitives :: Assoc Name Primitive
primitives = [ ("negate", Negate)
             , ("+", Add),      ("-", Subtract)
             , ("*", Multiply), ("/", Divide)
             , (">", Greater),  (">=", GreaterEq)
             , ("<", Less),     ("<=", LessEq)
             , ("==", Eq),      ("/=", NotEq)
             , ("if", If)
             , ("casePair", PrimCasePair), ("caseList", PrimCaseList)
             , ("abort", Abort)
             ]

allocateSc :: TiHeap -> CoreScDefn -> (TiHeap, (Name, Addr))
allocateSc heap scDefn = case scDefn of
  (name, args, body) -> (heap', (name, addr))
    where
      (heap', addr) = hAlloc heap (NSupercomb name args body)

allocatePrim :: TiHeap -> (Name, Primitive) -> (TiHeap, (Name, Addr))
allocatePrim heap (name, prim)
  = (heap', (name, addr))
    where
      (heap', addr) = hAlloc heap (NPrim name prim)
      
-- evaluator

eval :: TiState -> [TiState]
eval state = state : restStates
  where
    restStates
      | tiFinal state = []
      | otherwise     = eval nextState
    nextState = doAdmin (step state)

doAdmin :: TiState -> TiState
doAdmin state = case state of
  (a:stack, _, heap, _, _) -> case hLookup heap a of
    NSupercomb _ _ _ -> doAdminTotal $ doAdminSc state
    _                -> doAdminTotal $ state

doAdminTotal :: TiState -> TiState
doAdminTotal state = applyToStats tiStatIncTotalSteps state

doAdminSc :: TiState -> TiState
doAdminSc state = applyToStats tiStatIncScSteps state

doAdminPm :: TiState -> TiState
doAdminPm state = applyToStats tiStatIncPmSteps state

tiFinal :: TiState -> Bool
tiFinal state = case state of
  ([soleAddr], [], heap, _, _) -> isDataNode (hLookup heap soleAddr)
  ([], _, _, _, _)             -> error "Empty stack!"
  _                            -> False

isDataNode :: Node -> Bool
isDataNode node = case node of
  NNum _    -> True
  NData _ _ -> True
  _         -> False

step :: TiState -> TiState
step state = case state of
  (stack, dump, heap, globals, stats) -> dispatch (hLookup heap (head stack))
  where
    dispatch (NNum n)                  = numStep  state n
    dispatch (NInd a)                  = indStep  state a
    dispatch (NAp a1 a2)               = apStep   state a1 a2
    dispatch (NSupercomb sc args body) = scStep   state sc args body
    dispatch (NPrim name prim)         = primStep state prim
    dispatch (NData tag compts)        = dataStep state tag compts

numStep :: TiState -> Int -> TiState
numStep state n = case state of
  (_, stack' : dump, heap, globals, stats) -> (stack', dump, heap, globals, stats)

indStep :: TiState -> Addr -> TiState
indStep state a = case state of
  (stack, dump, heap, globals, stats) -> (a : tail stack, dump, heap, globals, stats)

apStep :: TiState -> Addr -> Addr -> TiState
apStep state a1 a2 = case state of
  (stack, dump, heap, globals, stats) -> apDispatch (hLookup heap a2)
    where
      apDispatch node = case node of
        NInd a3 -> (stack, dump, heap', globals, stats)
          where
            heap' = hUpdate heap apNode (NAp a1 a3)
            apNode = head stack
        _       -> (a1 : stack, dump, heap, globals, stats)
          

scStep :: TiState -> Name -> [Name] -> CoreExpr -> TiState
scStep state scName argNames body = case state of
  (stack, dump, heap, globals, stats)
    -> if length stack < length argNames + 1 then error "Too few argments given"
       else (stack', dump, heap', globals, stats)
    where
      stack' = drop (length argNames) stack
      root = head stack'
      heap' = instantiateAndUpdate body root heap (bindings ++ globals)
      bindings = zip argNames (getargs heap stack)

getargs :: TiHeap -> TiStack -> [Addr]
getargs heap stack = case stack of
  sc:stack' -> map getarg stack'
    where
      getarg addr = arg
        where
          NAp fun arg = hLookup heap addr

primStep :: TiState -> Primitive -> TiState
primStep state prim = case prim of
  Negate   -> primNegate state
  Add      -> primArith state (+)
  Subtract -> primArith state (-)
  Multiply -> primArith state (*)
  Divide   -> primArith state div
  Greater   -> primComp state (>)
  GreaterEq -> primComp state (>=)
  Less      -> primComp state (<)
  LessEq    -> primComp state (<=)
  Eq        -> primComp state (==)
  NotEq     -> primComp state (/=)
  PrimConstr tag arity -> primConstr state tag arity
  If           -> primIf state
  PrimCasePair -> primCasePair state
  PrimCaseList -> primCaseList state
  Abort -> error "Program abort!"
  
primNegate :: TiState -> TiState
primNegate state = case state of
  (stack, dump, heap, globals, stats)
    | length args /= 1         -> error "primNegate: wrong number of args"
    | not (isDataNode argNode) -> ([argAddr], stack':dump, heap,  globals, stats)
    | otherwise                -> (stack',           dump, heap', globals, stats)
    where
      args      = getargs heap stack
      [argAddr] = args
      argNode   = hLookup heap argAddr
      NNum v    = argNode
      stack'    = drop 1 stack
      root      = head stack'
      heap'     = hUpdate heap root (NNum (negate v))

primArith :: TiState -> (Int -> Int -> Int) -> TiState
primArith state op = primDyadic state op'
  where
    op' (NNum m) (NNum n) = NNum (m `op` n)

primComp :: TiState -> (Int -> Int -> Bool) -> TiState
primComp state op = primDyadic state op'
  where
    op' (NNum m) (NNum n)
      | m `op` n  = NData 2 []
      | otherwise = NData 1 []

primDyadic :: TiState -> (Node -> Node -> Node) -> TiState
primDyadic state op = case state of
  (stack, dump, heap, globals, stats)
    | length args /= 2          -> error "primArith: wrong number of args"
    | not (isDataNode arg1node) -> ([arg1addr], stack' : dump, heap, globals, stats)
    | not (isDataNode arg2node) -> ([arg2addr], stack' : dump, heap, globals, stats)
    | otherwise                 -> (stack', dump, heap', globals, stats)
    where
      args = getargs heap stack
      [arg1addr, arg2addr] = args
      arg1node = hLookup heap arg1addr
      arg2node = hLookup heap arg2addr
      stack'   = drop 2 stack
      root     = head stack'
      heap'    = hUpdate heap root (arg1node `op` arg2node)

primIf :: TiState -> TiState
primIf state = case state of
  (stack, dump, heap, globals, stats)
    | length args < 3         -> error "primIf: wrong number of args"
    | not (isDataNode a1node) -> ([a1], stack':dump, heap, globals, stats)
    | otherwise               -> (stack', dump, heap', globals, stats)
    where
      args = getargs heap stack
      (a1:a2:a3:as) = args
      a1node = hLookup heap a1
      stack' = drop 3 stack
      root = head stack'
      NData tag [] = a1node
      resAddr | tag == 2  = a2
              | otherwise = a3
      heap' = hUpdate heap root (NInd resAddr)

primCasePair :: TiState -> TiState
primCasePair state = case state of
  (stack, dump, heap, globals, stats)
    | length args < 2         -> error "primCasePair: wrong number of args"
    | not (isDataNode a1node) -> ([a1], stack':dump, heap, globals, stats)
    | otherwise               -> (stack', dump, heap', globals, stats)
    where
      args = getargs heap stack
      (a1:a2:as) = args
      a1node = hLookup heap a1
      stack' = drop 2 stack
      root   = head stack'
      NData tag [p1,p2] = a1node
      heap' = hUpdate heap1 root (NAp tmpAddr p2)
        where
          (heap1, tmpAddr) = hAlloc heap (NAp a2 p1)

primCaseList :: TiState -> TiState
primCaseList state = case state of
  (stack, dump, heap, globals, stats)
    | length args < 3         -> error "primCaseList: wrong number of args"
    | not (isDataNode a1node) -> ([a1], stack':dump, heap, globals, stats)
    | otherwise               -> (stack', dump, heap', globals, stats)
    where
      args = getargs heap stack
      (a1:a2:a3:as) = args
      a1node = hLookup heap a1
      stack' = drop 3 stack
      root = head stack'
      NData tag compts = a1node
      [hd,tl] = compts
      heap' | tag == 1  = hUpdate heap root (NInd a2)
            | otherwise = hUpdate heap1 root (NAp tmpAddr tl)
            where
              (heap1, tmpAddr) = hAlloc heap (NAp a3 hd)

primConstr :: TiState -> Int -> Int -> TiState
primConstr state tag arity = case state of
  (stack, dump, heap, globals, stats)
    | length args < arity -> error "primConstr: wrong number of args"
    | otherwise           -> (stack', dump, heap', globals, stats)
    where
      args = getargs heap stack
      stack' = drop arity stack
      root = head stack'
      heap' = hUpdate heap root (NData tag args)

dataStep :: TiState -> Int -> [Addr] -> TiState
dataStep state tag compts = case state of
  (_, stack:dump, heap, globals, stats)
    -> (stack, dump, heap, globals, stats)

instantiateAndUpdate :: CoreExpr
                     -> Addr
                     -> TiHeap
                     -> Assoc Name Addr
                     -> TiHeap
instantiateAndUpdate expr updAddr heap env = case expr of
  ENum n    -> hUpdate heap updAddr (NNum n)
  EVar v    -> hUpdate heap updAddr (NInd a)
               where
                 a = aLookup env v (error ("Undefined name " ++ show v))
  EAp e1 e2 -> hUpdate heap2 updAddr (NAp a1 a2)
               where
                 (heap1, a1) = instantiate e1 heap  env
                 (heap2, a2) = instantiate e2 heap1 env
  ELet isrec defs body
            -> instantiateAndUpdate body updAddr heap1 env'
               where
                 (heap1, extraBindings) = mapAccuml instantiateRhs heap defs
                 env'   = extraBindings ++ env
                 rhsEnv = if isrec then env' else env
                 instantiateRhs heap (name, rhs)
                   = (heap', (name, addr))
                     where
                       (heap', addr) = instantiate rhs heap rhsEnv
  EConstr tag arity
            -> instantiateAndUpdateConstr tag arity updAddr heap env


instantiateAndUpdateConstr :: Int -> Int -> Addr -> TiHeap -> Assoc Name Addr -> TiHeap
instantiateAndUpdateConstr tag arity updAddr heap env
  = hUpdate heap updAddr (NPrim "Cons" (PrimConstr tag arity))

instantiate :: CoreExpr         -- Body of suprercombinator
            -> TiHeap           -- Heap before instatiation
            -> Assoc Name Addr  -- Association of names to address
            -> (TiHeap, Addr)   -- Heap after instatiation, and address of root of instance
instantiate expr heap env = case expr of
  ENum n               -> hAlloc heap  (NNum n)
  EAp e1 e2            -> hAlloc heap2 (NAp a1 a2)
    where
      (heap1, a1) = instantiate e1 heap  env
      (heap2, a2) = instantiate e2 heap1 env 
  EVar v               -> (heap, aLookup env v (error ("Undefined name " ++ show v)))
  EConstr tag arity    -> instantiateConstr tag arity heap env
  ELet isrec defs body -> instantiateLet isrec defs body heap env
  ECase e alts         -> error "Can't instantiate case exprs"

instantiateConstr :: Int -> Int -> TiHeap -> Assoc Name Addr -> (TiHeap, Addr)
instantiateConstr tag arity heap env
  = hAlloc heap (NPrim "Cons" (PrimConstr tag arity))

instantiateLet :: IsRec -> [CoreBinding] -> CoreExpr -> TiHeap -> Assoc Name Addr -> (TiHeap, Addr)
instantiateLet isrec defs body heap env
  = instantiate body heap' env'
    where
      (heap', extraBindings) = mapAccuml instantiateRhs heap defs
      env' = extraBindings ++ env
      rhsEnv = if isrec then env' else env
      instantiateRhs heap (name, rhs)
        = (heap', (name, addr))
          where
            (heap', addr) = instantiate rhs heap rhsEnv

-- Formatting the results

showResults :: [TiState] -> String
showResults states
  = iDisplay (iConcat [ iLayn (map showState states)
                      , showStats (last states)
                      ])

showState :: TiState -> Iseq
showState (stack, dump, heap, globals, stats)
  = iConcat [ showStack heap stack, iNewline
            , showDump dump, iNewline
            , showHeap heap, iNewline
            ]

showStack :: TiHeap -> TiStack -> Iseq
showStack heap stack
  = iConcat
    [ iStr "Stack ["
    , iIndent (iInterleave iNewline (map showStackItem stack))
    , iStr " ]"
    ]
    where
      showStackItem addr
        = iConcat [ showFWAddr addr, iStr ": "
                  , showStkNode heap (hLookup heap addr)
                  ]

showDump :: TiDump -> Iseq
showDump dump = iConcat [iStr "Dump depth ", iNum (length dump) ]

showStkNode :: TiHeap -> Node -> Iseq
showStkNode heap (NAp funAddr argAddr)
  = iConcat [ iStr "NAp ", showFWAddr funAddr
            , iStr " ", showFWAddr argAddr, iStr " ("
            , showNode (hLookup heap argAddr), iStr ")"
            ]
showStkNode heap node = showNode node

showNode :: Node -> Iseq
showNode node = case node of
  NAp a1 a2 -> iConcat [ iStr "NAp ", showAddr a1
                       , iStr " ",    showAddr a2
                       ]
  NSupercomb name args body
             -> iStr ("NSupercomb " ++ name)
  NNum n     -> iStr "NNum " `iAppend` iNum n
  NInd a     -> iStr "NInd " `iAppend` showAddr a
  NPrim n p  -> iStr ("NPrim " ++ n)
  NData t cs -> iConcat [ iStr "NData ", iNum t, iStr "["
                        , iInterleave (iStr ",") (map showAddr cs)
                        , iStr "]"
                        ]

showAddr :: Addr -> Iseq
showAddr addr = iStr (show addr)

showFWAddr :: Addr -> Iseq
showFWAddr addr = iStr (space (4 - length str) ++ str)
  where
    str = show addr

showStats :: TiState -> Iseq
showStats (stack, dump, heap, globals, stats)
  = iConcat [ iNewline, iNewline, iStr "Total number of steps = "
            , iNum (tiStatGetTotalSteps stats)
            , showHeapStats heap
            ]

showHeapStats :: TiHeap -> Iseq
showHeapStats (_, (a, u, f))
  = iConcat [ iNewline, iStr "Heap allocation count = "
            , iNum a
            , iNewline, iStr "Heap update count     = "
            , iNum u
            , iNewline, iStr "Heap free count       = "
            , iNum f
            ]

showHeap :: TiHeap -> Iseq
showHeap heap@((_, _, useds), _)
  = iConcat
    [ iStr "Heap  ["
    , iIndent (iInterleave iNewline (map showHeapItem useds))
    , iStr " ]"
    ]
    where
      showHeapItem (addr, node)
        = iConcat [ showFWAddr addr, iStr ": "
                  , showNode node
                  ]
      
showStack' :: TiHeap -> TiStack -> Iseq
showStack' heap stack
  = iConcat
    [ iStr "Stk ["
    , iIndent (iInterleave iNewline (map showStackItem stack))
    , iStr " ]"
    ]
    where
      showStackItem addr
        = iConcat [ showFWAddr addr, iStr ": "
                  , showStkNode heap (hLookup heap addr)
                  ]

-- Test

testProg0, testProg1, testProg2 :: String
testProg0 = "main = S K K 3"
testProg1 = "main = S K K" -- wrong (not saturated)
testProg2 = "id x = x;\n\
            \main = twice twice id 3"

test :: Int -> String -> String
test steps = showResults . take steps . eval . compile . parse

testProg3 :: Bool -> String
testProg3 rec = intercalate ";\n" [pairs,fsts,snds,if rec then fxysrec else fxys ,mains]
  where
    pairs  = "pair x y f = f x y"
    fsts   = "fst p = p K"
    snds   = "snd p = p K1"
    fxys   = "f x y = " ++ letexpr
    fxysrec = "f x y = " ++ letrecexpr
    letrecexpr = "letrec\n\
         \            a = pair x b;\n\
         \            b = pair y a\n\
         \        in\n\
         \        fst (snd (snd (snd a)))"
    letexpr = "let\n\
         \    a = S K K x;\n\
         \    b = S K K y\n\
         \    in\n\
         \    fst (pair a b)"
    mains  = "main = f 4 3"

testProg4 :: String
testProg4 = "main = double (double 2);\n\
            \double x = x + x"

testProg5 :: String
testProg5 = "main = double (2 + 3);\n\
            \double x = x + x"

{-

   0) Stack [   1: NSupercomb main ]                                       1) main!
      Dump depth 0
      Heap  [   3: NPrim +
                2: NSupercomb double
                1: NSupercomb main ]
      
   1) Stack [   1: NAp    2    5 (NAp 2 4) ]                                 (1) @!          
      Dump depth 0                                                              / \         
      Heap  [   1: NAp 2 5                                                     /   \        
                5: NAp 2 4                                           (2) double     @ (5)  
                4: NNum 2                                                          / \      
                3: NPrim +                                                        /   \     
                2: NSupercomb double ]                                  (2) double     2 (4)
      
   2) Stack [   2: NSupercomb double                                         (1) @          
                1: NAp    2    5 (NAp 2 4) ]                                    / \         
      Dump depth 0                                                             /   \        
      Heap  [   1: NAp 2 5                                           (2) double!    @ (5)  
                5: NAp 2 4                                                         / \      
                4: NNum 2                                                         /   \     
                3: NPrim +                                              (2) double     2 (4)
                2: NSupercomb double ]
      
   3) Stack [   1: NAp    6    5 (NAp 2 4) ]                                 (1) @!          
      Dump depth 0                                                              / \          
      Heap  [   1: NAp 6 5                                                     /   \         
                6: NAp 3 5                                                (6) @     \        
                5: NAp 2 4                                                   / \_____@ (5)   
                4: NNum 2                                                   /       / \      
                3: NPrim +                                             (3) +       /   \     
                2: NSupercomb double ]                                   (2) double     2 (4)

   4) Stack [   6: NAp    3    5 (NAp 2 4)                                   (1) @          
                1: NAp    6    5 (NAp 2 4) ]                                    / \          
      Dump depth 0                                                             /   \         
      Heap  [   1: NAp 6 5                                                (6) @!    \        
                6: NAp 3 5                                                   / \_____@ (5)   
                5: NAp 2 4                                                  /       / \      
                4: NNum 2                                              (3) +       /   \     
                3: NPrim +                                               (2) double     2 (4)
                2: NSupercomb double ]
      
   5) Stack [   3: NPrim +                                                   (1) @           
                6: NAp    3    5 (NAp 2 4)                                      / \          
                1: NAp    6    5 (NAp 2 4) ]                                   /   \         
      Dump depth 0                                                        (6) @     \        
      Heap  [   1: NAp 6 5                                                   / \_____@ (5)   
                6: NAp 3 5                                                  /       / \      
                5: NAp 2 4                                             (3) +!      /   \     
                4: NNum 2                                                (2) double     2 (4)
                3: NPrim +
                2: NSupercomb double ]
      
   6) Stack [   5: NAp    2    4 (NNum 2) ]                                  (1) @           
      Dump depth 1                                                              / \          
      Heap  [   1: NAp 6 5                                                     /   \         
                6: NAp 3 5                                                (6) @     \        
                5: NAp 2 4                                                   / \_____@! (5) 
                4: NNum 2                                                   /       / \      
                3: NPrim +                                             (3) +       /   \     
                2: NSupercomb double ]                                   (2) double     2 (4)
                                                                       
   7) Stack [   2: NSupercomb double                                         (1) @           
                5: NAp    2    4 (NNum 2) ]                                     / \          
      Dump depth 1                                                             /   \         
      Heap  [   1: NAp 6 5                                                (6) @     \        
                6: NAp 3 5                                                   / \_____@ (5)   
                5: NAp 2 4                                                  /       / \      
                4: NNum 2                                              (3) +       /   \     
                3: NPrim +                                               (2) double!    2 (4)
                2: NSupercomb double ]
      
   8) Stack [   5: NAp    7    4 (NNum 2) ]                                  (1) @            
      Dump depth 1                                                              / \           
      Heap  [   5: NAp 7 4                                                     /   \          
                7: NAp 3 4                                                (6) @     \         
                1: NAp 6 5                                                   / \_____@! (5)   
                6: NAp 3 5                                                  /       / \       
                4: NNum 2                                              (3) +       /   \      
                3: NPrim +                                                    (7) @     \     
                2: NSupercomb double ]                                           / \_____2 (4)
                                                                                /             
                                                                           (3) +              
      
   9) Stack [   7: NAp    3    4 (NNum 2)                                    (1) @            
                5: NAp    7    4 (NNum 2) ]                                     / \           
      Dump depth 1                                                             /   \          
      Heap  [   5: NAp 7 4                                                (6) @     \         
                7: NAp 3 4                                                   / \_____@ (5)   
                1: NAp 6 5                                                  /       / \       
                6: NAp 3 5                                             (3) +       /   \      
                4: NNum 2                                                     (7) @!    \     
                3: NPrim +                                                       / \_____2 (4)
                2: NSupercomb double ]                                          /             
                                                                           (3) +

  10) Stack [   3: NPrim +                                                   (1) @            
                7: NAp    3    4 (NNum 2)                                       / \           
                5: NAp    7    4 (NNum 2) ]                                    /   \          
      Dump depth 1                                                        (6) @     \         
      Heap  [   5: NAp 7 4                                                   / \_____@ (5)   
                7: NAp 3 4                                                  /       / \       
                1: NAp 6 5                                             (3) +       /   \      
                6: NAp 3 5                                                    (7) @     \     
                4: NNum 2                                                        / \_____2 (4)
                3: NPrim +                                                      /             
                2: NSupercomb double ]                                     (3) +!        
      
  11) Stack [   5: NNum 4 ]                                                  (1) @            
      Dump depth 1                                                              / \           
      Heap  [   5: NNum 4                                                      /   \          
                7: NAp 3 4                                                (6) @     \         
                1: NAp 6 5                                                   / \_____4! (5)   
                6: NAp 3 5                                                  /
                4: NNum 2                                              (3) +
                3: NPrim +
                2: NSupercomb double ]

  12) Stack [   1: NAp    6    5 (NNum 4) ]                                  (1) @!       
      Dump depth 0                                                              / \       
      Heap  [   5: NNum 4                                                      /   \      
                7: NAp 3 4                                                (6) @     \     
                1: NAp 6 5                                                   / \_____4 (5)
                6: NAp 3 5                                                  /             
                4: NNum 2                                              (3) +              
                3: NPrim +
                2: NSupercomb double ]
      
  13) Stack [   6: NAp    3    5 (NNum 4)                                    (1) @
                1: NAp    6    5 (NNum 4) ]                                     / \       
      Dump depth 0                                                             /   \      
      Heap  [   5: NNum 4                                                 (6) @!    \     
                7: NAp 3 4                                                   / \_____4 (5)
                1: NAp 6 5                                                  /             
                6: NAp 3 5                                             (3) +              
                4: NNum 2
                3: NPrim +
                2: NSupercomb double ]
      
  14) Stack [   3: NPrim +                                                   (1) @       
                6: NAp    3    5 (NNum 4)                                       / \       
                1: NAp    6    5 (NNum 4) ]                                    /   \      
      Dump depth 0                                                        (6) @     \     
      Heap  [   5: NNum 4                                                    / \_____4 (5)
                7: NAp 3 4                                                  /             
                1: NAp 6 5                                             (3) +!              
                6: NAp 3 5
                4: NNum 2
                3: NPrim +
                2: NSupercomb double ]
      
  15) Stack [   1: NNum 8 ]                                                  (1) 8
      Dump depth 0
      Heap  [   1: NNum 8
                5: NNum 4
                7: NAp 3 4
                6: NAp 3 5
                4: NNum 2
                3: NPrim +
                2: NSupercomb double ]
-}

{-
   0) Stack [   1: NSupercomb main ]                                      (1) main!
      Dump depth 0
      Heap  [   3: NPrim +
                2: NSupercomb double
                1: NSupercomb main ]
      
   1) Stack [   1: NAp    2    7 (NAp 5 6) ]                                 (1) @!         
      Dump depth 0                                                              / \         
      Heap  [   1: NAp 2 7                                                     /   \        
                7: NAp 5 6                                           (2) double     @ (7)   
                6: NNum 3                                                          / \      
                5: NAp 3 4                                                        /   \     
                4: NNum 2                                                    (5) @     3 (6)
                3: NPrim +                                                      / \         
                2: NSupercomb double ]                                         /   \        
                                                                          (3) +     2 (4)   

   2) Stack [   2: NSupercomb double                                         (1) @         
                1: NAp    2    7 (NAp 5 6) ]                                    / \         
      Dump depth 0                                                             /   \        
      Heap  [   1: NAp 2 7                                           (2) double!    @ (7)   
                7: NAp 5 6                                                         / \      
                6: NNum 3                                                         /   \     
                5: NAp 3 4                                                   (5) @     3 (6)
                4: NNum 2                                                       / \         
                3: NPrim +                                                     /   \        
                2: NSupercomb double ]                                    (3) +     2 (4)   
      
   3) Stack [   1: NAp    8    7 (NAp 5 6) ]                                 (1) @!          
      Dump depth 0                                                              / \          
      Heap  [   1: NAp 8 7                                                     /   \         
                8: NAp 3 7                                                (8) @     \        
                7: NAp 5 6                                                   / \_____@ (7)   
                6: NNum 3                                                   /       / \      
                5: NAp 3 4                                                 +       /   \        
                4: NNum 2                                                     (5) @     3 (6)
                3: NPrim +                                                       / \         
                2: NSupercomb double ]                                          /   \        
                                                                           (3) +     2 (4)   
      
   4) Stack [   8: NAp    3    7 (NAp 5 6)                                   (1) @          
                1: NAp    8    7 (NAp 5 6) ]                                    / \          
      Dump depth 0                                                             /   \         
      Heap  [   1: NAp 8 7                                                (8) @!    \        
                8: NAp 3 7                                                   / \_____@ (7)   
                7: NAp 5 6                                                  /       / \      
                6: NNum 3                                                  +       /   \     
                5: NAp 3 4                                                    (5) @     3 (6)
                4: NNum 2                                                        / \         
                3: NPrim +                                                      /   \        
                2: NSupercomb double ]                                     (3) +     2 (4)   
      
   5) Stack [   3: NPrim +                                                   (1) @          
                8: NAp    3    7 (NAp 5 6)                                      / \          
                1: NAp    8    7 (NAp 5 6) ]                                   /   \         
      Dump depth 0                                                        (8) @     \        
      Heap  [   1: NAp 8 7                                                   / \_____@ (7)   
                8: NAp 3 7                                                  /       / \      
                7: NAp 5 6                                                 +!      /   \     
                6: NNum 3                                                     (5) @     3 (6)
                5: NAp 3 4                                                       / \         
                4: NNum 2                                                       /   \        
                3: NPrim +                                                 (3) +     2 (4)   
                2: NSupercomb double ]
      
   6) Stack [   7: NAp    5    6 (NNum 3) ]                                  (1) @          
      Dump depth 1                                                              / \          
      Heap  [   1: NAp 8 7                                                     /   \         
                8: NAp 3 7                                                (8) @     \        
                7: NAp 5 6                                                   / \_____@! (7)   
                6: NNum 3                                                   /       / \      
                5: NAp 3 4                                                 +       /   \     
                4: NNum 2                                                     (5) @     3 (6)
                3: NPrim +                                                       / \         
                2: NSupercomb double ]                                          /   \        
                                                                           (3) +     2 (4)   

   7) Stack [   5: NAp    3    4 (NNum 2)                                    (1) @          
                7: NAp    5    6 (NNum 3) ]                                     / \          
      Dump depth 1                                                             /   \         
      Heap  [   1: NAp 8 7                                                (8) @     \        
                8: NAp 3 7                                                   / \_____@ (7)   
                7: NAp 5 6                                                  /       / \      
                6: NNum 3                                                  +       /   \     
                5: NAp 3 4                                                    (5) @!    3 (6)
                4: NNum 2                                                        / \         
                3: NPrim +                                                      /   \        
                2: NSupercomb double ]                                     (3) +     2 (4)   
      
   8) Stack [   3: NPrim +                                                   (1) @          
                5: NAp    3    4 (NNum 2)                                       / \          
                7: NAp    5    6 (NNum 3) ]                                    /   \         
      Dump depth 1                                                        (8) @     \        
      Heap  [   1: NAp 8 7                                                   / \_____@ (7)   
                8: NAp 3 7                                                  /       / \      
                7: NAp 5 6                                                 +       /   \     
                6: NNum 3                                                     (5) @     3 (6)
                5: NAp 3 4                                                       / \         
                4: NNum 2                                                       /   \        
                3: NPrim +                                                 (3) +!    2 (4)   
                2: NSupercomb double ]
      
   9) Stack [   7: NNum 5 ]                                                  (1) @          
      Dump depth 1                                                              / \          
      Heap  [   7: NNum 5                                                      /   \         
                1: NAp 8 7                                                (8) @     \        
                8: NAp 3 7                                                   / \_____5! (7)   
                6: NNum 3                                                   /              
                5: NAp 3 4                                                 +               
                4: NNum 2
                3: NPrim +
                2: NSupercomb double ]

  10) Stack [   1: NAp    8    7 (NNum 5) ]                                  (1) @!         
      Dump depth 0                                                              / \        
      Heap  [   7: NNum 5                                                      /   \       
                1: NAp 8 7                                                (8) @     \      
                8: NAp 3 7                                                   / \_____5 (7)
                6: NNum 3                                                   /              
                5: NAp 3 4                                                 +               
                4: NNum 2
                3: NPrim +
                2: NSupercomb double ]
      
  11) Stack [   8: NAp    3    7 (NNum 5)                                    (1) @         
                1: NAp    8    7 (NNum 5) ]                                     / \        
      Dump depth 0                                                             /   \       
      Heap  [   7: NNum 5                                                 (8) @!    \      
                1: NAp 8 7                                                   / \_____5 (7)
                8: NAp 3 7                                                  /              
                6: NNum 3                                                  +               
                5: NAp 3 4
                4: NNum 2
                3: NPrim +
                2: NSupercomb double ]
      
  12) Stack [   3: NPrim +                                                   (1) @         
                8: NAp    3    7 (NNum 5)                                       / \        
                1: NAp    8    7 (NNum 5) ]                                    /   \       
      Dump depth 0                                                        (8) @     \      
      Heap  [   7: NNum 5                                                    / \_____5 (7)
                1: NAp 8 7                                                  /              
                8: NAp 3 7                                                 +!     
                6: NNum 3
                5: NAp 3 4
                4: NNum 2
                3: NPrim +
                2: NSupercomb double ]
      
  13) Stack [   1: NNum 10 ]                                                (1) 10
      Dump depth 0
      Heap  [   1: NNum 10
                7: NNum 5
                8: NAp 3 7
                6: NNum 3
                5: NAp 3 4
                4: NNum 2
                3: NPrim +
                2: NSupercomb double ]
-}

testProg6 :: String
testProg6 = "main = fst (snd (fst (MkPair (MkPair 1 (MkPair 2 3)) 4)))"
