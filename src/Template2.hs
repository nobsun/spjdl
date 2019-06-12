module Template2 where

import Utils
import Heap
import Iseq
import Language

-- Mark 2 : let(rec) expressions

--- Structure of the implementation

runProg :: String -> String
runProg = showResults . eval . compile . parse

type TiState = (TiStack, TiDump, TiHeap, TiGlobals, TiStats)

type TiStack = [Addr]

data TiDump = DummyTiDump deriving Show
initialTiDump = DummyTiDump

type TiHeap = Heap Node
data Node = NAp Addr Addr                   -- ^ Application
          | NSupercomb Name [Name] CoreExpr -- ^ Supercombinator
          | NNum Int                        -- ^ Number
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
extraPreludeDefs = []

buildInitialHeap :: [CoreScDefn] -> (TiHeap, TiGlobals)
buildInitialHeap scDefs = mapAccuml allocateSc hInitial scDefs

allocateSc :: TiHeap -> CoreScDefn -> (TiHeap, (Name, Addr))
allocateSc heap scDefn = case scDefn of
  (name, args, body) -> (heap', (name, addr))
    where
      (heap', addr) = hAlloc heap (NSupercomb name args body)

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
  ([soleAddr], _, heap, _, _) -> isDataNode (hLookup heap soleAddr)
  ([], _, _, _, _)            -> error "Empty stack!"
  _                           -> False

isDataNode :: Node -> Bool
isDataNode node = case node of
  NNum _ -> True
  _      -> False

step :: TiState -> TiState
step state = case state of
  (stack, dump, heap, globals, stats) -> dispatch (hLookup heap (head stack))
  where
    dispatch (NNum n)                  = numStep state n
    dispatch (NAp a1 a2)               = apStep  state a1 a2
    dispatch (NSupercomb sc args body) = scStep state sc args body

numStep :: TiState -> Int -> TiState
numStep state n = error "Number applied as a function"

apStep :: TiState -> Addr -> Addr -> TiState
apStep state a1 a2 = case state of
  (stack, dump, heap, globals, stats) -> (a1:stack, dump, heap, globals, stats)

scStep :: TiState -> Name -> [Name] -> CoreExpr -> TiState
scStep state scName argNames body = case state of
  (stack, dump, heap, globals, stats)
    -> if length stack < length argNames + 1 then error "Too few argments given"
       else (stack', dump, heap', globals, stats)
    where
      stack' = resultAddr : drop (length argNames + 1) stack
      (heap', resultAddr) = instantiate body heap env
      env = argBindings ++ globals
      argBindings = zip argNames (getargs heap stack)

getargs :: TiHeap -> TiStack -> [Addr]
getargs heap stack = case stack of
  sc:stack' -> map getarg stack'
    where
      getarg addr = arg
        where
          NAp fun arg = hLookup heap addr

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


instantiateConstr tag arity heap env
  = error "Can't instantiate constructors yet"

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
            , showHeap heap, iNewline]

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
  NNum n    -> iStr "NNum " `iAppend` iNum n

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
