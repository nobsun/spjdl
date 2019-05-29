module Template2 where

-- import Debug.Trace

import Utils
import Iseq
import Language

import Data.List (intercalate)

-- Mark 2 : let(rec) expression

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
  = ( Int  -- supercombinator reduction
    , Int  -- primitive reduction
    )

tiStatInitial :: TiStats
tiStatInitial  = (0, 0)

tiStatIncScSteps :: TiStats -> TiStats
tiStatIncScSteps (sc, pm) =(sc + 1, pm)

tiStatIncPmSteps :: TiStats -> TiStats
tiStatIncPmSteps (sc, pm) =(sc, pm + 1)

tiStatGetScSteps :: TiStats -> Int
tiStatGetScSteps stats = fst stats

tiStatGetPmSteps :: TiStats -> Int
tiStatGetPmSteps stats = snd stats

applyToStats :: (TiStats -> TiStats) -> TiState -> TiState
applyToStats func (stack, dump, heap, scDefs, stats)
  = (stack, dump, heap, scDefs, func stats)

-- compiler

testProg :: String
testProg = intercalate ";\n" [pairs,fsts,snds,fxys,mains]
pairs  = "pair x y f = f x y"
fsts   = "fst p = p K"
snds   = "snd p = p K1"
fxys   = "f x y = " ++ letrecexpr
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

compile :: CoreProgram -> TiState
compile program
  = (initialStack, initialTiDump, initialHeap, globals, tiStatInitial)
    where
      scDefs = program ++ preludeDefs ++ extraPreludeDefs
      (initialHeap, globals) = buildInitialHeap scDefs
      initialStack = [addressOfMain]
      addressOfMain = aLookup globals "main" (error "main is not defined")

extraPreludeDefs :: CoreProgram
extraPreludeDefs = []

buildInitialHeap :: [CoreScDefn] -> (TiHeap, TiGlobals)
buildInitialHeap scDefs = mapAccuml allocateSc hInitial scDefs

allocateSc :: TiHeap -> CoreScDefn -> (TiHeap, (Name, Addr))
allocateSc heap (name, args, body)
  = (heap', (name, addr))
    where
      (heap', addr) = hAlloc heap (NSupercomb name args body)

-- evaluator

eval :: TiState -> [TiState]
eval state = state : restStates
  where
    restStates
      | tiFinal state = []
      | otherwise     = eval nextState
    nextState = doAdminSc (step state)

doAdminSc :: TiState -> TiState
doAdminSc state = applyToStats tiStatIncScSteps state

doAdminPm :: TiState -> TiState
doAdminPm state = applyToStats tiStatIncPmSteps state

tiFinal :: TiState -> Bool
tiFinal ([soleAddr], _, heap, _, _)
  = isDataNode (hLookup heap soleAddr)
tiFinal ([], _, _, _, _) = error "Empty stack!"
tiFinal _ = False

isDataNode :: Node -> Bool
isDataNode (NNum _) = True
isDataNode _        = False

step :: TiState -> TiState
step state = dispatch (hLookup heap (head stack))
  where
    (stack, dump, heap, globals, stats) = state
    dispatch (NNum n)                  = numStep state n
    dispatch (NAp a1 a2)               = apStep  state a1 a2
    dispatch (NSupercomb sc args body) = scStep state sc args body

numStep :: TiState -> Int -> TiState
numStep state n = error "Number applied as a function"

apStep :: TiState -> Addr -> Addr -> TiState
apStep (stack, dump, heap, globals, stats) a1 a2
  = (a1 : stack, dump, heap, globals, stats)

scStep :: TiState -> Name -> [Name] -> CoreExpr -> TiState
scStep (stack, dump, heap, globals, stats) scName argNames body
  = if length stack < length argNames + 1 then error "Too few argments given"
    else (stack', dump, heap', globals, stats)
    where
      stack' = resultAddr : drop (length argNames + 1) stack
      (heap', resultAddr) = instantiate body heap env
      env = argBindings ++ globals
      argBindings = zip argNames (getargs heap stack)

getargs :: TiHeap -> TiStack -> [Addr]
getargs heap (sc:stack)
  = map getarg stack
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

instantiateLet :: Bool -> [CoreBinding] -> CoreExpr -> TiHeap -> Assoc Name Addr -> (TiHeap , Addr)
instantiateLet  isrec defs body heap env
  = instantiate body heap' env'
  where
    (heap', env') = foldr f (heap, env) defs
    f = if isrec then g else h
    g (name, expr) (heap, env) = case instantiate expr heap env' of
      (heap', addr) -> (heap', (name, addr) : env)
    h (name, expr) (heap, env) = case instantiate expr heap env of
      (heap', addr) -> (heap', (name, addr) : env)
      
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
            , iNum (tiStatGetScSteps stats)
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
