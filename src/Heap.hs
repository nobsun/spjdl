module Heap
  ( Addr
  , Heap
  , hInitial
  , hAlloc
  , hUpdate
  , hFree
  , hLookup
  , hAddresses
  , hSize
  , hNull
  , hIsNull
  , aLookup
  ) where

type Addr = Int

type Heap a = (Heap' a, HeapStats)
type HeapStats = (Int, Int, Int) -- alloc, update, free

type Heap' a = (Int, [Addr], [(Addr, a)])

hInitial :: Heap a
hAlloc   :: Heap a -> a -> (Heap a, Addr)
hUpdate  :: Heap a -> Addr -> a -> Heap a
hFree    :: Heap a -> Addr -> Heap a

hInitial
  =  ((0, [1..], []), (0, 0, 0))
hAlloc  ((size, next:free, cts), (a,u,f)) node
  = (((size+1, free,    (next, node) : cts), (a + 1, u, f)), next)
hUpdate ((size, free, cts), (a,u,f)) addr node
  = ((size,    free,   (addr, node) : remove cts addr), (a, u + 1, f))
hFree ((size, free, cts), (a,u,f)) addr
  = ((size-1,  addr:free, remove cts addr), (a, u, f+1))

hLookup    :: Heap a -> Addr -> a
hAddresses :: Heap a -> [Addr]
hSize      :: Heap a -> Int

hNull :: Addr
hNull = 0

hIsNull :: Addr -> Bool
hIsNull a = hNull == a

showaddr :: Addr -> String
showaddr = ('#' :) . show

hLookup ((size, free, cts), _) a
  = aLookup cts a (error ("can't find node " ++ showaddr a ++ " in heap"))
hAddresses ((size, free, cts), _)
  = [ addr | (addr, _) <- cts ]
hSize ((size, _, _), _) = size

remove :: [(Addr, a)] -> Addr -> [(Addr, a)]
remove [] a = error ("Attempt to update or free noexistent address " ++ showaddr a)
remove ((a', n) : cts) a
  | a == a'   = cts
  | otherwise = (a', n) : remove cts a

aLookup []         _  def = def
aLookup ((k,v):bs) k' def
  | k == k'   = v
  | otherwise = aLookup bs k' def

