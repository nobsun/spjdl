module Utils
  ( Heap
  , Assoc
  , Addr
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
  , aDomain
  , aRange
  , aEmpty
  , mapAccuml
  ) where

mapAccuml :: (acc -> a -> (acc, b)) -> acc -> [a] -> (acc, [b])
mapAccuml _ a []     = (a, [])
mapAccuml f a (x:xs) = case f a x of
  (a', y) -> case mapAccuml f a' xs of
    (a'', ys) -> (a'', y:ys)

type Heap a = (Int, [Addr], [(Addr, a)])

hInitial :: Heap a
hAlloc   :: Heap a -> a -> (Heap a, Addr)
hUpdate  :: Heap a -> Addr -> a -> Heap a
hFree    :: Heap a -> Addr -> Heap a

hInitial                         =  (0, [1..], [])
hAlloc  (size, next:free, cts) n = ((size+1, free,    (next, n) : cts)     , next)
hUpdate (size, free, cts) a n    =  (size,    free,   (a,n) : remove cts a)
hFree   (size, free, cts) a      =  (size-1,  a:free, remove cts a)

hLookup    :: Heap a -> Addr -> a
hAddresses :: Heap a -> [Addr]
hSize      :: Heap a -> Int

hNull :: Addr
hNull = 0

hIsNull :: Addr -> Bool
hIsNull a = hNull == a

showaddr :: Addr -> String
showaddr = ('#' :) . show

hLookup (size, free, cts) a
  = aLookup cts a (error ("can't find node " ++ showaddr a ++ " in heap"))
hAddresses (size, free, cts)
  = [ addr | (addr, _) <- cts ]
hSize (size, _, _) = size

remove :: [(Addr, a)] -> Addr -> [(Addr, a)]
remove [] a = error ("Attempt to update or free noexistent address " ++ showaddr a)
remove ((a', n) : cts) a
  | a == a'   = cts
  | otherwise = (a', n) : remove cts a

aLookup []         _  def = def
aLookup ((k,v):bs) k' def
  | k == k'   = v
  | otherwise = aLookup bs k' def

aDomain :: Assoc a b -> [a]
aDomain = map fst

aRange :: Assoc a b -> [b]
aRange = map snd

aEmpty :: Assoc a b
aEmpty = []

type Addr = Int
type Assoc a b = [(a, b)]

