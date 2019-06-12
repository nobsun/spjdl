module Utils
  ( Assoc
  , aDomain
  , aRange
  , aEmpty
  , mapAccuml
  , intercalate
  ) where

aDomain :: Assoc a b -> [a]
aDomain = map fst

aRange :: Assoc a b -> [b]
aRange = map snd

aEmpty :: Assoc a b
aEmpty = []

type Assoc a b = [(a, b)]

-- List utilities

mapAccuml :: (acc -> a -> (acc, b)) -> acc -> [a] -> (acc, [b])
mapAccuml f a xxs = case xxs of
  []   -> (a, [])
  x:xs -> case f a x of
    (a',y) -> case mapAccuml f a' xs of
      (a'',ys) -> (a'',y:ys)

intercalate :: [a] -> [[a]] -> [a]
intercalate sep xss = case xss of
  []   -> []
  _    -> foldr1 f xss
  where
    f xs ys = if null ys
      then xs
      else xs ++ (sep ++ ys)
