module Iseq 
  ( Iseq
  , iNil
  , iStr
  , iAppend
  , iNewline
  , iIndent
  , iConcat
  , iInterleave
  , iDisplay
  ) where

iNil     :: Iseq
iNil     = INil

iStr     :: String -> Iseq
iStr     = IStr

iAppend  :: Iseq -> Iseq -> Iseq
iAppend INil iseq = iseq
iAppend iseq INil = iseq
iAppend x    y    = IAppend x y

iNewline :: Iseq
iNewline = IStr "\n"

iIndent  :: Iseq -> Iseq
iIndent  = id

iConcat :: [Iseq] -> Iseq
iConcat = foldr iAppend iNil

iInterleave :: Iseq -> [Iseq] -> Iseq
iInterleave sep = foldr1 f
  where
    f x y = iConcat [x, sep, y]

iDisplay :: Iseq -> String
iDisplay = flatten . (: [])

flatten :: [Iseq] -> String
flatten [] = ""
flatten (IStr s : seqs) = s ++ flatten seqs
flatten (IAppend seq1 seq2 : seqs) = flatten (seq1 : seq2 : seqs) 

data Iseq = INil
          | IStr String
          | IAppend Iseq Iseq
