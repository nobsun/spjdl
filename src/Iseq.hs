module Iseq 
  ( Iseq
  , iNil
  , iStr
  , iNum
  , iFWNum
  , iAppend
  , iNewline
  , iIndent
  , iConcat
  , iInterleave
  , iDisplay
  ) where

iNil     :: Iseq
iNil     = INil

iNum     :: Int -> Iseq
iNum     = IStr . show

iFWNum   :: Int -> Int -> Iseq
iFWNum width n
  = iStr (space (width - length digits) ++ digits)
    where
      digits = show n

iLayn :: [Iseq] -> Iseq
iLayn seqs = iConcat (map layItem (zip [1..] seqs))
             where
               layItem (n, seq) = iConcat [ iFWNum 4 n, iStr ") ", iIndent seq, iNewline ]

iStr     :: String -> Iseq
iStr ""  = INil
iStr str = case break ('\n' ==) str of
  ("",_:str) -> iAppend INewline (iStr str)
  (s,"")     -> IStr str
  (s,_:t)    -> iAppend (IStr s) (iAppend INewline (iStr t))

iAppend  :: Iseq -> Iseq -> Iseq
iAppend INil iseq = iseq
iAppend iseq INil = iseq
iAppend x    y    = IAppend x y

iNewline :: Iseq
iNewline = INewline

iIndent  :: Iseq -> Iseq
iIndent  = IIndent

iConcat :: [Iseq] -> Iseq
iConcat = foldr iAppend iNil

iInterleave :: Iseq -> [Iseq] -> Iseq
iInterleave sep = foldr1 f
  where
    f x y = iConcat [x, sep, y]

iDisplay :: Iseq -> String
iDisplay seq = flatten 0 [(seq, 0)]

flatten :: Int            -- ^ Current column; 0 for first column
        -> [(Iseq, Int)]  -- ^ Work list
        -> String         -- ^ Result
flatten _ [] = ""
flatten col ((INil, _) : seqs) = flatten col seqs
flatten col ((IStr s, _) : seqs) = s ++ flatten (col + length s) seqs
flatten col ((IAppend seq1 seq2, indent) : seqs)
  = flatten col ((seq1, indent) : (seq2, indent) : seqs)
flatten col ((INewline, indent) : seqs)
  = '\n' : (space indent ++ flatten indent seqs)
flatten col ((IIndent seq, _) : seqs)
  = flatten col ((seq, col) : seqs)

space :: Int -> String
space = flip replicate ' '

data Iseq = INil
          | IStr String
          | IAppend Iseq Iseq
          | IIndent Iseq
          | INewline
          
