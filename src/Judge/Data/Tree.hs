-- | Labeled multi-way trees and zipper for them.
-- Zipper does not support removing elements.
module Judge.Data.Tree
  ( Tree (..)
  , isEmptyTree
  , TreeZ (..)
  , readTreeZ
  , writeTreeZ
  , modifyTreeZ
  , isTopTreeZ
  , isFirstTreeZ
  , isLastTreeZ
  , inTreeZ
  , outSeqZ
  , upTreeZ
  , firstTreeZ
  , lastTreeZ
  , beforeTreeZ
  , afterTreeZ
  ) where

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Judge.Data.NEStack (NEStack)
import qualified Judge.Data.NEStack as NEStack
import Judge.Data.SeqZ (SeqZ, inSeqZ, outSeqZ, modifySeqZ, isFirstSeqZ, isLastSeqZ, firstSeqZ, lastSeqZ, readSeqZ, beforeSeqZ, afterSeqZ)

data Tree x y z = Tree !x !(Seq (y, Either z (Tree x y z))) deriving (Eq, Show)

isEmptyTree :: Tree x y z -> Bool
isEmptyTree (Tree _ ss) = Seq.null ss

newtype TreeZ x y z = TreeZ (NEStack (x, SeqZ (y, Either z (Tree x y z)))) deriving (Eq, Show)

readTreeZ :: TreeZ x y z -> (x, y, Either z (Tree x y z))
readTreeZ (TreeZ ns) =
  let (x, sz) = NEStack.peek ns
      (y, e) = readSeqZ sz
  in (x, y, e)

writeTreeZ :: Either z (Tree x y z) -> TreeZ x y z -> TreeZ x y z
writeTreeZ e = modifyTreeZ (\_ _ _ -> e)

modifyTreeZ :: (x -> y -> Either z (Tree x y z) -> Either z (Tree x y z)) -> TreeZ x y z -> TreeZ x y z
modifyTreeZ f (TreeZ ns) =
  let ((x, sz), rest) = NEStack.pop ns
      sz' = modifySeqZ (\(y, e) -> (y, f x y e)) sz
  in TreeZ (NEStack.unpop (x, sz') rest)

isTopTreeZ :: TreeZ x y z -> Bool
isTopTreeZ (TreeZ ns) = NEStack.isBottom ns

isFirstTreeZ :: TreeZ x y z -> Bool
isFirstTreeZ (TreeZ ns) =
  let (_, sz) = NEStack.peek ns
  in isFirstSeqZ sz

isLastTreeZ :: TreeZ x y z -> Bool
isLastTreeZ (TreeZ ns) =
  let (_, sz) = NEStack.peek ns
  in isLastSeqZ sz

inTreeZ :: Int -> Tree x y z -> Maybe (TreeZ x y z)
inTreeZ i (Tree x ls) = fmap (\sz -> TreeZ (NEStack.singleton (x, sz))) (inSeqZ i ls)

firstTreeZ :: Tree x y z -> Maybe (TreeZ x y z)
firstTreeZ (Tree x ls) = fmap (\sz -> TreeZ (NEStack.singleton (x, sz))) (firstSeqZ ls)

lastTreeZ :: Tree x y z -> Maybe (TreeZ x y z)
lastTreeZ (Tree x ls) = fmap (\sz -> TreeZ (NEStack.singleton (x, sz))) (lastSeqZ ls)

outTreeZ :: TreeZ x y z -> Tree x y z
outTreeZ (TreeZ ns) =
  let ((x, sz), rest) = NEStack.pop ns
      t = Tree x (outSeqZ sz)
  in case rest of
    Nothing -> t
    Just ns' -> outTreeZ (writeTreeZ (Right t) (TreeZ ns'))

upTreeZ :: TreeZ x y z -> Maybe (TreeZ x y z)
upTreeZ (TreeZ ns) =
  let ((x, sz), rest) = NEStack.pop ns
      t = Tree x (outSeqZ sz)
  in fmap (writeTreeZ (Right t) . TreeZ) rest

beforeTreeZ :: TreeZ x y z -> Maybe (TreeZ x y z)
beforeTreeZ (TreeZ ns) =
  let ((x, sz), rest) = NEStack.pop ns
  in fmap (\sz' -> TreeZ (NEStack.unpop (x, sz') rest)) (beforeSeqZ sz)

afterTreeZ :: TreeZ x y z -> Maybe (TreeZ x y z)
afterTreeZ (TreeZ ns) =
  let ((x, sz), rest) = NEStack.pop ns
  in fmap (\sz' -> TreeZ (NEStack.unpop (x, sz') rest)) (afterSeqZ sz)
