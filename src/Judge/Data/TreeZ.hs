-- | Labeled multi-way trees and zipper for them.
-- Zipper does not support removing elements.
module Judge.Data.TreeZ
  ( TreeF (..)
  , Tree (..)
  , isEmptyTree
  , RecTree (..)
  , isEmptyRecTree
  , TreePos
  , pruneTree
  , TreeZ (..)
  , posTreeZ
  , readTreeZ
  , writeTreeZ
  , modifyTreeZ
  , stateTreeZ
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
  , rewindTreeZ
  , depthTreeZ
  , breadthTreeZ
  ) where

import Control.Applicative ((<|>))
import Control.Monad.Identity (Identity (..))
import Data.Bifunctor (first, second)
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.Sequence.NonEmpty (NESeq)
import qualified Data.Sequence.NonEmpty as NESeq
import Judge.Data.NEStack (NEStack)
import qualified Judge.Data.NEStack as NEStack
import Judge.Data.SeqZ (SeqZ, afterSeqZ, beforeSeqZ, firstSeqZ, inSeqZ, isFirstSeqZ, isLastSeqZ, lastSeqZ, outSeqZ,
                        readSeqZ, rewindSeqZ, stateSeqZ)
import Judge.Data.Validation (invalid, runValid)

data TreeF x y a =
    TreeF !x !(Seq (y, a))
  deriving (Eq, Show, Functor, Foldable, Traversable)

newtype Tree x y z = Tree (TreeF x y (Either z (Tree x y z))) deriving (Eq, Show)

isEmptyTree :: Tree x y z -> Bool
isEmptyTree (Tree (TreeF _ ss)) = Seq.null ss

newtype RecTree x y = RecTree (TreeF x y (RecTree x y)) deriving (Eq, Show)

isEmptyRecTree :: RecTree x y -> Bool
isEmptyRecTree (RecTree (TreeF _ ss)) = Seq.null ss

type TreePos x y = NEStack (x, y)

pruneTree :: Tree x y z -> Either (NESeq (TreePos x y, z)) (RecTree x y)
pruneTree = runValid . goRoot where
  goRoot (Tree (TreeF x ss)) = fmap (RecTree . TreeF x) (traverse (\(y, e) -> goRec (NEStack.singleton (x, y)) y e) ss)
  goBranch p (Tree (TreeF x ss)) = fmap (RecTree . TreeF x) (traverse (\(y, e) -> goRec (NEStack.push (x, y) p) y e) ss)
  goRec p y e =
    case e of
      Left z -> invalid (NESeq.singleton (p, z))
      Right t -> fmap (y,) (goBranch p t)

newtype TreeZ x y z = TreeZ (NEStack (x, SeqZ (y, Either z (Tree x y z)))) deriving (Eq, Show)

posTreeZ :: TreeZ x y z -> TreePos x y
posTreeZ (TreeZ ns) = fmap (\(x, sz) -> let (y, _) = readSeqZ sz in (x, y)) ns

readTreeZ :: TreeZ x y z -> (x, y, Either z (Tree x y z))
readTreeZ (TreeZ ns) =
  let (x, sz) = NEStack.peek ns
      (y, e) = readSeqZ sz
  in (x, y, e)

writeTreeZ :: Either z (Tree x y z) -> TreeZ x y z -> TreeZ x y z
writeTreeZ e = modifyTreeZ (\_ _ _ -> e)

modifyTreeZ :: (x -> y -> Either z (Tree x y z) -> Either z (Tree x y z)) -> TreeZ x y z -> TreeZ x y z
modifyTreeZ f = fst . runIdentity . stateTreeZ (\x y e -> Identity (f x y e, ()))

stateTreeZ :: Functor m => (x -> y -> Either z (Tree x y z) -> m (Either z (Tree x y z), s)) -> TreeZ x y z -> m (TreeZ x y z, s)
stateTreeZ f (TreeZ ns) =
  let ((x, sz), rest) = NEStack.pop ns
      msz' = stateSeqZ (\(y, e) -> fmap (first (y,)) (f x y e)) sz
  in fmap (first (\sz' -> TreeZ (NEStack.unpop (x, sz') rest))) msz'

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
inTreeZ i (Tree (TreeF x ls)) = fmap (\sz -> TreeZ (NEStack.singleton (x, sz))) (inSeqZ i ls)

firstTreeZ :: Tree x y z -> Maybe (TreeZ x y z)
firstTreeZ (Tree (TreeF x ls)) = fmap (\sz -> TreeZ (NEStack.singleton (x, sz))) (firstSeqZ ls)

lastTreeZ :: Tree x y z -> Maybe (TreeZ x y z)
lastTreeZ (Tree (TreeF x ls)) = fmap (\sz -> TreeZ (NEStack.singleton (x, sz))) (lastSeqZ ls)

outTreeZ :: TreeZ x y z -> Tree x y z
outTreeZ (TreeZ ns) =
  let ((x, sz), rest) = NEStack.pop ns
      t = Tree (TreeF x (outSeqZ sz))
  in case rest of
    Nothing -> t
    Just ns' -> outTreeZ (writeTreeZ (Right t) (TreeZ ns'))

upTreeZ :: TreeZ x y z -> Maybe (TreeZ x y z)
upTreeZ (TreeZ ns) =
  let ((x, sz), rest) = NEStack.pop ns
      t = Tree (TreeF x (outSeqZ sz))
  in fmap (writeTreeZ (Right t) . TreeZ) rest

beforeTreeZ :: TreeZ x y z -> Maybe (TreeZ x y z)
beforeTreeZ (TreeZ ns) =
  let ((x, sz), rest) = NEStack.pop ns
  in fmap (\sz' -> TreeZ (NEStack.unpop (x, sz') rest)) (beforeSeqZ sz)

afterTreeZ :: TreeZ x y z -> Maybe (TreeZ x y z)
afterTreeZ (TreeZ ns) =
  let ((x, sz), rest) = NEStack.pop ns
  in fmap (\sz' -> TreeZ (NEStack.unpop (x, sz') rest)) (afterSeqZ sz)

downTreeZ :: (Tree x y z -> Maybe (TreeZ x y z)) -> TreeZ x y z -> Maybe (TreeZ x y z)
downTreeZ f (TreeZ ns) =
  let (_, sz) = NEStack.peek ns
  in case snd (readSeqZ sz) of
    Left _ -> Nothing
    Right t -> fmap (\(TreeZ ns') -> TreeZ (ns' <> ns)) (f t)

rewindTreeZ :: TreeZ x y z -> TreeZ x y z
rewindTreeZ (TreeZ ns) = TreeZ (NEStack.modifyTop (second rewindSeqZ) ns)

enumerateTreeZ :: TreeZ x y z -> [TreeZ x y z]
enumerateTreeZ = go [] where
  go !acc tz = let acc' = tz:acc in maybe acc' (go acc') (afterTreeZ tz)

depthTreeZ :: TreeZ x y z -> Maybe (TreeZ x y z)
depthTreeZ tz = downTreeZ firstTreeZ tz <|> afterTreeZ tz <|> (upTreeZ tz >>= afterTreeZ)

breadthTreeZ :: TreeZ x y z -> Maybe (TreeZ x y z)
breadthTreeZ tz = foldr (\tz' mtz -> mtz <|> downTreeZ firstTreeZ tz') (afterTreeZ tz) (enumerateTreeZ (rewindTreeZ tz))
