module Judge.Data.Tree where

import Data.Sequence (Seq)
import Judge.Data.NEStack (NEStack)
import qualified Judge.Data.NEStack as NEStack
import Judge.Data.SeqZ (SeqZ, inSeqZ, outSeqZ)

data Tree x y z = Tree !x !(Seq (y, Either z (Tree x y z))) deriving (Eq, Show)

isEmptyTree :: Tree x y z -> Bool
isEmptyTree = undefined

newtype TreeZ x y z = TreeZ (NEStack (x, SeqZ (y, Either z (Tree x y z)))) deriving (Eq, Show)

isTopTreeZ :: TreeZ x y z -> Bool
isTopTreeZ = undefined

isFirstTreeZ :: TreeZ x y z -> Bool
isFirstTreeZ = undefined

isLastTreeZ :: TreeZ x y z -> Bool
isLastTreeZ = undefined

inTreeZ :: Int -> Tree x y z -> Maybe (TreeZ x y z)
inTreeZ i (Tree x ls) = fmap (\sz -> TreeZ (NEStack.singleton (x, sz))) (inSeqZ i ls)

firstTreeZ :: Tree x y z -> Maybe (TreeZ x y z)
firstTreeZ = undefined

lastTreeZ :: Tree x y z -> Maybe (TreeZ x y z)
lastTreeZ = undefined

outTreeZ :: TreeZ x y z -> Maybe (Tree x y z)
outTreeZ (TreeZ ns) =
  case NEStack.pop ns of
    ((x, sz), Nothing) -> Just (Tree x (outSeqZ sz))
    _ -> Nothing

upTreeZ :: TreeZ x y z -> Maybe (TreeZ x y z)
upTreeZ = undefined

beforeTreeZ :: TreeZ x y z -> Maybe (TreeZ x y z)
beforeTreeZ = undefined

afterTreeZ :: TreeZ x y z -> Maybe (TreeZ x y z)
afterTreeZ = undefined
