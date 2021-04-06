module Judge.Derivation where

import Data.Sequence (Seq (..), (><))
import qualified Data.Sequence as Seq
import Judge.NEStack (NEStack)
import qualified Judge.NEStack as NEStack

data SeqZ a = SeqZ !(Seq a) a !(Seq a) deriving (Eq, Show)

headSeqZ :: Seq a -> Maybe (SeqZ a)
headSeqZ s =
  case s of
    Empty -> Nothing
    a :<| ns -> Just (SeqZ Empty a ns)

lastSeqZ :: Seq a -> Maybe (SeqZ a)
lastSeqZ s =
  case s of
    Empty -> Nothing
    ps :|> a -> Just (SeqZ ps a Empty)

downSeqZ :: Int -> Seq a -> Maybe (SeqZ a)
downSeqZ i s =
  let (ps, ans) = Seq.splitAt i s
  in case ans of
    Empty -> Nothing
    a :<| ns -> Just (SeqZ ps a ns)

outSeqZ :: SeqZ a -> Seq a
outSeqZ (SeqZ ps a ns) = ps >< a :<| ns

beforeSeqZ :: SeqZ a -> Maybe (SeqZ a)
beforeSeqZ (SeqZ ps a ns) =
  case ps of
    Empty -> Nothing
    ps' :|> p -> Just (SeqZ ps' p (a :<| ns))

afterSeqZ :: SeqZ a -> Maybe (SeqZ a)
afterSeqZ (SeqZ ps a ns) =
  case ns of
    Empty -> Nothing
    n :<| ns' -> Just (SeqZ (ps :|> a) n ns')

data Tree x y z = Tree !x !(Seq (y, Either z (Tree x y z))) deriving (Eq, Show)

newtype TreeZ x y z = TreeZ (NEStack (x, SeqZ (y, Either z (Tree x y z)))) deriving (Eq, Show)

isEmptyTree :: Tree x y z -> Bool
isEmptyTree = undefined

isTopTreeZ :: TreeZ x y z -> Bool
isTopTreeZ = undefined

-- isHeadTreeZ = undefined
-- isLastTreeZ = undefined

downTreeZ :: Int -> Tree x y z -> Maybe (TreeZ x y z)
downTreeZ i (Tree x ls) = fmap (\sz -> TreeZ (NEStack.singleton (x, sz))) (downSeqZ i ls)

headTreeZ :: Tree x y z -> Maybe (TreeZ x y z)
headTreeZ = undefined

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

-- data DerivF h j x a = DerivF
--   { derivGoal :: !j
--   , derivSoln :: !x
--   , derivSubgoals :: !(Seq (h, a))
--   } deriving (Eq, Show, Functor, Foldable, Traversable)

-- newtype DerivTree h j x = DerivTree
--   { unDerivTree :: DerivF h j x (DerivTree h j x)
--   } deriving (Eq, Show)

-- newtype DerivCand h j x = DerivCand
--   { unDerivCand :: DerivF h j x (Either j (DerivTree h j x))
--   } deriving (Eq, Show)


-- data DerivD h a =
--     DerivRoot
--   | DerivChild !Int !h !a !(DerivD h a)
--   deriving (Eq, Show)

-- -- data DerivZ

-- --   { derivParentGoal :: !(DerivZ h j x a)
-- --   , derivPrevGoals :: !(Seq (h, a))
-- --   , derivCurHole :: !h
-- --   , derivCurGoal :: !a
-- --   , derivNextGoals :: !(Seq (h, a))
-- --   } deriving (Eq, Show)

-- -- down :: Int -> DerivF h j x a -> Maybe (DerivZ h j x a)
-- -- down i (DerivF g s sgs) = fmap (\(h, sg) -> undefined) (Seq.lookup i sgs)

-- -- up :: DerivZ h j x a -> Either (DerivZ h j x a) (Deriv)

-- -- data CandPointer h j x =
-- --     CandRoot !(Candidate h j x)
-- --   | CandSub !Int !h !j !()
-- --   deriving (Eq, Show)

-- -- data CandState h j x = CandState !j !(CandPointer h j x)

