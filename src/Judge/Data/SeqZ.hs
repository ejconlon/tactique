-- | A zipper for 'Seq'
module Judge.Data.SeqZ
  ( SeqZ (..)
  , isFirstSeqZ
  , isLastSeqZ
  , outSeqZ
  , firstSeqZ
  , lastSeqZ
  , inSeqZ
  , beforeSeqZ
  , afterSeqZ
  ) where

import Data.Sequence (Seq (..), (><))
import qualified Data.Sequence as Seq

data SeqZ a = SeqZ !(Seq a) a !(Seq a) deriving (Eq, Show)

isFirstSeqZ :: SeqZ a -> Bool
isFirstSeqZ = undefined

isLastSeqZ :: SeqZ a -> Bool
isLastSeqZ = undefined

outSeqZ :: SeqZ a -> Seq a
outSeqZ (SeqZ ps a ns) = ps >< a :<| ns

firstSeqZ :: Seq a -> Maybe (SeqZ a)
firstSeqZ s =
  case s of
    Empty -> Nothing
    a :<| ns -> Just (SeqZ Empty a ns)

lastSeqZ :: Seq a -> Maybe (SeqZ a)
lastSeqZ s =
  case s of
    Empty -> Nothing
    ps :|> a -> Just (SeqZ ps a Empty)

inSeqZ :: Int -> Seq a -> Maybe (SeqZ a)
inSeqZ i s =
  let (ps, ans) = Seq.splitAt i s
  in case ans of
    Empty -> Nothing
    a :<| ns -> Just (SeqZ ps a ns)

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
