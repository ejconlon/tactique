-- | A zipper for 'Seq'
-- Does not support removing elements.
module Judge.Data.SeqZ
  ( SeqZ (..)
  , readSeqZ
  , writeSeqZ
  , modifySeqZ
  , stateSeqZ
  , isFirstSeqZ
  , isLastSeqZ
  , outSeqZ
  , firstSeqZ
  , lastSeqZ
  , inSeqZ
  , beforeSeqZ
  , afterSeqZ
  , rewindSeqZ
  , enumerateSeqZ
  ) where

import Control.Monad.Identity (Identity (..))
import Data.Bifunctor (first)
import Data.Sequence (Seq (..), (><))
import qualified Data.Sequence as Seq

data SeqZ a = SeqZ !(Seq a) !a !(Seq a) deriving (Eq, Show)

readSeqZ :: SeqZ a -> a
readSeqZ (SeqZ _ a _) = a

writeSeqZ :: a -> SeqZ a -> SeqZ a
writeSeqZ = modifySeqZ . const

modifySeqZ :: (a -> a) -> SeqZ a -> SeqZ a
modifySeqZ f = fst . runIdentity . stateSeqZ (\a -> Identity (f a, ()))

stateSeqZ :: Functor m => (a -> m (a, s)) -> SeqZ a -> m (SeqZ a, s)
stateSeqZ f (SeqZ ps a ns) = fmap (first (\a' -> SeqZ ps a' ns)) (f a)

isFirstSeqZ :: SeqZ a -> Bool
isFirstSeqZ (SeqZ ps _ _) = Seq.null ps

isLastSeqZ :: SeqZ a -> Bool
isLastSeqZ (SeqZ _ _ ns) = Seq.null ns

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

rewindSeqZ :: SeqZ a -> SeqZ a
rewindSeqZ sz@(SeqZ ps a ns) =
  case ps of
    Empty -> sz
    p :<| ps' -> SeqZ Empty p (ps' <> (a :<| ns))

enumerateSeqZ :: SeqZ a -> [SeqZ a]
enumerateSeqZ = go [] where
  go !acc sz = let acc' = sz:acc in maybe acc' (go acc') (afterSeqZ sz)
