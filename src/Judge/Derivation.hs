module Judge.Derivation
  ( DerivError (..)
  , Evaluated (..)
  , DerivLabel (..)
  , DerivEnd
  , DerivTree
  , DerivTreeZ
  , DerivPos
  , startDeriv
  , derivGoal
  , derivZGoal
  , derivSubst
  ) where

import Control.Monad.Identity (Identity (..))
import Data.Foldable (toList)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq)
import Data.Sequence.NonEmpty (NESeq)
import Judge.Data.TreeZ (RecTree (..), Tree (..), TreeF (..), TreePos, TreeZ, readTreeZ)
import Judge.Holes (HasHoles (..))

data DerivError h j x e =
    DerivCustomError !e
  | DerivUnevalError !j
  | DerivSolveError !(NESeq (DerivPos h j x, j))
  deriving (Eq, Show)

data Evaluated = EvaluatedYes | EvaluatedNo deriving (Eq, Show)

data DerivLabel j x = DerivLabel
  { derivLabelGoal :: !j
  , derivLabelSoln :: !x
  } deriving (Eq, Show)

type DerivEnd h j x = RecTree (DerivLabel j x) h

type DerivTree h j x = Tree (DerivLabel j x) h j
type DerivTreeZ h j x = TreeZ (DerivLabel j x) h j
type DerivPos h j x = TreePos (DerivLabel j x) h

startDeriv :: j -> x -> Seq (h, j) -> DerivTree h j x
startDeriv j x hjs = Tree (TreeF (DerivLabel j x) (fmap (fmap Left) hjs))

derivGoal :: DerivTree h j x -> j
derivGoal (Tree (TreeF (DerivLabel j _) _)) = j

derivZGoal :: DerivTreeZ h j x -> (j, Evaluated)
derivZGoal tz =
  let (_, _, e) = readTreeZ tz
  in case e of
    Left j -> (j, EvaluatedNo)
    Right (Tree (TreeF (DerivLabel j _) _)) -> (j, EvaluatedYes)

derivSubst :: (Ord h, HasHoles h x) => DerivEnd h j x -> Either (NESeq h) x
derivSubst (RecTree (TreeF (DerivLabel _ x) hts)) = do
  hxs <- traverse (\(h, t) -> fmap (h,) (derivSubst t)) (toList hts)
  let hm = Map.fromList hxs
  runIdentity (trySubstHoles (\h -> Identity (Map.lookup h hm)) x)
