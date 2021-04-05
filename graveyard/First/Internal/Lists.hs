module Judge.Internal.Lists
  ( guardRight
  , unfoldSeq
  ) where

import Data.Sequence (Seq (..))
import ListT (ListT)
import qualified ListT

-- Given a selection function and a streaming list, buffers elements not selected
-- until a selected element is reached. Then returns the buffered elements, the
-- selected element and the rest of the streaming list.
findFirst :: Monad m => (a -> Either b c) -> ListT m a -> m (Seq b, Maybe (c, ListT m a))
findFirst f = go Empty where
  go !bs l = do
    m <- ListT.uncons l
    case m of
      Nothing -> pure (bs, Nothing)
      Just (hd, tl) ->
        case f hd of
          Left b -> go (bs :|> b) tl
          Right c -> pure (bs, Just (c, tl))

-- Consumes the streaming list until a 'Right' value is found. (Note that this will
-- not terminate for infinite lists!) When found, returns the whole list in order
-- with the front of the list reconstructed so as to not perform effects again.
-- If the list terminates without a 'Right' found, returns Nothing.
guardRight :: Monad m => ListT m (Either e a) -> m (Maybe (ListT m (Either e a)))
guardRight l = do
  (ls, m) <- findFirst (\e -> case e of { Left _ -> Left e; Right _ -> Right e }) l
  case m of
    Nothing -> pure Nothing
    Just (hd, tl) -> pure (Just (yieldFound ls hd tl))

-- Yields buffered start of list, middle element, and lazy tail.
yieldFound :: Monad m => Seq a -> a -> ListT m a -> ListT m a
yieldFound s hd tl =
  case s of
    Empty -> ListT.cons hd tl
    a :<| as -> ListT.cons a (yieldFound as hd tl)

-- Unfolds the entire list into a sequence (for cheap snoc).
unfoldSeq :: Monad m => ListT m a -> m (Seq a)
unfoldSeq = go Empty where
  go !acc l = do
    m <- ListT.uncons l
    case m of
      Nothing -> pure acc
      Just (hd, tl) -> go (acc :|> hd) tl
