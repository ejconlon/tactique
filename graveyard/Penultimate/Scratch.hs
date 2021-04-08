module Judge.Scratch where

-- import Control.Applicative (Alternative (..))
-- import Judge.Rule (RuleT)
-- import ListT (ListT)

-- newtype T j x s e m a = T { unT :: ListT m a }
--   deriving (Functor, Applicative, Monad, Alternative)

-- -- TODO something like this for subgoals
-- -- applying :: Functor m => TacticT j x s e m a -> [TacticT j x s e m a] -> TacticT j x s e m a

-- interleaving :: T j x s e m a -> T j x s e m a -> T j x s e m a
-- interleaving = undefined

-- committing :: T j x s e m a -> T j x s e m a -> T j x s e m a
-- committing = undefined

-- choosing :: (Foldable f, Monad m) => f (T j x s e m a) -> T j x s e m a
-- choosing = foldr interleaving empty

-- trying :: Monad m => T j x s e m () -> T j x s e m ()
-- trying t = t <|> pure ()

-- repeating :: Monad m => T j x s e m () -> T j x s e m ()
-- repeating t = trying (t *> repeating t)

-- goal :: T j x s e m j
-- goal = undefined

-- rule :: (j -> RuleT j x s e m x) -> T j x s e m ()
-- rule = undefined
