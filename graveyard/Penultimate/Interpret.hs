module Judge.Interpret
  ( Handler (..)
  , interpret
  ) where

import Judge.Internal (NatTrans (..), StatePair (..))
import Judge.Rule (RuleT (..))

data Handler j x s e a n = Handler
  { handleError :: e -> n ()
  , handleGetState :: n s
  , handlePutState :: s -> n ()
  , handleSubgoal :: j -> n x
  , handleMissing :: n x
  , handleValue :: a -> n ()
  }

interpret :: Monad n => NatTrans m n -> Handler j x s e a n -> RuleT j x s e m a -> n ()
interpret nat h = go where
  go r =
    case r of
      RuleError e -> handleError h e
      RuleState onState -> do
        s <- handleGetState h
        let StatePair s' r' = onState s
        handlePutState h s'
        go r'
      RuleEffect eff -> do
        r' <- runNatTrans nat eff
        go r'
      RuleSubgoal jdg onExt -> do
        x <- handleSubgoal h jdg
        go (onExt x)
      RuleMismatch onMis -> do
        x <- handleMissing h
        go (onMis x)
      RulePure val -> handleValue h val
