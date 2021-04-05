module Judge.Tactic
  ( TacticT (..)
  , Tactic
  -- , rule
  ) where

import Control.Monad (void)
import Control.Monad.Except (MonadError (..), ExceptT (..))
import Control.Monad.Identity (Identity)
import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Control.Monad.State (MonadState (..), StateT (..), gets, modify')
-- import Control.Monad.Morph (MFunctor (..))
import Control.Monad.Trans (MonadTrans (..))
import Judge.Rule (RuleT)
import Judge.Proof (ProofT, ProofStack)
import Judge.Interpret (Handler (..))

data TacticState j s = TacticState
  { tacStateStack :: !(ProofStack j)
  , tacStatePos :: !Int
  , tacStateBack :: !s
  } deriving (Eq, Show)

newtype TacticT j x s e m a = TacticT
  { unTacticT :: ReaderT (ProofT j s e m x) (ExceptT e (StateT (TacticState j s) m)) a
  } deriving (Functor, Applicative, Monad,
              MonadReader (ProofT j s e m x), MonadError e, MonadState (TacticState j s))

type Tactic j x s e a = TacticT j x s e Identity a

tacSubgoal :: j -> TacticT j x s e m x
tacSubgoal =

tacHandler :: Monad m => Handler j x s e a (TacticT j x s e m)
tacHandler = Handler
  { handleError = void . throwError
  , handleGetState = gets tacStateBack
  , handlePutState = \s -> modify' (\ts -> ts { tacStateBack = s })
  , handleSubgoal = \j -> Tacti

      ts@(TacticState stack pos back) <- get
      let stack' = stack -- TODO push


  , handleMissing = undefined
  , handleValue = undefined
  }



-- instance MonadTrans (TacticT j x e s) where
--   lift = TacticT . lift . lift . lift

-- instance MFunctor (TacticT j e s) where
--   hoist trans = TacticT . hoist trans . unTacticT

-- rule :: Functor m => (j -> RuleT j x s e m x) -> TacticT j x s e m ()
-- rule onJdg = tactic (\j -> convertRule (onJdg j) j)
