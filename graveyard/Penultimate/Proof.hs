module Judge.Proof where
  -- ( ProofPos (..)
  -- , ProofStack
  -- , ProofT (..)
  -- , Proof
  -- , runProofT
  -- ) where

import Control.Applicative (Alternative (..))
import Control.Monad (MonadPlus (..), void)
import Control.Monad.Except (ExceptT (..), MonadError (..), runExceptT)
import Control.Monad.Identity (Identity)
import Control.Monad.Logic.Class (MonadLogic (..))
import Control.Monad.Reader (MonadReader (..), ReaderT (..), asks)
import Control.Monad.State (MonadState (..), StateT (..), gets, modify')
import Control.Monad.Trans (MonadTrans (..))
import Control.Monad.Trans.Free (FreeF (..), FreeT (..), liftF)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Sequence (Seq (..))
import Judge.Internal (StatePair (..))
import Judge.Monads (F (..), Q (..), R (..), lowerQ, runF, runR, subStateR)
import Judge.Rule (RuleT (..))
import ListT (ListT (..))
import qualified ListT

data ProofPos j = ProofPos
  { proofPosIndex :: !Int
  , proofPosSubgoal :: !j
  } deriving (Eq, Show)

data InterpState s = InterpState
  { interpStateIndex :: !Int
  , interpStateThread :: !s
  } deriving (Eq, Show)

interpret :: Monad m => RuleT j x s e m a -> Q (ProofPos j) s e m x -> R (InterpState s) e m (Maybe a)
interpret r q =
  case r of
    RuleError e -> throwError e
    RuleState onState -> do
      s <- gets interpStateThread
      let StatePair s' r' = onState s
      modify' (\is -> is { interpStateThread = s' })
      interpret r' q
    RuleEffect eff -> do
      r' <- lift eff
      interpret r' q
    RuleSubgoal jdg onExt -> do
      idx <- gets interpStateIndex
      m <- subStateR interpStateThread (\s is -> is { interpStateThread = s }) (lowerQ q (ProofPos idx jdg))
      case m of
        Nothing -> pure Nothing
        Just x -> do
          let r' = onExt x
          modify' (\is -> is { interpStateIndex = succ (interpStateIndex is)})
          interpret r' q
    RuleMismatch _ -> pure Nothing
    RulePure val -> pure (Just val)

-- data TacState j s = TacState
--   { tacStateJudge :: !j
--   , tacStateThread :: !s
--   }

-- newtype TacEnv j x s e m = TacEnv
--   { tacEnvSearch :: Q (ProofPos j) s e m x
--   }

-- newtype T j x s e m a = T
--   { unT :: Q (TacEnv j x s e m) (TacState j s) e m a
--   } deriving newtype (Functor, Applicative, Monad, MonadError e, MonadState (TacState j s), MonadReader (TacEnv j x s e m))

-- rule :: Monad m => (j -> RuleT j x s e m a) -> T j x s e m a
-- rule f = do
--   q <- asks tacEnvSearch
--   jdg <- gets tacStateJudge
--   let r = f jdg
--       m = interpret r q
--   unRunQ (subStateR (InterpState 0 . tacStateThread) (\is ts -> ts { tacStateThread = interpStateThread is }) m)

data X j x a =
    XSub !j !(x -> a)
  | XMis !(x -> a)
  deriving (Functor)

data Y j x a =
    YSub !j !(x -> a)
  | YAlt a a
  | YEmpty
  deriving (Functor)

transXY :: X j x a -> Y j x a
transXY x =
  case x of
    XSub j k -> YSub j k
    XMis _ -> YEmpty

newtype D j x s e m a = D
  { unD :: F (Y j x) s e m a
  } deriving (Functor, Applicative, Monad, MonadError e, MonadState s)

instance Monad m => Alternative (D j x s e m) where
  empty = D (F (FreeT (pure (Free YEmpty))))
  D (F one) <|> D (F two) = D (F (FreeT (pure (Free (YAlt one two)))))

instance Monad m => MonadPlus (D j x s e m) where
  mzero = empty
  mplus = (<|>)

runD :: Monad m => m x -> D j x s e m a -> s -> ListT m (Either (e, s) (a, s, Seq j))
runD hole = go Empty . unD where
  go !goals f s = ListT $ do
    eas <- runF f s
    case eas of
      Left e -> pure (Just (Left (e, s), empty))
      Right (fy, s') ->
        case fy of
          Pure a -> pure (Just (Right (a, s', goals), empty))
          Free y ->
            case y of
              YSub g k -> do
                h <- hole
                ListT.uncons (go (goals :|> g) (k h) s')
              YAlt f1 f2 ->
                ListT.uncons (go Empty f1 s' <> go Empty f2 s')
              YEmpty -> pure Nothing

newtype T j x s e m a = T { unT :: ReaderT j (D j x s e m) a }
  deriving (Functor, Applicative, Monad, Alternative, MonadPlus, MonadError e)

runT :: T j x s e m a -> j -> D j x s e m a
runT = runReaderT . unT

-- | Get the current goal
goal :: Monad m => T j x s e m j
goal = T (ReaderT pure)

rule :: (j -> RuleT j x s e m x) -> T j x s e m ()
rule f = T (ReaderT (_ . f))

-- instance Monad m => Applicative (T j x s e m) where
--   pure = T . const . pure
--   (<*>) = ap

-- instance Monad m => Monad (T j x s e m) where

-- newtype Z j x s e m a = Z { unZ :: Y (X j x) s e m a }
--   deriving newtype (Functor, Applicative, Monad, MonadState s, MonadError e)

-- data V j x s e (m :: * -> *) a =
--     VDemand !(x -> V j x s e m a)
--   | VPure !a
--   deriving (Functor)

-- unfoldRule :: RuleT j x s e m a -> ListT (V j x s e m) j
-- unfoldRule = undefined

-- newtype P j s e m a = P { unP :: ListT (Q j s e m) a }
--   deriving newtype (
--     Functor, Applicative, Monad,
--     MonadError e, Alternative, MonadPlus)

-- instance Monad m => MonadState s (P j s e m) where
--   get = P (lift get)
--   put s = P (lift (put s))
--   state f = P (lift (state f))

-- instance Monad m => MonadReader j (P j s e m) where
--   ask = P (lift ask)
--   local f p = P (ListT (local f (ListT.uncons (unP p))))

-- instance Monad m => MonadLogic (P j s e m) where
--   msplit = P . ListT . fmap (fmap go) . ListT.uncons . unP where
--     go (a, t) = (Just (a, P t), empty)

-- unfoldP :: Monad m => (b -> Q j s e m (Maybe (a, b))) -> b -> P j s e m a
-- unfoldP f = P . ListT . go where
--   go b = do
--     mab <- f b
--     case mab of
--       Nothing -> pure Nothing
--       Just (a, b') -> pure (Just (a, ListT (go b')))

-- data ProofPos j = ProofPos
--   { pfEnvIndex :: !Int
--   , pfEnvSubgoal :: !j
--   } deriving (Eq, Show)

-- type ProofStack j = NEStack (ProofPos j)

-- newtype ProofT j s e m a = ProofT
--   { unProofT :: ReaderT (ProofStack j) (StateT s (ExceptT e (MaybeT m))) a
--   } deriving (Functor, Applicative, Monad,
--               MonadReader (ProofStack j), MonadError e, MonadState s,
--               Alternative, MonadPlus)

-- type Proof j s e a = ProofT j s e Identity a

-- runProofT :: ProofT j s e m a -> ProofStack j -> s -> m (Maybe (Either e (a, s)))
-- runProofT pf pe s = runMaybeT (runExceptT (runStateT (runReaderT (unProofT pf) pe) s))
