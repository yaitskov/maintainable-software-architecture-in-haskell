{-# LANGUAGE TemplateHaskell #-}
module BS.Effects.CdrStore where

import           BS.Types       (AccountId)
import qualified Data.Map       as M
import           Data.UUID      (UUID)
import           Polysemy
import           Polysemy.State

data CallType = Voice | Sms

newtype Duration = Duration { unDuration :: Int }
  deriving stock (Show, Eq)
  deriving newtype (Num)

data Cdr = Cdr
  { uuid         :: UUID
  , accountId    :: AccountId
  , callType     :: CallType
  , callDuration :: Duration
  }

doubleCallDuration :: Cdr -> Cdr
doubleCallDuration cdr = cdr { callDuration = 2 * callDuration cdr }

data CdrStore m a where
  FetchCdrs :: AccountId -> CdrStore m [Cdr]

makeSem ''CdrStore

type CdrMap = M.Map AccountId [Cdr]

runCdrStore ::
     Member (State CdrMap) r
  => Sem (CdrStore ': r) a
  -> Sem r a
runCdrStore = interpret $ \case
  FetchCdrs accountId -> gets (M.! accountId)

doubleCallDurationInterceptor ::
  forall r a.
  Member CdrStore r =>
  Sem r a ->
  Sem r a
doubleCallDurationInterceptor = intercept $ \case
  FetchCdrs accountId -> do
    cdrs <- fetchCdrs accountId
    pure $ doubleCallDuration <$> cdrs

doubleNumberOfCalls :: forall r a. Member CdrStore r => Sem r a -> Sem r a
doubleNumberOfCalls = intercept $ \case
  FetchCdrs accountId -> do
    cdrs <- fetchCdrs accountId
    pure $ cdrs <> cdrs

