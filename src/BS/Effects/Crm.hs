{-# LANGUAGE TemplateHaskell #-}
module BS.Effects.Crm where

import           BS.Types       (AccountId, Address, Cent, upCaseCity)
import qualified Data.Map       as M
import           Data.Text
import           Polysemy
import           Polysemy.State


data Plan = Plan
  { voiceCost :: Cent
  , smsCost   :: Cent
  }

data Profile = Profile
  { firstName :: Text
  , lastName  :: Text
  , address   :: Address
  , plan      :: Plan
  }

data Crm m a where
  GetProfile :: AccountId -> Crm m Profile

makeSem ''Crm

type CrmMap = M.Map AccountId Profile

runCrm ::
     Member (State CrmMap) r
  => Sem (Crm ': r) a
  -> Sem r a
runCrm = interpret $ \case
  GetProfile accountId -> gets (M.! accountId)

upCasingAddressInterceptor ::
  forall r a.
  Member Crm r =>
  Sem r a ->
  Sem r a
upCasingAddressInterceptor =
  intercept $ \case
    GetProfile accountId -> do
      pro <- getProfile accountId
      pure pro { address = upCaseCity $ address pro }
