{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module WhatIsListOutput where

import Data.Function ((&))
import Data.Maybe
import Polysemy
import Polysemy.Output
import Polysemy.Input

data Log m a where
  LogInfo :: String -> Log m ()

makeSem ''Log

runLog :: Member (Embed IO) r => Sem (Log ': r) a -> Sem r a
runLog = interpret $ \case
  LogInfo s -> embed (putStrLn $ "[info] " <> s)

myBusiness :: (Members [Log, Input Int, Output Int, Output Double] r) => Sem r ()
myBusiness = do
  _7 <- out7
  outPi
  () <- incIntInput
  logInfo $ "HELLO WORLD " <> show _7

out7 :: Member (Output Int) r => Sem r ()
out7 =
  output (7 :: Int)

outPi :: Member (Output Double) r => Sem r ()
outPi = output pi

incIntInput :: (Member (Input Int) r, Member (Output Int) r) => Sem r ()
incIntInput = do
  i <- input
  output $ i + 1


runAll :: IO ()
runAll = do
  is <- fst <$> (myBusiness & runInputConst 100 & runOutputList @Int & runOutputList @Double & runLog & runM)
  putStrLn $ "Is = " <> show is