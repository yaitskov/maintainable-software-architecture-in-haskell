{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE TemplateHaskell #-}
-- {-# LANGUAGE OverlappingInstances #-}

module WhatIsListOutput where

import Data.Function ((&))
import Data.Maybe
import Polysemy
import Polysemy.Output
import Polysemy.Input
import Polysemy.Tagged

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

-- type OutTaggedPi = Tagged "pi" (Output Double)
outUnit :: Member (Output ()) r => Sem r ()
outUnit = output ()

incIntInput :: (Member (Input Int) r, Member (Output Int) r) => Sem r ()
incIntInput = do
  i <- input
  output $ i + 1

class FlatTuple a b where
  flatuple :: a -> b

instance FlatTuple (a, (b, c)) (a, b, c) where
  flatuple (a, (b, c)) = (a, b, c)

instance FlatTuple (a, (b, (c, d))) (a, b, c, d) where
  flatuple (a, (b, (c, d))) = (a, b, c, d)

instance FlatTuple (a, b) (a,b) where
  flatuple (a, b) = (a, b)


runAll :: IO ()
runAll = do
  -- is :: ([()], ([Double], ([Int], ())))
  is :: ([()], [Double], [Int], ()) <- flatuple <$> (mb
    & runInputConst 100
    & runOutputList @Int
    & runOutputList @Double
    & runOutputList @()
    & runLog
    & runM)
  putStrLn $ "Is = " <> show is
  where
    mb :: Members [Output Int, Output Double, Output ()] r => Sem r ()
    mb = out7 >> outPi >> outUnit