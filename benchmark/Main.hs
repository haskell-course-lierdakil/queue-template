{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE StandaloneDeriving, FlexibleInstances, DeriveGeneric, BangPatterns #-}
import Criterion.Main
import Control.DeepSeq
import GHC.Generics
import Data.Maybe
import Data.Foldable

import Data.Queue
import Data.Queue.Internal

deriving instance Generic (Queue a)
deriving instance Generic1 Queue
instance NFData a => NFData (Queue a)
instance NFData1 Queue

main :: IO ()
main = defaultMain [
    bgroup "push is O(1)" $ map push1 range
  , bgroup "first pop is O(n)" $ map pop1 range
  , bgroup "consequent pops are O(1)" $ map pop2 range
  , bgroup "even with additional pushes" $ map poppush range
  ]
  where
  range = [10,100,1000]
  z = empty :: Queue Int
  sized n = foldr' push z [1..n]
  push1 n = bench (show n) $ whnf (head . enqueue . push 0) (sized n)
  pop1 n  = bench (show n) $ whnf (fst . fromJust . pop) (sized n)
  pop2 n  = bench (show n) $ whnf (fst . fromJust . pop) (snd . fromJust . pop $ sized n)
  poppush n = bench (show n) $ whnf (fst . fromJust . pop) (push 0 . snd . fromJust . pop $ sized n)
