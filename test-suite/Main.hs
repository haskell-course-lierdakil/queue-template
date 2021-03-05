import Test.Tasty
import Test.Tasty.Hspec
import Test.Tasty.QuickCheck
import Data.Function
import Data.Maybe
import Data.Foldable

import Data.Queue

main :: IO ()
main = do
    test <- testSpec "Spec" spec
    defaultMain $ testGroup "Test" [test, check]

spec :: Spec
spec = parallel $ do
    it "pop empty == Nothing" $ do
      pop (empty :: Queue Int) `shouldBe` Nothing
    it "keeps fifo order" $ do
      let q1 = empty :: Queue Int
          q2 = push 1 q1
          q3 = push 2 q2
      Just (x1, q4) <- pure $ pop q3
      let q5 = push 3 q4
      Just (x2, q6) <- pure $ pop q5
      Just (x3, q7) <- pure $ pop q6
      (x1, x2, x3) `shouldBe` (1, 2, 3)
      pop q7 `shouldBe` Nothing

check :: TestTree
check = testProperties "QuickCheck" [
    ("push x, pop == Just (x, empty)", property pushpop)
  , ("push x, push y, push z, pop, pop == push y, push z, pop", property pushpushpop)
  , ("push x, push y, ..., pop, ..., pop => [x,y,...]", property fifo)
  , ("push x, push y, ..., pop, ..., pop => [x,y,...]", property fifoInterleaved)
  ]
  where
  pop' = snd . fromJust . pop
  pushpop x = (z & push x & pop) == Just (x, z)
  pushpushpop x y w = (z
      & push x & push y & push w & pop' & pop'
      ) == (z & push y & push w & pop')
  fifo xs = pops [] (foldr push z xs) == xs
  z = empty :: Queue Int
  pops acc q = case pop q of
                Just (x, q') -> pops (x:acc) q'
                Nothing -> acc
  fifoInterleaved (Positive (Small n))
    = reverse (uncurry pops (foldl' go ([], z) [1..n])) == concat [ [1..i] | i <- [1..n]]
    where go (xs, q) i = let q' = foldr push q $ reverse [1..i]
                             Just (x, q'') = pop q'
                         in (x:xs, q'')
