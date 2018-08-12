{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment (getArgs)

import Data.Algorithm.Diff (Diff(..))
import Data.Algorithm.Diff.Gestalt (diff)
import Data.Algorithm.DiffContext (prettyContextDiff)
import Text.PrettyPrint.HughesPJ (text)

import Data.Algorithms.KMP (build, match)
import Data.SuffixTree (STree(..), construct, prefix)
import Data.Ratio ((%), Ratio(..))
import Data.List (inits, tails, maximumBy)
import Data.Ord (comparing)

import Data.Text as Text (pack, unpack, intercalate)
import Data.Text.Conversions (toText, ToText)
import Data.Text.Metrics 

import Debug.Trace (trace)

main = do
  fn1:fn2:_ <- getArgs

  file1 <- readFile fn1
  file2 <- readFile fn2
  
  let
    lines1 = lines file1
    lines2 = lines file2

  putStrLn $ show $ prettyContextDiff (text fn1) (text fn2) text [smlr 15 3 lines1 lines2]
--  putStrLn $ smlr' 10 9 lines1 lines2
--  putStrLn $ show $ longestSimilar 30 8 lines1 lines2
  
data Distance a = Distance (Ratio Int) [a] [a] | Void
  deriving Show

distnum :: Distance a -> Ratio Int
distnum (Distance i _ _) = i
distnum Void = 0

fstT :: Distance a -> [a]
fstT (Distance _ ls _) = ls
fstT Void = []

sndT :: Distance a -> [a]
sndT (Distance _ _ ls) = ls
sndT Void = []

longestSimilar :: (Eq a, Ord a, Show a, ToText a) => Int -> Int -> [a] -> [a] -> Distance a
longestSimilar i d as bs = maximumBy (comparing distnum) $ similarsBy i d as bs

similarsBy :: (Eq a, Ord a, Show a, ToText a) => Int -> Int -> [a] -> [a] -> [Distance a]
similarsBy i d as bs = map (distances $ take i $ inits bs) $ take i $ inits as

  where
    distances :: (Eq a, Ord a, Show a, ToText a) => [[a]] -> [a] -> Distance a
    distances bss as' = case mostSimilar bss as' of
                         Just a -> tameTails a
                         Nothing -> Void
    
    mostSimilar :: (Eq a, Show a, ToText a) => [[a]] -> [a] -> Maybe (Distance a)
    mostSimilar [] _ = Nothing
    mostSimilar edges a =
      Just $
      maximumBy (comparing distnum) $
      filter ((> d) . length . sndT) $
      map (measure a) edges
    
    tameTails m = let as = fstT m
                      bs = sndT m
                      cs = if (length as) < (length bs)
                           then
                             map (measure as) (inits bs)
                           else
                             map (flip measure bs) (inits as)
                  in case maximumBy (comparing distnum) cs of
                       Distance _ [] _ -> m
                       Distance _ _ [] -> m
                       x -> x
    
    measure ls ls' = Distance (damerauLevenshteinNormInd ls ls') ls ls'

smlr :: (Eq a, Ord a, Show a, ToText a) => Int -> Int -> [a] -> [a] -> [Diff [a]]
smlr l d [] [] = []
smlr l d as [] = [First as]
smlr l d [] bs = [Second bs]
smlr l d as bs = case longestSimilar l d as bs of
  Distance i [] _ -> diff as bs
  Distance i _ [] -> diff as bs
  Distance i similarA similarB
    -> let (beforeA, afterA) = splitBySimilar similarA as
           (beforeB, afterB) = splitBySimilar similarB bs
           
       in smlr l d beforeA beforeB ++
          diffByNeed i similarA similarB ++
          smlr l d afterA afterB

    where diffByNeed i as bs = if i > 0.95
                               then diff as bs
                               else [First as, Second bs]

splitBySimilar similar ls
  = let index = match (build similar) ls
    in case index of
         [] -> ([],[])
         [i] ->
           let 
             (before, _) = splitAt i ls
             (_ , after) = splitAt (i + length similar) ls
           in (before, after)
         otherwise -> ([],[])

{- measure distance -}

jaroWinklerInd :: (Eq a, Show a, ToText a) => [a] -> [a] -> Ratio Int
jaroWinklerInd a b = let as = Text.intercalate " " $ filter ((/="") . unpack) $map toText a
                         bs = Text.intercalate " " $ filter ((/="") . unpack) $map toText b
                     in jaroWinkler as bs

damerauLevenshteinNormInd :: (Eq a, Show a, ToText a) => [a] -> [a] -> Ratio Int
damerauLevenshteinNormInd a b = let as = Text.intercalate " " $ filter ((/="") . unpack) $map toText a
                                    bs = Text.intercalate " " $ filter ((/="") . unpack) $map toText b
                                in damerauLevenshteinNorm as bs

{- for debug -}

smlr' :: (Eq a, Ord a, Show a, ToText a) => Int -> Int -> [a] -> [a] -> String
smlr' l d [] [] = ""
smlr' l d as [] = ""
smlr' l d [] bs = ""
smlr' l d as bs = case longestSimilar l d as bs of
  Distance _ [] _ -> ""
  Distance _ _ [] -> ""
  Distance i similarA similarB
    -> let (beforeA, afterA) = splitBySimilar similarA as
           (beforeB, afterB) = splitBySimilar similarB bs
       in  (show i) ++
               "A:" ++ (show $ take 100 similarA) ++ "\n====\n" ++
               "B:" ++ (show $ take 100 similarB)

similarities as bs = map (compare $ construct bs) $ tails as
  where
    compare :: (Eq a, Show a, ToText a) => STree a -> [a] -> [(Ratio Int, ([a],[a]))]
    compare (Node edges) a = filter ((>0.8) . fst) $
      map (\(b,tree) -> (jaroWinklerInd a $ prefix b, ((take 10 a),(take 10 $ prefix b)))) edges
    compare Leaf _ = []

