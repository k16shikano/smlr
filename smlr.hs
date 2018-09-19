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
import Data.List (inits, tails, maximumBy, minimumBy)
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

  print $ prettyContextDiff (text fn1) (text fn2) text [smlr 20 2 lines1 lines2]
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

longestSimilar :: (ToText a) => Int -> Int -> [a] -> [a] -> Distance a
longestSimilar i d as bs = maximumBy (comparing distnum) $ similarsBy i d as bs

similarsBy :: (ToText a) => Int -> Int -> [a] -> [a] -> [Distance a]
similarsBy i d as bs =
  map (distances $ take i $ inits bs) $ take i $ inits as

  where
    distances :: (ToText a) => [[a]] -> [a] -> Distance a
    distances bss as' = case mostSimilar bss as' of
                         Just a -> tameTails a
                         Nothing -> Void
    
    mostSimilar :: (ToText a) => [[a]] -> [a] -> Maybe (Distance a)
    mostSimilar [] _ = Nothing
    mostSimilar edges a =
      Just $
      maximumBy (comparing distnum) $
      filter ((> d) . length . sndT) $
      map (measure a) edges
    
    tameTails m = let ass = fstT m
                      bss = sndT m
                      css = if (length ass) < (length bss)
                            then
                              map (measure ass) (inits bss)
                            else
                              map (flip measure bss) (inits ass)
                  in case minimumBy (comparing distnum) css of
                       Distance _ [] _ -> m
                       Distance _ _ [] -> m
                       x -> x
    
    measure ls ls' = Distance (damerauLevenshteinNormInd ls ls') ls ls'

smlr :: (Ord a, Show a, ToText a) => Int -> Int -> [a] -> [a] -> [Diff [a]]
smlr _ _ [] [] = []
smlr _ _ as [] = [First as]
smlr _ _ [] bs = [Second bs]
smlr l d as bs = case longestSimilar l d as bs of
  Void -> [First as, Second bs]
  Distance _ [] _ -> diff as bs
  Distance _ _ [] -> diff as bs
  Distance i similarA similarB
    -> let (beforeA, afterA) = splitBySimilar similarA as
           (beforeB, afterB) = splitBySimilar similarB bs
       in smlr l d beforeA beforeB ++
          diffByNeed i similarA similarB ++
          smlr l d afterA afterB
    where diffByNeed _ ass bss = if i > 0.75
                                 then diff ass bss
                                 else [First ass, Second bss]

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

jaroWinklerInd :: (ToText a) => [a] -> [a] -> Ratio Int
jaroWinklerInd a b = let as = Text.intercalate " " $ filter ((/="") . unpack) $ map toText a
                         bs = Text.intercalate " " $ filter ((/="") . unpack) $ map toText b
                     in jaroWinkler as bs

damerauLevenshteinNormInd :: (ToText a) => [a] -> [a] -> Ratio Int
damerauLevenshteinNormInd a b = let as = Text.intercalate " " $ filter ((/="") . unpack) $ map toText a
                                    bs = Text.intercalate " " $ filter ((/="") . unpack) $ map toText b
                                in damerauLevenshteinNorm as bs

{- for debug -}

smlr' :: (Ord a, Show a, ToText a) => Int -> Int -> [a] -> [a] -> String
smlr' _ _ [] [] = ""
smlr' _ _ _ [] = ""
smlr' _ _ [] _ = ""
smlr' l d as bs = case longestSimilar l d as bs of
  Void -> ""
  Distance _ [] _ -> ""
  Distance _ _ [] -> ""
  Distance i similarA similarB
    -> let (beforeA, afterA) = splitBySimilar similarA as
           (beforeB, afterB) = splitBySimilar similarB bs
       in  show i ++
               "A:" ++ show (take 100 similarA) ++ "\n====\n" ++
               "B:" ++ show (take 100 similarB)

similarities as bs = map (cmpr $ construct bs) $ tails as
  where
    cmpr :: (ToText a) => STree a -> [a] -> [(Ratio Int, ([a],[a]))]
    cmpr (Node edges) a = filter ((>0.8) . fst) $
      map (\(b,tree) -> (jaroWinklerInd a $ prefix b, (take 10 a, take 10 $ prefix b))) edges
    cmpr Leaf _ = []

