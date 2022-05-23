module Main where

import GHC.Exts.Heap
import System.Mem (performMajorGC)

import qualified Text.Regex.Base.RegexLike as Regex
import qualified Text.Regex.PCRE as Regex.PCRE

main :: IO ()
main = do
  print "regex"
  regex <- Regex.makeRegexOptsM Regex.PCRE.compCaseless Regex.PCRE.execBlank "ananas"
  print "gc"
  performMajorGC
  print "assert"
  print =<< isNF regex
  print "gc"
  performMajorGC

isNF :: a -> IO Bool
isNF x = isNFBoxed (asBox x)

isNFBoxed :: Box -> IO Bool
isNFBoxed b = do
  c <- getBoxedClosureData b
  nf <- isHNF c
  if nf
    then do
      c' <- getBoxedClosureData b
      allM isNFBoxed (allClosures c')
    else do
      return False

isHNF :: Closure -> IO Bool
isHNF c = do
  print c
  case c of
    ThunkClosure{} -> return False
    APClosure{} -> return False
    SelectorClosure{} -> return False
    BCOClosure{} -> return False
    _ -> return True

allM :: (Monad m) => (a -> m Bool) -> [a] -> m Bool
allM _ [] = return True
allM p (x : xs) = do
  q <- p x
  if q
    then allM p xs
    else return False
