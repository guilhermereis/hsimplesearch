{-# LANGUAGE DoAndIfThenElse #-}
-----------------------------------------------------------------------------
--
-- Module      :  Search.Indexer
-- Copyright   :  Francisco Soares
-- License     :  GPL (Just (Version {versionBranch = [2], versionTags = []}))
--
-- Maintainer  :  Francisco Soares
-- Stability   :  experimental
-- Portability :
--
-----------------------------------------------------------------------------

module Search.Indexer (
    indexFile,
    threadIndexFile,
    process
) where

import Search.Utils

import System.Directory  (getDirectoryContents, doesDirectoryExist)
import Data.Char         (isAlphaNum, toLower)
import Data.List         (groupBy, sort, isSuffixOf)
import qualified Control.Exception                                 as Exc
import System.FilePath
import Control.Concurrent

-- | Front for the indexing 
indexFile :: FilePath -> IO [(String, [(String, [Integer])])]
indexFile filename = do
        indexed <- indexFile' filename
        let x = sort indexed    
        let x' = groupBy eqFst x
        return $! toMap2 x'

-- | Performs the actual indexing, organizing how subdirectories are recursively processed 
-- and how text files are interpreted and indexed, all based on a root FilePath.
process:: MVar String -> MVar [(String, [(String, [Integer])])] -> IO ()
process mx mR = do
              filename <- takeMVar mx  
              contents <- Prelude.readFile filename :: IO String
              let contents' = map toLower contents
              let splitString = breakInto contents' isDesirableChar (/='-')
              putMVar mR $! toWordFileMap' filename $ toMap $ sort $ toPosition splitString  




threadIndexFile :: MVar String -> MVar String -> MVar String -> MVar String -> MVar [(String, [(String, [Integer])])] -> FilePath -> IO [(String, [(String, [Integer])])]
threadIndexFile m1 m2 m3 m4 mR filename = do
        existence <- doesDirectoryExist filename
        if existence then do
            subfiles <- getDirectoryContents filename
            let cleanSubfiles = filter (not . (`elem` [".",".."])) subfiles
            let realPathSubfiles = map (filename </>) cleanSubfiles

        

            
            results <- mapM indexFile realPathSubfiles
            return $! concat results


        else
            if takeExtension filename == ".txt" then do
                b1 <- tryPutMVar m1 filename
                if (b1) 
                then return [("lala", [("blabla",[1,2,3] )])]
                else do 
                        b2 <- tryPutMVar m2 filename
                        if (b2) 
                        then return [("lala", [("blabla",[1,2,3] )])]
                        else do b3 <- tryPutMVar m3 filename
                                if (b3) then return [("lala", [("blabla",[1,2,3] )])]
                                        else do 
                                                putMVar m4 filename
                                                return [("lala", [("blabla",[1,2,3] )])]
                takeMVar mR     
                

            else return [] 



indexFile' :: FilePath -> IO [(String, [(String, [Integer])])]
indexFile' filename = do
        existence <- doesDirectoryExist filename
        if existence then do
            subfiles <- getDirectoryContents filename
            let cleanSubfiles = filter (not . (`elem` [".",".."])) subfiles
            let realPathSubfiles = map (filename </>) cleanSubfiles
            results <- mapM indexFile realPathSubfiles
            return $! concat results
        else
            if takeExtension filename == ".txt" then do
                contents <- Prelude.readFile filename :: IO String
                let contents' = map toLower contents
                let splitString = breakInto contents' isDesirableChar (/='-')
                return $! toWordFileMap' filename $ toMap $ sort $ toPosition splitString        
            else return []


-- | returns a mapping from a filepath to the list of words contained in it, along with their positions.
toWordFileMap :: FilePath -> [(String, [Integer])] -> [(FilePath, [(String, [Integer])])]
toWordFileMap filename wordMap = [(filename, wordMap)]

-- | returns a mapping from a word to the list of filepaths it's contained in, along with the position 
-- where it's found in them.
toWordFileMap' :: FilePath -> [(String, [Integer])] -> [(String, [(FilePath, [Integer])])]
toWordFileMap' filename wordMap = map (wordInFile filename) wordMap where
        wordInFile :: FilePath -> (String,[Integer]) -> (String, [(FilePath, [Integer])])
        wordInFile filename (word, positions) = (word, [(filename, positions)])


-- == Utility functions ==

toPosition :: [a] -> [(a, Integer)]
toPosition list = toPosition' list 0 where
        toPosition' [] _        = []
        toPosition' (h:t) index = (h, index): toPosition' t (index+1)


toMap :: Eq a => [(a, b)] -> [(a, [b])]
toMap list = toMap' $ groupBy eqFst list
        

toMap' :: [[(a, b)]] -> [(a, [b])]
toMap' = map (\ h -> ((fst . head) h, map snd h))

toMap2 :: [[(a, [b])]] -> [(a, [b])]
toMap2 = map messInside

-- | assuming the first elements are all equal
messInside ::  [(a,[b])] -> (a, [b])
messInside complicatedMap = ((fst.head) complicatedMap, concatMap snd complicatedMap)
