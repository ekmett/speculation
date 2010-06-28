#!/usr/bin/env runhaskell
\begin{code}
{-# LANGUAGE CPP #-}

import System.Exit (ExitCode(..))
import Control.Monad (unless, mplus)
import Distribution.Simple.LocalBuildInfo ( LocalBuildInfo(..) )
import Distribution.Simple.Program (programFindLocation, lookupKnownProgram )
import Distribution.Verbosity (normal)
import Distribution.Simple (defaultMainWithHooks, UserHooks(..), simpleUserHooks) 
import System.IO (openFile, IOMode (..))
import System.FilePath ( (</>), splitDirectories, isAbsolute )
import System.Process 
import System.Directory (getCurrentDirectory, createDirectoryIfMissing, setCurrentDirectory, findExecutable, canonicalizePath)

main = defaultMainWithHooks simpleUserHooks { runTests = testHook }

findHPC :: LocalBuildInfo -> IO FilePath
findHPC lbi = do
    Just hpcProgram <- return $ lookupKnownProgram "hpc" $ withPrograms lbi
    Just hpc <- programFindLocation hpcProgram normal
    return hpc
 `catch` \e -> do
    Just hpc <- findExecutable "hpc"
    return hpc

testHook args0 _ _ lbi = do
    let args = if null args0 then [] else "-t" : args0
    -- dir <- getWorkingDirectory
    let testDir = buildDir lbi </> "test-speculation"
    baseDir <- getCurrentDirectory
    canonicalBuildDir <- canonicalizePath (buildDir lbi)
    setCurrentDirectory testDir
    exitcode <- system $ unwords $ "test-speculation" : args
    unless (exitcode == ExitSuccess) $ 
        fail "test failed"
    hpc <- findHPC lbi 
    exitcode <- system $ unwords $ hpc 
            : "report" 
            : "test-speculation" 
            : "--srcdir=../../.." 
            : []
    let markupDir base = base </> "doc" </> "html" </> "test-speculation"
    createDirectoryIfMissing True (markupDir canonicalBuildDir)
    exitcode <- system $ unwords $ hpc 
            : "markup" 
            : "test-speculation"
            : "--srcdir=../../.." 
            : ("--destdir=" ++ markupDir canonicalBuildDir) 
            : "--exclude=Main" 
            : []
    unless (exitcode == ExitSuccess) $ 
        fail "hpc report failed"
    putStrLn $ "Code coverage created: " ++ (markupDir (buildDir lbi) </> "hpc_index.html")
\end{code}
