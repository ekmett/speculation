#!/usr/bin/env runhaskell
\begin{code}
{-# LANGUAGE CPP #-}

import Control.Monad (when, unless, mplus)

import Data.Maybe (listToMaybe, fromMaybe)
import Distribution.PackageDescription     
    (PackageDescription, buildable, exeName, buildInfo, executables, customFieldsBI, BuildInfo)
import Distribution.Verbosity              (normal)
import Distribution.Simple.Build           (build)
import Distribution.Simple.LocalBuildInfo  (LocalBuildInfo(..))
import Distribution.Simple.PreProcess      (knownSuffixHandlers)
import Distribution.Simple.Program         (programFindLocation, lookupKnownProgram )
import Distribution.Simple.Setup           (defaultBuildFlags)
import Distribution.Simple 
    ( Args, defaultMainWithHooks, UserHooks(..), simpleUserHooks) 

import System.Exit       (ExitCode(..))
import System.FilePath   ( (</>), splitDirectories, isAbsolute )
import System.IO         (openFile, IOMode (..))
import System.Process 
import System.Directory 
    ( getCurrentDirectory, createDirectoryIfMissing
    , setCurrentDirectory, findExecutable, canonicalizePath
    , removeFile, doesDirectoryExist
    )

main :: IO ()
main = defaultMainWithHooks hooks

hooks :: UserHooks
hooks = simpleUserHooks { runTests = testHook } 

findHPC :: LocalBuildInfo -> IO FilePath
findHPC lbi = do
    Just hpcProgram <- return $ lookupKnownProgram "hpc" $ withPrograms lbi
    Just hpc <- programFindLocation hpcProgram normal
    return hpc
 `catch` \_ -> do
    Just hpc <- findExecutable "hpc"
    return hpc

testSpeculation :: a -> (BuildInfo -> a) -> PackageDescription -> a
testSpeculation dflt f pd = 
    fromMaybe dflt $ listToMaybe 
               [ f (buildInfo exe)
               | exe <- executables pd
               , exeName exe == "test-speculation" ]

testHook :: Args -> Bool -> PackageDescription -> LocalBuildInfo -> IO ()
testHook args0 _unknown pd lbi = do
    let args = if null args0 then [] else "-t" : args0
    -- dir <- getWorkingDirectory
    let testDir = buildDir lbi </> "test-speculation"
    baseDir <- getCurrentDirectory
    canonicalBuildDir <- canonicalizePath (buildDir lbi)
    t <- doesDirectoryExist testDir
    unless t $ do
        unless (testSpeculation False buildable pd) $ do
          fail "Reconfigure with 'cabal configure -ftests' or 'cabal install -ftests' and try again."
        putStrLn "building tests"
        build pd lbi defaultBuildFlags knownSuffixHandlers
        putStrLn "tests built"

    setCurrentDirectory testDir
    let customFields = testSpeculation [] customFieldsBI pd
        profiling = maybe False (const True) $ lookup "x-hpc" customFields

    when profiling $ do 
        removeFile "test-speculation.tix" 
        putStrLn $ "removed test-speculation.tix" 
       `catch` \_ -> return ()

    exitcode <- system $ unwords $ "test-speculation" : args
    unless (exitcode == ExitSuccess) $ 
        fail "test failed"

    when profiling $ do
      hpc <- findHPC lbi 

      exitcode <- system $ unwords $ hpc 
            : "report" 
            : "test-speculation" 
            : "--srcdir=../../.." 
            : []
      unless (exitcode == ExitSuccess) $ 
        fail "hpc report failed"

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


