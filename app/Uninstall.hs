{-# LANGUAGE OverloadedStrings #-}

module Uninstall where

import Data.List (isInfixOf)
import Data.Text (pack, replace, unpack)
import Install (ensureIsInRepoRoot, injectedConfig)
import System.Directory (doesDirectoryExist, removeDirectoryRecursive, renameFile)
import System.Exit (exitFailure)
import System.IO (readFile')
import System.IO.Temp (writeSystemTempFile)

deactivate :: IO ()
deactivate = do
  ensureIsInRepoRoot >> ensureIsInstalled >> ensureIsActivated >> deactivate'
  where
    ensureIsActivated = do
      config <- readFile' ".git/config"
      let activated = injectedConfig `isInfixOf` config
      if activated
        then putStrLn "Was activated."
        else do
          putStrLn "Already deactivated. Aborting."
          exitFailure
    deactivate' = do
      putStrLn "Deactivating..."
      uninstallGitConfig
      putStrLn "Done."

uninstall :: IO ()
uninstall = do
  ensureIsInRepoRoot >> ensureIsInstalled >> uninstall'
  where
    uninstall' = do
      putStrLn "Uninstalling..."
      uninstallGitConfig
      uninstallIntalUnity2TextGitDir
      putStrLn "Done."

ensureIsInstalled :: IO ()
ensureIsInstalled = do
  installed <- doesDirectoryExist ".git/unity2text"
  if installed
    then putStrLn "Install found."
    else do
      putStrLn "Not installed. Aborting."
      exitFailure

uninstallGitConfig :: IO ()
uninstallGitConfig = do
  putStrLn "    Updating local .git/config file..."
  let configFile = ".git/config"
  content <- readFile configFile
  let content' = replace (pack injectedConfig) "" $ pack content
  tmp <- writeSystemTempFile "unity2text-config" $ unpack content'
  renameFile tmp configFile

uninstallIntalUnity2TextGitDir :: IO ()
uninstallIntalUnity2TextGitDir = do
  putStrLn "    Removing local .git/unity2text directory..."
  removeDirectoryRecursive ".git/unity2text"