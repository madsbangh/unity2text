module Install where

import Data.List (isInfixOf)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist)
import System.Exit (exitFailure)
import System.IO (readFile')

activate :: IO ()
activate = do
  ensureIsInRepoRoot >> ensureInstalled >> ensureNotActivated >> activate'
  where
    ensureInstalled :: IO ()
    ensureInstalled = do
      installed <- doesDirectoryExist ".git/unity2text"
      if installed
        then mempty
        else do
          putStrLn "Not installed. Aborting."
          exitFailure
    ensureNotActivated = do
      config <- readFile' ".git/config"
      let activated = injectedConfig `isInfixOf` config
      if activated
        then do
          putStrLn "Already activated. Aborting."
          exitFailure
        else putStrLn "Not activated."
    activate' = do
      putStrLn "Activating..."
      installGitConfig
      putStrLn "Done."

install :: IO ()
install = do
  ensureIsInRepoRoot >> ensureNotInstalled >> install'
  where
    ensureNotInstalled = do
      installed <- doesDirectoryExist ".git/unity2text"
      if installed
        then do
          putStrLn "Already installed. Aborting."
          exitFailure
        else putStrLn "No existing install found."
    install' = do
      putStrLn "Installing..."
      installGitConfig
      installGitAttributes
      putStrLn "Done. Suggestion: Build asset-to-name lookup with --rebuild-lookup"

ensureIsInRepoRoot :: IO ()
ensureIsInRepoRoot = do
  isInRepoRoot <- doesDirectoryExist ".git"
  if isInRepoRoot
    then mempty
    else do
      putStrLn "Not in repo root. Aborting."
      exitFailure

installGitConfig :: IO ()
installGitConfig = do
  putStrLn "    Updating local .git/config file..."
  appendFile ".git/config" injectedConfig

installGitAttributes :: IO ()
installGitAttributes = do
  putStrLn "    Adding local .git/unity2text/attributes file..."
  createDirectoryIfMissing False ".git/unity2text"
  writeFile ".git/unity2text/attributes" $ "\n# unity2text\n" ++ foldMap (++ " diff=unity\n") unityAssetPatterns

unityAssetPatterns :: [String]
unityAssetPatterns =
  map
    ("*." ++)
    [ "unity",
      "prefab",
      "anim",
      "asset",
      "controller",
      "mask",
      "mat",
      "mixer",
      "overrideController",
      "physicMaterial",
      "physicsMaterial2D",
      "playable",
      "preset",
      "shadervariants",
      "spriteatlas"
    ]

injectedConfig :: String
injectedConfig =
  "\n[diff \"unity\"]\n\ttextconv = unity2text"
    ++ "\n[core]\n\tattributesfile = .git/unity2text/attributes"