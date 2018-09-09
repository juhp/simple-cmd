module SimpleCmd.Git (
  git,
  git_,
  gitBranch,
  isGitDir,
  rwGitDir) where

import Data.List (isPrefixOf)
import System.Directory (doesDirectoryExist, getCurrentDirectory)
import System.FilePath ((</>))

import SimpleCmd (cmd, cmd_, egrep_, removePrefix)

-- | Run git command and return output
git :: String -> [String] -> IO String
git c args =
  cmd "git" (c:args)

-- | Run git command with output to stdout and stderr
git_ :: String -> [String] -> IO ()
git_ c args =
  cmd_ "git" (c:args)

-- | Check if directory has a .git/ dir
isGitDir :: FilePath -> IO Bool
isGitDir dir = doesDirectoryExist (dir </> ".git")

-- | Return the git branch of the current directory
gitBranch :: IO String
gitBranch =
  removePrefix "* " . head . filter (isPrefixOf "* ") . lines <$> cmd "git" ["branch"]

-- | Check if a git repo is under ssh
rwGitDir :: IO Bool
rwGitDir = do
  gitDir <- getCurrentDirectory >>= isGitDir
  if gitDir
    then egrep_ "url = (ssh://|git@)" ".git/config"
    else return False

