module SimpleCmd.Git (
  git,
  git_,
  gitBranch,
  grepGitConfig,
  isGitDir,
  rwGitDir) where

import Data.List (isPrefixOf)
import System.Directory (doesDirectoryExist)
import System.FilePath ((</>))

import SimpleCmd (cmd, cmd_, cmdLines, egrep_, removePrefix)

#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,8,2))
#else
import Control.Applicative ((<$>))
#endif

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
  removePrefix "* " . head . filter (isPrefixOf "* ") <$> cmdLines "git" ["branch"]

-- | Check if a git repo is under ssh
rwGitDir :: IO Bool
rwGitDir =
  grepGitConfig "url = (ssh://|git@)"

-- | grep ".git/config"
-- | since 0.1.1
grepGitConfig :: String -> IO Bool
grepGitConfig key = do
  gitdir <- isGitDir "."
  if gitdir
    then egrep_ key ".git/config"
    else return False
