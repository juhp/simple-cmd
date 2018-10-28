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

-- | 'git c args' runs git command and return output
git :: String -- ^ git command
    -> [String] -- ^ arguments
    -> IO String -- ^ output
git c args =
  cmd "git" (c:args)

-- | 'git_ c args' run git command with output to stdout and stderr
git_ :: String -> [String] -> IO ()
git_ c args =
  cmd_ "git" (c:args)

-- | 'isGitDir dir' checks if directory has a .git/ subdir
isGitDir :: FilePath -> IO Bool
isGitDir dir = doesDirectoryExist (dir </> ".git")

-- | 'gitBranch' returns the git branch of the current directory
gitBranch :: IO String
gitBranch =
  removePrefix "* " . head . filter (isPrefixOf "* ") <$> cmdLines "git" ["branch"]

-- | 'rwGitDir' checks if a git repo is under ssh
rwGitDir :: IO Bool
rwGitDir =
  grepGitConfig "url = (ssh://|git@)"

-- | 'grepGitConfig pat' greps ".git/config" for extended regexp
--
-- @since 0.1.1
grepGitConfig :: String -> IO Bool
grepGitConfig key = do
  gitdir <- isGitDir "."
  if gitdir
    then egrep_ key ".git/config"
    else return False
