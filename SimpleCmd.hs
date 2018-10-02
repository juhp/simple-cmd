module SimpleCmd (
  cmd, cmd_,
  cmdBool,
  cmdIgnoreErr,
  cmdLines,
  cmdlog,
  cmdMaybe,
  cmdN,
  cmdQuiet,
  cmdSilent,
  cmdStdIn,
  cmdStdErr,
  egrep_, grep_,
  logMsg,
  removePrefix, removeStrictPrefix, removeSuffix,
  shell, shell_,
  sudo,
  (+-+)) where

#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,8,2))
#else
import Control.Applicative ((<$>))
#endif

import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)

import System.Exit (ExitCode (..))
import System.Process (readProcess, readProcessWithExitCode, rawSystem)

removeTrailingNewline :: String -> String
removeTrailingNewline "" = ""
removeTrailingNewline str =
  if last str == '\n'
  then init str
  else str

quoteCmd :: String -> [String] -> String
quoteCmd c args = "'" ++ unwords (c:args) ++ "'"

-- | Run a command in a process and return stdout
cmd :: String -> [String] -> IO String
cmd c args = cmdStdIn c args ""

-- | Run command in process, output goes to stdout and stderr
cmd_ :: String -> [String] -> IO ()
cmd_ c args = do
  ret <- rawSystem c args
  case ret of
    ExitSuccess -> return ()
    ExitFailure n -> error $ quoteCmd c args +-+ "failed with exit code" +-+ show n

-- | Run a command, and return Boolean status
cmdBool :: String -> [String] -> IO Bool
cmdBool c args = do
  ret <- rawSystem c args
  case ret of
    ExitSuccess -> return True
    ExitFailure _ -> return False

-- | Run a command in a process, maybe returning output if it succeeds
cmdMaybe :: String -> [String] -> IO (Maybe String)
cmdMaybe c args = do
  (ret, out, _err) <- readProcessWithExitCode c args ""
  case ret of
    ExitSuccess -> return $ Just $ removeTrailingNewline out
    ExitFailure _ -> return Nothing

-- | Run command, return list of stdout lines
-- | since 0.1.1
cmdLines :: String -> [String] -> IO [String]
cmdLines c args = lines <$> cmd c args

-- | Run a command, passing input string as stdin, and return stdout
cmdStdIn :: String -> [String] -> String -> IO String
cmdStdIn c args inp = removeTrailingNewline <$> readProcess c args inp

-- | Run a command string in a shell, and return stdout
shell :: String -> IO String
shell cs = cmd "sh" ["-c", cs]

-- | Run a command string in a shell, output goes to stdout
shell_ :: String -> IO ()
shell_ c = cmd_ "sh" ["-c", c]

-- | Log a command with a datestamp
cmdlog :: String -> [String] -> IO ()
cmdlog c args = do
  logMsg $ unwords $ c:args
  cmd_ c args

logMsg :: String -> IO ()
logMsg msg = do
  date <- cmd "date" ["+%T"]
  putStrLn $ date +-+ msg

-- | Dry-run a command: print it to stdout - more used for debugging
cmdN :: String -> [String] -> IO ()
cmdN c args = putStrLn $ unwords $ c:args

-- | Run command in a process, returning stdout and stderr
cmdStdErr :: String -> [String] -> IO (String, String)
cmdStdErr c args = do
  (_ret, out, err) <- readProcessWithExitCode c args ""
  return (removeTrailingNewline out, removeTrailingNewline err)

-- -- | Run command, if it fails output msg as error.
-- cmdAssert :: String -> String -> [String] -> IO ()
-- cmdAssert msg c args = do
--   ret <- rawSystem c args
--   case ret of
--     ExitSuccess -> return ()
--     ExitFailure _ -> error msg

-- | Run a command hiding stderr, if it succeeds return stdout
cmdQuiet :: String -> [String] -> IO String
cmdQuiet c args = do
  (ret, out, err) <- readProcessWithExitCode c args ""
  case ret of
    ExitSuccess -> return $removeTrailingNewline out
    ExitFailure n -> error $ quoteCmd c args +-+ "failed with status" +-+ show n ++ "\n" ++ err

-- | Run a command hiding stdout: stderr is only output if it fails.
cmdSilent :: String -> [String] -> IO ()
cmdSilent c args = do
  (ret, _, err) <- readProcessWithExitCode c args ""
  case ret of
    ExitSuccess -> return ()
    ExitFailure n -> error $ quoteCmd c args +-+ "failed with status" +-+ show n ++ "\n" ++ err

-- | Run a command, drop stderr, and return stdout
cmdIgnoreErr :: String -> [String] -> String -> IO String
cmdIgnoreErr c args input = do
  (_exit, out, _err) <- readProcessWithExitCode c args input
  return out

-- grep :: String -> FilePath -> IO [String]
-- grep pat file =
--   cmdLines "grep" [pat, file]

-- | grep a pattern in file and return Boolean status
grep_ :: String -> FilePath -> IO Bool
grep_ pat file =
  cmdBool "grep" ["-q", pat, file]

-- | grep for extended regexp in file, and return Boolean status
egrep_ :: String -> FilePath -> IO Bool
egrep_ pat file =
  cmdBool "grep" ["-q", "-e", pat, file]

-- | sudo a command
sudo :: String -> [String] -> IO ()
sudo c args = cmdlog "sudo" (c:args)

-- | Combine strings with a single space
infixr 4 +-+
(+-+) :: String -> String -> String
"" +-+ s = s
s +-+ "" = s
s +-+ t | last s == ' ' = s ++ t
        | head t == ' ' = s ++ t
s +-+ t = s ++ " " ++ t

-- singleLine :: String -> String
-- singleLine "" = ""
-- singleLine s = (head . lines) s

-- | Remove a prefix from a string if there
removePrefix :: String -> String-> String
removePrefix prefix orig =
  fromMaybe orig $ stripPrefix prefix orig

-- | Remove prefix, or fail with error
removeStrictPrefix :: String -> String -> String
removeStrictPrefix prefix orig =
  fromMaybe (error prefix +-+ "is not prefix of" +-+ orig) $ stripPrefix prefix orig

-- | Remove a suffix from a string if there
removeSuffix :: String -> String -> String
removeSuffix suffix orig =
  fromMaybe orig $ stripSuffix suffix orig
  where
    stripSuffix sf str = reverse <$> stripPrefix (reverse sf) (reverse str)
