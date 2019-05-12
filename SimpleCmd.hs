{-|
Some simple String wrappers of `readProcess`, `readProcessWithExitCode`,
`rawSystem` from the Haskell <https://hackage.haskell.org/package/process process> library.

Simplest is

@cmd_ :: String -> [String] -> IO ()@

which outputs to stdout. For example:

@cmd_ "git" ["clone", url]@

Then

@cmd :: String -> [String] -> IO String@

returns stdout as a @String@.

There are also @cmdBool@, @cmdMaybe@, @cmdLines@, @shell@, and others.

Other examples:

@grep_ pat file :: IO Bool@

@sudo c args :: IO ()@

-}

module SimpleCmd (
  cmd, cmd_,
  cmdBool,
  cmdIgnoreErr,
  cmdLines,
  cmdMaybe,
  cmdLog, cmdlog {-TODO: remove for 0.2 -},
  cmdN,
  cmdQuiet,
  cmdSilent,
  cmdStdIn,
  cmdStdErr,
  error',
  egrep_, grep, grep_,
  logMsg,
  removePrefix, removeStrictPrefix, removeSuffix,
  shell, shell_,
  sudo, sudo_,
  warning,
  (+-+)) where

#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,8,0))
#else
import Control.Applicative ((<$>))
#endif
import Control.Monad (when)

import Data.List (stripPrefix)
import Data.Maybe (isNothing, fromMaybe)

import System.Directory (findExecutable)
import System.Exit (ExitCode (..))
import System.IO (hPutStrLn, stderr)
import System.Posix.User (getEffectiveUserID)
import System.Process (readProcess, readProcessWithExitCode, rawSystem)

removeTrailingNewline :: String -> String
removeTrailingNewline "" = ""
removeTrailingNewline str =
  if last str == '\n'
  then init str
  else str

quoteCmd :: String -> [String] -> String
quoteCmd c args = "'" ++ unwords (c:args) ++ "'"

-- | Alias for errorWithoutStackTrace (for base >= 4.9)
--
-- @since 0.1.4
error' :: String -> a
#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,9,0))
error' = errorWithoutStackTrace
#else
error' = error
#endif

-- | 'cmd c args' runs a command in a process and returns stdout
cmd :: String -- ^ command to run
    -> [String] -- ^ list of arguments
    -> IO String -- ^ stdout
cmd c args = cmdStdIn c args ""

-- | 'cmd_ c args' runs command in a process, output goes to stdout and stderr
cmd_ :: String -> [String] -> IO ()
cmd_ c args = do
  ret <- rawSystem c args
  case ret of
    ExitSuccess -> return ()
    ExitFailure n -> error' $ quoteCmd c args +-+ "failed with exit code" +-+ show n

-- | 'cmdBool c args' runs a command, and return Boolean status
cmdBool :: String -> [String] -> IO Bool
cmdBool c args = do
  ret <- rawSystem c args
  case ret of
    ExitSuccess -> return True
    ExitFailure _ -> return False

-- | 'cmdMaybe c args' runs a command, maybe returning output if it succeeds
cmdMaybe :: String -> [String] -> IO (Maybe String)
cmdMaybe c args = do
  (ret, out, _err) <- readProcessWithExitCode c args ""
  case ret of
    ExitSuccess -> return $ Just $ removeTrailingNewline out
    ExitFailure _ -> return Nothing

-- | 'cmdLines c args' runs a command, and returns list of stdout lines
--
-- @since 0.1.1
cmdLines :: String -> [String] -> IO [String]
cmdLines c args = lines <$> cmd c args

-- | 'cmdStdIn c args inp' runs a command, passing input string as stdin, and returns stdout
cmdStdIn :: String -> [String] -> String -> IO String
cmdStdIn c args inp = removeTrailingNewline <$> readProcess c args inp

-- | 'shell cs' runs a command string in a shell, and returns stdout
shell :: String -> IO String
shell cs = cmd "sh" ["-c", cs]

-- | 'shell_ c' runs a command string in a shell, output goes to stdout
shell_ :: String -> IO ()
shell_ c = cmd_ "sh" ["-c", c]

-- | 'cmdLog c args' logs a command with a datestamp
--
-- @since 0.1.4
cmdLog :: String -> [String] -> IO ()
cmdLog c args = do
  logMsg $ unwords $ c:args
  cmd_ c args

-- | 'cmdlog' deprecated alias for 'cmdLog' (to be removed in 0.2)
cmdlog :: String -> [String] -> IO ()
cmdlog = cmdLog

-- | 'logMsg msg' outputs message with a timestamp
logMsg :: String -> IO ()
logMsg msg = do
  date <- cmd "date" ["+%T"]
  putStrLn $ date +-+ msg

-- | 'cmdN c args' dry-runs a command: prints command to stdout - more used for debugging
cmdN :: String -> [String] -> IO ()
cmdN c args = putStrLn $ unwords $ c:args

-- | 'cmdStdErr c args' runs command in a process, returning stdout and stderr
cmdStdErr :: String -> [String] -> IO (String, String)
cmdStdErr c args = do
  (_ret, out, err) <- readProcessWithExitCode c args ""
  return (removeTrailingNewline out, removeTrailingNewline err)

-- -- | 'cmdAssert msg c args' runs command, if it fails output msg as error'.
-- cmdAssert :: String -> String -> [String] -> IO ()
-- cmdAssert msg c args = do
--   ret <- rawSystem c args
--   case ret of
--     ExitSuccess -> return ()
--     ExitFailure _ -> error' msg

-- | 'cmdQuiet c args' runs a command hiding stderr, if it succeeds returns stdout
cmdQuiet :: String -> [String] -> IO String
cmdQuiet c args = do
  (ret, out, err) <- readProcessWithExitCode c args ""
  case ret of
    ExitSuccess -> return $removeTrailingNewline out
    ExitFailure n -> error' $ quoteCmd c args +-+ "failed with status" +-+ show n ++ "\n" ++ err

-- | 'cmdSilent c args' runs a command hiding stdout: stderr is only output if it fails.
cmdSilent :: String -> [String] -> IO ()
cmdSilent c args = do
  (ret, _, err) <- readProcessWithExitCode c args ""
  case ret of
    ExitSuccess -> return ()
    ExitFailure n -> error' $ quoteCmd c args +-+ "failed with status" +-+ show n ++ "\n" ++ err

-- | 'cmdIgnoreErr c args inp' runs a command with input, drops stderr, and return stdout
cmdIgnoreErr :: String -> [String] -> String -> IO String
cmdIgnoreErr c args input = do
  (_exit, out, _err) <- readProcessWithExitCode c args input
  return out

-- | 'grep pat file' greps pattern in file, and returns list of matches
--
-- @since 0.1.2
grep :: String -> FilePath -> IO [String]
grep pat file =
  cmdLines "grep" [pat, file]

-- | 'grep_ pat file' greps pattern in file and returns Boolean status
grep_ :: String -- ^ pattern
      -> FilePath -- ^ file
      -> IO Bool -- ^ result
grep_ pat file =
  cmdBool "grep" ["-q", pat, file]

-- | 'egrep_ pat file' greps extended regexp in file, and returns Boolean status
egrep_ :: String -> FilePath -> IO Bool
egrep_ pat file =
  cmdBool "grep" ["-q", "-e", pat, file]

-- | 'sudo c args' runs a command as sudo returning stdout
--
-- Result type changed from IO () to IO String in 0.2.0
sudo :: String -- ^ command
     -> [String] -- ^ arguments
     -> IO String
sudo c args = do
  uid <- getEffectiveUserID
  sd <- if uid == 0
    then return Nothing
    else findExecutable "sudo"
  let noSudo = isNothing sd
  when (uid /= 0 && noSudo) $
    warning "'sudo' not found"
  cmd (fromMaybe c sd) (if noSudo then args else c:args)

-- | 'sudo_ c args' runs a command as sudo
--
-- @since 0.2.0
sudo_ :: String -- ^ command
     -> [String] -- ^ arguments
     -> IO ()
sudo_ c args = do
  uid <- getEffectiveUserID
  sd <- if uid == 0
    then return Nothing
    else findExecutable "sudo"
  let noSudo = isNothing sd
  when (uid /= 0 && noSudo) $
    warning "'sudo' not found"
  cmdLog (fromMaybe c sd) (if noSudo then args else c:args)

-- | Combine two strings with a single space
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

-- | 'removePrefix prefix original' removes prefix from string if present
removePrefix :: String -> String-> String
removePrefix prefix orig =
  fromMaybe orig $ stripPrefix prefix orig

-- | 'removeStrictPrefix prefix original' removes prefix, or fails with error'
removeStrictPrefix :: String -> String -> String
removeStrictPrefix prefix orig =
  fromMaybe (error' prefix +-+ "is not prefix of" +-+ orig) $ stripPrefix prefix orig

-- | 'removeSuffix suffix original' removes suffix from string if present
removeSuffix :: String -> String -> String
removeSuffix suffix orig =
  fromMaybe orig $ stripSuffix suffix orig
  where
    stripSuffix sf str = reverse <$> stripPrefix (reverse sf) (reverse str)

-- | 'warning' outputs to stderr
warning :: String -> IO ()
warning = hPutStrLn stderr
