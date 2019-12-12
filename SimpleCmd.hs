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
  cmdIgnoreErr, {- badly named -}
  cmdLines,
  cmdMaybe,
  cmdLog, cmdlog {-TODO: remove for 0.3 -},
  cmdN,
  cmdQuiet,
  cmdSilent,
  cmdStdIn,
  cmdStdErr,
  cmdTry_,
  error',
  egrep_, grep, grep_,
  ifM,
  logMsg,
  needProgram,
  removePrefix, removeStrictPrefix, removeSuffix,
  shell, shell_,
  shellBool,
  sudo, sudo_,
  warning,
  PipeCommand,
  pipe, pipe_, pipeBool,
  pipe3_, pipeFile_,
  whenM,
  (+-+)) where

#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,8,0))
#else
import Control.Applicative ((<$>))
#endif
import Control.Monad

import Data.List (stripPrefix)
import Data.Maybe (isJust, isNothing, fromMaybe)

import System.Directory (findExecutable)
import System.Exit (ExitCode (..))
import System.IO (hGetContents, hPutStrLn, IOMode(ReadMode), stderr, withFile)
import System.Posix.User (getEffectiveUserID)
import System.Process (createProcess, proc, ProcessHandle, rawSystem, readProcess,
                       readProcessWithExitCode, runProcess, showCommandForUser,
                       std_in, std_out, StdStream(CreatePipe, UseHandle),
                       waitForProcess, withCreateProcess)

removeTrailingNewline :: String -> String
removeTrailingNewline "" = ""
removeTrailingNewline str =
  if last str == '\n'
  then init str
  else str

quoteCmd :: String -> [String] -> String
quoteCmd = showCommandForUser

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

boolWrapper :: IO ExitCode -> IO Bool
boolWrapper pr = do
  ret <- pr
  case ret of
    ExitSuccess -> return True
    ExitFailure _ -> return False

-- | 'cmdBool c args' runs a command, and return Boolean status
cmdBool :: String -> [String] -> IO Bool
cmdBool c args =
  boolWrapper (rawSystem c args)

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

-- | 'shell_ cs' runs a command string in a shell, output goes to stdout
shell_ :: String -> IO ()
shell_ cs = cmd_ "sh" ["-c", cs]

-- | 'shellBool cs' runs a command string in a shell, output goes to stdout
--
-- @since 0.2.0
shellBool :: String -> IO Bool
shellBool cs =
  boolWrapper (rawSystem "sh" ["-c", cs])

-- | 'cmdLog c args' logs a command with a datestamp
--
-- @since 0.1.4
cmdLog :: String -> [String] -> IO ()
cmdLog c args = do
  logMsg $ unwords $ c:args
  cmd_ c args

-- | 'cmdlog' deprecated alias for 'cmdLog' (will be removed in 0.3)
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

-- | 'cmdTry_ c args' runs the command if available
--
-- @since 0.2.1
cmdTry_ :: String -> [String] -> IO ()
cmdTry_ c args = do
  have <- findExecutable c
  when (isJust have) $
    cmd_ c args

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
sudo = sudoInternal cmd

-- | 'sudo_ c args' runs a command as sudo
--
-- @since 0.2.0
sudo_ :: String -- ^ command
     -> [String] -- ^ arguments
     -> IO ()
sudo_ = sudoInternal cmdLog

sudoInternal :: (String -> [String] -> IO a) -> String -> [String] -> IO a
sudoInternal exc c args = do
  uid <- getEffectiveUserID
  sd <- if uid == 0
    then return Nothing
    else findExecutable "sudo"
  let noSudo = isNothing sd
  when (uid /= 0 && noSudo) $
    warning "'sudo' not found"
  exc (fromMaybe c sd) (if noSudo then args else c:args)

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
--
-- @since 0.2.0
warning :: String -> IO ()
warning = hPutStrLn stderr


-- | Type alias for a command in a pipe
--
-- @since 0.2.0
type PipeCommand = (String,[String])

-- | Return stdout from piping the output of one process to another
--
-- @since 0.2.0
pipe :: PipeCommand -> PipeCommand -> IO String
pipe (c1,args1) (c2,args2) =
  withCreateProcess ((proc c1 args1) { std_out = CreatePipe }) $
    \ _si (Just ho1) _se p1 -> do
      (_, Just ho2, _, p2) <- createProcess ((proc c2 args2) {std_in = UseHandle ho1, std_out = CreatePipe})
      out <- hGetContents ho2
      void $ waitForProcess p1
      void $ waitForProcess p2
      return out

-- | Pipe two commands without returning anything
--
-- @since 0.2.0
pipe_ :: PipeCommand -> PipeCommand -> IO ()
pipe_ (c1,args1) (c2,args2) =
  void $ pipeInternal (c1,args1) (c2,args2) >>= waitForProcess

-- | Bool result of piping of commands
--
-- @since 0.2.0
pipeBool :: PipeCommand -> PipeCommand -> IO Bool
pipeBool (c1,args1) (c2,args2) =
  boolWrapper $ pipeInternal (c1,args1) (c2,args2) >>= waitForProcess

pipeInternal :: PipeCommand -> PipeCommand -> IO ProcessHandle
pipeInternal (c1,args1) (c2,args2) =
  -- nicer with process-typed:
  -- withProcess_ (setStdout createPipe proc1) $ \ p -> runProcess (setStdin (useHandleClose (getStdout p)) proc2)
  withCreateProcess ((proc c1 args1) { std_out = CreatePipe }) $
    \ _si so _se p1 -> do
      p2 <- runProcess c2 args2 Nothing Nothing so Nothing Nothing
      void $ waitForProcess p1
      return p2

-- | Pipe 3 commands, no returning anything
--
-- @since 0.2.0
pipe3_ :: PipeCommand -> PipeCommand -> PipeCommand -> IO ()
pipe3_ (c1,a1) (c2,a2) (c3,a3) =
  withCreateProcess ((proc c1 a1) { std_out = CreatePipe }) $
  \ _hi1 (Just ho1) _he1 p1 ->
    withCreateProcess ((proc c2 a2) {std_in = UseHandle ho1, std_out = CreatePipe}) $
    \ _hi2 ho2 _he2 p2 -> do
      p3 <- runProcess c3 a3 Nothing Nothing ho2 Nothing Nothing
      void $ waitForProcess p1
      void $ waitForProcess p2
      void $ waitForProcess p3

-- | Pipe a file to the first of a pipe of commands
--
-- @since 0.2.0
pipeFile_ :: FilePath -> PipeCommand -> PipeCommand -> IO ()
pipeFile_ infile (c1,a1) (c2,a2) =
  withFile infile ReadMode $
  \ hin ->
    withCreateProcess ((proc c1 a1) { std_in = UseHandle hin, std_out = CreatePipe }) $
    \ _si so _se p1 -> do
      p2 <- runProcess c2 a2 Nothing Nothing so Nothing Nothing
      void $ waitForProcess p1
      void $ waitForProcess p2

-- | Monadic if
-- @ifM test actTrue actFalse@
-- (taken from protolude)
--
-- @since 0.2.1
ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM p x y = p >>= \b -> if b then x else y

-- | Monadic when
-- @whenM test action@
--
-- @since 0.2.1
whenM :: Monad m => m Bool -> m () -> m ()
whenM p x = p >>= \b -> when b x

-- | Assert program in PATH
-- @needProgram progname@
--
-- @since 0.2.1
needProgram :: String -> IO ()
needProgram prog = do
  mx <- findExecutable prog
  unless (isJust mx) $ error' $ "program needs " ++ prog
