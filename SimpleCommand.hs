module SimpleCommand (
  cmd,
  cmd_,
  cmdBool,
  cmdIgnoreErr,
  cmdlog,
  cmdMaybe,
  cmdQuiet,
  cmdSilent,
  notNull,
  shell,
  sudo,
  (+-+)) where

#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,8,2))
#else
import Control.Applicative ((<$>))
#endif
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Process (readProcess, readProcessWithExitCode, rawSystem)

removeTrailingNewline :: String -> String
removeTrailingNewline "" = ""
removeTrailingNewline str =
  if last str == '\n'
  then init str
  else str

cmd :: String -> [String] -> IO String
cmd c args = removeTrailingNewline <$> readProcess c args ""

cmd_ :: String -> [String] -> IO ()
cmd_ c args = do
  ret <- rawSystem c args
  case ret of
    ExitSuccess -> return ()
    ExitFailure n -> error $ "\"" ++ unwords (c:args) ++ "\" failed with exit code" +-+ show n

cmdMaybe :: String -> [String] -> IO (Maybe String)
cmdMaybe c args = do
  (ret, out, _err) <- readProcessWithExitCode c args ""
  case ret of
    ExitSuccess -> return $ Just $ removeTrailingNewline out
    ExitFailure _ -> return Nothing

cmdStdErr :: String -> [String] -> IO (String, String)
cmdStdErr c args = do
  (_ret, out, err) <- readProcessWithExitCode c args ""
  return (removeTrailingNewline out, removeTrailingNewline err)

-- dry-run
cmdN :: String -> [String] -> IO ()
cmdN c args = putStrLn $ unwords $ c:args

cmdAssert :: String -> String -> [String] -> IO ()
cmdAssert msg c args = do
  ret <- rawSystem c args
  case ret of
    ExitSuccess -> return ()
    ExitFailure _ -> error msg

cmdlog :: String -> [String] -> IO ()
cmdlog c args = do
  logMsg $ unwords $ c:args
  cmd_ c args

logMsg :: String -> IO ()
logMsg msg = do
  date <- cmd "date" ["+%T"]
  putStrLn $ date +-+ msg

cmdBool :: String -> [String] -> IO Bool
cmdBool c args = do
  ret <- rawSystem c args
  case ret of
    ExitSuccess -> return True
    ExitFailure _ -> return False

sudo :: String -> [String] -> IO ()
sudo c args = cmdlog "sudo" (c:args)

shell :: String -> IO String
shell c = cmd "sh" ["-c", c]


-- hide stderr
cmdQuiet :: String -> [String] -> IO String
cmdQuiet c args = do
  (ret, out, err) <- readProcessWithExitCode c args ""
  case ret of
    ExitSuccess -> return $removeTrailingNewline out
    ExitFailure n -> error $ "'" ++ unwords (c:args) ++ "'" +-+ "failed with status" +-+ show n ++ "\n" ++ err

-- hide stdout
cmdSilent :: String -> [String] -> IO ()
cmdSilent c args = do
  (ret, _, err) <- readProcessWithExitCode c args ""
  case ret of
    ExitSuccess -> return ()
    ExitFailure n -> error $ "'" ++ unwords (c:args) ++ "'" +-+ "failed with status" +-+ show n ++ "\n" ++ err

cmdIgnoreErr :: FilePath -> [String] -> String -> IO String
cmdIgnoreErr c args input = do
  (_exit, out, _err) <- readProcessWithExitCode c args input
  return out

infixr 4 +-+
(+-+) :: String -> String -> String
"" +-+ s = s
s +-+ "" = s
s +-+ t | last s == ' ' = s ++ t
        | head t == ' ' = s ++ t
s +-+ t = s ++ " " ++ t

#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,8,2))
notNull :: [a] -> Bool
#else
notNull :: Foldable t => t a -> Bool
#endif
notNull = not . null

-- singleLine :: String -> String
-- singleLine "" = ""
-- singleLine s = (head . lines) s
