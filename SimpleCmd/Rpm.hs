module SimpleCmd.Rpm (
  rpmspec
  ) where

import SimpleCmd (cmdLines)

-- | Run rpmspec query on file with optional args, a newline is appended to any queryformat
rpmspec :: [String] -> Maybe String -> FilePath -> IO [String]
rpmspec args mqf spec = do
  let qf = maybe [] (\ q -> ["--queryformat", q ++ "\n"]) mqf
  cmdLines "rpmspec" (["-q"] ++ args ++ qf ++ [spec])
