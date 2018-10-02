[![Build Status](https://travis-ci.org/juhp/simple-cmd.png)](https://travis-ci.org/juhp/simple-cmd)
[![Hackage](http://img.shields.io/hackage/v/cabal-rpm.png)](http://hackage.haskell.org/package/simple-cmd)

# simple-cmd

Some simple String wrappers of `readProcess`, `readProcessWithExitCode`,
`rawSystem` from the Haskell `process` library.

## Examples

- - `cmd_ :: String -> [String] -> IO ()

`cmd_ c args :: IO ()` outputs to stdout.

```
cmd_ "git" ["clone", url]
```
It can shortened to `git_ clone [url]`.

Other examples:
```
gitBranch :: IO String
grep_ pat file :: IO Bool
sudo c args :: IO ()
```
