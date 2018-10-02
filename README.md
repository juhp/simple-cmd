[![Build Status](https://travis-ci.org/juhp/simple-cmd.png)](https://travis-ci.org/juhp/simple-cmd)
[![Hackage](http://img.shields.io/hackage/v/simple-cmd.png)](http://hackage.haskell.org/package/simple-cmd)

# simple-cmd

Some simple String wrappers of `readProcess`, `readProcessWithExitCode`,
`rawSystem` from the Haskell `process` library.

## Usage

```haskell
import SimpleCmd
```

```haskell
cmd_ :: String -> [String] -> IO ()
```
outputs to stdout. For example

```haskell
cmd_ "git" ["clone", url]
```
This can shortened to `git_ "clone" [url]`.

```haskell
cmd :: String -> [String] -> IO String
```
returns stdout as a `String`.

There are also `cmdBool`, `cmdMaybe`, `cmdList`, `shell`, and others.

Other examples:
```haskell
gitBranch :: IO String
grep_ pat file :: IO Bool
sudo c args :: IO ()
```

See the library documentation for more details.
