# Revision history for simple-cmd

## 0.1.3.1
- sudo: ignored for root or when no sudo installed

## 0.1.3 -- 2019-02-20
- gitDiffQuiet
- fix rwGitDir regexp
- use errorWithoutStackTrace

## 0.1.2 -- 2018-10-28
- grep
- improve haddock documentation

## 0.1.1 -- 2018-10-02
- cmdLines
- Git: grepGitConfig
- new Rpm: rpmspec

## 0.1.0.0  -- 2018-09-13

- Initial release, providing:
  cmd, cmd_, cmdBool, cmdMaybe, cmdStdIn, cmdlog, cmdN,
  cmdIgnoreErr, cmdQuiet, cmdSilent, cmdStdErr,
  egrep_, grep_, logMsg,
  removePrefix, removeStrictPrefix, removeSuffix,
  shell, shell_, sudo, (+-+)
- A few git commands

# Local Variables:
# mode: text
# End:
