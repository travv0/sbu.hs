  $ cp "$TESTDIR/config" config

edit group
  $ "$TESTDIR/vbu" -c config edit test -p "/edited" -g none
  \x1b[0;93mWarning: Path doesn't exist: /edited\x1b[0m (esc)
  Name: test
  Path: /test/game/path -> /edited
  Glob: 
  

  $ "$TESTDIR/vbu" -c config edit test -p "/edited"
  \x1b[0;93mWarning: Path doesn't exist: /edited\x1b[0m (esc)
  Name: test
  Path: /edited
  

  $ "$TESTDIR/vbu" -c config edit another -n new -p "/edited" -g ".*"
  \x1b[0;93mWarning: Path doesn't exist: /edited\x1b[0m (esc)
  Name: another -> new
  Path: /another/path -> /edited
  Glob: save* -> .*
  

  $ "$TESTDIR/vbu" -c config edit new -g ""
  Name: new
  Path: /edited
  Glob: .* -> 
  