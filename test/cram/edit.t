  $ cp "$TESTDIR/config" config

edit group
  $ "$TESTDIR/vbu" -c config edit test -p "/edited" -g none
  Name: test
  Path: /test/game/path -> /edited
  Glob: 
  

  $ "$TESTDIR/vbu" -c config edit test -p "/edited"
  Name: test
  Path: /edited
  

  $ "$TESTDIR/vbu" -c config edit another -n new -p "/edited" -g ".*"
  Name: another -> new
  Path: /another/path -> /edited
  Glob: save* -> .*
  

  $ "$TESTDIR/vbu" -c config edit new -g ""
  Name: new
  Path: /edited
  Glob: .* -> 
  