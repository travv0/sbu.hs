  $ cp "$TESTDIR/config" config

edit game
  $ "$TESTDIR/sbu" -c config edit test -p "/edited" -g none
  Name: test
  Save path: /test/game/path -> /edited
  Save glob: 
  

  $ "$TESTDIR/sbu" -c config edit test -p "/edited"
  Name: test
  Save path: /edited
  

  $ "$TESTDIR/sbu" -c config edit another -n new -p "/edited" -g ".*"
  Name: another -> new
  Save path: /another/path -> /edited
  Save glob: save* -> .*
  

  $ "$TESTDIR/sbu" -c config edit new -g ""
  Name: new
  Save path: /edited
  Save glob: .* -> 
  