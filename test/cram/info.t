  $ cp "$TESTDIR/config" config

list info for all games
  $ "$TESTDIR/sbu" -c config info
  Name: another
  Save path: /another/path
  Save glob: save*
  
  Name: test
  Save path: /test/game/path
  

list info for selected games
  $ "$TESTDIR/sbu" -c config info another
  Name: another
  Save path: /another/path
  Save glob: save*
  

  $ "$TESTDIR/sbu" -c config info another new
  Name: another
  Save path: /another/path
  Save glob: save*
  
  \x1b[0;93mWarning: No game named `new'\x1b[0m (esc)
