  $ cp "$TESTDIR/config" config

list info for all groups
  $ "$TESTDIR/vbu" -c config info
  Name: another
  Path: /another/path
  Glob: save*
  
  Name: test
  Path: /test/group/path
  

list info for selected groups
  $ "$TESTDIR/vbu" -c config info another
  Name: another
  Path: /another/path
  Glob: save*
  

  $ "$TESTDIR/vbu" -c config info another new
  Name: another
  Path: /another/path
  Glob: save*
  
  \x1b[0;93mWarning: No group named `new'\x1b[0m (esc)
