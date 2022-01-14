  $ cp "$TESTDIR/config" config

add groups to config
  $ "$TESTDIR/vbu" -c config add new -p "/new" -g "*"
  \x1b[0;93mWarning: Path doesn't exist: /new\x1b[0m (esc)
  Group added successfully:
  
  Name: new
  Path: /new
  Glob: *
  
  $ "$TESTDIR/vbu" -c config add asdf -p "/asdf"
  \x1b[0;93mWarning: Path doesn't exist: /asdf\x1b[0m (esc)
  Group added successfully:
  
  Name: asdf
  Path: /asdf
  