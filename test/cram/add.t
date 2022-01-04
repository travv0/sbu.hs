  $ cp "$TESTDIR/config" config

add groups to config
  $ "$TESTDIR/vbu" -c config add new -p "/new" -g "*"
  Group added successfully:
  
  Name: new
  Path: /new
  Glob: *
  
  $ "$TESTDIR/vbu" -c config add asdf -p "/asdf"
  Group added successfully:
  
  Name: asdf
  Path: /asdf
  