  $ cp "$TESTDIR/config" config

add games to config
  $ "$TESTDIR/sbu" -c config add new -p "/new" -g "*"
  Game added successfully:
  
  Name: new
  Save path: /new
  Save glob: *
  
  $ "$TESTDIR/sbu" -c config add asdf -p "/asdf"
  Game added successfully:
  
  Name: asdf
  Save path: /asdf
  