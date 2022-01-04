  $ cp "$TESTDIR/config" config

remove group
  $ echo n | "$TESTDIR/vbu" -c config remove test
  Permanently delete test? (y/N) 

  $ "$TESTDIR/vbu" -c config list
  another
  test

  $ echo y | "$TESTDIR/vbu" -c config remove test
  Permanently delete test? (y/N) 
  Removed test

  $ "$TESTDIR/vbu" -c config list
  another

  $ "$TESTDIR/vbu" --config config remove another -y
  Removed the following groups:
  another

  $ "$TESTDIR/vbu" --config config list
  