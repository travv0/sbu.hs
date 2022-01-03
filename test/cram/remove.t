  $ cp "$TESTDIR/config" config

remove game
  $ echo n | "$TESTDIR/sbu" -c config remove test
  Permanently delete test? (y/N) 

  $ "$TESTDIR/sbu" -c config list
  another
  test

  $ echo y | "$TESTDIR/sbu" -c config remove test
  Permanently delete test? (y/N) 
  Removed test

  $ "$TESTDIR/sbu" -c config list
  another

  $ "$TESTDIR/sbu" --config config remove another -y
  Removed the following games:
  another

  $ "$TESTDIR/sbu" --config config list
  