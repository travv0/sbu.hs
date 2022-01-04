  $ cp "$TESTDIR/config" config
  $ cp -r "$TESTDIR/files" files

backup files
  $ "$TESTDIR/vbu" --config config add files -p files -g "**/*.txt" | sed -E -e 's/[^ ]+cramtests-[^/]+/$TMP_DIR/'
  Group added successfully:
  
  Name: files
  Path: $TMP_DIR/backup.t/files
  Glob: **/*.txt
  


  $ "$TESTDIR/vbu" --config config config -p backups | sed -E -e 's/[^ ]+cramtests-[^/]+/$TMP_DIR/'
  Backup path: /tmp/backups -> $TMP_DIR/backup.t/backups
  Backup frequency (in minutes): 15
  Number of backups to keep: 20
  

  $ "$TESTDIR/vbu" --config config backup -v | sed -E -e 's/[0-9]+\.[0-9]+/$SECONDS/' -E -e 's/(Finished backing up [0-9]+ files? for files in \$SECONDSs on).*/\1 $DATE_AND_TIME/' -E -e 's/[^ ]+cramtests-[^/]+/$TMP_DIR/' | sort
  \x1b[0;93mWarning: Path set for another doesn't exist: /another/path\x1b[0m (esc)
  \x1b[0;93mWarning: Path set for test doesn't exist: /test/group/path\x1b[0m (esc)
  
  
      $TMP_DIR/backup.t/backups/files/a.txt
      $TMP_DIR/backup.t/backups/files/files/b.txt
      $TMP_DIR/backup.t/backups/files/files/c.txt
  $TMP_DIR/backup.t/files/a.txt ==>
  $TMP_DIR/backup.t/files/files/b.txt ==>
  $TMP_DIR/backup.t/files/files/c.txt ==>
  Finished backing up 3 files for files in $SECONDSs on $DATE_AND_TIME
