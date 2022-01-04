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
  Number of backups to keep: 3
  

  $ "$TESTDIR/vbu" --config config backup -v | sed -E -e 's/[0-9]+\.[0-9]+/$SECONDS/' -E -e 's/(Finished backing up [0-9]+ files? for files in \$SECONDSs on).*/\1 $DATE_AND_TIME/' -E -e 's/[^ ]+cramtests-[^/]+/$TMP_DIR/' | sort
  \x1b[0;93mWarning: Path set for another doesn't exist: /another/path\x1b[0m (esc)
  \x1b[0;93mWarning: Path set for test doesn't exist: /test/game/path\x1b[0m (esc)
  
  
      $TMP_DIR/backup.t/backups/files/a.txt
      $TMP_DIR/backup.t/backups/files/files/b.txt
      $TMP_DIR/backup.t/backups/files/files/c.txt
      $TMP_DIR/backup.t/backups/files/files/files/[0-9].txt
  $TMP_DIR/backup.t/files/a.txt ==>
  $TMP_DIR/backup.t/files/files/b.txt ==>
  $TMP_DIR/backup.t/files/files/c.txt ==>
  $TMP_DIR/backup.t/files/files/files/[0-9].txt ==>
  Finished backing up 4 files for files in $SECONDSs on $DATE_AND_TIME

  $ "$TESTDIR/vbu" --config config backup -v
  \x1b[0;93mWarning: Path set for another doesn't exist: /another/path\x1b[0m (esc)
  \x1b[0;93mWarning: Path set for test doesn't exist: /test/game/path\x1b[0m (esc)

check backup directory contents
  $ ls backups/files
  a.txt
  files

  $ ls backups/files/files
  b.txt
  c.txt
  files

  $ ls backups/files/files/files
  [0-9].txt

check versioning
  $ sleep 1 && touch files/files/b.txt && touch files/files/files/\[0-9\].txt


  $ "$TESTDIR/vbu" --config config backup -v | sed -E -e 's/[0-9]+\.[0-9]+/$SECONDS/' -E -e 's/(Finished backing up [0-9]+ files? for files in \$SECONDSs on).*/\1 $DATE_AND_TIME/' -E -e 's/[^ ]+cramtests-[^/]+/$TMP_DIR/'
  \x1b[0;93mWarning: Path set for another doesn't exist: /another/path\x1b[0m (esc)
  \x1b[0;93mWarning: Path set for test doesn't exist: /test/game/path\x1b[0m (esc)
  $TMP_DIR/backup.t/files/files/b.txt ==>
      $TMP_DIR/backup.t/backups/files/files/b.txt
  $TMP_DIR/backup.t/files/files/files/[0-9].txt ==>
      $TMP_DIR/backup.t/backups/files/files/files/[0-9].txt
  
  Finished backing up 2 files for files in $SECONDSs on $DATE_AND_TIME
  


  $ ls backups/files/files | sed -E -e 's/bak\.[0-9_]+/bak.$TIMESTAMP/'
  b.txt
  b.txt.bak.$TIMESTAMP
  c.txt
  files

check cleanup
  $ sleep 1 && touch files/files/b.txt && touch files/files/files/\[0-9\].txt


  $ "$TESTDIR/vbu" --config config backup -v | sed -E -e 's/[0-9]+\.[0-9]+/$SECONDS/' -E -e 's/(Finished backing up [0-9]+ files? for files in \$SECONDSs on).*/\1 $DATE_AND_TIME/' -E -e 's/[^ ]+cramtests-[^/]+/$TMP_DIR/'
  \x1b[0;93mWarning: Path set for another doesn't exist: /another/path\x1b[0m (esc)
  \x1b[0;93mWarning: Path set for test doesn't exist: /test/game/path\x1b[0m (esc)
  $TMP_DIR/backup.t/files/files/b.txt ==>
      $TMP_DIR/backup.t/backups/files/files/b.txt
  $TMP_DIR/backup.t/files/files/files/[0-9].txt ==>
      $TMP_DIR/backup.t/backups/files/files/files/[0-9].txt
  
  Finished backing up 2 files for files in $SECONDSs on $DATE_AND_TIME
  


  $ sleep 1 && touch files/files/b.txt && touch files/files/files/\[0-9\].txt


  $ "$TESTDIR/vbu" --config config backup -v 2>&1 | sed -E -e 's/[0-9]+\.[0-9]+/$SECONDS/' -E -e 's/(Finished backing up [0-9]+ files? for files in \$SECONDSs on).*/\1 $DATE_AND_TIME/' -E -e 's/bak\.[0-9_]+/bak.$TIMESTAMP/' -E -e 's/[^ ]+cramtests-[^/]+/$TMP_DIR/'
  \x1b[0;93mWarning: Path set for another doesn't exist: /another/path\x1b[0m (esc)
  $TMP_DIR/backup.t/files/files/b.txt ==>
      $TMP_DIR/backup.t/backups/files/files/b.txt
  \x1b[0;94mInfo: Deleting $TMP_DIR/backup.t/backups/files/files/b.txt.bak.$TIMESTAMP\x1b[0m (esc)
  $TMP_DIR/backup.t/files/files/files/[0-9].txt ==>
      $TMP_DIR/backup.t/backups/files/files/files/[0-9].txt
  \x1b[0;94mInfo: Deleting $TMP_DIR/backup.t/backups/files/files/files/[0-9].txt.bak.$TIMESTAMP\x1b[0m (esc)
  
  Finished backing up 2 files for files in $SECONDSs on $DATE_AND_TIME
  
  \x1b[0;93mWarning: Path set for test doesn't exist: /test/game/path\x1b[0m (esc)



  $ ls backups/files/files | sed -E -e 's/bak\.[0-9_]+/bak.$TIMESTAMP/'
  b.txt
  b.txt.bak.$TIMESTAMP
  b.txt.bak.$TIMESTAMP
  c.txt
  files

  $ ls backups/files/files/files | sed -E -e 's/bak\.[0-9_]+/bak.$TIMESTAMP/'
  [0-9].txt
  [0-9].txt.bak.$TIMESTAMP
  [0-9].txt.bak.$TIMESTAMP


keep all
  $ "$TESTDIR/vbu" --config config config --keep 0 | sed -E -e 's/[^ ]+cramtests-[^/]+/$TMP_DIR/'
  Backup path: $TMP_DIR/backup.t/backups
  Backup frequency (in minutes): 15
  Number of backups to keep: 3 -> 0
  

  $ sleep 1 && touch files/files/b.txt && touch files/files/files/\[0-9\].txt


  $ "$TESTDIR/vbu" --config config backup -v | sed -E -e 's/[0-9]+\.[0-9]+/$SECONDS/' -E -e 's/(Finished backing up [0-9]+ files? for files in \$SECONDSs on).*/\1 $DATE_AND_TIME/' -E -e 's/bak\.[0-9_]+/bak.$TIMESTAMP/' -E -e 's/[^ ]+cramtests-[^/]+/$TMP_DIR/'
  \x1b[0;93mWarning: Path set for another doesn't exist: /another/path\x1b[0m (esc)
  \x1b[0;93mWarning: Path set for test doesn't exist: /test/game/path\x1b[0m (esc)
  $TMP_DIR/backup.t/files/files/b.txt ==>
      $TMP_DIR/backup.t/backups/files/files/b.txt
  $TMP_DIR/backup.t/files/files/files/[0-9].txt ==>
      $TMP_DIR/backup.t/backups/files/files/files/[0-9].txt
  
  Finished backing up 2 files for files in $SECONDSs on $DATE_AND_TIME
  


  $ ls backups/files/files | sed -E -e 's/bak\.[0-9_]+/bak.$TIMESTAMP/'
  b.txt
  b.txt.bak.$TIMESTAMP
  b.txt.bak.$TIMESTAMP
  b.txt.bak.$TIMESTAMP
  c.txt
  files

  $ ls backups/files/files/files | sed -E -e 's/bak\.[0-9_]+/bak.$TIMESTAMP/'
  [0-9].txt
  [0-9].txt.bak.$TIMESTAMP
  [0-9].txt.bak.$TIMESTAMP
  [0-9].txt.bak.$TIMESTAMP
