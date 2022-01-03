edit config
  $ "$TESTDIR/sbu" -c config config | sed -E -e "s#$HOME#\$HOME#"
  Creating new config file at `config'.
  Use the `config' command to update default values, which are:
  
  Backup path: $HOME/sbu_backups
  Backup frequency (in minutes): 15
  Number of backups to keep: 20
  
  Backup path: $HOME/sbu_backups
  Backup frequency (in minutes): 15
  Number of backups to keep: 20
  

  $ "$TESTDIR/sbu" -c config config -p /edited -f 5 -k 6 | sed -E -e "s#$HOME#\$HOME#"
  Backup path: $HOME/sbu_backups -> /edited
  Backup frequency (in minutes): 15 -> 5
  Number of backups to keep: 20 -> 6
  

  $ "$TESTDIR/sbu" -c config config -f 15
  Backup path: /edited
  Backup frequency (in minutes): 5 -> 15
  Number of backups to keep: 6
  

  $ "$TESTDIR/sbu" -c config config
  Backup path: /edited
  Backup frequency (in minutes): 15
  Number of backups to keep: 6
  
