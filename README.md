# sbu
Backup your saves when Steam won't do it for you.

This is a tool for easily backing up game saves to cloud storage or a network drive when the game doesn't support cloud saves.

Start by setting the location you want your save backups to be stored in:  
`sbu config --path /path/to/network/drive`

Then add a game to be backed up:  
`sbu add dark_souls_3 --path /path/to/ds3/saves`

Now you can run either  
`sbu backup dark_souls_3`  
to back up your Dark Souls 3 saves or  
`sbu backup`  
to back up saves for all games you've added.

You can keep it running, backing up your saves at the interval specified by the frequency in your config by running  
`sbu backup --loop`

If you want to rename dark_souls_3 to ds3 for brevity, you can run  
`sbu edit dark_souls_3 --name ds3`

To see all commands available, run  
`sbu --help`  
and to see how to use various commands (in this example, the `info` command), run  
`sbu info --help`
