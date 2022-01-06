# vbu

![Haskell CI](https://github.com/travv0/vbu/workflows/Haskell%20CI/badge.svg)

Small utility to create versioned backups of files in a directory that match a glob. I originally wrote it to backup video game saves to Google Drive that didn't have cloud save support.

Start by setting the location you want your backups to be stored in:
```sh
vbu config --path /path/to/network/drive
```

Then add a path to be backed up with the given name (called a group):
```sh
vbu add dark_souls_3 --path /path/to/ds3/saves
```

Now you can run either
```sh
vbu backup dark_souls_3
```
to back up your Dark Souls 3 saves or
```sh
vbu backup
```
to back up files for all the groups you've added.

You can keep it running, backing up your files at the interval specified by the frequency in your config by running
```sh
vbu backup --loop
```

If you want to rename dark_souls_3 to ds3 for brevity, you can run
```sh
vbu edit dark_souls_3 --name ds3
```

To see all commands available, run
```sh
vbu --help
```
and to see how to use various commands (in this example, the info command), run
```sh
vbu info --help
```
