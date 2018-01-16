# scala-gdrive-project

## Compilation

With a recent version of sbt, you should be able to just compile and run it with `sbt run`. You can also run `sbt assembly` to create a standalone jar file in `target/scala-2.12.4/`.

## Commands

ls - list files  
cd - changes directory  
rm - deletes files/directories
mv - moves files/directories
mkdir - creates directories
exit - exits the program
forceUpdate - forces a download/sync of metadata information from Google Drive

Type <command> -h for command help.

## Design Decisions

Limited functionality for directory names because two directories can have the same. 
Memory-based rather than asking Drive every single time (which means you need to call forceUpdate on an external change).
