-module(make_dir).
-export(start/1).
-import(file).

start(FTPConnPid, State, DirName) -> make_dir(DirName).
