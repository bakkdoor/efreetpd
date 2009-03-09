-module(makeDir).
-export(start/1).
-import(file).

start(DirName) -> make_dir(DirName).
