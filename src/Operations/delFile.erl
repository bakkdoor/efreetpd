-module(delFile).
-export(start/1).
-import(file).

start(File) -> delete(File).
