-module(changeWorkDir).
-export(start/1).

start(Dir) -> cd(Dir).
