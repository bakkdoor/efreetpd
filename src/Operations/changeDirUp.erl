-module(changeDirUp).
-export(start/0).

%ueberpr�fen: userverzeichnis oder nicht (zusaetzliche Parameter!)

start() -> cd("..").
