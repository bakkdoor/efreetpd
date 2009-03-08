-module(changeDirUp).
-export(start/0).

%ueberprüfen: userverzeichnis oder nicht (zusaetzliche Parameter!)

start() -> cd("..").
