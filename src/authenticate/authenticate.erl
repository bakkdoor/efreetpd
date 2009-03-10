-module(authenticate).
-export([proove/3]).
-author({"Denis Meyer", "calltopower88@web.de"}).

% Looks up the Name and the Password
proove(Pid, Name, Password) ->
    % Name und Passwort aus Tabelle/Datei mit uebergebenem Namen und Passwort vergleichen und auth_status setzen
    Pid ! {true, Pid, Name}.    % Nur zu Testzwecken: Alles authentifizieren (true)

% TODO
% Comparison Name & Password with Database/Table of Name and Passwords
% Later:
% Encryption/Decryption of the (Name and) Password
