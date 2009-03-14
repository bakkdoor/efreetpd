%% module for some tcp/ip related helper functions
-module(tcp).
-export([receive_binary/1, crnl/0, parse_address/1, format_address/2, get_request/2, parse_request/1]).
-author({"Christopher Bertels", "bakkdoor@flasht.de"}).


-spec receive_binary(port()) -> {ok, binary()} | {error, string() | atom()}.

receive_binary(Socket) ->
    receive_binary(Socket, []).


-spec receive_binary(port(), list()) -> {ok, binary()} | {error, string() | atom()}.

receive_binary(Socket, BinAcc) ->
    case gen_tcp:recv(Socket, 0) of
	{ok, B} ->
	    receive_binary(Socket, [B|BinAcc]);
	{error, closed} ->
	    {ok, list_to_binary(lists:reverse(BinAcc))}
    end.


%% returns the line (message) terminator string for ftp-protocol-messages.
crnl() ->
    "\r\n".


parse_address(Str) ->
    parse_address(Str, 0, []).

parse_address([X|Xs],N,Acc) when X >= $0, X =< $9 -> parse_address(Xs, N*10+(X-$0), Acc);
parse_address([X|Xs],_N,Acc) when X >= $A, X =< $F -> parse_address(Xs,(X-$A)+10, Acc);
parse_address([X|Xs],_N,Acc) when X >= $a, X =< $f -> parse_address(Xs, (X-$a)+10, Acc);
parse_address([$,,$,|_Xs], _N, _Acc) -> error;
parse_address([$,|Xs], N, Acc) -> parse_address(Xs, 0, [N|Acc]);
parse_address([],P2,[P1,D4,D3,D2,D1]) -> {ok,{{D1,D2,D3,D4}, P1*256+P2}};
parse_address([],P2,[P1|As]) when length(As) == 32 ->
    case addr6(As,[]) of
	{ok,Addr} -> {ok, {Addr, P1*256+P2}};
	error -> error
    end;
parse_address(_, _, _) -> error.



addr6([H4,H3,H2,H1|Addr],Acc) when H4<16,H3<16,H2<16,H1<16 ->
    addr6(Addr, [H4 + H3*16 + H2*256 + H1*4096 |Acc]);
addr6([], Acc) -> {ok, list_to_tuple(Acc)};
addr6(_, _) -> error.


format_address({A,B,C,D}, Port) ->
    integer_to_list(A) ++ "," ++
    integer_to_list(B) ++ "," ++
    integer_to_list(C) ++ "," ++
    integer_to_list(D) ++ "," ++
    integer_to_list(Port div 256) ++ "," ++
    integer_to_list(Port rem 256);
format_address({N1,N2,N3,N4,N5,N6,N7,N8},Port) ->
    h4(N1) ++ "," ++ h4(N2) ++ "," ++ h4(N3) ++ "," ++ h4(N4) ++ "," ++
    h4(N5) ++ "," ++ h4(N6) ++ "," ++ h4(N7) ++ "," ++ h4(N8) ++ "," ++
	integer_to_list(Port div 256) ++ "," ++
	integer_to_list(Port rem 256).

h4(N) ->
    [hx(N bsr 12),$,,hx(N bsr 8),$,,hx(N bsr 4),$,, hx(N)].

hx(N) ->
    N1 = N band 16#f,
    if N1 < 10 -> N1+$0;
       true -> (N1-10)+$A
    end.



%% parse a command and arguments
%% must be case insensitive on commands and type letters but
%% sensitive on path/user 
%% 
parse_request([L1,L2,L3 | T]) ->
    C1 = alpha(L1),
    C2 = alpha(L2),
    C3 = alpha(L3),
    case T of
	[] -> parse_request([C1,C2,C3], []);
	[$ | Arg] -> parse_request([C1,C2,C3],Arg);
	[C4] -> parse_request([C1,C2,C3,alpha(C4)],[]);
	[C4,$  | Arg] -> parse_request([C1,C2,C3,alpha(C4)],Arg);
	_ -> error
    end;
parse_request(_) -> error.

parse_request(Command, Args) ->
    {ftp_driver:convert_command(Command),
     Args}.

    
%% return lower letter space or ?		 
alpha(X) when X >= $A, X =< $Z -> (X-$A)+$a;
alpha(X) when X >= $a, X =< $z -> X;
alpha(X) when X == $  -> X;
alpha(_) -> $?.


get_request(S, Buf) ->
    case split_line(Buf) of
	more ->
	    case gen_tcp:recv(S,0) of
		{ok, Cs} ->
		    Buf1 = Buf++Cs,
		    case split_line(Buf1) of
			more -> get_request(S, Buf1);
			Done -> Done
		    end;
		Error -> Error
	    end;
	Done -> Done
    end.

%% split a line after CRLF
split_line(Bin) when is_binary(Bin) ->
    split_line(binary_to_list(Bin));
split_line(Cs) ->
    split_line(Cs, []).

split_line([$\r,$\n|Cs], Buf) ->
    {ok, lists:reverse(Buf), Cs};
split_line([X|Cs], Buf) ->
    split_line(Cs, [X|Buf]);
split_line([], _) ->
    more.

