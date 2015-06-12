-module(tictactoe).

-export([start/0, stop/0]).
-export([connect/0]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).
-export([room/1]).

-behavior(gen_server).


%---server---
host() ->
	'jo@127.0.0.1'.

start() ->
	gen_server:start({local, ?MODULE}, ?MODULE, [], []).

stop() ->
	gen_server:cast(?MODULE, stop).

init(_Args) -> 
	{ok, []}.

handle_call(connect, _From, []) ->
	{ok, Socket} = gen_tcp:listen(0, [binary]),
	spawn(tictactoe, room, [Socket]),
	{ok, Port} = inet:port(Socket),
	{reply, Port, [Socket]};

handle_call(connect, _From, [Socket]) ->
	{ok, Port} = inet:port(Socket),
	{reply, Port, []}.

handle_cast(stop, [Socket]) ->
	gen_tcp:close(Socket),
	{stop, normal, []};

handle_cast(stop, []) ->
	{stop, normal, []}.

terminate(_, _) ->
	ok.


room(Socket) ->
	case gen_tcp:accept(Socket) of
		{ok, S1} ->
			room(Socket, S1);
		_Any ->
			io:format("Something wrong is occured~n")
	end.

room(Socket, S1) ->
	case gen_tcp:accept(Socket) of
		{ok, S2} ->
			gen_tcp:close(Socket),
			room({S1, $x}, {S2, $o}, empty_board(), turn);
		_Any ->
			io:format("Something wrong is occured~n")
	end.

room({S1, _P1}, {S2, _P2}, Board, win) ->
	send(S2, {Board, win}),
	send(S1, {Board, lose}),
	gen_tcp:close(S1),
	gen_tcp:close(S2);


room({S1, _P1}, {S2, _P2}, Board, draw) ->
	send(S2, {Board, draw}),
	send(S1, {Board, draw}),
	gen_tcp:close(S1),
	gen_tcp:close(S2);

room({S1, P1}, {S2, P2}, Board, turn) ->
	send(S1, {Board, turn}),
	{R, C} = recv(S1),
	{NewS1, NewS2, NewBoard, NewState} = 
		try move(Board, R, C, P1) of
			Res -> 
				{{S2, P2}, {S1, P1}, Res, get_state(Res)}
		catch
			_: _Error ->
				{{S1, P1}, {S2, P2}, Board, turn}
		end,
	room(NewS1, NewS2, NewBoard, NewState).

%---game---
empty_board() ->
	["...", "...", "..."].

get(Board, Row, Col) ->
	lists:nth(Col, lists:nth(Row, Board)).

put(Board, Row, Col, C) ->
	R = lists:nth(Row, Board),
	NewR = lists:append(lists:append(lists:sublist(R, Col - 1), [C]), lists:nthtail(Col, R)),
	lists:append(lists:append(lists:sublist(Board, Row - 1), [NewR]), lists:nthtail(Row, Board)).

move(Board, Row, Col, C) ->
	case get(Board, Row, Col) == $. of
		true ->
			put(Board, Row, Col, C);
		false ->
			throw(bad)
	end.

is_equal(Board, R1, C1, R2, C2, R3, C3) ->
	(get(Board, R1, C1) == get(Board, R2, C2)) and (get(Board, R2, C2) == get(Board, R3, C3)).

check_win(Board) ->
	((get(Board, 1, 1) /= $.) and is_equal(Board, 1, 1, 2, 1, 3, 1)) or
	((get(Board, 1, 2) /= $.) and is_equal(Board, 1, 2, 2, 2, 3, 2)) or
	((get(Board, 1, 3) /= $.) and is_equal(Board, 1, 3, 2, 3, 3, 3)) or
	((get(Board, 1, 1) /= $.) and is_equal(Board, 1, 1, 1, 2, 1, 3)) or
	((get(Board, 2, 1) /= $.) and is_equal(Board, 2, 1, 2, 2, 2, 3)) or
	((get(Board, 3, 1) /= $.) and is_equal(Board, 3, 1, 3, 2, 3, 3)) or
	((get(Board, 1, 1) /= $.) and is_equal(Board, 1, 1, 2, 2, 3, 3)) or
	((get(Board, 1, 3) /= $.) and is_equal(Board, 1, 3, 2, 2, 3, 1)).

check_full(Board) ->
	(get(Board, 1, 1) /= $.) and (get(Board, 1, 2) /= $.) and (get(Board, 1, 3) /= $.) and 
	(get(Board, 2, 1) /= $.) and (get(Board, 2, 2) /= $.) and (get(Board, 2, 3) /= $.) and 
	(get(Board, 3, 1) /= $.) and (get(Board, 3, 2) /= $.) and (get(Board, 3, 3) /= $.).


get_state(Board) ->
	case check_win(Board) of
		true ->
			win;
		false ->
			case check_full(Board) of
				true ->
					draw;
				false ->
					turn
			end
	end.

%---client and support---
read_move() ->
	case io:fread("Your turn: ", "~d~d") of
		{ok, [R, C]} -> 
			{R, C};
		{error, _Any} ->
			read_move()
	end.

send(Socket, Msg) ->
	gen_tcp:send(Socket, term_to_binary(Msg)).

recv(Socket) ->
	receive 
		{tcp, Socket, Msg} ->
			binary_to_term(Msg)
	end.

game_loop(Socket) ->
	case recv(Socket) of
		{_Board, win} ->
			io:format("WIN~n"),
			gen_tcp:close(Socket);
		{_Board, lose} ->
			io:format("LOSE :(~n"),
			gen_tcp:close(Socket);
		{_Board, draw} ->
			io:format("DRAW~n"),
			gen_tcp:close(Socket);
		{[R1, R2, R3], turn} ->
			io:format("~p~n~p~n~p~n", [R1, R2, R3]),
			{R, C} = read_move(),
			send(Socket, {R, C}),
			game_loop(Socket);
		{_Board, try_again} ->
			io:format("Invalid turn. Try again.~n", []),
			{R, C} = read_move(),
			send(Socket, {R, C}),
			game_loop(Socket)
	end.


uid(0) ->
	"";

uid(Len) ->
	[C|_] = io_lib:format("~c", [96 + random:uniform(26)]),
	string:concat(C, uid(Len - 1)).

connect() ->
	random:seed(now()),
	UID = list_to_atom(uid(16)),
	net_kernel:start([UID]),
	Port = rpc:call(host(), gen_server, call, [?MODULE, connect]),
	{ok, Socket} = gen_tcp:connect("localhost", Port, [binary]),
	game_loop(Socket).
