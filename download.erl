-module(download).
-export([start/0, save/1]).


get_start_base() -> "http://airwar.ru/".
get_start_url() -> get_start_base() ++ "image/".
out_file_name() -> "pictures.txt".


start() ->
  inets:start(),
  Writer = start_write(),
  MyPid = self(),
  spawn(fun() -> process_page(Writer, MyPid, get_start_url()) end),
  collect(1),
  stop_write(Writer).

start_write() -> 
  spawn(fun write_proc/0).

stop_write(W) ->
  W ! stop.

write(W, String) ->
  W ! {write, String}.

write_proc() -> write_loop([], 0).
write_loop(Data, DataLen) ->
  receive
    stop ->
      io:format("Saving ~b entries~n", [DataLen]),
      {ok, F} = file:open(out_file_name(), write),
      [io:format(F, "~s~n", [S]) || S <- Data],
      file:close(F),
      io:format("Done~n");
    {write, String} ->
      %io:format("Adding: ~s~n", [String]),
      case DataLen rem 1000 of
        0 -> 
          io:format("Downloaded: ~p~n", [DataLen]);
        _ -> 
          ok
      end,
      write_loop([String|Data], 1 + DataLen)
  after 10000 ->
    io:format("Stop on timeout~n"),
    stop_write(self()),
    write_loop(Data, DataLen)
  end.

process_page(W, Parent, Url) ->
  case get_url_contents(Url) of
    {ok, Data} ->
      Strings = string:tokens(Data, "\n"),
      try mochiweb_html:parse(Strings) of
        Tree ->
          collect(process_tree(W, Url, Tree))
      catch
        error:_Error ->
          io:format("Bad URL: ~p~n", [Url])
      end;
    _ -> ok
  end,
  done(Parent).

%% returns number of created processes
process_tree(W, Url, {Tag, Properties, []}) ->
  process_tag(W, Url, {Tag, Properties});

process_tree(W, Url, {Tag, Properties, [C | Content]}) ->
  process_tree(W, Url, C) + process_tree(W, Url, {Tag, Properties, Content});

process_tree(_W, _Url, _Smth) ->
  0.

process_tag(W, Url, {Tag, Properties}) ->
  case binary_to_list(Tag) of
    "a" ->
      case get_link(Properties) of
        {ok, Link} ->
          process_link(W, Url, Link);
        false ->
          0
      end;
    _Smth ->
      0
  end.

get_link([]) ->
  false;

get_link([{Property, Value} | Properties]) ->
  case binary_to_list(Property) of
    "href" ->
      {ok, binary_to_list(Value)};
    _ ->
      get_link(Properties)
  end.

collect(0) -> 
  ok;
collect(N) ->
  %io:format("To collect: ~p~n", [N]),
  receive
    done -> collect(N - 1)
  end.

get_url_contents(Url) -> 
  get_url_contents(Url, 5).
get_url_contents(Url, 0) -> 
  failed;
get_url_contents(Url, MaxFailures) ->
  case httpc:request(Url) of
    {ok, {{_, RetCode, _}, _, Result}} -> 
      if 
        RetCode == 200; RetCode == 201 ->
          {ok, Result};
        RetCode >= 500 ->
          % server error, retry
          %io:format("HTTP code ~p~n", [RetCode]),
          timer:sleep(1000),
          get_url_contents(Url, MaxFailures-1);
        true ->
          % all other errors
          failed
      end;
    {error, _Why} ->
      %io:format("failed request: ~s : ~w~n", [Url, Why]),
      timer:sleep(1000),
      get_url_contents(Url, MaxFailures-1)
  end.

done(Parent) ->
  Parent ! done.


process_link(W, Dir, Url) ->
  case get_link_type(Url) of
    image -> 
      process_image(W, Dir ++ Url),
      0;
    page ->
      MyPid = self(),
      spawn(fun() -> process_page(W, MyPid, Dir ++ Url) end), 
      1;
    _ -> 
      process_other(W, Dir ++ Url),
      0
  end.

%% Site-specific heuristics.

get_link_type(Url) ->
  {ok, ReImg} = re:compile("\\.(gif|jpg|jpeg|png)", [extended, caseless]),
  {ok, RePage} = re:compile("^[^/]+/$"),
  case re:run(Url, ReImg) of
    {match, _} -> 
      image;
  _ -> 
    case re:run(Url, RePage) of
      {match, _} -> 
        page;
      _ -> 
        strange
    end
  end.

process_image(W, Url) ->
  write(W, Url).

process_other(_W, _Url) ->
  %io:format("Unexpected URL: ~p~n", [Url])
  ok.

save(Path) ->
  inets:start(),
  {ok, Data} = file:read_file(Path),
  L = string:tokens(binary_to_list(Data), "\n"),
  save_loop(0, L).


save_loop(0, []) ->
  io:format("saving done~n", []);

save_loop(Running, []) ->
  receive
    done ->
      io:format("to save: ~p~n", [Running]),
      save_loop(Running - 1, [])
  end;

save_loop(Running, [U|Us]) when Running < 200 ->
  S = self(),
  spawn(fun() -> save_url(S, U) end),
  save_loop(Running + 1, Us);

save_loop(Running, Us) ->
  receive
    done ->
      io:format("to save: ~p~n", [Running + length(Us)]),
      save_loop(Running - 1, Us)
  end.

save_url(Parent, Url) ->
  Path = url_to_path(Url),
  ensure_dir(".", filename:split(Path)),
  case get_url_contents(Url) of
    {ok, Data} ->
      %io:format("saving ~p~n", [Url]),
      file:write_file(Path, Data);
    _ -> 
      ok
  end,
  done(Parent).

url_to_path(Url) ->
  string:substr(Url, length(get_start_base())+1).

ensure_dir(_Dir, [_FileName]) -> 
  ok;

ensure_dir(Dir, [NextDir|RestDirs]) ->
  DirName = filename:join(Dir, NextDir),
  file:make_dir(DirName),
  ensure_dir(DirName, RestDirs).
