-module(download).
-compile(export_all).


get_start_base() -> "http://airwar.ru/".
get_start_url() -> get_start_base() ++ "image/".
out_file_name() -> "pictures.txt".

start() ->
  inets:start(),
  Writer = start_write(),
  process_page(Writer, get_start_url()),
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
      case DataLen rem 1000 of
        0 -> io:format("Downloaded: ~p~n", [DataLen]);
        _ -> ok
      end,
      write_loop([String|Data], 1 + DataLen)
  after 10000 ->
    io:format("Stop on timeout~n"),
    stop_write(self()),
    write_loop(Data, DataLen) 
  end.                

extract_link(W,Pid,Dir,[{U,Ost}],[Ostatok])->
  H=binary_to_list(Ost),
  case lists:last(H) of 
    47->
      if Ostatok/=<<" Parent Directory">>->
          process_page(W,Dir++H);
         Ostatok==<<" Parent Directory">>->ok
      end;
    Other->
      process_image(W,Dir++H)
  end.
  
extract_image(W,Pid,Dir,[{U,Ost},C])->
  H=binary_to_list(Ost),
  process_image(W,Dir++H).

extract(W,Pid,Dir,[{Tag,H,Ostatok}])->
  case Tag of
  <<"a">>->
    extract_link(W,Pid,Dir,H,Ostatok);
  <<"img">>->
    extract_image(W,Pid,Dir,H);
  Other->
    extract(W,Pid,Dir,Ostatok)
end;
extract(W,Pid,Dir,[{Tag,H,Ostatok}|Ostatok2])->
  extract(W,Pid,Dir,Ostatok),
  extract(W,Pid,Dir,Ostatok2);
extract(W,Pid,Dir,[H])->ok.
  
process_page(W, Url) ->
  MyPid = self(),
  case get_url_contents(Url) of
    {ok, Data} ->
    Strings=mochiweb_html:parse(Data),
    extract(W,MyPid,Url,[Strings]);
    _ -> ok
  end.

get_url_contents(Url) -> get_url_contents(Url, 5).
get_url_contents(_, 0) -> failed;
get_url_contents(Url, MaxFailures) ->
  case httpc:request(Url) of
    {ok, {{_, RetCode, _}, _, Result}} -> 
      if 
        RetCode == 200; RetCode == 201 -> {ok, Result};
        RetCode >= 500 ->
          timer:sleep(1000),
          get_url_contents(Url, MaxFailures - 1);
        true -> failed
      end;
    {error, _Why} ->
      timer:sleep(1000),
      get_url_contents(Url, MaxFailures - 1)
  end.

process_image(W, Url) ->
  write(W, Url).

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
  spawn(fun () -> save_url(S, U) end),
  save_loop(Running + 1, Us);
save_loop(Running, Us) ->
  receive
    done ->
      io:format("to save: ~p~n", [Running + length(Us)]),
      save_loop(Running - 1 , Us)
  end.
  
done(Parent) ->
  Parent ! done.
  
save_url(Parent, Url) ->
  Path = url_to_path(Url),
  ensure_dir(".", filename:split(Path)),
  case get_url_contents(Url) of
    {ok, Data} ->
      file:write_file(Path, Data);
    _ -> ok
  end,
  done(Parent).

url_to_path(Url) ->
  string:substr(Url, length(get_start_base()) + 1).

ensure_dir(_, [_FN]) -> ok;
ensure_dir(Dir, [NextDir|RestDirs]) ->
  DirName = filename:join(Dir, NextDir),
  file:make_dir(DirName),
  ensure_dir(DirName, RestDirs).
