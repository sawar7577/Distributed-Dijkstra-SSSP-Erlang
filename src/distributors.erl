-module(distributors).

-define(Inf, 100000000).
-export([register_proc/2, read_int_line/1, send_to_neighbours/2, read_and_send/2, distribute_graph/6]).


spawner(NumVertices, NumProcs, Rank) ->
    spawn(main, proc_run, [NumVertices, NumProcs, Rank, []]).

register_proc(Pid, Id) ->
    ets:insert(procTable, {Id, Pid}),
    ets:insert(idTable, {Pid, Id}).

read_int_line(Device) ->
    Row = file:read_line(Device),
    case Row of 
        eof ->
            [];
        {ok, Line} ->
             RetList = lists:map(
                 fun(X) -> 
                     {Int, _ } = string:to_integer(X),
                     Int end,
                 string:split(string:trim(Line), " ", all)
                ),
            RetList
    end.


send_to_neighbours(Neighs, Msg) ->
    lists:foreach(
            fun(X) ->
                [{_, SPid}] = ets:lookup(procTable, X),
                SPid ! Msg
            end,
            Neighs
        ).

read_and_send(Device, Id) ->
    % hello("Id"),
    % hello(Id),
    [{_, SPid}] = ets:lookup(procTable, Id),
    Row = read_int_line(Device),
    case Row of 
        [] -> 
            ok;
        [H|T] ->
            Msg = {"input", Row},
            % hello(Msg),
            SPid ! Msg,
            [H|T]
    end.


distribute_graph(Device, NumVertices, NumProcs, Displs, CurRow, CurIndex) ->
    EndRow = lists:nth(CurIndex, Displs),
    if 
        CurRow > NumVertices ->
            % hello("out"),
            ok;
        CurRow =< EndRow ->
            read_and_send(Device, CurIndex),
            distribute_graph(Device, NumVertices, NumProcs, Displs, CurRow + 1, CurIndex);
        true ->
            register_proc(
                    spawner(NumVertices, NumProcs, CurIndex+1),                
                    CurIndex+1
                ),
            distribute_graph(Device, NumVertices, NumProcs, Displs, CurRow, CurIndex+1)
    end.
