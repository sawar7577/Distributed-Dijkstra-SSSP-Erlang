-module(distributors).

-define(Inf, 100000000).
-export([register_proc/2, proc_run/4, read_int_line/1, send_to_neighbours/2, distribute_graph/6, wait_for_response/3, wait_for_response/4]).
-import(helpers, [hello/1]).

spawner(NumVertices, NumProcs, Rank) ->
    spawn(distributors, proc_run, [NumVertices, NumProcs, Rank, []]).

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

wait_for_response(Pids, Response, Accumulator) -> 
    wait_for_response(Pids, Response, Accumulator, {?Inf, -1}).

wait_for_response([], _, _, Result) -> Result;
wait_for_response(Pids, Response, Accumulator, Result) ->
    receive
        {Response, Pid, Data} ->
            wait_for_response(
                lists:delete(Pid, Pids), 
                Response, 
                Accumulator, 
                Accumulator(Result, Data)
            )
    end.

read_and_send(Device, Id) ->
    [{_, SPid}] = ets:lookup(procTable, Id),
    Row = read_int_line(Device),
    case Row of 
        [] -> 
            ok;
        [H|T] ->
            Msg = {input, Row},
            SPid ! Msg,
            [H|T]
    end.


proc_run(NumVertices, NumProcs, Rank, LocalData) ->
    receive
        {input, Data} ->
            proc_run(
                NumVertices, 
                NumProcs, 
                Rank, 
                lists:append(LocalData, Data)
            );
        {init, Source} ->
            dijkstra:init_dijkstra(
                NumVertices, 
                NumProcs, 
                Source, 
                LocalData
            ),
            ok;
        stop ->
            ok
    end.

distribute_graph(Device, NumVertices, NumProcs, Displs, CurRow, CurIndex) ->
    EndRow = lists:nth(CurIndex, Displs),
    if 
        CurRow > NumVertices ->
            ok;
        CurRow =< EndRow ->
            read_and_send(Device, CurIndex),
            distribute_graph(
                Device, 
                NumVertices, 
                NumProcs, 
                Displs, 
                CurRow + 1, 
                CurIndex
            );
        true ->
            register_proc(
                    spawner(NumVertices, NumProcs, CurIndex+1),                
                    CurIndex+1
                ),
            distribute_graph(
                Device, 
                NumVertices, 
                NumProcs, 
                Displs, 
                CurRow, 
                CurIndex+1
            )
    end.
