-module(main).

-export([start/0, hello/1, proc_run/4]).

-import(distributors, [register_proc/2]).

-define(Inf, 100000000).
-define(GetElement(R,C,N,L), lists:nth(R*N + C, L)).

hello(Line)->
    % io:fwrite("hello"),
    io:fwrite("~p~n", [Line]).

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

spawner(NumVertices, NumProcs, Rank) ->
    spawn(main, proc_run, [NumVertices, NumProcs, Rank, []]).

proc_run(NumVertices, NumProcs, Rank, LocalData) ->
    receive
        {input, Data} ->
            proc_run(NumVertices, NumProcs, Rank, lists:append(LocalData, Data));
        {other, Data} ->
            proc_run(NumVertices, NumProcs, Rank, LocalData);
        {init, Source} ->
            dijkstra:init_dijkstra(NumVertices, NumProcs, Source, LocalData),
            ok;
        stop ->
            hello([self(), LocalData]),
            ok
    end.
                
start() ->
    {ok, Device} = file:open("./graph.txt", [read]),
    [NumVertices, NumProcs, Source] = read_int_line(Device),
    hello(NumVertices),
    hello(NumProcs),
    hello(Source),
    Displs = lists:append(helpers:make_displs(NumVertices, NumProcs), [ ?Inf ] ),
    
    ets:new(procTable, [named_table, public]),
    ets:new(idTable, [named_table, public]),
    ets:new(visited, [named_table, public]),
    
    GetData = fun F(Data, CurRow, EndRow) ->
                if 
                    CurRow > EndRow ->
                        Data;
                    true ->
                        Row = read_int_line(Device),
                        F(lists:append(Data, Row), CurRow+1, EndRow)
                end
            end,
    
    LocalData = GetData([], 1, lists:nth(1, Displs)),
    
    hello([self(), LocalData]),
    distributors:register_proc(self(), 1),
    distributors:distribute_graph(Device, NumVertices, NumProcs, Displs, lists:nth(1, Displs)+1, 1),
    distributors:send_to_neighbours(lists:seq(2, NumProcs), {init, 1}),
    dijkstra:init_dijkstra(NumVertices, NumProcs, 1, LocalData),
   
    hello(["hey", helpers:get_proc_rank(Displs, 2)]),
    ets:delete(procTable),
    ets:delete(idTable),
    ets:delete(visited),

    file:close(Device).

    