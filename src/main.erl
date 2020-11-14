-module(main).

-export([start/0]).
-on_load(start/0).

-import(distributors, [register_proc/2, read_int_line/1]).
-import(helpers, [make_displs/2, hello/1]).
-import(dijkstra, [init_dijkstra/4]).
-define(Inf, 100000000).
-define(GetElement(R,C,N,L), lists:nth(R*N + C, L)).

compile_files(Files) ->
    lists:foreach(fun(X) ->
        compile:file(X)
    end,
    Files
    ).

                
start() ->
    compile_files([
        distributors,
        helpers,
        dijkstra 
    ]),
    
    {ok, Device} = file:open("../test/test.txt", [read]),
    [NumVertices, NumProcs, Source] = distributors:read_int_line(Device),
    helpers:hello([NumVertices, NumProcs, Source]),
    Displs = lists:append(helpers:make_displs(NumVertices, NumProcs), [ ?Inf ] ),
    
    ets:new(procTable, [named_table, public]),
    ets:new(idTable, [named_table, public]),
    
    GetData = fun F(Data, CurRow, EndRow) ->
                if 
                    CurRow > EndRow ->
                        Data;
                    true ->
                        Row = distributors:read_int_line(Device),
                        F(lists:append(Data, Row), CurRow+1, EndRow)
                end
            end,
    

    helpers:hello(["start", erlang:system_time(), erlang:timestamp()]),
    LocalData = GetData([], 1, lists:nth(1, Displs)),
    
    distributors:register_proc(self(), 1),
    distributors:distribute_graph(Device, NumVertices, NumProcs, Displs, lists:nth(1, Displs)+1, 1),
    distributors:send_to_neighbours(lists:seq(2, NumProcs), {init, Source}),
    
    dijkstra:init_dijkstra(
        NumVertices,
        NumProcs, 
        Source, 
        LocalData
    ),
   
    ets:delete(procTable),
    ets:delete(idTable),
  
    file:close(Device).