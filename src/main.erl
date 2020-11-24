-module(main).

-export([start/0]).
-on_load(start/0).

-import(distributors, [register_proc/2, read_int_line/1, wait_for_response/2]).
-import(helpers, [make_displs/2, hello/1]).
-import(dijkstra, [init_dijkstra/6, proc_run/5, distribute_graph/6]).
-include("macros.hrl").

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
    
    {ok, Device} = file:open("./test.txt", [read]),
    [NumVertices, NumProcs, Source] = distributors:read_int_line(Device),
    helpers:hello([NumVertices, NumProcs, Source]),
    SysProps = {NumVertices, NumProcs},
    Displs = lists:append(helpers:make_displs(NumVertices, NumProcs), [ ?Inf ] ),
    
    
    GetData = fun F(Data, CurRow, EndRow) ->
                if 
                    CurRow > EndRow ->
                        Data;
                    true ->
                        Row = distributors:read_int_line(Device),
                        F(lists:append(Data, Row), CurRow+1, EndRow)
                end
            end,
    

    helpers:hello(["starting", erlang:system_time(), erlang:timestamp()]),
    LocalData = GetData([], 1, lists:nth(1, Displs)),
    Pids = dijkstra:distribute_graph(Device, SysProps, Displs, lists:nth(1, Displs)+1, 1, self()),
    distributors:send_to_neighbours(Pids, {init, {0, Source}}),
    helpers:hello([wait_for_response(Pids, ready)]),
    {Time, _} = timer:tc(dijkstra,init_dijkstra,[
        1,
        helpers:get_bounds(SysProps, 1),
        LocalData,
        SysProps, 
        {0, Source}, 
        Pids
    ]),
   helpers:hello([Time/1000000]),
   file:close(Device).