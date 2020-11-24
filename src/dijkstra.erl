-module(dijkstra).

-export([init_dijkstra/6, proc_run/5, distribute_graph/6]).

-import(helpers,[make_displs/2, get_minimum_vert/2, get_minimum_vert/3, get_minimum_vert/4, get_proc_rank/2, get_proc_rank/3, get_rank/0, get_bounds/2, get_row/3, get_col/3, hello/1]).
-import(distributors,[send_to_neighbours/2, wait_for_response/3, wait_for_response/4, read_and_send/2]).
-include("macros.hrl").


% SysProps = {NumVertices, NumProcs}
% ProcProps = {StartRow, EndRow}
% ProcData = {LocalDist, Visited}

spawner(SysProps, Rank) ->
    spawn(dijkstra, proc_run, [Rank, helpers:get_bounds(SysProps, Rank), [], SysProps, [self()]]).

reduce_task(ProcProps, ProcData, LocalData, SysProps, SourceProps, Pids)->
    {_, Vis} = ProcData,
    helpers:hello(["visited", Vis, self()]),
    {NumVertices, _} = SysProps,
    {StartRow, EndRow} = ProcProps,
    UpdateProcData = relax_edges(
                        SourceProps,
                        ProcData,
                        get_col(LocalData, element(2, SourceProps), NumVertices)
                    ),
    
    distributors:send_to_neighbours(
            Pids,
            {
                reduction, 
                self(),
                get_minimum_vert(
                    UpdateProcData,
                    StartRow 
                )     
            }
        ),

    receive
        {reduction, MinVertexProps} ->
            reduce_task(
                ProcProps,
                UpdateProcData,
                LocalData,
                SysProps,
                MinVertexProps,
                Pids
            );
        stop ->
            distributors:send_to_neighbours(
                Pids,
                {
                    result, 
                    self(),
                    lists:zip(lists:seq(StartRow, EndRow), element(1, UpdateProcData))     
                }
            ),
            ok
    end.
    

map_task(ProcProps, ProcData, LocalData, SysProps, SourceProps, Pids) ->
    {_, Vis} = ProcData,
    helpers:hello(["visited", Vis]),
    {NumVertices, _} = SysProps,
    {StartRow, EndRow} = ProcProps,
    {StartRow, EndRow} = ProcProps,
    Accumulator = fun(X, Y) ->
                case element(1, X) < element(1, Y) of
                    true ->
                        X;
                    false ->
                        Y
                end
            end, 
    
    UpdateProcData = relax_edges(
                        SourceProps,
                        ProcData,
                        get_col(LocalData, element(2, SourceProps), NumVertices)
                    ),
    MinVertexProps = distributors:wait_for_response(
                Pids,
                reduction,
                Accumulator,
                helpers:get_minimum_vert(
                    UpdateProcData,
                    StartRow
                )
            ),
   
    case element(1, MinVertexProps) of
        ?Inf ->
            distributors:send_to_neighbours(
                Pids, 
                stop
            ),
            Result = distributors:wait_for_response(
                Pids,
                result,
                fun(X, Y) ->
                    lists:append(X, Y)
                end,
                lists:zip(lists:seq(StartRow, EndRow), element(1, UpdateProcData))
            ),
            Result;
        _ ->
            distributors:send_to_neighbours(
                Pids,
                {
                    reduction, 
                    MinVertexProps  
                }
            ),
            map_task(
                ProcProps,
                UpdateProcData,
                LocalData,
                SysProps,
                MinVertexProps,
                Pids
            )
    end.


relax_edges(SourceProps, ProcData, Edges) ->
    { 
        relax_edges(Edges, element(1, ProcData), 1, element(1, SourceProps)),
        lists:append(element(2, ProcData), [element(2, SourceProps)])
    }.
relax_edges(_, [], _, _) -> [];
relax_edges([Edge | RestEdges], [H|T], Index, BaseDist) ->
    case BaseDist + Edge < H of
        true -> 
            lists:append([BaseDist + Edge], relax_edges(RestEdges, T, Index+1, BaseDist));
        false ->
            lists:append([H], relax_edges(RestEdges, T, Index+1, BaseDist))
    end.



proc_run(Rank, ProcProps, LocalData, SysProps, Pids) ->
    receive
        {input, Data} ->
            proc_run(
                Rank,
                ProcProps,
                lists:append(LocalData, Data),
                SysProps,  
                Pids
            );
        {init, SourceProps} ->
            helpers:hello(["in proc run", Rank, self()]),
            distributors:send_to_neighbours(
                Pids,
                ready
            ),
            dijkstra:init_dijkstra(
                Rank,
                ProcProps,
                LocalData,
                SysProps, 
                SourceProps, 
                Pids
            ),
            ok;
        stop ->
            ok
    end.


init_dijkstra(Rank, ProcProps, LocalData, SysProps, SourceProps, Pids) ->
    helpers:hello([Rank, self()]),
    ProcProps = helpers:get_bounds(SysProps, Rank),
    {StartRow, EndRow} = ProcProps,
    {_, Source} = SourceProps,
    LocalDist = case { StartRow =< Source , Source =< EndRow } of
                    {true, true} ->
                        lists:append([lists:duplicate(Source-StartRow, ?Inf), [0], lists:duplicate(EndRow-Source, ?Inf)]);
                    {_, _} ->
                        lists:duplicate(EndRow-StartRow+1, ?Inf)
                end,
    
    ProcData = {LocalDist, []},
    case Rank of
        1 ->
            map_task(
                ProcProps,
                ProcData,
                LocalData,
                SysProps,
                SourceProps,
                Pids
            );

        _ ->         
            reduce_task(
                    ProcProps,
                    ProcData,
                    LocalData,
                    SysProps,
                    SourceProps,
                    Pids
            )
    end.


distribute_graph(Device, SysProps, Displs, CurRow, CurIndex, CurPid) ->
    helpers:hello(["row no", CurRow]),
    EndRow = lists:nth(CurIndex, Displs),
    if 
        CurRow > element(1, SysProps) ->
            [];
        CurRow =< EndRow ->
            distributors:read_and_send(Device, CurPid),
            distribute_graph(
                Device, 
                SysProps, 
                Displs, 
                CurRow + 1, 
                CurIndex,
                CurPid
            );
        true ->
            Pid = spawner(SysProps, CurIndex+1),
            lists:append(
                [Pid],
                distribute_graph(
                    Device, 
                    SysProps, 
                    Displs, 
                    CurRow, 
                    CurIndex+1,
                    Pid
                ))
    end.