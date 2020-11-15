-module(dijkstra).

-export([init_dijkstra/4]).

-import(helpers,[make_displs/2, get_minimum_vert/3, get_minimum_vert/4, get_proc_rank/2, get_proc_rank/3, get_rank/0, get_bounds/2, get_row/3, get_col/3, hello/1]).
-import(distributors,[send_to_neighbours/2, wait_for_response/3, wait_for_response/4]).
-include("macros.hrl").

reduce_task(NumVertices, NumProcs, Source, SourceDist, LocalData, LocalDist, Visited)->
    Rank = helpers:get_rank(),
    {StartRow, EndRow} = helpers:get_bounds(NumVertices, NumProcs),
    { UpdateDist , UpdateVisited } = relax_edges(
                                        Source,
                                        get_col(LocalData, Source, NumVertices),
                                        LocalDist,
                                        Visited,
                                        SourceDist
                                    ),
    
    distributors:send_to_neighbours(
            [1],
            {
                reduction, 
                Rank,
                get_minimum_vert(
                    UpdateDist,
                    StartRow, 
                    UpdateVisited
                )     
            }
        ),

    receive
        {reduction, MinVertex, MinVal} ->
            reduce_task(
                NumVertices, 
                NumProcs, 
                MinVertex,
                MinVal, 
                LocalData, 
                UpdateDist, 
                UpdateVisited
            );
        stop ->
            distributors:send_to_neighbours(
                [1],
                {
                    result, 
                    Rank,
                    lists:zip(lists:seq(StartRow, EndRow), UpdateDist)     
                }
            ),
            ok
    end.
    

map_task(NumVertices, NumProcs, Source, SourceDist, LocalData, LocalDist, Visited) ->
    Rank = helpers:get_rank(),
    Neighs = lists:delete(Rank, lists:seq(1, NumProcs)),
    {StartRow, EndRow} = helpers:get_bounds(NumVertices, NumProcs),
    Accumulator = fun(X, Y) ->
                case element(1, X) < element(1, Y) of
                    true ->
                        X;
                    false ->
                        Y
                end
            end, 
    
    { UpdateDist, UpdateVisited } = relax_edges(
                                        Source,
                                        get_col(LocalData, Source, NumVertices),
                                        LocalDist,
                                        Visited,
                                        SourceDist
                                    ),
    {MinVal, MinVertex} = distributors:wait_for_response(
                Neighs,
                reduction,
                Accumulator,
                helpers:get_minimum_vert(
                    UpdateDist,
                    StartRow,
                    UpdateVisited
                )
            ),
   
    case MinVal of
        ?Inf ->
            distributors:send_to_neighbours(
                Neighs, 
                stop
            ),
            Result = distributors:wait_for_response(
                Neighs,
                result,
                fun(X, Y) ->
                    lists:append(X, Y)
                end,
                lists:zip(lists:seq(StartRow, EndRow), UpdateDist)
            ),
            helpers:hello(["result", Result]),
            helpers:hello(["end", erlang:system_time(), erlang:timestamp()]),
            ok;
        _ ->
            distributors:send_to_neighbours(
                Neighs,
                {
                    reduction, 
                    MinVertex,
                    MinVal        
                }
            ),
            map_task(
                NumVertices, 
                NumProcs, 
                MinVertex,
                MinVal, 
                LocalData, 
                UpdateDist, 
                UpdateVisited
            )
    end.




relax_edges(Vertex, Edges, LocalDist, Visited, BaseDist) ->
    { 
        relax_edges(Edges, LocalDist, 1, BaseDist),
        lists:append(Visited, [Vertex])
    }.
relax_edges(_, [], _, _) -> [];
relax_edges([Edge | RestEdges], [H|T], Index, BaseDist) ->
    case BaseDist + Edge < H of
        true -> 
            lists:append([BaseDist + Edge], relax_edges(RestEdges, T, Index+1, BaseDist));
        false ->
            lists:append([H], relax_edges(RestEdges, T, Index+1, BaseDist))
    end.


init_dijkstra(NumVertices, NumProcs, Source, LocalData) ->
    Rank = get_rank(),
    {StartRow, EndRow} = helpers:get_bounds(NumVertices, NumProcs),
    LocalDist = case { StartRow =< Source , Source =< EndRow } of
                    {true, true} ->
                        lists:append([lists:duplicate(Source-StartRow, ?Inf), [0], lists:duplicate(EndRow-Source, ?Inf)]);
                    {_, _} ->
                        lists:duplicate(EndRow-StartRow+1, ?Inf)
                end,
    
    case Rank of
        1 ->
            map_task(
                NumVertices,
                NumProcs,
                Source,
                0,
                LocalData,
                LocalDist,
                []
            );

        _ ->         
            reduce_task(
                    NumVertices,
                    NumProcs,
                    Source,
                    0,
                    LocalData,
                    LocalDist,
                    []
            )
    end.