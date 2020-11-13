-module(dijkstra).

-export([init_dijkstra/4]).

-import(helpers,[make_displs/2, get_minimum_vert/3, get_minimum_vert/4, get_proc_rank/2, get_proc_rank/3, get_rank/0, get_bounds/2, get_row/3, hello/1]).
-import(distributors,[send_to_neighbours/2, wait_for_response/3, wait_for_response/4]).
-include("macros.hrl").

reduce_task(NumVertices, NumProcs, Source, LocalData, LocalDist, Visited)->
    Rank = helpers:get_rank(),
    {StartRow, EndRow} = helpers:get_bounds(NumVertices, NumProcs),
    distributors:send_to_neighbours([helpers:get_proc_rank(NumVertices, NumProcs, Source)] , {reduction, Rank, get_minimum_vert(lists:sublist(LocalDist, StartRow, EndRow-StartRow+1), StartRow, Visited)}),
    wait_command(
        NumVertices, 
        NumProcs, 
        Source, 
        LocalData, 
        LocalDist, 
        lists:append(Visited, [Source])
    ).
    

wait_command(NumVertices, NumProcs, Source, LocalData, LocalDist, Visited) ->
    Rank = helpers:get_rank(),
    receive
        {mapping, Rank, Vertex} ->
            map_task(
                NumVertices, 
                NumProcs, 
                Vertex, 
                LocalData, 
                LocalDist, 
                Visited
            );
        {reduction, _, Vertex} ->
            reduce_task(
                NumVertices, 
                NumProcs, 
                Vertex, 
                LocalData, 
                LocalDist, 
                Visited
            );
        {update, GlobalDist} ->
            wait_command(
                NumVertices, 
                NumProcs, 
                Source, 
                LocalData, 
                GlobalDist, 
                Visited
            );
        stop ->
            ok
    end.


map_task(NumVertices, NumProcs, Source, LocalData, LocalDist, Visited) ->
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
    
    
    GlobalDist = relax_edges(Source, get_row(LocalData, Source-StartRow, NumVertices), LocalDist),
    distributors:send_to_neighbours(
        Neighs, 
        {
            update,
            GlobalDist
        }
    ),
    distributors:send_to_neighbours(
        Neighs, 
        {
            reduction,
            Rank,
            Source
        }
    ),

    {MinVal, MinVertex} = distributors:wait_for_response(
                Neighs,
                reduction,
                Accumulator,
                helpers:get_minimum_vert(
                    lists:sublist(LocalDist, StartRow, EndRow-StartRow+1),
                    StartRow,
                    Visited
                )
            ),
    
    MinRank = helpers:get_proc_rank(NumVertices, NumProcs, MinVertex),

    case MinVal of
        ?Inf ->
            distributors:send_to_neighbours(
                Neighs, 
                stop
            ),
            helpers:hello(["result", GlobalDist]),
            ok;
        _ ->
            distributors:send_to_neighbours(
                [MinRank],
                {
                    mapping,
                    MinRank, 
                    MinVertex
                }
            ),
            wait_command(
                NumVertices, 
                NumProcs, 
                MinVertex, 
                LocalData, 
                GlobalDist, 
                lists:append(Visited, [Source])
            )
    end.

relax_edges(Vertex, Edges, LocalDist) ->
    relax_edges(Vertex, Edges, LocalDist, 1, lists:nth(Vertex, LocalDist)).
relax_edges(_, _, [], _, _) -> [];
relax_edges(Vertex, Edges, [H|T], Index, BaseDist) ->
    case BaseDist + lists:nth(Index, Edges) < H of
        true -> 
            lists:append([BaseDist + lists:nth(Index, Edges)], relax_edges(Vertex, Edges, T, Index+1, BaseDist));
        false ->
            lists:append([H], relax_edges(Vertex, Edges, T, Index+1, BaseDist))
    end.


init_dijkstra(NumVertices, NumProcs, Source, LocalData) ->
    {StartRow, EndRow} = helpers:get_bounds(NumVertices, NumProcs),
    case { StartRow =< Source , Source =< EndRow } of
        {true, true} ->
            map_task(
                NumVertices, 
                NumProcs, 
                Source, 
                LocalData, 
                lists:append([lists:duplicate(Source-1, ?Inf), [0], lists:duplicate(NumVertices-Source, ?Inf)]),
                []
            );
        {_, _} ->
            wait_command(
                NumVertices, 
                NumProcs, 
                Source, 
                LocalData, 
                lists:duplicate(NumVertices, ?Inf),
                []
            )
    end.
