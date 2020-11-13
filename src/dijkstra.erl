-module(dijkstra).

-export([map_task/6, reduce_task/6, relax_edges/3, relax_edges/5, get_bounds/3, init_dijkstra/6]).

-import(helpers,[make_displs/2, get_minimum_vert/2, get_minimum_vert/3, get_proc_rank/2, get_proc_rank/3]).
-import(distributors,[register_proc/2, read_int_line/1, send_to_neighbours/2, read_and_send/2, distribute_graph/6]).


map_task(NumVertices, NumProcs, Rank, Source, LocalData, LocalDist) ->
    {StartRow, EndRow} = get_bounds(NumVertices, NumProcs, Rank),
    send_to_neighbours( 
                lists:delete(Rank, lists:seq(1, NumProcs)), 
                {
                    update,
                    relax_edges(
                        Source, 
                        % get_row(LocalData, Source-StartRow),
                        1,
                        lists:append([lists:sublist(Source-1, LocalDist), 0, lists:nthtail(NumVertices-Source, LocalDist)])
                    ) 
                }).
%     get_minimum_vert(LocalDist).

reduce_task(NumVertices, NumProcs, Rank, Source, LocalData, LocalDist)->
    {StartRow, EndRow} = get_bounds(NumVertices, NumProcs, Rank),
    send_to_neighbours([get_proc_rank(NumVertices, NumProcs, Source)] , get_minimum_vert(LocalDist, StartRow)),
    receive
        {mapper, Rank} ->
            % map_task();
            ok;
        {reducer, Rank} ->
            ok; %Add something here
        true ->
            ok
    end.


relax_edges(Vertex, Edges, LocalDist) ->
    ets:insert(
            visited,
            {Vertex, true}
        ),
    relax_edges(Vertex, Edges, LocalDist, 1, list:nth(Vertex, LocalDist)).

relax_edges(Vertex, Edges, [H|T], Index, BaseDist) ->
    case BaseDist + list:nth(Index, Edges) < H of
        true -> 
            lists:append([BaseDist + list:nth(Index, Edges)], relax_edges(Vertex, Edges, [T], Index+1, BaseDist));
        false ->
            lists:append([H], relax_edges(Vertex, Edges, [T], Index+1, BaseDist))
    end.


get_bounds(NumVertices, NumProcs, Rank) ->
    StartRow = (Rank-1)*(NumVertices div NumProcs)+1,
    EndRow = case Rank of
        NumProcs ->
            Rank*(NumVertices div NumProcs) + NumVertices rem NumProcs;
        _ ->
            Rank*(NumVertices div NumProcs)
        end,
    {
        StartRow,
        EndRow
    }.

init_dijkstra(NumVertices, NumProcs, Rank, Source, LocalData, LocalDist) ->
    {StartRow, EndRow} = get_bounds(NumVertices, NumProcs, Rank),
    case { StartRow =< Source , Source >= EndRow } of
        {true, true} ->
            send_to_neighbours( 
                lists:delete(Source, lists:seq(1, NumProcs)), 
                {
                    update,
                    relax_edges(
                        Source, 
                        get_row(LocalData, Source-StartRow),
                        lists:append([lists:sublist(Source-1, LocalDist), 0, lists:nthtail(NumVertices-Source, LocalDist)])
                    ) 
                });
        {_, _} ->
            reduce_task(NumVertices, NumProcs, Rank, Source, LocalData, LocalDist)
        end.
