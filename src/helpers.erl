-module(helpers).
-export([make_displs/2, get_minimum_vert/2, get_minimum_vert/3, get_proc_rank/2, get_proc_rank/3]).

-include("macros.hrl").

make_displs(NumVertices, NumProcs) -> 
    PerProc = NumVertices div NumProcs,
    RemProc = NumVertices rem NumProcs,
    InitDispls = lists:droplast(lists:seq(PerProc, NumVertices, PerProc)),
    lists:append(InitDispls, [lists:last(InitDispls)+PerProc+RemProc]).

get_minimum_vert(Dist, BaseVert) ->
    get_minimum_vert(Dist, 1, BaseVert).

get_minimum_vert([], _, _) ->
    {?Inf, -1};

get_minimum_vert([H | T], Index, BaseVert) ->
    RetList = get_minimum_vert(T, Index+1, BaseVert),
    case {element(1, RetList) < H, ets:member(visited, H)} of 
        {false, false} ->
            {H, Index};
        {_, _} ->
            RetList
    end.

get_proc_rank(Displs, RowNumber)->
    get_proc_rank(Displs, 1, RowNumber).
get_proc_rank(NumVertices, NumProcs, Vertex) when is_list(NumVertices) == false ->
    get_proc_rank(make_displs(NumVertices, NumProcs), Vertex);  
get_proc_rank(Displs, Rank, RowNumber)  ->
    case lists:nth(Rank, Displs) =< RowNumber of
        true ->
            get_proc_rank(Displs, Rank+1, RowNumber);
        false ->
            Rank-1
    end.
