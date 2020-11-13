-module(helpers).
-export([
    make_displs/2,
    get_minimum_vert/2,
    get_minimum_vert/3,
    get_proc_rank/2,
    get_proc_rank/3,
    get_bounds/2,
    get_bounds/3,
    get_pid/1,
    get_rank/0,
    get_rank/1,
    get_row/3]).

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
    case element(1, RetList) < H orelse ets:member(visited, Index+BaseVert-1) of 
        false ->
            {H, Index+BaseVert-1};
        true ->
            RetList
    end.


get_proc_rank(Displs, RowNumber)->
    get_proc_rank(Displs, 1, RowNumber).
get_proc_rank(NumVertices, NumProcs, Vertex) when is_list(NumVertices) == false ->
    get_proc_rank(make_displs(NumVertices, NumProcs), Vertex);  
get_proc_rank(Displs, Rank, RowNumber)  ->
   case lists:nth(Rank, Displs) < RowNumber of
        true ->
            get_proc_rank(Displs, Rank+1, RowNumber);
        false ->
            Rank
    end.

get_bounds(NumVertices, NumProcs) -> 
    get_bounds(NumVertices, NumProcs, get_rank()).
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


get_pid(Rank) ->
    [{_, Pid}] = ets:lookup(procTable, Rank),
    Pid.
get_rank() -> 
    get_rank(self()).
get_rank(Pid) ->
    [{_, Rank}] = ets:lookup(idTable, Pid),
    Rank.

get_row(Data, RowNumber, NumVertices) ->
    % hello(["get row", lists:sublist(Data, NumVertices*RowNumber+1, NumVertices)]),
    lists:sublist(Data, NumVertices*RowNumber+1, NumVertices).

