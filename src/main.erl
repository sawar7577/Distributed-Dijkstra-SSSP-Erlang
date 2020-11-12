-module(main).
-export([start/0, hello/1, proc_run/4]).

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


% ToDo - read_int_line, read_and_send, send_to_neighbours, distribute_graph
% reduce_task, map_task, init_dijkstra
% helpers - make_displs, get_proc_rank, get_minimum_vert

make_displs(NumVertices, NumProcs) -> 
    PerProc = NumVertices div NumProcs,
    RemProc = NumVertices rem NumProcs,
    InitDispls = lists:droplast(lists:seq(PerProc, NumVertices, PerProc)),
    lists:append(InitDispls, [lists:last(InitDispls)+PerProc+RemProc]).


spawner(NumVertices, NumProcs, Rank) ->
    spawn(main, proc_run, [NumVertices, NumProcs, Rank, []]).


get_proc_rank(Displs, RowNumber)->
    get_proc_rank(Displs, 1, RowNumber).
get_proc_rank(NumVertices, NumProcs, Vertex) when is_list(NumVertices) == false ->
    get_proc_rank(make_displs(NumVertices, NumProcs), Vertex);  
get_proc_rank(Displs, Rank, RowNumber)  ->
    % hello(["getting proc rank", Displs, Rank, RowNumber]),
    % hello(["checks", lists:nth(Rank, Displs), RowNumber]),
    case lists:nth(Rank, Displs) < RowNumber of
        true ->
            get_proc_rank(Displs, Rank+1, RowNumber);
        false ->
            Rank
    end.

get_minimum_vert(Dist, BaseVert) ->
    % hello([" init get min vert ", self(), get_rank(), Dist]),
    get_minimum_vert(Dist, 1, BaseVert).

get_minimum_vert([], _, _) ->
    % hello([" get min vert index", self(), -1, -1]),
    {?Inf, -1};


% [1]
% index = 1, retlist = {inf, -1} 
get_minimum_vert([H | T], Index, BaseVert) ->
    % hello([" get min vert index", self(), Index, BaseVert]),
    RetList = get_minimum_vert(T, Index+1, BaseVert),
    % hello(["get min vert", self(), get_rank(), Index, RetList]),
    % hello(["get min vert H T", self(), get_rank(), Index,H, T]),
    case element(1, RetList) < H orelse ets:member(visited, Index+BaseVert-1) of 
        false ->
            % hello(["false get min result", self(), get_rank(), {H, Index+BaseVert-1}]),
            {H, Index+BaseVert-1};
        true ->
            % hello(["true get min result", self(), get_rank(), RetList]),
            RetList
    end.


proc_run(NumVertices, NumProcs, Rank, LocalData) ->
    % hello(LocalData),
    % hello("self id\n"),
    % hello([self(), LocalData]),
    receive
        {input, Data} ->
            proc_run(NumVertices, NumProcs, Rank, lists:append(LocalData, Data));
        {other, Data} ->
            proc_run(NumVertices, NumProcs, Rank, LocalData);
        % {"source", Source} ->
        %     dijkstra(NumVertices, NumProcs, StartRow, EndRow, LocalData, Source);
        {init, Source} ->
            init_dijkstra(NumVertices, NumProcs, Source, LocalData),
            ok;
        stop ->
            % hello("stop"),
            hello([self(), LocalData]),
            ok
    end.

send_to_neighbours(Neighs, Msg) ->
    lists:foreach(
            fun(X) ->
                hello(["stn", self(), X, ets:lookup(procTable, X), Msg]),
                [{_, SPid}] = ets:lookup(procTable, X),
                SPid ! Msg
            end,
            Neighs
        ).

wait_for_response(Pids, Response, Accumulator) -> 
    wait_for_response(Pids, Response, Accumulator, {?Inf, -1}).

wait_for_response([], Response, Accumulator, Result) -> Result;
wait_for_response(Pids, Response, Accumulator, Result) ->
    % hello(["waiting ", self(), Pids]),
    receive
        {Response, Pid, Data} ->
            wait_for_response(lists:delete(Pid, Pids), Response, Accumulator, Accumulator(Result, Data))
    end.

read_and_send(Device, Id) ->
    % hello("Id"),
    % hello(Id),
    [{_, SPid}] = ets:lookup(procTable, Id),
    Row = read_int_line(Device),
    case Row of 
        [] -> 
            ok;
        [H|T] ->
            Msg = {input, Row},
            % hello(Msg),
            SPid ! Msg,
            [H|T]
    end.


distribute_graph(Device, NumVertices, NumProcs, Displs, CurRow, CurIndex) ->
    EndRow = lists:nth(CurIndex, Displs),
    if 
        CurRow > NumVertices ->
            % hello("out"),
            ok;
        CurRow =< EndRow ->
            read_and_send(Device, CurIndex),
            distribute_graph(Device, NumVertices, NumProcs, Displs, CurRow + 1, CurIndex);
        true ->
            register_proc(
                    spawner(NumVertices, NumProcs, CurIndex+1),                
                    CurIndex+1
                ),
            distribute_graph(Device, NumVertices, NumProcs, Displs, CurRow, CurIndex+1)
    end.
    

% updateDist(ResultVec, LocalData, Source, StartRow, EndRow) ->
    
% collect(ReduceProc) ->
%     ReduceProc ! {collect, self()}

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

reduce_task(NumVertices, NumProcs, Source, LocalData, LocalDist)->
    Rank = get_rank(),
    % ok.
    {StartRow, EndRow} = get_bounds(NumVertices, NumProcs),
    hello(["reduce", self(), LocalDist, StartRow, EndRow]),

    % hello(["sending to ", get_proc_rank(NumVertices, NumProcs, Source)]),
    send_to_neighbours([get_proc_rank(NumVertices, NumProcs, Source)] , {reduction, Rank, get_minimum_vert(lists:sublist(LocalDist, StartRow, EndRow-StartRow+1), StartRow)}),
    wait_command(NumVertices, NumProcs, Source, LocalData, LocalDist).
    

wait_command(NumVertices, NumProcs, Source, LocalData, LocalDist) ->
    Rank = get_rank(),
    receive
        {mapping, Rank, Vertex} ->
            hello(["going to mapper", self(), get_rank(), LocalData, LocalDist]),
            map_task(NumVertices, NumProcs, Vertex, LocalData, LocalDist);
        {reduction, VRank, Vertex} ->
            reduce_task(NumVertices, NumProcs, Vertex, LocalData, LocalDist);
        {update, GlobalDist} ->
            wait_command(NumVertices, NumProcs, Source, LocalData, GlobalDist)
    end.


map_task(NumVertices, NumProcs, Source, LocalData, LocalDist) ->
    % hello(["map", self()]),
    Rank = get_rank(),
    % ok.
    Neighs = lists:delete(Rank, lists:seq(1, NumProcs)),
    {StartRow, EndRow} = get_bounds(NumVertices, NumProcs),
    Accumulator = fun(X, Y) ->
                case element(1, X) < element(1, Y) of
                    true ->
                        X;
                    false ->
                        Y
                end
            end, 
    
    
    hello(["mapper", self(), get_rank(), Source, LocalData, LocalDist]),
    GlobalDist = relax_edges(Source, get_row(LocalData, Source-StartRow, NumVertices), LocalDist),
    send_to_neighbours(
        Neighs, 
        {
            update,
            GlobalDist
        }
    ),
    send_to_neighbours(
        Neighs, 
        {
            reduction,
            Rank,
            Source
        }
    ),

    {MinVal, MinVertex} = wait_for_response(
                Neighs,
                reduction,
                Accumulator,
                get_minimum_vert(lists:sublist(LocalDist, StartRow, EndRow-StartRow+1), StartRow)
            ),
    
    MinRank = get_proc_rank(NumVertices, NumProcs, MinVertex),


    send_to_neighbours(
        [MinRank],
        {
            mapping,
            MinRank, 
            MinVertex
        }
    ),

    wait_command(NumVertices, NumProcs, MinVertex, LocalData, GlobalDist).

relax_edges(Vertex, Edges, LocalDist) ->
    % hello(["start relaxing", Vertex, Edges, LocalDist]),
    ets:insert(
            visited,
            {Vertex, true}
        ),
    relax_edges(Vertex, Edges, LocalDist, 1, lists:nth(Vertex, LocalDist)).

relax_edges(Vertex, Edges, [], Index, BaseDist) -> [];
relax_edges(Vertex, Edges, [H|T], Index, BaseDist) ->
    % hello(["relaxing", self(), Vertex, H, T, Index]),
    case BaseDist + lists:nth(Index, Edges) < H of
        true -> 
            lists:append([BaseDist + lists:nth(Index, Edges)], relax_edges(Vertex, Edges, T, Index+1, BaseDist));
        false ->
            lists:append([H], relax_edges(Vertex, Edges, T, Index+1, BaseDist))
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

init_dijkstra(NumVertices, NumProcs, Source, LocalData) ->
    {StartRow, EndRow} = get_bounds(NumVertices, NumProcs),
    hello(["init", self(), get_rank(), LocalData]),
    % ok.
    case { StartRow =< Source , Source =< EndRow } of
        {true, true} ->
            hello("mapper"),
            map_task(NumVertices, NumProcs, Source, LocalData, lists:append([lists:duplicate(Source-1, ?Inf), [0], lists:duplicate(NumVertices-Source, ?Inf)]));
        {_, _} ->
            hello(["reducer", self(), get_rank(), LocalData]),
            wait_command(NumVertices, NumProcs, Source, LocalData, lists:duplicate(NumVertices, ?Inf))
    end.
 
register_proc(Pid, Id) ->
    ets:insert(procTable, {Id, Pid}),
    ets:insert(idTable, {Pid, Id}).

                
start() ->
    {ok, Device} = file:open("./graph.txt", [read]),
    [NumVertices, NumProcs, Source] = read_int_line(Device),
    hello(NumVertices),
    hello(NumProcs),
    hello(Source),
    Displs = lists:append(make_displs(NumVertices, NumProcs), [ ?Inf ] ),
    
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
    register_proc(self(), 1),
    distribute_graph(Device, NumVertices, NumProcs, Displs, lists:nth(1, Displs)+1, 1),
    % hello(NumVertices),
    % hello(NumProcs),
    % hello(Displs),  
    % [{_, SPid1}] = ets:lookup(procTable, 2),
    % SPid1 ! stop,
    % [{_, SPid2}] = ets:lookup(procTable, 3),
    % SPid2 ! stop,
    send_to_neighbours(lists:seq(2, NumProcs), {init, 1}),
    init_dijkstra(NumVertices, NumProcs, 1, LocalData),
    % send_to_neighbours([2, 3], stop),
    hello(["hey", get_proc_rank(Displs, 2)]),
    % hello(["minvert", get_minimum_vert([5, 6, 7, 8, 9, 1, 2, 3])]),
    ets:delete(procTable),
    ets:delete(idTable),
    ets:delete(visited),

    file:close(Device).

    