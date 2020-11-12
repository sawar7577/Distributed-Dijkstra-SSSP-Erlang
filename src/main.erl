-module(main).
-export([start/0, hello/1, proc_run/4]).

-define(Inf, 100000000).
-define(GetElement(R,C,N,L), list:nth(R*N + C, L)).

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


make_displs(NumVertices, NumProcs) ->
    PerProc = NumVertices div NumProcs,
    RemProc = NumVertices rem NumProcs,
    InitDispls = lists:droplast(lists:seq(PerProc, NumVertices, PerProc)),
    lists:append(InitDispls, [lists:last(InitDispls)+PerProc+RemProc]).


spawner(NumVertices, NumProcs, Rank) ->
    spawn(main, proc_run, [NumVertices, NumProcs, Rank, []]).

get_proc_rank(Displs, RowNumber)->
    get_proc_rank(Displs, 1, RowNumber).

get_proc_rank(Displs, Rank, RowNumber) ->
    case lists:nth(Rank, Displs) =< RowNumber of
        true ->
            get_proc_rank(Displs, Rank+1, RowNumber);
        false ->
            Rank-1
    end.

get_minimum_vert(Dist) ->
    get_minimum_vert(Dist, 1, {?Inf, -1}).

get_minimum_vert([], Index, _) ->
    {?Inf, Index};

get_minimum_vert([H | T], Index, MinTup) ->
    RetList = get_minimum_vert(T, Index+1, MinTup),
    case element(1, RetList) < H of 
        true ->
            RetList;
        false ->
            {H, Index}
        end.


proc_run(NumVertices, NumProcs, Rank, LocalData) ->
    % hello(LocalData),
    % hello("self id\n"),
    % hello([self(), LocalData]),
    receive
        {"input", Data} ->
            proc_run(NumVertices, NumProcs, Rank, lists:append(LocalData, Data));
        {"other", Data} ->
            proc_run(NumVertices, NumProcs, Rank, LocalData);
        % {"source", Source} ->
        %     dijkstra(NumVertices, NumProcs, StartRow, EndRow, LocalData, Source);
        stop ->
            % hello("stop"),
            hello([self(), LocalData]),
            ok
    end.

send_to_neighbours(Neighs, Msg) ->
    lists:foreach(
            fun(X) ->
                [{_, SPid}] = ets:lookup(procTable, X),
                SPid ! Msg
            end,
            Neighs
        ).

read_and_send(Device, Id) ->
    % hello("Id"),
    % hello(Id),
    [{_, SPid}] = ets:lookup(procTable, Id),
    Row = read_int_line(Device),
    case Row of 
        [] -> 
            ok;
        [H|T] ->
            Msg = {"input", Row},
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

map_task(NumVertices, NumProcs, Rank, LocalData, LocalDist) ->
    get_minimum_vert(LocalDist).

reduce_task(NumVertices, NumProcs, Rank, LocalData, LocalDist)->
    get_minimum_vert(LocalDist).


% init_dijkstra(NumVertices, NumProcs, StartRow, EndRow, Source) ->
%     case { StartRow =< Source , Source >= EndRow } of
%         {true, true} ->

%         {_, _} ->




% dijkstra(NumVertices, NumProcs, StartRow, EndRow, LocalData, Source) ->

%     LocalVec = list:duplicate(?Inf, EndRow - StartRow),
%     ResultVec = list:duplicate(?Inf, NumVertices),
%     IsSource = fun (StartRow, EndRow, Source) ->
%             if Source < StartRow ->
%                 false;
%             Source > EndRow ->
%                 false;
%             true ->
%                 true
%             end
%         end.
%     % Source = IsSource(StartRow, EndRow, Source),
%     % if Source ->
%         % NewRes = updateDist(ResultVec, LocalData, Source, StartRow, EndRow)
    
%     % end.


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
    % hello(Displs),
    % ets:insert(
    %             procTable, 
    %             {
    %                 2, 
    %                 spawner(NumVertices, NumProcs, lists:nth(1, Displs)+1, lists:nth(2, Displs))
    %             }
    %         ),
   
%[2, 4, 6, inf]
% (4,3)
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
    send_to_neighbours([2, 3], stop),
    hello(["hey", get_proc_rank(Displs, 2)]),
    hello(["minvert", get_minimum_vert([5, 6, 7, 8, 9, 1, 2, 3])]),
    ets:delete(procTable),
    ets:delete(idTable),

    file:close(Device).

    