-module(main).
-export([start/0, hello/1, proc_run/4]).

-include("macros.hrl").

-import(distributors,[register_proc/2, read_int_line/1, send_to_neighbours/2, read_and_send/2, distribute_graph/6]).
-import(helpers,[make_displs/2, get_minimum_vert/2, get_minimum_vert/3, get_proc_rank/2, get_proc_rank/3]).


% ToDo - read_int_line, read_and_send, send_to_neighbours, distribute_graph
% reduce_task, map_task, init_dijkstra
% helpers - make_displs, get_proc_rank, get_minimum_vert
hello(Line)->
    % io:fwrite("hello"),
    io:fwrite("~p~n", [Line]).

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

wait_for_response([], Response, Accumulator, Result) -> Result;
wait_for_response(Pids, Response, Accumulator, Result) ->
    receive
        {Response, Pid, Data} ->
            wait_for_response(lists:delete(Pid, Pids), Response, Accumulator, Accumulator(Result, Data))
    end.

    
% % updateDist(ResultVec, LocalData, Source, StartRow, EndRow) ->
    
% % collect(ReduceProc) ->
% %     ReduceProc ! {collect, self()}



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
    send_to_neighbours([2, 3], stop),
    hello(["hey", get_proc_rank(Displs, 2)]),
    hello(["minvert", get_minimum_vert([5, 6, 7, 8, 9, 1, 2, 3],1)]),
    ets:delete(procTable),
    ets:delete(idTable),

    file:close(Device).

    