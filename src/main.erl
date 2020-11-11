-module(main).
-export([start/0, hello/1, proc_run/5]).

-define(Inf, 100000000).

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


spawner(NumVertices, NumProcs, StartRow, EndRow) ->
    spawn(main, proc_run, [NumVertices, NumProcs, StartRow, EndRow, []]).


proc_run(NumVertices, NumProcs, StartRow, EndRow, LocalData) ->
    % hello(LocalData),
    % hello("self id\n"),
    % hello([self(), LocalData]),
    receive
        {"input", Data} ->
            proc_run(NumVertices, NumProcs, StartRow, EndRow, lists:append(LocalData, Data));
        {"other", Data} ->
            proc_run(NumVertices, NumProcs, StartRow, EndRow, LocalData);
        stop ->
            % hello("stop"),
            hello([self(), LocalData]),
            ok
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
        CurRow ==  EndRow ->
            % hello(["created", CurIndex]),
            ets:insert(
                procTable, 
                {
                    CurIndex, 
                    spawner(NumVertices, NumProcs, CurRow, lists:nth(CurIndex, Displs))
                }
            ),
            read_and_send(Device, CurIndex),
            distribute_graph(Device, NumVertices, NumProcs, Displs, CurRow+1, CurIndex+1);
        true ->
            read_and_send(Device, CurIndex-1),
            distribute_graph(Device, NumVertices, NumProcs, Displs, CurRow+1, CurIndex)
    end.
    

                
start() ->
    {ok, Device} = file:open("./graph.txt", [read]),
    [NumVertices, NumProcs] = read_int_line(Device),
    hello(NumVertices),
    hello(NumProcs),
    Displs = lists:append(make_displs(NumVertices, NumProcs), [ ?Inf ] ),
    
    ets:new(procTable, [named_table, public]),
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
    ets:insert(
                procTable, 
                {
                    2, 
                    spawner(NumVertices, NumProcs, lists:nth(1, Displs), lists:nth(2, Displs))
                }
            ),
   

    hello([self(), LocalData]),
    distribute_graph(Device, NumVertices, NumProcs, Displs, lists:nth(2, Displs), 3),
    % hello(NumVertices),
    % hello(NumProcs),
    % hello(Displs),  
    [{_, SPid1}] = ets:lookup(procTable, 2),
    SPid1 ! stop,
    [{_, SPid2}] = ets:lookup(procTable, 3),
    SPid2 ! stop,

    ets:delete(procTable),
    file:close(Device).

    