-module(distributors).

-include("macros.hrl").
-export([
    % register_proc/2, 
    read_int_line/1, 
    send_to_neighbours/2, 
    wait_for_response/3,
    wait_for_response/4, 
    read_and_send/2]).

-import(helpers, [hello/1]).




% register_proc(Pid, Id) ->
%     ets:insert(procTable, {Id, Pid}),
%     ets:insert(idTable, {Pid, Id}).

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


send_to_neighbours(Neighs, Msg) ->
    lists:foreach(
            fun(Pid) ->
                % helpers:hello(["pid", Pid, self(), Msg]),
            %    [{_, SPid}] = ets:lookup(procTable, Pid),
                Pid ! Msg
            end,
            Neighs
        ).

wait_for_response(Pids, Response, Accumulator) -> 
    wait_for_response(Pids, Response, Accumulator, {?Inf, -1}).

wait_for_response([], _, _, Result) -> Result;
wait_for_response(Pids, Response, Accumulator, Result) ->
    receive
        {Response, Pid, Data} ->
            wait_for_response(
                lists:delete(Pid, Pids), 
                Response, 
                Accumulator, 
                Accumulator(Result, Data)
            )
    end.

read_and_send(Device, Id) ->
    % [{_, SPid}] = ets:lookup(procTable, Id),
    Row = read_int_line(Device),
    case Row of 
        [] -> 
            ok;
        [H|T] ->
            Msg = {input, Row},
            Id ! Msg,
            [H|T]
    end.

