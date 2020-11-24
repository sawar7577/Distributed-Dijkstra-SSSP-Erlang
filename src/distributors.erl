-module(distributors).

-include("macros.hrl").
-export([
    read_int_line/1, 
    send_to_neighbours/2, 
    wait_for_response/2, 
    wait_for_response/3,
    wait_for_response/4, 
    read_and_send/2]).

-import(helpers, [hello/1]).

read_int_line(Device) ->
    Row = file:read_line(Device),
    Check_inf = fun(X) -> 
            {Int, _ } = string:to_integer(X),
            case Int == -1 of
                true -> ?Inf;
                false -> Int
            end
        end,
    case Row of 

        eof ->
            [];
        {ok, Line} ->
            lists:map(
                 Check_inf,
                 string:split(string:trim(Line), " ", all)
                )
    end.


send_to_neighbours(Neighs, Msg) ->
    lists:foreach(
            fun(Pid) ->
                % helpers:hello(["pid", Pid, self(), Msg]),
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
wait_for_response([], _) -> ok;
wait_for_response(Pids, Response) ->
    receive
        {Response, Pid} ->
            wait_for_response(
                lists:delete(Pid, Pids),
                Response
            )
    end.

read_and_send(Device, Id) ->
    Row = read_int_line(Device),
    case Row of 
        [] -> 
            ok;
        [H|T] ->
            Msg = {input, Row},
            Id ! Msg,
            [H|T]
    end.

