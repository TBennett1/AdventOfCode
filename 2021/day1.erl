-module(day1).
-export([solve/0]).

get_input(Filename) ->
    {ok, File} = file:read_file(Filename),
    LinesBin = binary:split(File, <<"\n">>, [global]),
    [binary_to_integer(Bin) || Bin <- LinesBin, Bin /= <<>>].

sum([X,Y,Z | _]) -> X+Y+Z;
sum(_) -> undefined.

solve()->
    Input = get_input("input.txt"),
    Acc = 0,
    solve1(Input, Acc),
    solve2(Input, Acc).
solve1(Input, Acc)->
    % C  = length(Input),
    % io:format("~p~n",[C]).
    % io:format("~p~n",[Input]).
    case Input of
        [_] ->
            Acc;
        [] ->
            Acc;
        [H | T] when T =/= [] -> 
            N = lists:nth(1,T),
            if 
                H< N->
                    solve1(T, 1+Acc);
                true->
                    solve1(T,Acc)
            end
    end.
solve2([], Acc) ->
    Acc;
solve2([_ | R] = Data, Acc) ->
    case {sum(Data), sum(R)} of
        {A,B} when is_integer(A), is_integer(B), B > A ->
            solve2(R, Acc+1);
        _ ->
            solve2(R, Acc)
    end.