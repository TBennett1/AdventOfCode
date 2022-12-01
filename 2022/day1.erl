-module(day1).

-export([solve/0]).

get_input(Filename) ->
    {ok, File} = file:read_file(Filename),
    RawLinesBin = binary:split(File, <<"\n\n">>, [global]),
    LinesBin = lists:map(fun(Bin) -> binary:replace(Bin, <<"\n">>, <<",">>, [global]) end, RawLinesBin),
    LBins = [binary:split(LineBin, <<",">>, [global]) || LineBin <- LinesBin],
    [lists:map(fun(Bin) -> binary_to_integer(Bin) end, Bins) || Bins <- LBins].


solve() ->
    Input = get_input("input1-1.txt"), % lists of lists of int
    io:format("Part 1: ~w~n", [solve1(Input)]),
    io:format("Part 2: ~w~n", [solve2(Input)]).

solve1(Input) ->
    lists:max(lists:map(fun lists:sum/1, Input)).

solve2(Input) ->
    % sum sublist; sort in descending order; grab first 3; get sum
    lists:sum(lists:sublist((lists:sort(fun(X,Y) -> X>Y end, lists:map(fun lists:sum/1, Input))), 3)).