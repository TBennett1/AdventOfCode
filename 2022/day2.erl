-module(day2).

-export([solve/0]).

get_input(Filename) ->
    {ok, File} = file:read_file(Filename),
    RawLinesBin = binary:split(File, <<"\n">>, [global]),
    lists:map(fun binary:bin_to_list/1, RawLinesBin).



solve() ->
    Input = get_input("input2.txt"), 
    % io:format("Input: ~w~n", [Input]), 
    io:format("Part 1: ~w~n", [solve1(Input)]),
    io:format("Part 2: ~w~n", [solve2(Input)]).

solve1(Input) ->
    Points = #{
                "A Y" => 8, 
                "A X" => 4, 
                "A Z" => 3, 
                "B Y" => 5, 
                "B X" => 1, 
                "B Z" => 9,
                "C Y" => 2, 
                "C X" => 7, 
                "C Z" => 6
            },
    lists:foldl(fun(Match, Sum) -> maps:get(Match, Points)+Sum end, 0, Input).

solve2(Input) ->
    Points = #{
                "A Y" => 4, 
                "A X" => 3, 
                "A Z" => 8, 
                "B Y" => 5, 
                "B X" => 1, 
                "B Z" => 9,
                "C Y" => 6, 
                "C X" => 2, 
                "C Z" => 7
            },
    lists:foldl(fun(Match, Sum) -> maps:get(Match, Points)+Sum end, 0, Input).