-module(day4).

-export([solve/0]).

solve() ->
    Input = get_input("input4.txt"),
    io:format("Part 1: ~w~n", [solve1(Input)]),
    io:format("Part 2: ~w~n", [solve2(Input)]).

get_input(Filename) ->
    {ok, File} = file:read_file(Filename),
    RawLinesBin = binary:split(File, <<"\n">>, [global]),
    lists:map(fun(Line) -> 
        [R1, R2, R3, R4] = binary:split(Line, [<<",">>, <<"-">>], [global]),
        S1 = sets:from_list(lists:seq(list_to_integer(binary:bin_to_list(R1)), list_to_integer(binary:bin_to_list(R2)))),
        S2 = sets:from_list(lists:seq(list_to_integer(binary:bin_to_list(R3)), list_to_integer(binary:bin_to_list(R4)))),
        {S1, S2}
    end, RawLinesBin).

solve1(Input) ->
    lists:foldl(fun({S1,S2}, Sum) -> 
        case {sets:is_subset(S1, S2), sets:is_subset(S2, S1)} of
            {true, _} ->
                1+Sum;
            {_, true} ->
                1+Sum;
            {false, false} ->
                Sum
        end
    end, 0, Input).

solve2(Input) ->
    lists:foldl(fun({S1,S2}, Sum) -> 
        case sets:size(sets:intersection(S1,S2)) of 
            0 ->
                Sum;
            _ ->
                1+Sum
        end
    end, 0, Input).