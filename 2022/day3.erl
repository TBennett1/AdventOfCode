-module(day3).

-export([solve/0]).

solve() ->
    Input = get_input("input3.txt"),
    io:format("Part 1: ~w~n", [solve1(Input)]),
    io:format("Part 2: ~w~n", [solve2(Input)]).

get_input(Filename) ->
    {ok, File} = file:read_file(Filename),
    RawLinesBin = binary:split(File, <<"\n">>, [global]),
    lists:map(fun binary:bin_to_list/1, RawLinesBin).

solve1(Rucks) ->
    SplitRucks = lists:map(fun(Ruck) -> lists:split(length(Ruck) div 2, Ruck) end, Rucks),
    lists:foldl(fun({L1, L2}, Sum) -> 
        S1 = sets:from_list(L1),
        S2 = sets:from_list(L2),
        Item = sets:to_list(sets:intersection(S1, S2)),
        case is_upper(Item) of
            true ->
                (hd(Item)-38)+Sum;
            false ->
                (hd(Item)-96)+Sum
        end
     end, 0, SplitRucks).

solve2(Rucks) ->
    Groups = group(Rucks, []),
    lists:foldl(fun({E1, E2, E3}, Sum) -> 
        S1 = sets:from_list(E1),
        S2 = sets:from_list(E2),
        S3 = sets:from_list(E3),
        S4 = sets:intersection(S1, S2),
        Badge = sets:to_list(sets:intersection(S3, S4)),
        case is_upper(Badge) of
            true ->
                (hd(Badge)-38)+Sum;
            false ->
                (hd(Badge)-96)+Sum
        end    
     end, 0, Groups).

group([], Acc) ->
    Acc;
group([A,B,C|Rest], Acc) ->
    group(Rest, [{A,B,C} | Acc]).

is_upper(Char) ->
    case re:run(Char, "[A-Z]") of
        {match, _} ->
            true;
        nomatch -> 
            false
    end.