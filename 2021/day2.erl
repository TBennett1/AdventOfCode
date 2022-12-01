-module(day2).
-export([solve/0]).

get_input(Filename) ->
    {ok, File} = file:read_file(Filename),
    LinesBin = binary:split(File, <<"\n">>, [global]),
    L = [Bin || Bin <- LinesBin, Bin /= <<>>],
    [{binary_to_atom(Dir),binary_to_integer(Amount)} || D<-L, {Dir,Amount} <- binary:split(D,<<" ">>,[global])].


solve() ->
    Data = get_input("input2.txt"),
    io:format("~p~n",[Data]).