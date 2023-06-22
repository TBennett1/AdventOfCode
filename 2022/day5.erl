-module(day5).

-export([solve/0]).

solve() ->
    Input = get_input("input5.txt"),
    io:format("Part 1: ~w~n", [solve1(Input)]),
    io:format("Part 2: ~w~n", [solve2(Input)]).

get_input(Filename) ->
    {ok, File} = file:read_file(Filename),
    [State0, Ins0] = binary:split(File, <<"\n\n">>, [global]),
    State1 = binary:split(State0, <<"\n">>, [global]),
    Ins = binary:split(Ins0, <<"\n">>, [global]), 
    RowNums = lists:nth(length(State1), State1),
    RowNums0 = [binary_to_integer(binary:replace(RowNum, <<" ">>, <<>>, [global])) || RowNum <- binary:split(RowNums, <<"  ">>, [global])],
    Nums = [{X, []} || X <- lists:seq(1, length(RowNums0))],
    PreState = maps:from_list(Nums),
    
    State = lists:foldl(fun(Row, St) ->
                        io:format("Current Row: ~p~n", [Row]),
                        <<F:3/binary, _:1/binary, S:3/binary, _:1/binary, T:3/binary>> = Row,
                        io:format("F: ~p~nS: ~p~nT: ~p~n", [F,S,T]),
                        OldF = maps:get(1, St),
                        NewF = binary:bin_to_list(binary:replace(F, <<"   ">>, <<>>)) ++ OldF,

                        OldS = maps:get(2, St),
                        NewS = binary:bin_to_list(binary:replace(S, <<"   ">>, <<>>)) ++ OldS,

                        OldT = maps:get(3, St),
                        NewT = binary:bin_to_list(binary:replace(T, <<"   ">>, <<>>)) ++ OldT,

                        NewState = [{1, NewF}, {2, NewS}, {3, NewT}],
                        io:format("NewState: ~p~n", [NewState]),
                        maps:merge(St, maps:from_list(NewState))
                    end,
                    PreState,
                    lists:droplast(State1)),
    { State,
     lists:map(fun(Bin) ->
                  <<_:5/binary,
                    Amount:1/binary,
                    _:6/binary,
                    From:1/binary,
                    _:4/binary,
                    To:1/binary>> =
                      Bin,
                  {binary_to_integer(Amount), binary_to_integer(From), binary_to_integer(To)}
               end,
               Ins)}.

solve1(Input) ->
    % [
    %   {1,{"[]","[D]","[]"}},
    %   {2,{"[N]","[C]","[]"}},
    %   {3,{"[Z]","[M]","[P]"}}
    % ]
    % state is a 2d list; ins is list of {Amount, From, To}
    {State, Ins} = Input,
    % State.
    lists:foldl(fun(I, S) -> instruct({S, I}) end, State, Ins).

instruct({State, {Amount, From, To}}) ->
    FromCol = maps:get(From, State),
    ToCol = maps:get(To, State),
    What = lists:nthtail(length(FromCol) - Amount, FromCol),
    NewState0 = maps:update(To, ToCol++What, State),
    maps:update(From, FromCol--What, NewState0).

solve2(_Input) ->
    ok.
