#!/usr/bin/env escript

-mode(compile).

main([]) ->
    io:format("syntax: smoke_basin <1|2> <file.txt>~n"),
    halt(1);

main(["1", File]) ->
    Matrix = get_data(File),
    {LowN, _Pos} = solve1(Matrix),
    io:format("~b~n", [lists:sum(LowN) + length(LowN)]),
    ok;

main(["2", File]) ->
    Matrix = get_data(File),
    {_LowN, Pos} = solve1(Matrix),
    [A, B, C|_] = solve2(Matrix, Pos),
    io:format("~b~n", [A * B * C]),
    ok.

get_data(File) ->
    {ok, Content} = file:read_file(File),
    [ [ C - $0 || C <- binary_to_list(L) ] || L <- binary:split(Content, <<"\n">>, [global]), L =/= <<>> ].

solve1(Data) ->
    solve1(Data, 1, 1, [], []).

solve1(Data, I, _J, Acc, Pos) when length(Data) < I -> {Acc, Pos};
solve1([Row|_] = Data, I, J, Acc, Pos) when length(Row) < J -> solve1(Data, I+1, 1, Acc, Pos);
solve1(Data, I, J, Acc, Pos) ->
    N = cell(I, J, Data),
    Adjacent = lists:filter(fun(X) -> X =/= undefined end, [
        cell(I+1, J, Data),
        cell(I-1, J, Data),
        cell(I, J+1, Data),
        cell(I, J-1, Data)
    ]),
    case lists:all(fun(M) -> M > N end, Adjacent) of
        true -> solve1(Data, I, J+1, [N|Acc], [{I, J}|Pos]);
        false -> solve1(Data, I, J+1, Acc, Pos)
    end.

solve2(Data, Pos) ->
    solve2(Data, Pos, []).

solve2(_Data, [], Acc) ->
    lists:sort(fun(A, B) -> A >= B end, Acc);
solve2(Data, [Pos|RestPos], Acc) ->
    LenBasin = length(solve2_pos(Data, Pos, [])),
    solve2(Data, RestPos, [LenBasin|Acc]).

solve2_pos(Data, {I, J}, Acc) ->
    case {cell(I, J, Data), lists:member({I, J}, Acc)} of
        {undefined, _} -> Acc;
        {9, _} -> Acc;
        {_, true} -> Acc;
        _ ->
            Adjacent = [
                {I-1, J},
                {I+1, J},
                {I, J-1},
                {I, J+1}
            ],
            lists:foldl(fun(Pos, A) ->
                solve2_pos(Data, Pos, A)
            end, [{I, J}|Acc], Adjacent)
    end.

cell(I, _J, _Data) when I < 1 -> undefined;
cell(_I, J, _Data) when J < 1 -> undefined;
cell(I, _J, Data) when length(Data) < I -> undefined;
cell(_I, J, [Row|_]) when length(Row) < J -> undefined;
cell(I, J, Data) ->
    lists:nth(J, lists:nth(I, Data)).
