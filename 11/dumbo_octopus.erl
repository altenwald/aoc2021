#!/usr/bin/env escript

-mode(compile).

main([]) ->
    io:format("syntax: dumbo_octopus <1|2> <file.txt>~n"),
    halt(1);

main(["1", File]) ->
    Matrix = get_data(File),
    io:format("~p~n", [solve1(Matrix, 100)]),
    ok;

main(["2", File]) ->
    Matrix = get_data(File),
    io:format("~p~n", [solve2(Matrix)]),
    ok.

solve2(Matrix) ->
    solve2(Matrix, 1).

solve2(Matrix, Step) ->
    NewMatrix = solve_step(Matrix, []),
    case count_flashes(NewMatrix) of
        100 -> Step;
        _ -> solve2(NewMatrix, Step+1)
    end.

solve1(Matrix, Steps) ->
    solve1(Matrix, Steps, 0).

solve1(_Matrix, 0, Flahses) -> Flahses;
solve1(Matrix, Step, Flashes) ->
    NewMatrix = solve_step(Matrix, []),
    solve1(NewMatrix, Step-1, Flashes + count_flashes(NewMatrix)).

count_flashes(Matrix) ->
    length(lists:filter(fun(N) -> N =:= 0 end, lists:flatten(Matrix))).

solve_step(Matrix, Incr) ->
    {NewMatrix, NewIncr} = solve_step(Matrix, [[]], 1, 1, Incr),
    solve_incrs(NewMatrix, NewIncr).

solve_incrs(Matrix, []) -> Matrix;
solve_incrs(Matrix, Incr) ->
    {NewMatrix, NewIncr} = solve_incr(Matrix, [[]], 1, 1, Incr),
    solve_incrs(NewMatrix, NewIncr).

solve_incr([], [_EmptyRow | Matrix], _I, _J, Incr) ->
    {lists:reverse(Matrix), Incr};
solve_incr([[] | Rows], [NewRow | NewRows], _I, J, Incr) ->
    solve_incr(Rows, [[], lists:reverse(NewRow) | NewRows], 1, J + 1, Incr);
solve_incr([[0 | Row] | Rows], [NewRow | NewRows], I, J, Incr) ->
    NewIncr = lists:filter(fun(P) -> P =/= {I, J} end, Incr),
    NewMatrix = [[0 | NewRow] | NewRows],
    solve_incr([Row | Rows], NewMatrix, I+1, J, NewIncr);
solve_incr([[Cell | Row] | Rows], [NewRow | NewRows], I, J, Incr) ->
    case lists:member({I, J}, Incr) of
        true ->
            {Points, Incr0} = lists:partition(fun(P) -> P =:= {I, J} end, Incr),
            case length(Points) + Cell of
                N when N > 9 ->
                    NewMatrix = [[0 | NewRow] | NewRows],
                    Incr1 = adjacents(I, J) ++ Incr0,
                    solve_incr([Row | Rows], NewMatrix, I+1, J, Incr1);
                N ->
                    NewMatrix = [[N | NewRow] | NewRows],
                    solve_incr([Row | Rows], NewMatrix, I+1, J, Incr0)
            end;
        false ->
            solve_incr([Row | Rows], [[Cell | NewRow] | NewRows], I+1, J, Incr)
    end.

solve_step([], [_EmptyRow | Matrix], _I, _J, Incr) ->
    {lists:reverse(Matrix), Incr};
solve_step([[] | Rows], [NewRow | NewRows], _I, J, Incr) ->
    solve_step(Rows, [[], lists:reverse(NewRow) | NewRows], 1, J + 1, Incr);
solve_step([[9 | Row] | Rows], [NewRow | NewRows], I, J, Incr) ->
    NewMatrix = [[0 | NewRow] | NewRows],
    NewIncr = adjacents(I, J) ++ Incr,
    solve_step([Row | Rows], NewMatrix, I+1, J, NewIncr);
solve_step([[Cell | Row] | Rows], [NewRow | NewRows], I, J, Incr) ->
    NewMatrix = [[Cell + 1 | NewRow] | NewRows],
    solve_step([Row | Rows], NewMatrix, I+1, J, Incr).

get_data(File) ->
    {ok, Content} = file:read_file(File),
    [ [ C - $0 || C <- binary_to_list(L) ] || L <- binary:split(Content, <<"\n">>, [global]), L =/= <<>> ].

adjacents(I, J) ->
    Adjacent = [
        {I-1, J-1}, {I, J-1}, {I+1, J-1},
        {I-1, J}, {I+1, J},
        {I-1, J+1}, {I, J+1}, {I+1, J+1}
    ],
    lists:filter(fun({X, Y}) -> cell(X, Y) end, Adjacent).

cell(I, _J) when I < 1 -> false;
cell(I, _J) when 10 < I -> false;
cell(_I, J) when J < 1 -> false;
cell(_I, J) when 10 < J -> false;
cell(_I, _J) -> true.
