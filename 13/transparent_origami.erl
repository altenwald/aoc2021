#!/usr/bin/env escript

-mode(compile).

main([]) ->
    io:format("syntax: transparent_origami <1|2> <file.txt>~n"),
    halt(1);

main(["1", File]) ->
    {Points, [Fold | _]} = get_data(File),
    NewPoints = fold(Points, Fold),
    io:format("~b~n", [length(lists:usort(NewPoints))]),
    ok;

main(["2", File]) ->
    {Points, Folds} = get_data(File),
    NewPoints = lists:foldl(fun (Fold, AccPoints) -> fold(AccPoints, Fold) end, Points, Folds),
    draw(NewPoints),
    ok.

get_max_x(Points) ->
    lists:foldl(fun({X, _}, Acc) when X > Acc -> X; (_, Acc) -> Acc end, 0, Points).

get_max_y(Points) ->
    lists:foldl(fun({_, Y}, Acc) when Y > Acc -> Y; (_, Acc) -> Acc end, 0, Points).

draw(Points) ->
    XMax = get_max_x(Points),
    YMax = get_max_y(Points),
    lists:foreach(fun(Y) ->
        lists:foreach(fun(X) ->
            case lists:member({X, Y}, Points) of
                true -> io:format("#");
                false -> io:format(".")
            end
        end, lists:seq(XMax, 0, -1)),
        io:format("\n")
    end, lists:seq(YMax, 0, -1)),
    Points.

fold(Points, {fold, y, N}) ->
    [ {X, abs(Y - N) - 1} || {X, Y} <- Points, abs(Y - N) =/= 0 ];

fold(Points, {fold, x, N}) ->
    [ {abs(X - N) - 1, Y} || {X, Y} <- Points, abs(X - N) =/= 0 ].

get_data(File) ->
    {ok, Content} = file:read_file(File),
    Output = [ to_point(binary:split(L, <<",">>)) || L <- binary:split(Content, <<"\n">>, [global]), L =/= <<>> ],
    lists:partition(fun(S) -> tuple_size(S) =:= 2 end, Output).

to_point([X, Y]) ->
    {binary_to_integer(X), binary_to_integer(Y)};
to_point([<<"fold along y=", N/binary>>]) ->
    {fold, y, binary_to_integer(N)};
to_point([<<"fold along x=", N/binary>>]) ->
    {fold, x, binary_to_integer(N)}.
