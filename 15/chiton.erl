#!/usr/bin/env escript

-mode(compile).

main([]) ->
    io:format("syntax: chiton <1|2> <file.txt>~n"),
    halt(1);

main(["1", File]) ->
    Matrix = get_data(File),
    io:format("~p~n", [solve1(Matrix)]),
    ok;

main(["2", File]) ->
    BigMatrix = expand_data(get_data(File)),
    io:format("~p~n", [solve1(BigMatrix)]),
    ok.

get_data(File) ->
    {ok, Content} = file:read_file(File),
    [ [ C - $0 || C <- binary_to_list(L) ] || L <- binary:split(Content, <<"\n">>, [global]), L =/= <<>> ].

expand_data(Matrix) ->
    [A1, A2, A3, A4, A5] =
        lists:foldl(fun(Row, [A1, A2, A3, A4, A5]) ->
            NewRow = expand_row(Row),
            [B1, B2, B3, B4, B5] =
                lists:map(fun(I) ->
                    [ mod(Cell + I) || Cell <- NewRow ]
                end, lists:seq(0, 4)),
            [A1 ++ [B1], A2 ++ [B2], A3 ++ [B3], A4 ++ [B4], A5 ++ [B5]]
        end, [[], [], [], [], []], Matrix),
    A1 ++ A2 ++ A3 ++ A4 ++ A5.

mod(N) when N > 9 -> N - 9;
mod(N) when N >= 1 andalso N =< 9 -> N.

expand_row(Row) ->
    expand_row(Row, Row, 1, lists:reverse(Row)).

expand_row(_OriginalRow, [], 4, FinalRow) ->
    lists:reverse(FinalRow);
expand_row(OriginalRow, [], I, FinalRow) ->
    expand_row(OriginalRow, OriginalRow, I + 1, FinalRow);
expand_row(OriginalRow, [Cell | Row], I, FinalRow) ->
    expand_row(OriginalRow, Row, I, [mod(Cell + I) | FinalRow]).

solve1([FirstRow|_] = Matrix) ->
    XMax = length(FirstRow),
    YMax = length(Matrix),
    io:format("~p~n", [{XMax, YMax}]),
    {_, Graph} =
        lists:foldl(fun(Row, {J, Graph}) ->
            {_, TmpGraph} =
                lists:foldl(fun(_Weight, {I, G}) ->
                    {I + 1, maps:put({I, J}, adjacents(I, J, Matrix), G)}
                end, {1, Graph}, Row),
            {J + 1, TmpGraph}
        end, {1, #{}}, Matrix),
    % io:format("~p~n", [Graph]),
    dijkstrafy(Graph, {1, 1}, {XMax, YMax}).

adjacents(I, J, Data) ->
    lists:filter(fun({X, _}) -> X =/= undefined end, [
        {cell(I+1, J, Data), {I+1, J}},
        {cell(I, J+1, Data), {I, J+1}},
        {cell(I, J-1, Data), {I, J-1}},
        {cell(I-1, J, Data), {I-1, J}}
    ]).

cell(I, _J, _Data) when I < 1 -> undefined;
cell(_I, J, _Data) when J < 1 -> undefined;
cell(I, _J, Data) when length(Data) < I -> undefined;
cell(_I, J, [Row|_]) when length(Row) < J -> undefined;
cell(I, J, Data) ->
    lists:nth(J, lists:nth(I, Data)).

dijkstrafy(Graph, Start, End) when is_map(Graph) ->
    shortest_path(Graph, [{0, Start}], End, #{}).
 
shortest_path(_Graph, [], _End, _Visited) ->
    % if we're not going anywhere, it's time to start going back
    {0, []};
shortest_path(_Graph, [{Cost, End} | _ ], End, _Visited) ->
    % this is the base case, and finally returns the distance and the path
    Cost;
shortest_path(Graph, [{Cost, Node} | Routes], End, Visited) ->
    % this is the recursive case.
    % here we build a list of new "unvisited" routes, where the stucture is
    % a tuple of cost, then a list of paths taken to get to that cost from the "Start"
    NewRoutes = [{Cost + NewCost, NewNode}
        || {NewCost, NewNode} <- maps:get(Node, Graph),
            not is_map_key(NewNode, Visited)],
    % io:format("Max ~p Node (~p): ~p~n", [End, Cost, Node]),
    {_, FinalRoutes} =
        lists:sort(lists:foldl(fun
            ({N, _C}, {CurrNode, Acc}) when N =:= CurrNode -> {CurrNode, Acc};
            ({N, C}, {_CurrNode, Acc}) -> {N, [{C, N} | Acc]}
        end, {undefined, []}, lists:usort([ {N, C} || {C, N} <- NewRoutes ++ Routes ]))),

    shortest_path(
        Graph,
        % add the routes we ripped off earlier onto the new routes
        % that we want to visit. sort the list of routes to get the
        % shortest routes (lowest cost) at the beginning.
        % Erlangs sort is already good enough, and it will sort the
        % tuples by the number at the beginning of each (the cost).
        FinalRoutes,
        End,
        Visited#{Node => true}
    ).
