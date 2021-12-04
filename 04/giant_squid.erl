#!/usr/bin/env escript

-mode(compile).

main([]) ->
    io:format("syntax: giant_squid <1|2> <file.txt>~n"),
    halt(1);

main(["1", File]) ->
    {Numbers, Boards} = get_data(File),
    {NumberCalled, BoardWinner} = who_wins(Numbers, Boards),

    BoardSum =
        lists:sum([ E || E <- lists:flatten(BoardWinner), E =/= undefined ]),

    io:format("~b~n", [BoardSum * NumberCalled]),
    ok;

main(["2", File]) ->
    {Numbers, Boards} = get_data(File),
    [{NumberCalled, BoardWinner} | _] = ranking_inv(Numbers, Boards),

    BoardSum =
        lists:sum([ E || E <- lists:flatten(BoardWinner), E =/= undefined ]),

    io:format("~b~n", [BoardSum * NumberCalled]),
    ok.

get_data(File) ->
    {ok, Content} = file:read_file(File),
    [RawNumbers, _Sep | RawBoards] = [ L || L <- binary:split(Content, <<"\n">>, [global]) ],

    Numbers = [ binary_to_integer(N) || N <- binary:split(RawNumbers, <<",">>, [global]) ],

    Boards = parse_board(RawBoards),

    {Numbers, Boards}.

parse_board(Lines) ->
    parse_board(Lines, <<>>, [[]]).

parse_board([], _Sep, [[]|Acc]) -> lists:reverse(Acc);
parse_board([], _Sep, Acc) -> lists:reverse(Acc);
parse_board([Sep|Rest], Sep, [Board|Acc]) ->
    parse_board(Rest, Sep, [[], lists:reverse(Board)|Acc]);
parse_board([Line|Rest], Sep, [Board|Acc]) ->
    parse_board(Rest, Sep, [[parse_line(Line)|Board]|Acc]).

parse_line(RawLine) ->
    [ binary_to_integer(L) || L <- binary:split(RawLine, <<" ">>, [global, trim_all]) ].

who_wins([], _Boards) -> throw({error, nowinner});
who_wins([Number|Numbers], Boards) ->
    % eliminar ese número de los cartones
    NewBoards = mark_called(Number, Boards),

    % comprobar si hay un cartón ganador
    case get_winner(NewBoards) of
        undefined ->
            who_wins(Numbers, NewBoards);

        BoardWinner ->
            {Number, BoardWinner}
    end.

ranking_inv(Numbers, Boards) ->
    ranking_inv(Numbers, Boards, []).

ranking_inv([], _Boards, _Acc) -> throw({error, nowinner});
ranking_inv(_Numbers, [], Acc) -> lists:reverse(Acc);
ranking_inv([Number|Numbers], Boards, Acc) ->
    % eliminar ese número de los cartones
    NewBoards = mark_called(Number, Boards),

    % comprobar si hay un cartón ganador
    case get_winners(NewBoards) of
        undefined ->
            ranking_inv(Numbers, NewBoards);

        BoardWinners ->
            FilteredBoards = NewBoards -- BoardWinners,
            WinnersAcc = Acc ++ [ {Number, BoardWinner} || BoardWinner <- BoardWinners ],
            ranking_inv(Numbers, FilteredBoards, WinnersAcc)
    end.

mark_called(Number, Boards) ->
    lists:map(fun(Board) ->
        lists:map(fun(Row) ->
            lists:map(fun(Cell) when Cell =:= Number -> undefined;
                         (Cell) -> Cell
                      end, Row)
        end, Board)
    end, Boards).

zip(Lists) ->
    zip(Lists, []).

zip([[] | _], Acc) -> lists:reverse(Acc);
zip(Lists, Acc) ->
    {Row, NewLists} =
        lists:foldl(fun([H|T], {Row, AccLists}) ->
            {[H|Row], [T|AccLists]}
        end, {[], []}, Lists),
    zip(lists:reverse(NewLists), [lists:reverse(Row)|Acc]).

get_winners(Boards) ->
    get_winners(Boards, []).

get_winners([], []) -> undefined;
get_winners([], Acc) -> lists:reverse(Acc);
get_winners([Board|Boards], Acc) ->
    case winner_by_rows(Board) orelse winner_by_rows(zip(Board)) of
        true -> get_winners(Boards, [Board|Acc]);
        false -> get_winners(Boards, Acc)
    end.

get_winner([]) -> undefined;
get_winner([Board|Boards]) ->
    case winner_by_rows(Board) orelse winner_by_rows(zip(Board)) of
        true -> Board;
        false -> get_winner(Boards)
    end.

winner_by_rows(Board) ->
    lists:any(fun(Row) ->
        lists:all(fun(Cell) -> Cell =:= undefined end, Row)
    end, Board).
