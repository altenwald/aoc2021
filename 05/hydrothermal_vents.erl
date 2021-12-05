#!/usr/bin/env escript

%% When we activate this... FAILS!
%%-mode(compile).

main([]) ->
    io:format("syntax: hydrothermal_vents <1|2> <file.txt>~n"),
    halt(1);

main(["1", File]) ->
    Lines = get_data(File),
    Points = expand_lines(v1, Lines),
    MaxPoints = length([ V || V <- maps:values(Points), V >= 2 ]),
    io:format("~b~n", [MaxPoints]),
    ok;

main(["2", File]) ->
    Lines = get_data(File),
    Points = expand_lines(v2, Lines),
    MaxPoints = length([ V || V <- maps:values(Points), V >= 2 ]),
    io:format("~b~n", [MaxPoints]),
    ok.

expand_lines(Version, Lines) ->
    expand_lines(Version, Lines, #{}).

-spec expand_lines(v1 | v2, [term()], #{}) -> #{}.
expand_lines(_Version, [], Acc) -> Acc;
expand_lines(Version, [{vertical, X, Y1, Y2}|Lines], Acc) ->
    NewAcc =
        lists:foldl(fun(Yi, A) ->
            case A of
                #{{X, Yi} := V} -> maps:put({X, Yi}, V + 1, A);
                _ -> maps:put({X, Yi}, 1, A)
            end
        end, Acc, lists:seq(Y1, Y2)),

    expand_lines(Version, Lines, NewAcc);
expand_lines(Version, [{horizonal, X1, Y, X2}|Lines], Acc) ->
    NewAcc =
        lists:foldl(fun(Xi, A) ->
            case A of
                #{{Xi, Y} := V} -> maps:put({Xi, Y}, V + 1, A);
                _ -> maps:put({Xi, Y}, 1, A)
            end
        end, Acc, lists:seq(X1, X2)),

    expand_lines(Version, Lines, NewAcc);
expand_lines(v1, [{diagonal, _X, _Y, _Size}|Lines], Acc) ->
    expand_lines(v1, Lines, Acc);
expand_lines(v2, [{diagonal, X1, Y1, {{X2, XIncr}, {Y2, YIncr}}}|Lines], Acc) ->
    NewAcc =
        lists:foldl(fun({Xi, Yi}, A) ->
            case A of
                #{{Xi, Yi} := V} -> maps:put({Xi, Yi}, V + 1, A);
                _ -> maps:put({Xi, Yi}, 1, A)
            end
        end, Acc, lists:zip(lists:seq(X1, X2, XIncr), lists:seq(Y1, Y2, YIncr))),

    expand_lines(v2, Lines, NewAcc).

get_data(File) ->
    {ok, Content} = file:read_file(File),
    [ parse_line(L) || L <- binary:split(Content, <<"\n">>, [global]), L =/= <<>> ].

parse_line(Line) ->
    Regexp = "^(\\d+),(\\d+) -> (\\d+),(\\d+)$",
    Opts = [{capture, all_but_first, binary}],
    case re:run(Line, Regexp, Opts) of
        {match, [X1, Y, X2, Y]} ->
            Yint = binary_to_integer(Y),
            case {binary_to_integer(X1), binary_to_integer(X2)} of
                {X1int, X2int} when X1int < X2int -> ok;
                {X2int, X1int} -> ok
            end,
            {horizonal, X1int, Yint, X2int};

        {match, [X, Y1, X, Y2]} ->
            Xint = binary_to_integer(X),
            case {binary_to_integer(Y1), binary_to_integer(Y2)} of
                {Y1int, Y2int} when Y1int < Y2int -> ok;
                {Y2int, Y1int} -> ok
            end,
            {vertical, Xint, Y1int, Y2int};

        {match, [X1, Y1, X2, Y2]} ->
            X1int = binary_to_integer(X1),
            Y1int = binary_to_integer(Y1),
            X2int = binary_to_integer(X2),
            Y2int = binary_to_integer(Y2),
            S1 = if X1int > X2int -> {X2int, -1};
                    true -> {X2int, 1}
                 end,
            S2 = if Y1int > Y2int -> {Y2int, -1};
                    true -> {Y2int, 1}
                 end,
            {diagonal, X1int, Y1int, {S1, S2}}
    end.
