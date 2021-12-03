#!/usr/bin/env escript

-mode(compile).

main([]) ->
    io:format("syntax: sonar_sweep <1|2> <file.txt>~n"),
    halt(1);

main(["1", File]) ->
    Lines = get_data(File),
    io:format("~b~n", [count_increases(Lines)]),
    ok;

main(["2", File]) ->
    Lines = get_data(File),
    Windows = get_windows(Lines),
    io:format("~b~n", [count_increases(Windows)]),
    ok.

get_data(File) ->
    {ok, Content} = file:read_file(File),
    [ binary_to_integer(L) || L <- binary:split(Content, <<"\n">>, [global]), L =/= <<>> ].

count_increases(Lines) ->
    count_increases(Lines, 0).

count_increases([_], Acc) -> Acc;
count_increases([A, B|L], Acc) when A < B ->
    count_increases([B|L], Acc + 1);
count_increases([_A, B|L], Acc) ->
    count_increases([B|L], Acc).

get_windows(Lines) ->
    get_windows(Lines, []).

get_windows([A, B, C], Acc) ->
    lists:reverse([A + B + C|Acc]);
get_windows([A, B, C|Lines], Acc) ->
    get_windows([B, C|Lines], [A + B + C|Acc]).
