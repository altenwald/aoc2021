#!/usr/bin/env escript

-mode(compile).

main([]) ->
    io:format("syntax: dive <1|2> <file.txt>~n"),
    halt(1);

main(["1", File]) ->
    Lines = get_data(File),
    {Horizontal, Depth} = dive1(Lines),
    io:format("~b~n", [Horizontal * Depth]),
    ok;

main(["2", File]) ->
    Lines = get_data(File),
    {Horizontal, Depth, _Aim} = dive2(Lines),
    io:format("~b~n", [Horizontal * Depth]),
    ok.

dive1(Data) ->
    dive1(Data, {0, 0}).

dive1([], Acc) -> Acc;
dive1([{horizontal, X} | Rest], {Horizontal, Depth}) ->
    dive1(Rest, {Horizontal + X, Depth});
dive1([{vertical, X} | Rest], {Horizontal, Depth}) ->
    dive1(Rest, {Horizontal, Depth + X}).

dive2(Data) ->
    dive2(Data, {0, 0, 0}).

dive2([], Acc) -> Acc;
dive2([{horizontal, X} | Rest], {Horizontal, Depth, Aim}) ->
    dive2(Rest, {Horizontal + X, Depth + Aim * X, Aim});
dive2([{vertical, X} | Rest], {Horizontal, Depth, Aim}) ->
    dive2(Rest, {Horizontal, Depth, Aim + X}).

get_data(File) ->
    {ok, Content} = file:read_file(File),
    [ to_command(L) || L <- binary:split(Content, <<"\n">>, [global]), L =/= <<>> ].

to_command(<<"forward ", X/binary>>) -> {horizontal, binary_to_integer(X)};
to_command(<<"up ", X/binary>>) -> {vertical, -binary_to_integer(X)};
to_command(<<"down ", X/binary>>) -> {vertical, binary_to_integer(X)}.
