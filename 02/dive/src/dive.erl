-module(dive).

-export([process/1]).

process(File) ->
    Lines = get_data(File),
    lists:foreach(fun dive_server:move/1, Lines).

get_data(File) ->
    {ok, Content} = file:read_file(File),
    [ to_command(L) || L <- binary:split(Content, <<"\n">>, [global]), L =/= <<>> ].

to_command(<<"forward ", X/binary>>) -> {horizontal, binary_to_integer(X)};
to_command(<<"up ", X/binary>>) -> {vertical, -binary_to_integer(X)};
to_command(<<"down ", X/binary>>) -> {vertical, binary_to_integer(X)}.