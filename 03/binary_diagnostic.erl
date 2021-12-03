#!/usr/bin/env escript

-mode(compile).

main([]) ->
    io:format("syntax: binary_diagnostic <1|2> <file.txt>~n"),
    halt(1);

main(["1", File]) ->
    Lines = get_data(File),
    io:format("~b~n", [get_gamma_and_epsilon(Lines)]),
    ok;

main(["2", File]) ->
    Lines = get_data(File),
    io:format("~b~n", [oxygen_rating(Lines) * co2_scrubber_rating(Lines)]),
    ok.

get_data(File) ->
    {ok, Content} = file:read_file(File),
    [ binary_to_list(L) || L <- binary:split(Content, <<"\n">>, [global]), L =/= <<>> ].

get_gamma_and_epsilon(Data) ->
    Data1 = lists:foldl(fun(Elements, Acc) ->
        {Zeros, Ones} = lists:partition(fun(E) -> E =:= $0 end, Elements),
        case {length(Zeros), length(Ones)} of
            {Z, O} when Z >= O -> [$0|Acc];
            _ -> [$1|Acc]
        end
    end, [], zip(Data)),
    GammaStr = lists:reverse(Data1),
    EpsilonStr = [ if G =:= $0 -> $1; true -> $0 end || G <- GammaStr ],
    list_to_integer(GammaStr, 2) * list_to_integer(EpsilonStr, 2).

zip(Lists) ->
    zip(Lists, []).

zip([[] | _], Acc) -> lists:reverse(Acc);
zip(Lists, Acc) ->
    {Row, NewLists} =
        lists:foldl(fun([H|T], {Row, AccLists}) ->
            {[H|Row], [T|AccLists]}
        end, {[], []}, Lists),
    zip(lists:reverse(NewLists), [lists:reverse(Row)|Acc]).

oxygen_rating(Data) ->
    rating(fun
      (Zeros, Ones) when Zeros > Ones -> $0;
      (Zeros, Ones) when Zeros < Ones -> $1;
      (_Zeros, _Ones) -> $1
    end, 1, Data).
  
co2_scrubber_rating(Data) ->
    rating(fun
      (Zeros, Ones) when Zeros > Ones -> $1;
      (Zeros, Ones) when Zeros < Ones -> $0;
      (_Zeros, _Ones) -> $0
    end, 1, Data).

rating(_F, _I, [One]) -> list_to_integer(One, 2);
rating(F, I, Data) ->
    Elements = [ {J, lists:nth(I, J)} || J <- Data ],
    {Zeros, Ones} = lists:partition(fun({_, E}) -> E =:= $0 end, Elements),
    Common = F(length(Zeros), length(Ones)),
    NewData = lists:filter(fun(Element) -> lists:nth(I, Element) =/= Common end, Data),
    rating(F, I + 1, NewData).
