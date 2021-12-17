#!/usr/bin/env escript

-mode(compile).

main([]) ->
    io:format("syntax: trick_shot <1|2> <file.txt>~n"),
    halt(1);

main(["1", File]) ->
    TargetArea = get_data(File),
    io:format("~p~n", [solve1(TargetArea)]),
    ok;

main(["2", _File]) ->
    ok.

solve1({X1, Y1, X2, Y2, TargetArea}) ->
    io:format("Target Area (~b,~b)-(~b,~b)~n", [X1, Y1, X2, Y2]),
    VelX = work_out_x(0, X2),
    io:format("Vel X=~b~n", [VelX]),
    {Y, MaxY} = work_out_y(false, X2, min(Y1, Y2), VelX * 2, VelX, TargetArea),
    io:format("Vel Y=~b~nMaxY=~b~n", [Y, MaxY]),
    MaxY.

work_out_y({true, PrevData}, X1, MinY, VelY, Steps, TargetArea) ->
    io:format(" status: true ~p VelY=~p~n", [PrevData, VelY]),
    case shot_probe(X1, 0, Steps, TargetArea, VelY, MinY, 0) of
        {true, MaxY} ->
            io:format("max Y=~p~n", [MaxY]),
            work_out_y({true, {VelY, MaxY}}, X1, MinY, VelY+1, Steps, TargetArea);
        false -> PrevData
    end;
work_out_y(false, X1, MinY, VelY, Steps, TargetArea) ->
    io:format(" status: false VelY=~p~n", [VelY]),
    case shot_probe(X1, 0, Steps, TargetArea, VelY, MinY, 0) of
        {true, MaxY} ->
            io:format("max Y=~p~n", [MaxY]),
            work_out_y({true, {VelY, MaxY}}, X1, MinY, VelY+1, Steps, TargetArea);
        false -> work_out_y(false, X1, MinY, VelY+1, Steps, TargetArea)
    end.

shot_probe(X1, Y, 0, _TargetArea, _VelY, MinY, _MaxY) when Y < MinY ->
    io:format("x(~b,~b) ", [X1, Y]),
    false;
shot_probe(X1, Y, 0, TargetArea, VelY, MinY, MaxY) ->
    io:format("?(~b,~b) ", [X1, Y]),
    case gb_sets:is_element({X1, Y}, TargetArea) of
        true -> {true, MaxY};
        false -> shot_probe(X1, Y+VelY, 0, TargetArea, VelY-1, MinY, max(MaxY, Y))
    end;
shot_probe(X1, Y, Steps, TargetArea, VelY, MinY, MaxY) ->
    io:format("v(~b,~b) ", [X1, Y]),
    shot_probe(X1, Y+VelY, Steps-1, TargetArea, VelY-1, MinY, max(MaxY, Y)).

work_out_x(I, X1) when X1 =< 0 -> I;
work_out_x(I, X1) -> work_out_x(I+1, X1-(I+1)).

get_data(File) ->
    {ok, Content} = file:read_file(File),
    to_area(Content).

to_area(<<"target area: ", Data/binary>>) ->
    case re:run(Data, "^x=(-?[0-9]+)\\.\\.(-?[0-9-]+), y=(-?[0-9]+)\\.\\.(-?[0-9]+)", [{capture, all_but_first, list}]) of
        {match, [X1L, X2L, Y1L, Y2L]} ->
            X1 = list_to_integer(X1L),
            X2 = list_to_integer(X2L),
            Y1 = list_to_integer(Y1L),
            Y2 = list_to_integer(Y2L),
            TargetArea = [ {X, Y} || X <- lists:seq(X1, X2), Y <- lists:seq(Y1, Y2) ],
            {X1, Y1, X2, Y2, gb_sets:from_list(TargetArea)};
        nomatch ->
            throw({nooo, Data})
    end.
