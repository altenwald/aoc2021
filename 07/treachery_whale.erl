#!/usr/bin/env escript

%% When we activate this... FAILS!
%%-mode(compile).

main([]) ->
    io:format("syntax: treachery_whale <1|2> <file.txt>~n"),
    halt(1);

main(["1", File]) ->
    CrabPositions = get_data(File),
    ShortestPath = shortest_moves(CrabPositions),
    io:format("~p~n", [ShortestPath]),
    ok;

main(["2", File]) ->
    CrabPositions = get_data(File),
    ShortestPath = shortest_moves_sum(CrabPositions),
    io:format("~p~n", [ShortestPath]),
    ok.

% log(Data, _Label) ->
%     io:format("~p: ~p~n", [Label, Data]),
%     Data.

shortest_moves(CrabPositions) ->
    lists:foldl(fun(Position, MinorCost) ->
        min(lists:sum([ abs(Crab - Position) || Crab <- CrabPositions ]), MinorCost)
    end, undefined, lists:usort(CrabPositions)).

shortest_moves_sum(CrabPositions) ->
    Min = lists:min(CrabPositions),
    Max = lists:max(CrabPositions),
    % {T1, Sums} = timer:tc(fun() ->
    %     maps:from_list([ {X, sum(X)} || X <- lists:seq(Min, Max) ])
    % end),
    {T2, Return} = timer:tc(fun() ->
        %% #7: ~4.5-5.0s
        % lists:min(lists:map(fun(Position) ->
        %     lists:sum(lists:map(fun(Crab) -> fuel(abs(Crab - Position)) end, CrabPositions))
        % end, lists:seq(Min, Max)))
        lists:foldl(fun(Position, MinorCost) ->
            %% #1: ~5-5.5s 
            % min(lists:sum([ maps:get(abs(Crab - Position), Sums) || Crab <- CrabPositions ]), MinorCost)
            %% #2: ~11-11.5s 
            % find_shortest(Position, MinorCost, CrabPositions, Sums, 0)
            %% #3: ~4-4.5s
            % min(lists:sum(lists:map(fun(Crab) -> maps:get(abs(Crab - Position), Sums) end, CrabPositions)), MinorCost)
            %% #4: ~6-6.5s
            % min(MinorCost, lists:foldl(fun(Crab, Acc) ->
            %     maps:get(abs(Crab - Position), Sums) + Acc
            % end, 0, CrabPositions))
            %% #5: ~3.5-4.0s (Adolfo Neto)
            % min(lists:sum([ fuel(abs(Crab - Position)) || Crab <- CrabPositions ]), MinorCost)
            %% #6: ~3.5-4.0s
            min(lists:sum(lists:map(fun(Crab) -> fuel(abs(Crab - Position)) end, CrabPositions)), MinorCost)
        end, undefined, lists:seq(Min, Max))
    end),
    % io:format("t1: ~p~nt2: ~p~n", [T1, T2]),
    io:format("t2: ~p~n", [T2]),
    Return.

fuel(X) -> X * (X + 1) div 2.

% find_shortest(_Position, MinorCost, _CrabPositions, _Sums, Cost) when Cost > MinorCost -> MinorCost;
% find_shortest(_Position, _MinorCost, [], _Sums, Cost) -> Cost;
% find_shortest(Position, MinorCost, [Crab|CrabPositions], Sums, Cost) ->
%     CrabCost = maps:get(abs(Crab - Position), Sums),
%     find_shortest(Position, MinorCost, CrabPositions, Sums, Cost + CrabCost).

% sum(N) -> sum(abs(N), 0).

% sum(0, Acc) -> Acc;
% sum(N, Acc) -> sum(N-1, Acc+N).

get_data(File) ->
    {ok, Content} = file:read_file(File),
    [ binary_to_integer(L) || L <- binary:split(Content, [<<",">>, <<"\n">>], [global, trim]) ].
