%%%-------------------------------------------------------------------
%% @doc dive public API
%% @end
%%%-------------------------------------------------------------------

-module(dive_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    dive_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
