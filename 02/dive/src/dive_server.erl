-module(dive_server).

-behaviour(gen_server).

-export([move/1, result/0]).

-export([
    start_link/0,
    init/1,
    handle_cast/2,
    handle_call/3
]).

-record(state, {
    h = 0 :: integer(),
    d = 0 :: integer()
}).

move(Action) -> gen_server:cast(?MODULE, Action).

result() -> gen_server:call(?MODULE, result).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, #state{}}.

handle_cast({horizontal, X}, #state{h = H} = State) ->
    {noreply, State#state{h = H + X}};
handle_cast({depth, X}, #state{d = D} = State) ->
    {noreply, State#state{d = D + X}}.

handle_call(result, _From, #state{h = H, d = D} = State) ->
    {reply, H * D, State}.
