-module(client_mgr).
-behaviour(supervisor).

-export([start/0, start_link/0, add/0, register/1, get/1]).
-export([init/1]).

add() ->
    supervisor:start_child(?MODULE, []).

register(Id) ->
    gproc:reg({n, l, {client, Id}}, ignored).

get(Id) ->
    gproc:where({n, l, {client, Id}}).

start_link() ->
    supervisor:start_link({local, client_mgr}, ?MODULE, []).

start() ->   
    {ok, Pid} = start_link(),
    unlink(Pid),
    {ok, Pid}.

init(_Args) ->
    {ok, ListenSocket} = gen_tcp:listen(2238, [{active, false}, binary]),
    {ok, {{simple_one_for_one, 10, 60}
	 , [{client ,
	     {client, start_link, [ListenSocket]},
	     permanent, 1000, worker, [client]}]}}.
