-module(client).
-behaviour(gen_server).

-export([start_link/1, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([init/1]).

-include("messages.hrl").

-record(client, {socket
		 %% the client will keep its local mappings 
		 %% (id => pid), a premature optimization
		,entities=gb_trees:empty()
		,maps=gb_trees:empty()}).

start_link(Socket) ->
    gen_server:start_link(?MODULE, Socket, []).

init(Socket) ->
    gen_server:cast(self(), accept),
    {ok, #client{socket=Socket}}.

handle_cast(accept, #client{socket=ListenSocket}=S) ->
    {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
    inet:setopts(AcceptSocket, [{active, once}]),
    {noreply, S#client{socket=AcceptSocket}}.

handle_info({tcp, _Socket,
	     <<?NETMSG_LOGIN:32/unsigned-integer-little
	       , BinName:120/binary
	       , PassHash:32/binary
	       , LangPack:32/integer-little
	       , _TextMsgHash:10/binary-unit:32
	       , _ItemTypeHash:14/binary-unit:32
	       , DefaultCombatMode:8/integer-little
	       , _Rest/binary>>}
	    , #client{socket=Sock}=S) ->
    Name = lists:filter(fun(C) -> C =/= 0 end, binary:bin_to_list(BinName)),
    io:format("Login: ~s,~p,~p,~n", [Name, PassHash, {LangPack, DefaultCombatMode}]),
    case Name of
	[] -> 
	    gen_tcp:close(Sock),
	    {stop, closed, S};
	_ ->
	    gen_tcp:send(Sock, <<?NETMSG_LOGIN_SUCCESS:32/unsigned-integer-little>>),
	    inet:setopts(Sock, [{active, once}]),
	    {noreply, S}
    end;
	
handle_info({tcp_closed, _Socket}, S) ->
    {stop, closed, S};

handle_info(Other, S) ->
    io:format("Unknown message: ~p~n", [Other]),
    {noreply, S}.

terminate(_Reason, #client{socket=Sock}) ->
    gen_tcp:close(Sock),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
