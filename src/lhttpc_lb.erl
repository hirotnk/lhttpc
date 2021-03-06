%%% Load balancer for lhttpc, replacing the older lhttpc_manager.
%%% Takes a similar stance of storing used-but-not-closed sockets.
%%% Also adds functionality to limit the number of simultaneous
%%% connection attempts from clients.
-module(lhttpc_lb).
-behaviour(gen_server).
-export([start_link/5, checkout/5, checkin/4, connection_count/3, connection_count/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

-define(SHUTDOWN_DELAY, 10000).

%% TODO: transfert_socket, in case of checkout+give_away

-record(state, {host :: host(),
                port :: port_number(),
                ssl :: boolean(),
                max_conn :: max_connections(),
                timeout :: timeout(),
                clients :: ets:tid(),
                free=[] :: list()}).

-export_types([host/0, port_number/0, socket/0, max_connections/0, connection_timeout/0]).
-type host() :: inet:ip_address()|string().
-type port_number() :: 1..65535.
-type socket() :: gen_tcp:socket() | ssl:sslsocket().
-type max_connections() :: pos_integer().
-type connection_timeout() :: timeout().


-spec start_link(host(), port_number(), SSL::boolean(),
                 max_connections(), connection_timeout()) -> {ok, pid()}.
start_link(Host, Port, Ssl, MaxConn, ConnTimeout) ->
    gen_server:start_link(?MODULE, {Host, Port, Ssl, MaxConn, ConnTimeout}, []).

-spec checkout(host(), port_number(), SSL::boolean(),
               max_connections(), connection_timeout()) ->
        {ok, socket()} | retry_later | no_socket.
checkout(Host, Port, Ssl, MaxConn, ConnTimeout) ->
    Lb = find_lb({Host,Port,Ssl}, {MaxConn, ConnTimeout}),
    gen_server:call(Lb, {checkout, self()}, infinity).

%% Called when we're done and the socket can still be reused
-spec checkin(host(), port_number(), SSL::boolean(), Socket::socket()) -> ok.
checkin(Host, Port, Ssl, Socket) ->
    case find_lb({Host,Port,Ssl}) of
        {error, undefined} ->
            %% should we close the socket? We're not keeping it! There are no
            %% Lbs available!
            ok;
        {ok, Pid} ->
            %% Give ownership back to the server ASAP. The client has to have
            %% kept the socket passive. We rely on its good behaviour.
            %% If the transfer doesn't work, we don't notify.
            case lhttpc_sock:controlling_process(Socket, Pid, Ssl) of
                ok -> gen_server:cast(Pid, {checkin, self(), Socket});
                _ -> ok
            end
    end.

%% Returns a tuple with the number of active (currently in use) and
%% the number of idle (open but not currently in use) connections for
%% the host, port, and SSL state.
-spec connection_count(host(), port_number(), Ssl::boolean()) ->
                       {ActiveConnections::integer(), IdleConnections::integer()}.
connection_count(Host, Port, Ssl) ->
    connection_count({Host, Port, Ssl}).

connection_count(Name) ->
    case find_lb(Name) of
        {error, undefined} -> {0, 0};
        {ok, Pid} -> gen_server:call(Pid, {connection_count})
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% GEN_SERVER CALLBACKS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init({Host,Port,Ssl,MaxConn,ConnTimeout}) ->
    %% we must use insert_new because it is possible a concurrent request is
    %% starting such a server at exactly the same time.
    case ets:insert_new(?MODULE, {{Host,Port,Ssl}, self()}) of
        true ->
            {ok, #state{host=Host,
                        port=Port,
                        ssl=Ssl,
                        max_conn=MaxConn,
                        timeout=ConnTimeout,
                        clients=ets:new(clients, [set, private])}};
        false ->
            ignore
    end.

handle_call({checkout,Pid}, _From, S = #state{free=[], max_conn=Max, clients=Tid}) ->
    Size = ets:info(Tid, size),
    case Max > Size of
        true ->
            %% We don't have an open socket, but the client can open one.
            add_client(Tid, Pid),
            {reply, no_socket, S};
        false ->
            {reply, retry_later, S}
    end;
handle_call({checkout,Pid}, _From, S = #state{free=[{Taken,Timer}|Free], clients=Tid, ssl=Ssl}) ->
    lhttpc_sock:setopts(Taken, [{active,false}], Ssl),
    case lhttpc_sock:controlling_process(Taken, Pid, Ssl) of
        ok ->
            cancel_timer(Timer, Taken),
            add_client(Tid,Pid),
            {reply, {ok, Taken}, S#state{free=Free}};
        {error, badarg} ->
            %% The caller died.
            lhttpc_sock:setopts(Taken, [{active, once}], Ssl),
            {noreply, S};
        {error, _Reason} -> % socket is closed or something
            cancel_timer(Timer, Taken),
            handle_call({checkout,Pid}, _From, S#state{free=Free})
    end;
handle_call({connection_count}, _From, S = #state{free=Free, clients=Tid}) ->
    {reply, {ets:info(Tid, size), length(Free)}, S};

handle_call(_Msg, _From, S) ->
    {noreply, S}.

handle_cast({checkin, Pid, Socket}, S = #state{ssl=Ssl, clients=Tid, free=Free, timeout=T}) ->
    lhttpc_stats:record(end_request, Socket),
    remove_client(Tid, Pid),
    %% the client cast function took care of giving us ownership
    case lhttpc_sock:setopts(Socket, [{active, once}], Ssl) of
        ok ->
            Timer = start_timer(Socket,T),
            {noreply, S#state{free=[{Socket,Timer}|Free]}};
        {error, _E} -> % socket closed or failed
            noreply_maybe_shutdown(S)
    end;
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', _Ref, process, Pid, Reason}, S=#state{clients=Tid}) ->
    %% Client died
    case Reason of
        normal      -> ok;
        timeout     -> lhttpc_stats:record(close_connection_timeout, Pid);
        OtherReason -> io:format(standard_error, "DOWN ~p\n", [ OtherReason ])
    end,
    remove_client(Tid,Pid),
    noreply_maybe_shutdown(S);
handle_info({tcp_closed, Socket}, State) ->
    noreply_maybe_shutdown(remove_socket(Socket,State));
handle_info({ssl_closed, Socket}, State) ->
    noreply_maybe_shutdown(remove_socket(Socket,State));
handle_info({timeout, Socket}, State) ->
    noreply_maybe_shutdown(remove_socket(Socket,State));
handle_info({tcp_error, Socket, _}, State) ->
    noreply_maybe_shutdown(remove_socket(Socket,State));
handle_info({ssl_error, Socket, _}, State) ->
    noreply_maybe_shutdown(remove_socket(Socket,State));
handle_info({tcp, Socket, _}, State) ->
    noreply_maybe_shutdown(remove_socket(Socket,State));
handle_info({ssl, Socket, _}, State) ->
    noreply_maybe_shutdown(remove_socket(Socket,State));
handle_info(timeout, State) ->
    {stop, normal, State};
handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, #state{host=H, port=P, ssl=Ssl, free=Free, clients=Tid}) ->
    ets:delete(Tid),
    ets:delete(?MODULE,{H,P,Ssl}),
    lists:foreach(fun ({Socket, _TimerRef}) ->
                          lhttpc_stats:record(close_connection_local, Socket),
                          lhttpc_sock:close(Socket,Ssl)
                  end, Free),
    ok.

%%%%%%%%%%%%%%%
%%% PRIVATE %%%
%%%%%%%%%%%%%%%

%% Potential race condition: if the lb shuts itself down after a while, it
%% might happen between a read and the use of the pid. A busy load balancer
%% should not have this problem.
-spec find_lb(Name::{host(),port_number(),boolean()}, {max_connections(), connection_timeout()}) -> pid().
find_lb(Name = {Host,Port,Ssl}, Args={MaxConn, ConnTimeout}) ->
    case ets:lookup(?MODULE, Name) of
        [] ->
            case supervisor:start_child(lhttpc_lb_sup, [Host,Port,Ssl,MaxConn,ConnTimeout]) of
                {ok, undefined} -> find_lb(Name,Args);
                {ok, Pid} -> Pid
            end;
        [{_Name, Pid}] ->
            case is_process_alive(Pid) of % lb died, stale entry
                true -> Pid;
                false ->
                    ets:delete(?MODULE, Name),
                    find_lb(Name,Args)
            end
    end.

%% Version of the function to be used when we don't want to start a load balancer
%% if none is found
-spec find_lb(Name::{host(),port_number(),boolean()}) -> {error,undefined} | {ok,pid()}.
find_lb(Name={_Host,_Port,_Ssl}) ->
    case ets:lookup(?MODULE, Name) of
        [] -> {error, undefined};
        [{_Name, Pid}] ->
            case erlang:is_process_alive(Pid) of
                true -> {ok, Pid};
                false -> % lb died, stale entry
                    ets:delete(?MODULE,Name),
                    {error, undefined}
            end
    end.

-spec add_client(ets:tid(), pid()) -> true.
add_client(Tid, Pid) ->
    Ref = erlang:monitor(process, Pid),
    ets:insert(Tid, {Pid, Ref}).

-spec remove_client(ets:tid(), pid()) -> true.
remove_client(Tid, Pid) ->
    case ets:lookup(Tid, Pid) of
        [] -> ok; % client already removed
        [{_Pid, Ref}] ->
            erlang:demonitor(Ref, [flush]),
            ets:delete(Tid, Pid)
    end.

-spec remove_socket(socket(), #state{}) -> #state{}.
remove_socket(Socket, S = #state{ssl=Ssl, free=Free}) ->
    lhttpc_stats:record(close_connection_local, Socket),
    lhttpc_sock:close(Socket, Ssl),
    S#state{free=drop_and_cancel(Socket,Free)}.

-spec drop_and_cancel(socket(), [{socket(), reference()}]) -> [{socket(), reference()}].
drop_and_cancel(_, []) -> [];
drop_and_cancel(Socket, [{Socket, TimerRef} | Rest]) ->
    cancel_timer(TimerRef, Socket),
    Rest;
drop_and_cancel(Socket, [H|T]) ->
    [H | drop_and_cancel(Socket, T)].

-spec cancel_timer(reference(), socket()) -> ok.
cancel_timer(TimerRef, Socket) ->
    case erlang:cancel_timer(TimerRef) of
        false ->
            receive
                {timeout, Socket} -> ok
            after 0 -> ok
            end;
        _ -> ok
    end.

-spec start_timer(socket(), connection_timeout()) -> reference().
start_timer(_, infinity) -> make_ref(); % dummy timer
start_timer(Socket, Timeout) ->
    erlang:send_after(Timeout, self(), {timeout,Socket}).

noreply_maybe_shutdown(S=#state{clients=Tid, free=Free}) ->
    case Free =:= [] andalso ets:info(Tid, size) =:= 0 of
        true -> % we're done for
            {noreply,S,?SHUTDOWN_DELAY};
        false ->
            {noreply, S}
    end.
