-module(server_worker).

-behaviour(gen_server).

-export([init/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]). % gen_server

-define(MSG(Msg), {tcp, _Port, Msg}).

-record(state,
        {socket,
         main_server,
         client_name
        }).

init([MainServer, ListenSocket]) ->
    log:log("Starting server's client processing gen_server"),
    process_flag(trap_exit, true),
    gen_server:cast(self(), wait_for_connection),
    {ok, #state{}}.

handle_call(_Any, _From, State) ->
    {noreply, State}.

handle_cast(wait_for_connection, _From, State) ->
    case gen_tcp:accept(ListenSocket) of
        {ok, Socket} ->
            {ok, State#state.socket = Socket};
        {error, Reason} ->
            log:log("stop: accept socket error"),
            {stop, normal, State}
    end;
handle_cast(_Any, _From, State) ->
    {noreply, State}.

handle_info(?MSG(<<"name", Name/binary>>), State) ->
    gen_server:cast(State#state.main_server, {client_connected, Name}),
    {noreply, State};
handle_info(?MSG(<<"send", NameAndMessage/binary>>), State) ->
    [Name, Message] = binary:split(NameAndMessage, [<<0>>]),
    gen_server:cast(State#state.main_server, {send_message, Name, Message}),
    {noreply, State};
handle_info() ->


handle_info({tcp_closed, Socket}, State) ->
    log:log("stop: client closed connection"),
    {stop, normal, State};
handle_info({tcp_error, Socket, Reason}, State) ->
    log:log("stop: TCP error occured: ~p", [Reason]),
    {stop, normal, State};
handle_info(_Any, State) ->
    {noreply, State}.

terminate(Reason, State) ->
    log:log("Worker gen_server terminated. Reason: ~p", [Reason]),
    gen_server:cast(State#state.main_server, {server_worker_terminated, self()}),
    gen_tcp:close(State#state.socket),
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.
