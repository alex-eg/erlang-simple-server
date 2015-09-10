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
            {ok, State#socket = Socket};
        {error, Reason} ->
            log:log("stop: accept socket error"),
            {stop, normal, State}
    end;
handle_cast(_Any, _From, State) ->
    {noreply, State}.

handle_info({tcp_closed, Socket}, State) ->
    log:log("stop: client closed connection"),
    {stop, normal, State};
handle_info({tcp_error, Socket}, State) ->
    log:log("stop: TCP error occured"),
    {stop, normal, State};
handle_info(_Any, State) ->
    {noreply, State}.

terminate(accept_socket_error, State) ->
    gen_server:cast(State#state.main_server, accept_server_errored),
    ok;
terminate(Reason, State) ->
    log:log("Worker gen_server terminated. Reason: ~p", [Reason]),
    gen_tcp:close(State#state.socket),
    ok.
