-module(client).

-behaviour(application).
-behaviour(gen_server).

-export([start/2, stop/1]).
-export([init/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

init([State, Port]) ->
    log:log("Starting client's main gen_server"),
    process_flag(trap_exit, true),
    {ok, ListenSocket} = gen_tcp:listen(Port, ?TCP_OPTIONS),
    {ok, Pid} = gen_server:start_link(server_worker, [self(), Port], []),
    {ok, State#state{listen_socket = ListenSocket,
                     current_accept_server = Pid}}.

handle_call(get_user_list, From, State) ->
    {noreply, State};
handle_call({get_history, User}, From, State) ->
    {noreply, State}.

handle_cast({send_message, Recepient}, State) ->
    {noreply, State}.

handle_info(_Any, State) ->
    {noreply, State}.

terminate(Reason, State) ->
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.
