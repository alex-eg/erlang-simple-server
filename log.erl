-module(log).

-export([log/1, log/2]).

log(Message) ->
    io:format("~p[log] ~s~n", [self(), Message]).

log(Message, Args) ->
    io:format("~p[log] ~s~n", [self(), lists:flatten(io_lib:format(Message, Args))]).
