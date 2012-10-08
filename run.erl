-module(run).
-export([run/0]).

run() ->
io:format("Compiling...~n",[]),
  c:c("src/mb.erl",[{outdir,"ebin/"},nowarn_unused_function, nowarn_unused_vars]),
io:format("Initializing...~n",[]),
  mb:reset(),
io:format("running tests...~n", []),
  mb:test(all).

