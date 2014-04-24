-module(safe_prime_app).
-behavior(application).

-export([start/2, stop/1]).


start(normal, _Args) ->
	safe_prime_sup:start_link().

stop(_State) ->
	ok.
