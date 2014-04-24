-module(prime_check_sup).
-behavior(supervisor).

-export([start_link/0]).
-export([init/1]).


start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	Procs = [{prime_check,
					{prime_check, start_link, []},
					transient, 10000, worker, [prime_check]}],
	{ok, {{simple_one_for_one, 60, 3600}, Procs}}.
