-module(safe_prime_sup).
-behavior(supervisor).

-export([start_link/0]).
-export([init/1]).


start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	Procs = [{prime_check_sup,
						{prime_check_sup, start_link, []},
						permanent, 10000, supervisor, [prime_check_sup]},
					{prime_server,
						{prime_server, start_link, []},
						permanent, 10000, worker, [prime_server]}
					],
	{ok, {{one_for_one, 60, 3600}, Procs}}.
