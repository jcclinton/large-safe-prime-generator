-module(prime_check).
-behavior(gen_server).

-record(state, {
	n
							 }).

-include("include/binary.hrl").

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-export([is_safe_prime/1]).

	


start_link(N) ->
	gen_server:start_link(?MODULE, N, []).

init(N) ->
	%io:format("check SERVER: started for ~p~n", [N]),
	gen_server:cast(self(), check),
	{ok, #state{n=N}}.


handle_call(Msg, _From, State) ->
	io:format("received unknown call message: ~p~n", [Msg]),
	{noreply, State}.

handle_cast(check, State=#state{n=N}) ->
	IsPrime=miller_rabin:is_prime(N),
	if IsPrime ->
		IsSafePrime = is_safe_prime(N),
		if IsSafePrime ->
				gen_server:cast(prime_server, {prime, N});
			true -> ok
		end;
		true -> ok
	end,
	{stop, normal, State};
handle_cast(Msg, State) ->
	io:format("received unknown cast message: ~p~n", [Msg]),
	{noreply, State}.

handle_info(Msg, State) ->
	io:format("received unknown message: ~p~n", [Msg]),
	{noreply, State}.


code_change(_OldVsn, State, _Extra) ->
	io:format("code change~n"),
	{ok, State}.

terminate(normal, _State) ->
	ok;
terminate(_Reason, _State) ->
	ok.

is_safe_prime(N) ->
	P = round((N-1)/2),
	miller_rabin:is_prime(P).
