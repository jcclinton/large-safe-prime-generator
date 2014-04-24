-module(prime_server).
-behavior(gen_server).

-record(state, {
	n,
	ending_n,
	primes = []
							 }).

-include("include/binary.hrl").

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).






start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
	%% this is the smallest 32 bit number
	StartingN = 452312848583266388373324160190187140051835877600158453279131187530910662656,
	%StartingN = 1,
	io:format("prime SERVER: started with ~p~n", [StartingN]),
	%% this is the largest 32 bit number
	EndingN = 115792089237316195423570985008687907853269984665640564039457584007913129639935,
	%EndingN = 10000,
	gen_server:cast(self(), increment),
	{ok, #state{n=StartingN, ending_n=EndingN}}.


handle_call(Msg, _From, State) ->
	io:format("received unknown call message: ~p~n", [Msg]),
	{noreply, State}.

handle_cast(increment, State=#state{n=N, ending_n=EndingN}) ->
	NewN = if N > EndingN ->
			io:format("prime SERVER: ended with ~p~n", [N]),
			gen_server:cast(self(), done),
			N;
		true ->
			gen_server:cast(self(), {check, N}),
			gen_server:cast(self(), increment),
			N + 1
		end,
	{noreply, State#state{n=NewN}};
handle_cast({check, N}, State) ->
	%% spawn new process to check N
	supervisor:start_child(prime_check_sup, [N]),
	{noreply, State};
handle_cast({prime, N}, State=#state{primes=Primes}) ->
	NewPrimes = [N|Primes],
	io:format("~p is a safe prime~n", [N]),
	{noreply, State#state{primes=NewPrimes}};
handle_cast(done, State=#state{primes=Primes}) ->
	Len = length(Primes),
	io:format("there are ~p safe primes~n", [Len]),
	{noreply, State};
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
