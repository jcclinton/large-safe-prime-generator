-module(prime_server).
-behavior(gen_server).

-record(state, {
	n,
	starting_n,
	ending_n,
	primes = []
							 }).

-include("include/binary.hrl").

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-export([get_count/0, get_current_number/0]).



get_count() ->
	supervisor:count_children(prime_check_sup).

get_current_number() ->
	{N, StartingN, EndingN} = gen_server:call(prime_server, current_num),
	DiffStart = N - StartingN,
	DiffEnd = EndingN - N,
	io:format("current is ~p which is ~p above starting and ~p away from ending~n", [N, DiffStart, DiffEnd]).






start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
	%% this is the smallest 32 bit number
	StartingN = 452312848583266388373324160190187140051835877600158453279131187530910662656,
	NewStartingN = 172116699 + StartingN,
	%StartingN = 1,
	io:format("prime SERVER: started with ~p~n", [StartingN]),
	%% this is the largest 32 bit number
	EndingN = 115792089237316195423570985008687907853269984665640564039457584007913129639935,
	%EndingN = 10000,
	gen_server:cast(self(), increment),
	{ok, #state{starting_n=StartingN, n=NewStartingN, ending_n=EndingN}}.


handle_call(current_num, _From, State=#state{n=N, ending_n=EndingN, starting_n=StartingN}) ->
	{reply, {N, StartingN, EndingN}, State};
handle_call(Msg, _From, State) ->
	io:format("received unknown call message: ~p~n", [Msg]),
	{noreply, State}.

handle_cast(increment, State=#state{n=N, ending_n=EndingN}) ->
	NewN = if N > EndingN ->
			io:format("prime SERVER: ended with ~p~n", [N]),
			gen_server:cast(self(), done),
			N;
		true ->
			Num = 4,
			lists:foreach(fun(N2) ->
				%io:format("~p~n", [N2]),
				supervisor:start_child(prime_check_sup, [N2])
			end, lists:seq(N, N+Num)),
			gen_server:cast(self(), increment),
			N + Num + 1
		end,
	{noreply, State#state{n=NewN}};
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
