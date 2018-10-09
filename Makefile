all: compile

clean:
	@rebar3 clean
	@rebar3 as test clean

compile:
	@rebar3 compile

eunit: compile
	@rebar3 eunit

dialyzer: compile
	@rebar3 dialyzer
