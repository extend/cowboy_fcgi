# See LICENSE for licensing information.

DIALYZER = dialyzer
REBAR = rebar

.PHONY: all app deps clean tests eunit ct build-plt dialyze docs

all: app

app: deps
	@$(REBAR) compile

deps:
	@$(REBAR) get-deps

clean:
	@$(REBAR) clean
	rm -f test/*.beam
	rm -f erl_crash.dump

tests: clean app eunit ct

eunit:
	@$(REBAR) eunit skip_deps=true

ct:
	@$(REBAR) ct

build-plt: .cowboy_fcgi_dialyzer.plt

.cowboy_fcgi_dialyzer.plt: deps all
	@$(DIALYZER) --build_plt --output_plt $@ \
		--apps kernel stdlib sasl inets crypto public_key ssl \
		-pa deps/*/ebin deps/*/ebin/*.beam

dialyze:
	@$(DIALYZER) --src src --plt $< \
		-Wbehaviours -Werror_handling \
		-Wrace_conditions -Wunmatched_returns # -Wunderspecs

docs:
	@$(REBAR) doc
