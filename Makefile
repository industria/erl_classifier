ERL = erl
ERLC = erlc
EBIN = ebin
MNESIA = datastore
NODENAME = ec

dummy := $(shell test -d $(EBIN) || mkdir -p $(EBIN))
dummy := $(shell test -d $(MNESIA) || mkdir -p $(MNESIA))

compile: appfile
	@$(ERLC) -v -W -DNOTEST -o $(EBIN) src/*.erl

compiletest: appfile
	@$(ERLC) -v -W -DTEST -o $(EBIN) src/*.erl

appfile:
	@cp src/erl_classifier.app $(EBIN)

eunit: compiletest
	@$(ERL) -noshell -pa $(EBIN) -mnesia dir '"$(MNESIA)"' -sname $(NODENAME) -eval 'mnesia:start(), eunit:test("$(EBIN)", [verbose])' -s init stop


console: compile
	@$(ERL) -pa $(EBIN) -mnesia dir '"$(MNESIA)"' -eval 'mnesia:start(), application:start(erl_classifier)' -sname $(NODENAME)

clean:
	@rm -Rf $(EBIN)/*.beam $(EBIN)/*.app
