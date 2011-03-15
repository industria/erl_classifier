ERL = erl
ERLC = erlc
EBIN = ebin

dummy := $(shell test -d $(EBIN) || mkdir -p $(EBIN))

compile: appfile
	@$(ERLC) -v -W -DNOTEST -o $(EBIN) src/*.erl

compiletest: appfile
	@$(ERLC) -v -W -DTEST -o $(EBIN) src/*.erl

appfile:
	@cp src/erl_classifier.app $(EBIN)

eunit: compiletest
	@$(ERL) -noshell -pa $(EBIN) -eval 'eunit:test("$(EBIN)", [verbose])' -s init stop


console:
	@$(ERL) -pa $(EBIN)

clean:
	@rm -Rf $(EBIN)/*.beam $(EBIN)/*.app

