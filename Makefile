ERL = erl
ERLC = erlc
EBIN = ebin

dummy := $(shell test -d $(EBIN) || mkdir -p $(EBIN))

compile:
	@$(ERLC) -v -W -DNOTEST -o $(EBIN) src/*.erl

compiletest:
	@$(ERLC) -v -W -DTEST -o $(EBIN) src/*.erl

eunit: compiletest
	@$(ERL) -noshell -pa $(EBIN) -eval 'eunit:test("$(EBIN)", [verbose])' -s init stop


console:
	@$(ERL) -pa $(EBIN)

clean:
	@rm -Rf $(EBIN)/*.beam $(EBIN)/*.app

