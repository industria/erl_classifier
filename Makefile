ERL = erl
ERLC = erlc
EBIN = ebin
TESTEBIN = test/ebin

dummy := $(shell test -d $(EBIN) || mkdir -p $(EBIN))
dummy := $(shell test -d $(TESTEBIN) || mkdir -p $(TESTEBIN))

compile:
	@$(ERLC) -v -W -o $(EBIN) src/*.erl
	@$(ERLC) -v -W -pa $(EBIN) -o $(TESTEBIN) test/src/*.erl

eunit: compile
	@$(ERL) -noshell -pa $(EBIN) -pa $(TESTEBIN) -eval 'eunit:test("$(TESTEBIN)", [verbose])' -s init stop


console:
	@$(ERL) -pa $(EBIN)

clean:
	@rm -Rf $(EBIN)/*.beam $(EBIN)/*.app
	@rm -Rf $(TESTEBIN)/*.beam $(TESTEBIN)/*.app
