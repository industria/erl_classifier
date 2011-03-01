ERL = erl
ERLC = erlc
EBIN = ebin




compile:
	@$(ERLC) -v -W -o $(EBIN) src/*.erl
	@cp src/ostia.app $(EBIN)/

console:
	@$(ERL) -pa $(EBIN)

clean:
	@rm -Rf $(EBIN)/*.beam $(EBIN)/*.app