ERL = erl
ERLC = erlc
EBIN = ebin




compile:
	@$(ERLC) -v -W -o $(EBIN) src/*.erl

console:
	@$(ERL) -pa $(EBIN)

clean:
	@rm -Rf $(EBIN)/*.beam $(EBIN)/*.app