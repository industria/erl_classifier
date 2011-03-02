ERL = erl
ERLC = erlc
EBIN = ebin


dummy := $(shell test -d $(EBIN) || mkdir -p $(EBIN))

compile:
	@$(ERLC) -v -W -o $(EBIN) src/*.erl

console:
	@$(ERL) -pa $(EBIN)

clean:
	@rm -Rf $(EBIN)/*.beam $(EBIN)/*.app

