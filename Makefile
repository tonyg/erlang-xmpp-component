all: compile

compile:
	./rebar compile

clean:
	./rebar clean
	-rmdir ebin

veryclean: clean
	rm -rf rel
	rm -f erl_crash.dump

rel:
	mkdir rel
	(cd rel; ../rebar create-node nodeid=xmpp_component)
	./rebar generate
	chmod a+x rel/xmpp_component/bin/xmpp_component

run: compile
	erl -pa ebin \
		-boot start_sasl \
		-run xmpp_component_demo start localhost 8888 secret "e.$$(hostname -f)"
