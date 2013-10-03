all: rel

get-deps:
	./rebar get-deps

compile: get-deps
	./rebar compile

rel: compile
	mv rel/webdemo rel/webdemo_previous
	./rebar generate

clean:
	./rebar clean
	rm -rf rel/webdemo apps/*/ebin