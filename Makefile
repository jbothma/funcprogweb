all: rel

get-deps:
	./rebar get-deps

compile: get-deps
	./rebar compile

rel: compile
	rm -rf rel/webdemo
	./rebar generate

clean:
	./rebar clean
	rm -rf rel/webdemo apps/*/ebin