all: compile

get-deps:
	./rebar get-deps

compile: get-deps
	./rebar compile

rel: compile
	rm -rf rel/webdemo
	./rebar generate

dev-rel: rel
	rm -rf rel/webdemo/lib/webdemo-1
	ln -s `pwd`/apps/webdemo rel/webdemo/lib/webdemo-1
clean:
	./rebar clean
	rm -rf rel/webdemo apps/*/ebin