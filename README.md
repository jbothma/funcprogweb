This is a little demo of [Erlang/OTP](http://www.erlang.org/faq/introduction.html),
WebSocket with [Cowboy](https://github.com/extend/cowboy)
and REST with [Webmachine](https://github.com/basho/webmachine/wiki)
for [A Day in the Functional Web](http://www.funcprogweb.se/).


Prod build with

    make rel

Dev build with

    make dev-rel

Start dev node with

    rel/webdemo/bin/webdemo console

Recompile with

    make

Point your browser at http://127.0.0.1:8002/index.html
