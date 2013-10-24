# Erlang for your Web backend and API

This is a little demo of [Erlang/OTP](http://www.erlang.org/faq/introduction.html),
WebSocket with [Cowboy](https://github.com/extend/cowboy)
and REST with [Webmachine](https://github.com/basho/webmachine/wiki)
for [A Day in the Functional Web](http://www.funcprogweb.se/).

It's certainly not an example of best practises or a showcase of the current
best tools. It's only a demo of how some very common needs can be met with known
good tools.

## Building

Prod build with

    make rel

Dev build with

    make dev-rel

Start dev node with

    rel/webdemo/bin/webdemo console

Recompile with

    make

## Playing

### Browsers

    http://127.0.0.1:8003/index.html

### WebSocket

    ws://http://127.0.0.1:8001

### REST

```
$ curl -i http://127.0.0.1:8003/worker
HTTP/1.1 200 OK
Server: MochiWeb/1.1 WebMachine/1.10.5 (jokes are better explained)
Date: Thu, 24 Oct 2013 19:19:20 GMT
Content-Type: application/json
Content-Length: 2

[]
```
```
$ curl -i -XPOST -H"Content-Type: application/json" http://127.0.0.1:8003/worker
HTTP/1.1 201 Created
Server: MochiWeb/1.1 WebMachine/1.10.5 (jokes are better explained)
Location: http://127.0.0.1:8003/worker/42b5feff-e56c-4b2b-a25f-d94cb7f3f08f
Date: Thu, 24 Oct 2013 19:19:56 GMT
Content-Type: application/json
Content-Length: 0
```
```
$ curl -i http://127.0.0.1:8003/worker
HTTP/1.1 200 OK
Server: MochiWeb/1.1 WebMachine/1.10.5 (jokes are better explained)
Date: Thu, 24 Oct 2013 19:21:43 GMT
Content-Type: application/json
Content-Length: 40

["14c548c6-64e7-4bb7-8d16-3d939b8ac3eb"]
```
```
$ curl -i -XDELETE http://127.0.0.1:8003/worker/14c548c6-64e7-4bb7-8d16-3d939b8ac3eb
HTTP/1.1 204 No Content
Server: MochiWeb/1.1 WebMachine/1.10.5 (jokes are better explained)
Date: Thu, 24 Oct 2013 19:24:22 GMT
Content-Type: text/html
Content-Length: 0
```
```
$ curl -i http://127.0.0.1:8003/worker
HTTP/1.1 200 OK
Server: MochiWeb/1.1 WebMachine/1.10.5 (jokes are better explained)
Date: Thu, 24 Oct 2013 19:24:45 GMT
Content-Type: application/json
Content-Length: 2

[]
```