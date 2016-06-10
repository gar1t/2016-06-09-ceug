# Overview

This is a simple web app written in Erlang that illustrates that
illustrates a "build your own web framework" pattern of development.

This was used for a presentation at Chicago Erlang User Group on June
9, 2016. The presentation was inspired by a talk that Ian Bicking gave
many many years ago here in Chicago on the same topic, but in Python.

The app uses the <a href="https://github.com/gar1t/psycho">Psycho web
server</a>. Psycho makes use of a WSGI like interface for app
customization. It provides some light weight libraries that perform
some core web functions such as serving static content, routing HTTP
requests, and validating user input.

# Getting started

After cloning this repo, change to its directory and run:

```
make shell
```

Visit http://localhost:8333

Rock on!

# Benchmarking

`hello.py` is provided as a point of comparing "performance" of the
app to something similar in Python. HelloWorld.java is an equivalently
silly app in Java.

To benchmark, naively, first run the server you want to test:

Erlang: `make shell`

Python: `make serve-python-hello`

Java: `make serve-java-hello`

Use `ab` to measure stuff:

```
ab -r -n 10000 -c 100 http://127.0.0.1:PORT/
```

The ports are:

Python: `8334`
Java: `8335`
Erlang: `8336`

Benchmarks like this are silly and don't really prove anything. But
it's fun to think they do. Even so, the Erlang server is reasonably
performant under load.

For a more performant and better behaved Erlang server (uses a
connection pool) see Cowboy.
