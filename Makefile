compile:
	./rebar3 compile

shell:
	ERL_LIBS=build/default/lib erl -s e2_reloader -s ceug_list

serve-python:
	python hello.py

serve-java: HelloWorld.class
	java -cp .:java/servlet-api-2.5.jar:java/jetty-all-9.4.0.M0-uber.jar HelloWorld

%.class: %.java
	javac -cp java/servlet-api-2.5.jar:java/jetty-all-9.4.0.M0-uber.jar $<

bench-easy-python:
	ab -r -n 10000 -c 100 http://127.0.0.1:8334/

bench-easy-java:
	ab -r -n 10000 -c 100 http://127.0.0.1:8335/

bench-easy-psycho:
	ab -r -n 10000 -c 100 http://127.0.0.1:8336/

bench-medium-python:
	ab -r -n 10000 -c 1000 http://127.0.0.1:8334/

bench-medium-java:
	ab -r -n 10000 -c 1000 http://127.0.0.1:8335/

bench-medium-psycho:
	ab -r -n 10000 -c 1000 http://127.0.0.1:8336/

bench-hard-python:
	ab -r -n 50000 -c 10000 http://127.0.0.1:8334/

bench-hard-java:
	ab -r -n 50000 -c 10000 http://127.0.0.1:8335/

bench-hard-psycho:
	ab -r -n 50000 -c 10000 http://127.0.0.1:8336/

