compile:
	./rebar3 compile

shell:
	ERL_LIBS=build/default/lib erl -s e2_reloader -s ceug_list
