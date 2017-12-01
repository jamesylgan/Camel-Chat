test:
	ocamlbuild -use-ocamlfind try_server.byte && ./try_server.byte -port ${PORT}
#eg make PORT=9999 test

client:
	ocamlbuild -use-ocamlfind client_view.byte && ./client_view.byte -host ${HOST} -port ${PORT}
#eg make HOST="127.0.0.1" PORT=9999 client

test_state:
	ocamlbuild -use-ocamlfind test_state.byte && ./test_state.byte

test_client:
	ocamlbuild -use-ocamlfind test_client.byte && ./test_client.byte

test_server:
	ocamlbuild -use-ocamlfind test_server.byte && ./test_server.byte

server:
	ocamlbuild -use-ocamlfind server.byte && ./server.byte -port ${PORT}

check:
	bash checkenv.sh

clean:
	ocamlbuild -clean
