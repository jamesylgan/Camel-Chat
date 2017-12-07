server:
	ocamlbuild -use-ocamlfind server_view.byte && ./server_view.byte -port ${PORT}
#eg make PORT=9999 server

client:
	ocamlbuild -use-ocamlfind client_view.byte && ./client_view.byte -host ${HOST} -port ${PORT}
#eg make HOST="127.0.0.1" PORT=9999 client

test:
	ocamlbuild -use-ocamlfind test_state.byte && ./test_state.byte
	ocamlbuild -use-ocamlfind test_client.byte && ./test_client.byte
	ocamlbuild -use-ocamlfind test_server.byte && ./test_server.byte

test_state:
	ocamlbuild -use-ocamlfind test_state.byte && ./test_state.byte

test_client:
	ocamlbuild -use-ocamlfind test_client.byte && ./test_client.byte

test_server:
	ocamlbuild -use-ocamlfind test_server.byte && ./test_server.byte

check:
	bash checkenv.sh

clean:
	ocamlbuild -clean
