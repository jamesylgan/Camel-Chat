test:
	ocamlbuild -use-ocamlfind test_server.byte && ./test_server.byte -port ${PORT}
#eg make PORT=9999 test

tclient:
	ocamlbuild -use-ocamlfind test_client.byte && ./test_client.byte -host ${HOST} -port ${PORT}
#eg make HOST="127.0.0.1" PORT=9999 tclient

tstate:
	ocamlbuild -use-ocamlfind test_state.byte && ./test_state.byte

server:
	ocamlbuild -use-ocamlfind server.byte && ./server.byte -port ${PORT}

client:
	ocamlbuild -use-ocamlfind client.byte && ./client.byte -ip ${IP} -port ${PORT}

check:
	bash checkenv.sh

clean:
	ocamlbuild -clean
