test:
	ocamlbuild -use-ocamlfind test_server.byte && ./test_server.byte

server:
	ocamlbuild -use-ocamlfind server.byte && ./server.byte

client:
	ocamlbuild -use-ocamlfind client.byte && ./client.byte

check:
	bash checkenv.sh 

clean:
	ocamlbuild -clean
