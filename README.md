# cs3110-final-project

A client-server architecture for a chat system build on OCaml.

Make sure async and core are installed.

```
opam install async
```

```
opam install core
```

In order to run Camel Chat, we need a computer hosting the server. We can do this by running

```
make PORT=9999 server
```

in the command line.

In order to connect with a Camel Chat server, we need to run

```
make HOST="[host IP]" PORT=9999 client
```

in the command line.

To test, you can run multiple clients on your own computer after hosting the server, and connecting to host IP: 127.0.0.1

The host IP to put in should be the IP of the computer running the server.
