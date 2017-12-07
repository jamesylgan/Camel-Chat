Camel Chat
Ruixin Ng (rn279), Xianyi Han (xh87), Joshua Sones (js2572), James Gan (jg893)

Required external dependencies:
	Async: opam install async
	Core: opam install core

Building Camel Chat:

1. Unzip the src.zip file in a directory of your choice and go to the directory
in your terminal.
2. Download the external dependencies if necessary.
3. Run "make check" to ensure that you have the right versions of all
dependencies.
4. Start the Camel Chat server by running the following command in your terminal:
	make PORT=<port number> server
	(e.g. make PORT=9999 server)
5. To run the client, run the command on your client computer:
	make HOST="<host IP>" PORT=<IP port> client
	(e.g. make HOST="127.0.0.1" PORT=9999 client)
6. Enjoy chatting on Camel Chat!

Note: The port specified in the make client command must be the same as the
one used in the make server command, and the host IP argument should be the IP
of the computer running the server.
