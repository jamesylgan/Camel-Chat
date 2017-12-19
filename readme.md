# Camel Chat

![intro message](https://user-images.githubusercontent.com/8934469/34177805-c3964d58-e4b9-11e7-818d-6748bcb8dbbf.png)

![help message](https://user-images.githubusercontent.com/8934469/34177841-ee4ba002-e4b9-11e7-943e-88cd2e835384.png)

# System Description

Camel Chat is a chat system built on OCaml that allows individuals to chat with one another.

Camel Chat offers numerous features:

*A lobby where you can meet new people

* Private chat with a single person

* View and start public group chats

* Access a chat history of the current session

* Customizable usernames

* View a list of ongoing chats or private chats they are in

* Colored text

Users receive color-coded messages throughout their experience, and they are also able to customize the color of the messages they send
Camel Chat is a chat system with client-server architecture. The server can be hosted on a local computer while the client interfaces with the server through the command line. Users initialize with a username, through which they are uniquely identified and can create public chats or private chats with other users. Any user may join a public chat, but not a private chat between two users. Camel Chat offers users a number of other functions, such as viewing the chat history or switching their local context between chats.

# System Design

## Server
Our Server is a TCP server that handles the connection with clients asynchronously. It stores the data in dictionaries, sends the response to the client, and updates the server state accordingly. The client server API can be found in Appendix A for more information. A user may type “#quit” to exit the server at any point. 

State is used to simulate the database of the server. It implements the dictionaries (list dictionaries), which contain a record on information such as the current users, chats, and chat messages. 

## Client 
Client initializes a command line interface through which users may start Camel Chat. It allows users to receive messages asynchronously from the server while sending messages or handling other local commands through the command line. 

# Usage Instructions

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
