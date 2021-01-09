# wchttpserver
HTTP2/1.1 server on Lazarus (Free Pascal)

The project builds on fpWeb modules and extends them to increase functionality:
1. Client management using cookies (saving and maintaining sessions);
2. Saving information about clients and sessions in SQLite database;
3. Saving information about the latest requests and saving logs in SQLite database;
4. Multithreading preparation and execution of requests based on threads pools;
5. Built-in support for gzip and deflate compression methods;
6. Ability to start the server both in HTTP 2 mode and in HTTP 1.1 mode;
7. Modified OpenSSL modules (added necessary TLS extensions) in order to create and maintain HTTP 2 connections;
8. Added the ability to save the master key and a random set of client data on the server side (necessary for debugging TLS dumps using WireShark).
