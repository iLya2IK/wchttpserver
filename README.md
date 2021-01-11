![alt text](https://github.com/iLya2IK/wchttpserver/blob/main/wclogo_c.svg?raw=true)

# wchttpserver
HTTP2+1.1 server on Lazarus (Free Pascal)

The project builds on fpWeb modules and extends them to increase functionality:
1. Client management using cookies (saving and maintaining sessions).
2. Saving information about clients and sessions in SQLite database.
3. Saving information about the latest requests and saving logs in SQLite database.
4. Multithreading preparation and execution of requests based on threads pools e.g. helpful classes to work with EventSources.
6. Clients ranking based on clients frequency of requests.
7. Built-in support for gzip and deflate compression methods.
7. Ability to start the server both in HTTP/2 mode and in HTTP 1.1 mode.
8. Modified OpenSSL modules (added necessary TLS extensions) to create and maintain HTTP/2 connections.
9. Added the ability to save the master key and a random set of client data on the server-side (necessary for debugging TLS dumps using WireShark).
10. Working both under Windows and Linux.

# What project contains?
* Source files with abstract classes, structures, and commonly used utils. 
* Example - the HTTP/2 server (wchttpserver.lpi, wchttpserver.lpr, wctestclient.pas, wcservertestjobs.pas, wcmaintest.pas, sample site ./webclienttest/*).

# How to deal with the example?
* Build it using the necessary development environment and libraries or download precompiled release.
* Do not forget to generate a certificate and key file for your localhost (put them in ./openssl folder). 
* Command-line to start server: "wchttpserver {PORTNUM} [-debug]"

# Development environment
Free Pascal (v.3.2.0) + Lazarus (v.2.0.10)

# Necessary libraries
1. SQLite
2. OpenSSL (v.1.0.2 or higher)
