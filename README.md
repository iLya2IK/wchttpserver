![alt text](https://github.com/iLya2IK/wchttpserver/blob/main/wclogo_c.svg?raw=true)

# wchttpserver
HTTP/2+1.1 server on Lazarus (Free Pascal)

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
* Example - the HTTP/2 server (wchttpserver.lpi, wchttpserver.lpr, wctestclient.pas, wcservertestjobs.pas, wcmaintest.pas, server.cfg, sample site ./webclienttest/*).

# How to deal with the example?
* Build it using the necessary development environment and libraries or download precompiled release.
* Do not forget to generate a certificate and key file for your localhost (put them in ./openssl folder). 
* Command-line to start testing server: "wchttpserver {PORTNUM} [-debug]" (PORTNUM - is a number of the listening port - 8080 for example)

# How to write your own server?
* Rewrite wchttpserver.lpr - write here locations for your own files (certificates, keys, mime file, site files, session database, log database, list of using ciphers, list of necessary protocols, initial values for http/2 headers, num of threads) or do it by editing external config file "server.cfg".
* Rewrite wcmaintest.pas - write here your own TWCPreAnalizeClientJob descendant class to implement the task which pre-analyzing requests and creating corresponding async tasks. Adwise you to using data trees like in example to realize such pre-analyzing task. 
* Rewrite wctestclient.pas - implement here your own descendant class for TWebClient where add your own properties and functionality (just look how this is done in example file).
* Rewrite wcservertestjobs.pas - write your own server's async tasks here (descendant classes for TWCMainClientJob). Every task is connected to the requesting client.
* Add your own site files - scripts, pages, CSS, images, and so on in your site folder.

# Development environment
Free Pascal (v3.2.0) + Lazarus (v2.0.10)

# Necessary libraries
1. SQLite
2. OpenSSL (v1.1.0 or higher)
3. CommonUtils - you can download sources [here](https://github.com/iLya2IK/commonutils). 
```
Note: unpack the CommonUtils archive into the same directory as the server directory:
example: 
your wchttpserver dir:     /home/projects/wchttpserver
then your commonutils dir: /home/projects/commonutils
```

# Copyrights
* [fcl-web (fpWeb) - part of Free Pascal's Free Component Library (FCL), focusing on web (related) application development](https://wiki.lazarus.freepascal.org/fcl-web)
* [Ararat Synapse - SSL support by OpenSSL - Copyright (c) 1999-2005, Lukas Gebauer](http://www.ararat.cz/synapse/doku.php/start)
* [CommonUtils - lightweight lists, collections, seqs and hashes, helping classes for sqlite3 extension, gz compression, data streams - Copyright (c) 2018-2021, Ilya Medvedkov](https://github.com/iLya2IK/commonutils)
* WCHTTPServer - Copyright (c) 2020-2021, Ilya Medvedkov
