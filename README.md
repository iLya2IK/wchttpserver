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

# Testing results
Benchmark program - [h2load](https://nghttp2.org/documentation/h2load.1.html)
Testing includes 3 repetitive circular GET requests (index.html, main.css, connect.svgz).
Testing was done with both cookie unset and set. When cookie unset - every request followed with allocation client instancing.

> h2load -n120 -c1 -m1 -t1 --h1 --input-file=ngtesting.txt --header=connection:keep-alive

```
starting benchmark...
spawning thread #0: 1 total client(s). 120 total requests
TLS Protocol: TLSv1.2
Cipher: ECDHE-RSA-AES256-GCM-SHA384
Server Temp Key: X25519 253 bits
Application protocol: http/1.1

finished in 10.72s, 11.19 req/s, 22.00KB/s
requests: 120 total, 120 started, 120 done, 120 succeeded, 0 failed, 0 errored, 0 timeout
status codes: 120 2xx, 0 3xx, 0 4xx, 0 5xx
traffic: 235.99KB (241656) total, 18.96KB (19416) headers (space savings 0.00%), 212.46KB (217560) data
                     min         max         mean         sd        +/- sd
time for request:    59.95ms    180.98ms     89.15ms     15.41ms    82.50%
time for connect:    24.03ms     24.03ms     24.03ms         0us   100.00%
time to 1st byte:   125.24ms    125.24ms    125.24ms         0us   100.00%
req/s           :      11.19       11.19       11.19        0.00   100.00%
```

> h2load -n120 -c1 -m10 -t1 --input-file=ngtesting.txt

```
starting benchmark...
spawning thread #0: 1 total client(s). 120 total requests
TLS Protocol: TLSv1.2
Cipher: ECDHE-RSA-AES256-GCM-SHA384
Server Temp Key: X25519 253 bits
Application protocol: h2

finished in 5.59s, 21.47 req/s, 39.63KB/s
requests: 120 total, 120 started, 120 done, 120 succeeded, 0 failed, 0 errored, 0 timeout
status codes: 120 2xx, 0 3xx, 0 4xx, 0 5xx
traffic: 221.52KB (226834) total, 6.91KB (7078) headers (space savings 63.07%), 212.46KB (217560) data
                     min         max         mean         sd        +/- sd
time for request:   143.07ms    829.57ms    456.86ms    114.58ms    77.50%
time for connect:    24.35ms     24.35ms     24.35ms         0us   100.00%
time to 1st byte:   471.93ms    471.93ms    471.93ms         0us   100.00%
req/s           :      21.47       21.47       21.47        0.00   100.00%
```

> h2load -n120 -c1 -m1 -t1 --h1 --input-file=ngtesting.txt --header=connection:keep-alive --header=cookie:cid=11

```
Application protocol: http/1.1

finished in 3.82s, 31.42 req/s, 59.31KB/s
requests: 120 total, 120 started, 120 done, 120 succeeded, 0 failed, 0 errored, 0 timeout
status codes: 120 2xx, 0 3xx, 0 4xx, 0 5xx
traffic: 226.48KB (231920) total, 9.92KB (10160) headers (space savings 0.00%), 212.46KB (217560) data
                     min         max         mean         sd        +/- sd
time for request:    17.83ms     48.84ms     31.64ms      4.83ms    89.17%
time for connect:    19.88ms     19.88ms     19.88ms         0us   100.00%
time to 1st byte:    53.22ms     53.22ms     53.22ms         0us   100.00%
req/s           :      31.43       31.43       31.43        0.00   100.00%
```

> h2load -n120 -c1 -m10 -t1 --input-file=ngtesting.txt --header=cookie:cid=11

```
Application protocol: h2

finished in 1.48s, 81.25 req/s, 145.66KB/s
requests: 120 total, 120 started, 120 done, 120 succeeded, 0 failed, 0 errored, 0 timeout
status codes: 120 2xx, 0 3xx, 0 4xx, 0 5xx
traffic: 215.14KB (220307) total, 551B (551) headers (space savings 94.45%), 212.46KB (217560) data
                     min         max         mean         sd        +/- sd
time for request:    60.80ms    154.30ms    113.75ms     20.78ms    58.33%
time for connect:    52.57ms     52.57ms     52.57ms         0us   100.00%
time to 1st byte:   115.10ms    115.10ms    115.10ms         0us   100.00%
req/s           :      81.27       81.27       81.27        0.00   100.00%
```

# Development environment
Free Pascal (v3.2.0) + Lazarus (v2.0.10)

# Necessary libraries
1. SQLite
2. OpenSSL (v1.1.0 or higher)

# Copyrights
* [fcl-web (fpWeb) - part of Free Pascal's Free Component Library (FCL), focusing on web (related) application development](https://wiki.lazarus.freepascal.org/fcl-web)
* [Ararat Synapse - SSL support by OpenSSL - Copyright (c) 1999-2005, Lukas Gebauer](http://www.ararat.cz/synapse/doku.php/start)
* kcThreadPool - thread pools - Copyright (c) 2011, Maciej Kaczkowski / keit.co
* OGLFastList - lightweighted lists, collections and seqs - Copyright (c) 2018-2019, Ilya Medvedkov
* WCHTTPServer - Copyright (c) 2020-2021, Ilya Medvedkov
