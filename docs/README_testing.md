# Testing for compliance with specification requirements
Testing program - [h2spec](https://github.com/summerwind/h2spec). 

> h2spec -p 8080 http2 -t -k -o 20

```
94 tests, 91 passed, 0 skipped, 3 failed
Hypertext Transfer Protocol Version 2 (HTTP/2)
  4. HTTP Frames
    4.2. Frame Size      
      × 1: Sends a DATA frame with 2^14 octets in length
        -> The endpoint MUST be capable of receiving and minimally processing frames up to 2^14 octets in length.
           Expected: HEADERS Frame (stream_id:1)
             Actual: Connection closed
            Comment: Error in h2spec - DATA payload size didn't equal the value of "content-length" header.  
 
  5. Streams and Multiplexing
    5.5. Extending HTTP/2      
      × 2: Sends an unknown extension frame in the middle of a header block
        -> The endpoint MUST treat as a connection error of type PROTOCOL_ERROR.
           Expected: GOAWAY Frame (Error Code: PROTOCOL_ERROR)
                     Connection closed
             Actual: Timeout
            Comment: I didn't find any references in RFC 7540 that unknown extension frame in the middle
                     of a header block should cause a connection error
 
  8. HTTP Message Exchanges
    8.1. HTTP Request/Response Exchange
      8.1.2. HTTP Header Fields
        8.1.2.2. Connection-Specific Header Fields          
          × 1: Sends a HEADERS frame that contains the connection-specific header field
            -> The endpoint MUST respond with a stream error of type PROTOCOL_ERROR.
               Expected: GOAWAY Frame (Error Code: PROTOCOL_ERROR)
                         RST_STREAM Frame (Error Code: PROTOCOL_ERROR)
                         Connection closed
                 Actual: DATA Frame (length:845, flags:0x01, stream_id:1)
                Comment: I know that this violates the requirements of RFC 7540, but I sincerely consider them
                         redundant and the type of reaction to the header value should remain on the server side   
```

# Speed testing
Benchmark program - [h2load](https://nghttp2.org/documentation/h2load.1.html). Testing includes 3 repetitive circular GET requests (index.html, main.css, connect.svgz). Testing was done with both cookie unset and set. When cookie unset - every request followed with allocation client instancing. Benchmark shows that the average request processing time decreases when using http/2, but the processing time for one request increases. This is due to the parallelization of the execution of requests, which, in the case of http/2, enter the pipeline simultaneously, in contrast to http/1.1, in which requests are executed sequentially. In this regard, we also tested a parallel request for six http/1.1 connections using the same settings. The result of such testing is shown last. Summary:

```
********************************************************
| protocol |  cookie  | num connections | speed, req/s |
***********+**********+*****************+***************
| http/1.1 | unset    |     one         |    12.60     |
| http/2   | unset    |     one         |    19.70     |
| http/1.1 | set      |     one         |    50.61     |
| http/2   | set      |     one         |    149.37    |
| http/1.1 | set      |     six         |    317.93    |
********************************************************
```

> h2load -n120 -c1 -m1 -t1 --h1 --input-file=ngtesting.txt --header=connection:keep-alive
> h2load -n120 -c1 -m10 -t1 --input-file=ngtesting.txt
> h2load -n120 -c1 -m1 -t1 --h1 --input-file=ngtesting.txt --header=connection:keep-alive --header=cookie:cid=11
> h2load -n120 -c1 -m10 -t1 --input-file=ngtesting.txt --header=cookie:cid=11
> h2load -n120 -c6 -m1 -t6 --h1 --input-file=ngtesting.txt --header=connection:keep-alive --header=cookie:cid=11

# Hardload testing
Benchmark program - [h2load](https://nghttp2.org/documentation/h2load.1.html). This test runs 32,000 concurrent queries both for http/2 and http/1.1 server.

> h2load -n32000 -c32 -m10 --header=cookie:cid=11  https://localhost:8080

```
starting benchmark...
spawning thread #0: 32 total client(s). 32000 total requests
TLS Protocol: TLSv1.2
Cipher: ECDHE-RSA-AES256-GCM-SHA384
Server Temp Key: X25519 253 bits
Application protocol: h2

finished in 16.93s, 1889.72 req/s, 1.95MB/s
requests: 32000 total, 32000 started, 32000 done, 32000 succeeded, 0 failed, 0 errored, 0 timeout
status codes: 32000 2xx, 0 3xx, 0 4xx, 0 5xx
traffic: 32.96MB (34562336) total, 126.16KB (129184) headers (space savings 95.25%), 32.29MB (33856000) data
                     min         max         mean         sd        +/- sd
time for request:    13.35ms    736.76ms    149.03ms     55.56ms    89.53%
time for connect:    13.39ms       1.93s       1.14s    599.26ms    65.63%
time to 1st byte:    62.68ms       1.96s       1.18s    604.35ms    65.63%
req/s           :      59.06       71.18       62.32        4.12    81.25%
```

> h2load -n32000 -c32 -t8 -m1 --h1 --header=connection:keep-alive --header=cookie:cid=11  https://localhost:8080

```
-t: warning: the number of threads is greater than hardware cores.
starting benchmark...
spawning thread #0: 4 total client(s). 4000 total requests
spawning thread #1: 4 total client(s). 4000 total requests
spawning thread #2: 4 total client(s). 4000 total requests
spawning thread #3: 4 total client(s). 4000 total requests
spawning thread #4: 4 total client(s). 4000 total requests
spawning thread #5: 4 total client(s). 4000 total requests
spawning thread #6: 4 total client(s). 4000 total requests
spawning thread #7: 4 total client(s). 4000 total requests
TLS Protocol: TLSv1.2
Cipher: ECDHE-RSA-AES256-GCM-SHA384
Server Temp Key: X25519 253 bits
Application protocol: http/1.1
 
finished in 28.49s, 1123.25 req/s, 1.26MB/s
requests: 32000 total, 32000 started, 32000 done, 32000 succeeded, 0 failed, 0 errored, 0 timeout
status codes: 32000 2xx, 0 3xx, 0 4xx, 0 5xx
traffic: 36.01MB (37760000) total, 2.66MB (2784000) headers (space savings 0.00%), 32.29MB (33856000) data
                     min         max         mean         sd        +/- sd
time for request:     8.25ms    132.36ms     26.24ms      5.01ms    94.38%
time for connect:     8.35ms       1.89s    920.98ms    602.20ms    62.50%
time to 1st byte:    39.63ms       1.91s    957.78ms    611.37ms    65.63%
req/s           :      35.10       38.89       36.85        1.17    68.75%
```
