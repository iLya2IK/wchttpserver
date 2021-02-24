const
  CFG_MAIN_SEC       = $01;
  CFG_SITE_FOLDER    = $11;
  CFG_SERVER_NAME    = $21;
  CFG_MAIN_URI       = $31;
  CFG_SESSIONS_LOC   = $41;
  CFG_CLIENTS_DB     = $51;
  CFG_LOG_DB         = $61;
  CFG_MIME_NAME      = $71;
  CFG_MAIN_THREAD_CNT= $81;
  CFG_PRE_THREAD_CNT = $91;
  CFG_JOB_TO_JOB_WAIT= $A1;
  CFG_JOB_TO_JOB_WAIT_ADAPT_MIN= $B1;
  CFG_JOB_TO_JOB_WAIT_ADAPT_MAX= $C1;
  CFG_OPENSSL_SEC      = $02;
  CFG_USE_SSL          = $12;
  CFG_HOST_NAME        = $22;
  CFG_SSL_LOC          = $32;
  CFG_SSL_CIPHER       = $42;
  CFG_PRIVATE_KEY      = $52;
  CFG_CERTIFICATE      = $62;
  CFG_TLSKEY_LOG       = $72;
  CFG_ALPN_USE_HTTP2   = $82;
  CFG_CLIENTS_SEC           = $03;
  CFG_CLIENT_COOKIE_MAX_AGE = $13;
  CFG_CLIENT_TIMEOUT        = $23;
  CFG_HTTP2_SEC        = $04;
  CFG_H2SET_HEADER_TABLE_SIZE      = $14;
  CFG_H2SET_ENABLE_PUSH            = $24;
  CFG_H2SET_MAX_CONCURRENT_STREAMS = $34;
  CFG_H2SET_INITIAL_WINDOW_SIZE    = $44;
  CFG_H2SET_MAX_FRAME_SIZE         = $54;
  CFG_H2SET_MAX_HEADER_LIST_SIZE   = $64;
  CFG_WEBFILES_SEC                 = $05;
  CFG_IGNORE_FILES                 = $15;
  CFG_EXCLUDE_IGNORE_FILES         = $25;
  CFG_COMPRESS_LIMIT               = $35;
  CFG_MAINTAIN_SEC                 = $06;
  CFG_SHUTDOWN                     = $16;

  WC_CFG_CONFIGURATION : TWCConfiguration = (
    (ParentHash:CFG_ROOT_HASH; Hash:CFG_MAIN_SEC;     Name:'Main'   ),
    (ParentHash:CFG_ROOT_HASH; Hash:CFG_OPENSSL_SEC;  Name:'OpenSSL'),
    (ParentHash:CFG_ROOT_HASH; Hash:CFG_CLIENTS_SEC;  Name:'Clients'),
    (ParentHash:CFG_ROOT_HASH; Hash:CFG_HTTP2_SEC;    Name:'Http2'),
    (ParentHash:CFG_ROOT_HASH; Hash:CFG_WEBFILES_SEC; Name:'WebFiles'),
    (ParentHash:CFG_ROOT_HASH; Hash:CFG_MAINTAIN_SEC; Name:'Maintaining'),

    (ParentHash:CFG_MAIN_SEC; Hash:CFG_SITE_FOLDER    ;Name:'SiteFolder'    ),
    (ParentHash:CFG_MAIN_SEC; Hash:CFG_SERVER_NAME    ;Name:'ServerName'    ),
    (ParentHash:CFG_MAIN_SEC; Hash:CFG_MAIN_URI       ;Name:'MainURI'       ),
    (ParentHash:CFG_MAIN_SEC; Hash:CFG_SESSIONS_LOC   ;Name:'SessionsLoc'   ),
    (ParentHash:CFG_MAIN_SEC; Hash:CFG_CLIENTS_DB     ;Name:'ClientsDb'     ),
    (ParentHash:CFG_MAIN_SEC; Hash:CFG_LOG_DB         ;Name:'LogDb'         ),
    (ParentHash:CFG_MAIN_SEC; Hash:CFG_MIME_NAME      ;Name:'MimeName'      ),
    (ParentHash:CFG_MAIN_SEC; Hash:CFG_MAIN_THREAD_CNT;Name:'MainThreadsCnt'),
    (ParentHash:CFG_MAIN_SEC; Hash:CFG_PRE_THREAD_CNT ;Name:'PreThreadsCnt' ),
    (ParentHash:CFG_MAIN_SEC; Hash:CFG_JOB_TO_JOB_WAIT ;Name:'ThreadJobToJobWaitMs' ),
    (ParentHash:CFG_MAIN_SEC; Hash:CFG_JOB_TO_JOB_WAIT_ADAPT_MIN ;Name:'AdaptThreadJobToJobMinWaitMs' ),
    (ParentHash:CFG_MAIN_SEC; Hash:CFG_JOB_TO_JOB_WAIT_ADAPT_MAX ;Name:'AdaptThreadJobToJobMaxWaitMs' ),
    (ParentHash:CFG_OPENSSL_SEC; Hash:CFG_USE_SSL        ; Name:'UseSSL'         ),
    (ParentHash:CFG_OPENSSL_SEC; Hash:CFG_HOST_NAME      ; Name:'HostName'       ),
    (ParentHash:CFG_OPENSSL_SEC; Hash:CFG_SSL_LOC        ; Name:'SSLLoc'         ),
    (ParentHash:CFG_OPENSSL_SEC; Hash:CFG_SSL_CIPHER     ; Name:'SSLCipherList'  ),
    (ParentHash:CFG_OPENSSL_SEC; Hash:CFG_PRIVATE_KEY    ; Name:'PrivateKeyLoc'  ),
    (ParentHash:CFG_OPENSSL_SEC; Hash:CFG_CERTIFICATE    ; Name:'CertificateLoc' ),
    (ParentHash:CFG_OPENSSL_SEC; Hash:CFG_TLSKEY_LOG     ; Name:'TLSKeyLog'      ),
    (ParentHash:CFG_OPENSSL_SEC; Hash:CFG_ALPN_USE_HTTP2 ; Name:'UseHTTP2'       ),
    (ParentHash:CFG_CLIENTS_SEC; Hash:CFG_CLIENT_COOKIE_MAX_AGE; Name:'CookieMaxAgeSec'),
    (ParentHash:CFG_CLIENTS_SEC; Hash:CFG_CLIENT_TIMEOUT       ; Name:'TimeoutSec'     ),
    (ParentHash:CFG_HTTP2_SEC; Hash:CFG_H2SET_HEADER_TABLE_SIZE      ; Name:'HEADER_TABLE_SIZE'     ),
    (ParentHash:CFG_HTTP2_SEC; Hash:CFG_H2SET_ENABLE_PUSH            ; Name:'ENABLE_PUSH'           ),
    (ParentHash:CFG_HTTP2_SEC; Hash:CFG_H2SET_MAX_CONCURRENT_STREAMS ; Name:'MAX_CONCURRENT_STREAMS'),
    (ParentHash:CFG_HTTP2_SEC; Hash:CFG_H2SET_INITIAL_WINDOW_SIZE    ; Name:'INITIAL_WINDOW_SIZE'   ),
    (ParentHash:CFG_HTTP2_SEC; Hash:CFG_H2SET_MAX_FRAME_SIZE         ; Name:'MAX_FRAME_SIZE'        ),
    (ParentHash:CFG_HTTP2_SEC; Hash:CFG_H2SET_MAX_HEADER_LIST_SIZE   ; Name:'MAX_HEADER_LIST_SIZE'  ),
    (ParentHash:CFG_WEBFILES_SEC; Hash:CFG_COMPRESS_LIMIT ;      Name:'CompressLimitByte' ),
    (ParentHash:CFG_WEBFILES_SEC; Hash:CFG_IGNORE_FILES ;        Name:'WebFilesIgnore' ),
    (ParentHash:CFG_WEBFILES_SEC; Hash:CFG_EXCLUDE_IGNORE_FILES ;Name:'WebFilesExcludeIgnore' ),
    (ParentHash:CFG_MAINTAIN_SEC; Hash:CFG_SHUTDOWN ;            Name:'Shutdown' )
    );
