const
  CFG_MAIN_SEC       = 'Main';
  CFG_OPENSSL_SEC    = 'OpenSSL';

  CFG_SITE_FOLDER    = $10;
  CFG_SERVER_NAME    = $20;
  CFG_MAIN_URI       = $30;
  CFG_SESSIONS_LOC   = $40;
  CFG_CLIENTS_DB     = $50;
  CFG_LOG_DB         = $60;
  CFG_MIME_NAME      = $70;
  CFG_COMPRESS_LIMIT = $80;
  CFG_MAIN_THREAD_CNT= $90;
  CFG_PRE_THREAD_CNT = $A0;
  CFG_USE_SSL        = $11;
  CFG_HOST_NAME      = $21;
  CFG_SSL_LOC        = $31;
  CFG_SSL_CIPHER     = $41;
  CFG_PRIVATE_KEY    = $51;
  CFG_CERTIFICATE    = $61;
  CFG_TLSKEY_LOG     = $71;
  CFG_ALPN_USE_HTTP2 = $81;

  WC_CFG_CONFIGURATION : TWCConfiguration = (
    (Name:'SiteFolder';     Hash:CFG_SITE_FOLDER    ),
    (Name:'ServerName';     Hash:CFG_SERVER_NAME    ),
    (Name:'MainURI';        Hash:CFG_MAIN_URI       ),
    (Name:'SessionsLoc';    Hash:CFG_SESSIONS_LOC   ),
    (Name:'ClientsDb';      Hash:CFG_CLIENTS_DB     ),
    (Name:'LogDb';          Hash:CFG_LOG_DB         ),
    (Name:'MimeName';       Hash:CFG_MIME_NAME      ),
    (Name:'CompressLimit';  Hash:CFG_COMPRESS_LIMIT ),
    (Name:'MainThreadsCnt'; Hash:CFG_MAIN_THREAD_CNT),
    (Name:'PreThreadsCnt';  Hash:CFG_PRE_THREAD_CNT ),
    (Name:'CompressLimit';  Hash:CFG_COMPRESS_LIMIT ),
    (Name:'UseSSL';         Hash:CFG_USE_SSL        ),
    (Name:'HostName';       Hash:CFG_HOST_NAME      ),
    (Name:'SSLLoc';         Hash:CFG_SSL_LOC        ),
    (Name:'SSLCipherList';  Hash:CFG_SSL_CIPHER     ),
    (Name:'PrivateKeyLoc';  Hash:CFG_PRIVATE_KEY    ),
    (Name:'CertificateLoc'; Hash:CFG_CERTIFICATE    ),
    (Name:'TLSKeyLog';      Hash:CFG_TLSKEY_LOG     ),
    (Name:'UseHTTP2';       Hash:CFG_ALPN_USE_HTTP2 )
    );
