{
  This project file is the example how can be realized WCHTTPServer
  Project contains:
  wchttpserver.lpi, 
  wchttpserver.lpr, 
  wctestclient.pas, 
  wcservertestjobs.pas, 
  wcmaintest.pas, 
  sample site ./webclienttest/*
  
  How to deal with the example?

  * Build it using the necessary development environment and 
    ibraries or download precompiled release.
  * Do not forget to generate a certificate and key file for your 
    localhost (put them in ./openssl folder).
    Command-line to start testing server: 
    "wchttpserver <PORTNUM> [-debug]" 
    (PORTNUM - is a number of the listening port - 
     8080 for example)

  How to write your own server?

  * Rewrite wchttpserver.lpr - write here locations for your own 
    files (certificates, keys, mime file, site files, session 
    database, log database, list of using ciphers, list of 
    necessary protocols, initial values for http/2 headers, num of 
    threads) or do it more clever way - by external config file 
    for example.
  * Rewrite wcmaintest.pas - write here your own 
    TWCPreAnalizeClientJob descendant class to implement the task 
    which pre-analyzing requests and creating corresponding async 
    tasks. Adwise you to using data trees like in example to 
    realize such pre-analyzing task.
  * Rewrite wctestclient.pas - implement here your own descendant 
    class for TWebClient where add your own properties and 
    functionality (just look how this is done in example file).
  * Rewrite wcservertestjobs.pas - write your own server's async 
    tasks here (descendant class for TWCMainClientJob). Every task 
    is connected to the requesting client.
  * Add your own site files - scripts, pages, CSS, images, and 
    so on in the projects folder.

}

program wchttpserver;

{$mode objfpc}{$H+}
{$DEFINE UseCThreads}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}
  {$ENDIF}
  Classes,
  SysUtils,
  Interfaces,
  { you can add units after this }
  extopensslsockets,
  sslbase,
  {$IFDEF LOAD_DYNAMICALLY}
  SQLite3Dyn,
  {$ENDIF}
  WCMainTest,
  wcapplication,
  fphttp,
  http2consts,
  IniFiles,
  WCTestClient, SortedThreadPool, wcconfig;

var Conf : TWCConfig;
{$IFDEF LOAD_DYNAMICALLY}
vLibPath : String;
{$ENDIF}
begin
  Randomize;
  Application.ConfigFileName := ExtractFilePath(Application.ExeName) + 'server.cfg';
  if not assigned(Application.Config) then
     raise Exception.Create('Unexpected config error');
  {$IFDEF LOAD_DYNAMICALLY}
  vLibPath := ExtractFilePath(Application.ExeName);
  {$IFDEF Windows}
  {$IF defined(Win32)}
  vLibPath := vLibPath + 'libs\win32\';
  {$ElseIf defined(Win64)}
  vLibPath := vLibPath + 'libs\win64\';
  {$ENDIF}
  {$else}
  {$ENDIF}
  InitializeSQLite(UnicodeString(vLibPath + Sqlite3Lib));
  {$ENDIF}
  Application.LegacyRouting := true;
  Application.Threaded:=True;
  Conf := Application.Config;
  Conf.SetDefaultValue(CFG_SERVER_NAME, 'WCTestServer');
  // WebFilesLoc - location of site files
  // for example if location of executable is /home/folder/
  // then site location will be home/folder/CFG_SITE_FOLDER/
  Conf.SetDefaultValue(CFG_SITE_FOLDER, 'webclienttest');
  // MainURI - location of index file
  // then index location will be home/folder/CFG_SITE_FOLDER/CFG_MAIN_URI
  Conf.SetDefaultValue(CFG_MAIN_URI, 'index.html');
  // SessionsLoc - location of sessions
  // then sessions location will be home/folder/CFG_SITE_FOLDER/CFG_SESSIONS_LOC
  Conf.SetDefaultValue(CFG_SESSIONS_LOC, 'sessions');
  // SessionsDb - location of database with sessions, clients data and network dumps
  // then sessions database location will be home/folder/CFG_SITE_FOLDER/CFG_SESSIONS_LOC/CFG_CLIENTS_DB
  Conf.SetDefaultValue(CFG_CLIENTS_DB, 'clients.db');
  // LogDb - location of database with log
  // then log database location will be home/folder/CFG_LOG_DB
  Conf.SetDefaultValue(CFG_LOG_DB, 'logwebtest.db');
  // MimeLoc - location of mime file
  // then mime file location will be home/folder/CFG_SITE_FOLDER/CFG_MIME_NAME
  Conf.SetDefaultValue(CFG_MIME_NAME, 'mime.txt');
  //SSL/TLS configuration
  Conf.SetDefaultValue(CFG_USE_SSL, true);
  Conf.SetDefaultValue(CFG_HOST_NAME, 'localhost');
  // SSLLoc - location of openssl keys, certificates and logs
  // then openssl location will be home/folder/CFG_SSL_LOC
  Conf.SetDefaultValue(CFG_SSL_LOC, 'openssl');
  Conf.SetDefaultValue(CFG_SSL_CIPHER,
                'ECDHE-RSA-AES128-GCM-SHA256:'+
                'ECDHE-ECDSA-AES128-GCM-SHA256:'+
                'ECDHE-ECDSA-CHACHA20-POLY1305:'+
                'ECDHE-RSA-AES128-SHA256:'+
                'AES128-GCM-SHA256:'+
                'ECDHE-ECDSA-AES256-GCM-SHA384:'+
                'ECDHE-ECDSA-AES256-SHA384'+
                '');
  // PrivateKey - location of openssl keys
  // then keys location will be home/folder/CFG_SSL_LOC/CFG_PRIVATE_KEY
  Conf.SetDefaultValue(CFG_PRIVATE_KEY, 'localhost.key');
  // Certificate - location of openssl certificates
  // then certificates location will be home/folder/CFG_SSL_LOC/CFG_CERTIFICATE
  Conf.SetDefaultValue(CFG_CERTIFICATE, 'localhost.crt');
  // SSLMasterKeyLog - location of openssl keys log
  // then tls keys log location will be home/folder/CFG_SSL_LOC/CFG_TLSKEY_LOG
  Conf.SetDefaultValue(CFG_TLSKEY_LOG, ''); // tlskey.log
  Application.ESServer.SSLType:= stTLSv1_2;
  Conf.SetDefaultValue(CFG_ALPN_USE_HTTP2, True);
  Conf.SetDefaultValue(CFG_COMPRESS_LIMIT, 500);
  Conf.SetDefaultValue(CFG_MAIN_THREAD_CNT, 6);
  Conf.SetDefaultValue(CFG_PRE_THREAD_CNT, 5);

  if HTTP2ServerSettingsSize = 0 then begin
    HTTP2ServerSettingsSize := 3 * H2P_SETTINGS_BLOCK_SIZE;
    HTTP2ServerSettings := GetMem(HTTP2ServerSettingsSize);
    PHTTP2SettingsPayload(HTTP2ServerSettings)^[0].Identifier := H2SET_MAX_CONCURRENT_STREAMS;
    PHTTP2SettingsPayload(HTTP2ServerSettings)^[0].Value := 100;
    PHTTP2SettingsPayload(HTTP2ServerSettings)^[1].Identifier := H2SET_INITIAL_WINDOW_SIZE;
    PHTTP2SettingsPayload(HTTP2ServerSettings)^[1].Value := $ffff;
    PHTTP2SettingsPayload(HTTP2ServerSettings)^[2].Identifier := H2SET_HEADER_TABLE_SIZE;
    PHTTP2SettingsPayload(HTTP2ServerSettings)^[2].Value := HTTP2_SET_INITIAL_VALUES[H2SET_HEADER_TABLE_SIZE];
  end;

  Application.ServerAnalizeJobClass:= WCMainTest.TWCPreThread;
  Application.WebClientClass:= WCTestClient.TWCTestWebClient;
  //
  InitializeJobsTree;
  try
    Application.Initialize;
    Application.Run;
  finally
    DisposeJobsTree;
  end;
end.
