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
  WCTestClient;

{$IFDEF LOAD_DYNAMICALLY}
var vLibPath : String;
{$ENDIF}
begin
  Randomize;
  InitializeJobsTree;
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
  Application.Title:='WCTestServer';
  Application.LegacyRouting := true;
  Application.Threaded:=True;
  Application.MainURI:= 'index.html';
  Application.SessionsLoc:= 'sessions';
  Application.SessionsDb := 'clients.db';
  Application.LogDb := 'logwebtest.db';
  Application.MimeLoc := 'mime.txt';
  //SSL/TLS configuration
  Application.UseSSL:=true;
  Application.HostName:='localhost';
  Application.SSLLoc := 'openssl' + cSysDelimiter;
  Application.ESServer.CertificateData.CipherList :=
                'ECDHE-RSA-AES128-GCM-SHA256:'+
                'ECDHE-ECDSA-AES128-GCM-SHA256:'+
                'ECDHE-ECDSA-CHACHA20-POLY1305:'+
                'ECDHE-RSA-AES128-SHA256:'+
                'AES128-GCM-SHA256:'+
                'ECDHE-ECDSA-AES256-GCM-SHA384:'+
                'ECDHE-ECDSA-AES256-SHA384'+
                '';
  Application.ESServer.PrivateKey:='localhost.key';
  Application.ESServer.Certificate:='localhost.crt';
  Application.ESServer.SSLMasterKeyLog := 'tlskey.log';
  Application.ESServer.SSLType:= stTLSv1_2;
  Application.ESServer.AlpnList.Add('h2');  // comment this line to turn off http2
  Application.ESServer.AlpnList.Add('http/1.1');
  //
  HTTP2ServerSettingsSize := 3 * H2P_SETTINGS_BLOCK_SIZE;
  HTTP2ServerSettings := GetMem(HTTP2ServerSettingsSize);
  PHTTP2SettingsPayload(HTTP2ServerSettings)^[0].Identifier := H2SET_MAX_CONCURRENT_STREAMS;
  PHTTP2SettingsPayload(HTTP2ServerSettings)^[0].Value := 100;
  PHTTP2SettingsPayload(HTTP2ServerSettings)^[1].Identifier := H2SET_INITIAL_WINDOW_SIZE;
  PHTTP2SettingsPayload(HTTP2ServerSettings)^[1].Value := $ffff;
  PHTTP2SettingsPayload(HTTP2ServerSettings)^[2].Identifier := H2SET_HEADER_TABLE_SIZE;
  PHTTP2SettingsPayload(HTTP2ServerSettings)^[2].Value := HTTP2_SET_INITIAL_VALUES[H2SET_HEADER_TABLE_SIZE];
  Application.WebFilesLoc := 'webclienttest' + cSysDelimiter;
  Application.MaxPrepareThreads := 5;
  Application.MaxMainThreads := 6;
  Application.ServerAnalizeJobClass:= WCMainTest.TWCPreThread;
  Application.WebClientClass:= WCTestClient.TWCTestWebClient;
  Application.Initialize;
  Application.Run;
  DisposeJobsTree;
end.


