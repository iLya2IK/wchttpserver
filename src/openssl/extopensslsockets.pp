{
 ExtOpenSSL:
   SSL SocketHandler for ExtOpenSSL based on opensslsockets:
   Added Alpn wrapper
   Added SSLMasterKeyLog wrapper for better debugging with WireShark

   Part of WCHTTP2Server project

   Copyright (c) 2021 by Ilya Medvedkov

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit ExtOpenSSLSockets;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sockets, ssockets, sslsockets, sslbase, openssl, ExtOpenSSL;

Type

  ESSLIOError = class(ESocketError);

  TExtOpenSSLFunc = (esslfUnknown, esslfConnect, esslfAccept, esslfRead, esslfWrite, esslfShutdown);

  { TExtOpenSSLSocketHandler }

  TExtOpenSSLSocketHandler = Class(TExtSSLSocketHandler)
  Private
    FOnConnected : TNotifyEvent;
    FSSL: TExtSSL;
    FCTX : TExtSSLContext;
    FSSLLastErrorString: string;
    FSSLLastError : Integer;
    FLocation : TExtOpenSSLFunc;
    FLastResult : Integer;
    FSSLMasterKeyLog: String;
    FAlpnList : TStringList;
    FUseGlobalContext : Boolean;
    function GetAlpnList : String;
    function GetGlobalContext : TExtSSLContext;
    procedure SetAlpnList(AValue : String);
    procedure SetGlobalContext(AValue : TExtSSLContext);
    procedure SetSSLMasterKeyLog(AValue: String);
    procedure TryToSaveMasterKey;
    function OpenSSLFuncToStr() : String;
  Protected
    procedure SetSSLLastErrorString(AValue: string);
    Function FetchErrorInfo: Boolean;
    procedure HandleSSLIOError(isSend: Boolean);
    procedure HandleSSLLastError();
    function CheckSSL(SSLResult: Integer): Boolean;
    function CheckSSL(SSLResult: Pointer): Boolean;
    function InitContext(NeedCertificate: Boolean): Boolean; virtual;
    function DoneContext: Boolean; virtual;
    function InitSslKeys: boolean;virtual;
    function AlpnSelect(outv : PPChar; outl : PChar; inv : PChar; inlen : Cardinal) : integer; virtual;
  Public
    Constructor create; override;
    destructor destroy; override;
    function CreateCertGenerator: TX509Certificate; override;
    function Connect : Boolean; override;
    function Close : Boolean; override;
    function Accept : Boolean; override;
    function Shutdown(BiDirectional : Boolean): boolean; override;
    function Send(Const Buffer; Count: Integer): Integer; override;
    function Recv(Const Buffer; Count: Integer): Integer; override;
    function BytesAvailable: Integer; override;
    function ContextToGlobal : TExtSSLContext;
    // Result of last CheckSSL call.
    Function SSLLastError: integer;
    property SSLLastErrorString: string read FSSLLastErrorString write SetSSLLastErrorString;
    property SSLMasterKeyLog : String read FSSLMasterKEyLog write SetSSLMasterKeyLog;
    property GlobalContext : TExtSSLContext read GetGlobalContext write SetGlobalContext;
    property AlpnList : String read GetAlpnList write SetAlpnList;
    property OnConnected : TNotifyEvent read FOnConnected write FOnConnected;
  end;

implementation

{$IFDEF WINDOWS}
uses Winsock2, Windows;

function IsNonFatalError(const anError: Integer): Boolean; inline;
begin
  Result := (anError = 0)
         or (anError = WSAEINVAL) or (anError = WSAEFAULT)
         or (anError = WSAEOPNOTSUPP) or (anError = WSAEMSGSIZE)
         or (anError = WSAEADDRNOTAVAIL) or (anError = WSAEAFNOSUPPORT)
         or (anError = WSAEDESTADDRREQ);
end;

function IsPipeError(const anError: Integer): Boolean; inline;
begin
  {$WARNING check these ambiguous errors}
  Result := anError = WSAECONNRESET;
end;

{$ELSE}

// unix

uses BaseUnix, NetDB;

function IsNonFatalError(const anError: Integer): Boolean; inline;
begin
  Result := (anError = 0) or (anError = Low(SocketError))
         or (anError = ESysEINTR) or (anError = ESysEMSGSIZE)
         or (anError = ESysEFAULT) or (anError = ESysEINVAL)
         or (anError = ESysEOPNOTSUPP);
end;

function IsPipeError(const anError: Integer): Boolean; inline;
begin
  Result := anError = ESysEPIPE;
end;

{$ENDIF}

{ TSocketHandler }
Resourcestring
  SErrNoLibraryInit = 'Could not initialize OpenSSL library';

Procedure MaybeInitSSLInterface;
begin
  if not IsSSLloaded then
    if not InitSSLInterface then
      Raise EInOutError.Create(SErrNoLibraryInit);
end;

function IsSSLBlockError(const anError: Longint): Boolean; inline;
begin
  Result := (anError = SSL_ERROR_WANT_READ) or (anError = SSL_ERROR_WANT_WRITE) or (anError = SSL_ERROR_ZERO_RETURN);
end;

function IsSSLNonFatalError(const anError, aRet: Longint; out aErr : Longint): Boolean; inline;
begin
  aErr := SSL_ERROR_NONE;
  Result := (anError <> SSL_ERROR_SSL); // SSL_ERROR_SSL - fatal error
  if (anError = SSL_ERROR_SYSCALL) then begin
    aErr := ErrPeekLastError();
    if aErr = 0 then begin // we need to check the ret
      if aRet < 0 then
      begin
        {$ifdef unix}
        aErr := fpgeterrno;
        {$else}
        {$ifdef windows}
        aErr := WSAGetLastError;
        {$endif}{$endif}
        Result := IsNonFatalError(aErr);
      end
      else
        aErr := SSL_ERROR_SYSCALL; //unexpected EOF, ignore
    end else // check what exactly
    begin
      Result := IsNonFatalError(aErr);
    end;
  end else
  begin
    aErr := ErrPeekLastError();
  end;
  ErrClearError; // we need to empty the queue
end;

function TExtOpenSSLSocketHandler.CreateCertGenerator: TX509Certificate;
begin
  Result:=TExtOpenSSLX509Certificate.Create;
end;

procedure TExtOpenSSLSocketHandler.SetSSLMasterKeyLog(AValue: String);
var FS : TextFile;
begin
  if FSSLMasterKeyLog=AValue then Exit;
  FSSLMasterKeyLog:=AValue;
  if (Length(AValue) > 0) and
     not FileExists(AValue) then
  begin
    AssignFile(FS, FSSLMasterKeyLog);
    Rewrite(FS);
    CloseFile(FS);
  end;
end;

function TExtOpenSSLSocketHandler.GetAlpnList: String;
begin
  Result := FAlpnList.Text;
end;

function TExtOpenSSLSocketHandler.GetGlobalContext : TExtSSLContext;
begin
  if FUseGlobalContext then
    Result := FCTX else
    Result := nil;
end;

procedure TExtOpenSSLSocketHandler.SetAlpnList(AValue: String);
begin
  FAlpnList.Text:=AValue;
end;

procedure TExtOpenSSLSocketHandler.SetGlobalContext(AValue : TExtSSLContext);
begin
  FUseGlobalContext := true;
  FCTX := AValue;
end;

procedure TExtOpenSSLSocketHandler.TryToSaveMasterKey;
var S : PSSL_SESSION;
    MK, clrand : String;
    FT : TextFile;
begin
  if FileExists(FSSLMasterKEyLog) then
  begin
    S := SSLGetSession(FSSL.FSSL);
    if assigned(S) then
    begin
      SSLGetClientRandom(FSSL.FSSL, clrand);
      SSLSESSIONGetMasterKey(s, mk);
      if (Length(mk) > 0) and (Length(clrand) > 0) then
      begin
        AssignFile(FT, FSSLMasterKEyLog);
        Append(FT);
        WriteLn(FT, 'CLIENT_RANDOM ' + clrand + ' ' + mk);
        CloseFile(FT);
      end;
    end;
  end;
end;

function TExtOpenSSLSocketHandler.OpenSSLFuncToStr() : String;
begin
  case FLocation of
    esslfConnect : Result := 'Conenct';
    esslfAccept : Result := 'Accept';
    esslfRead : Result := 'Read';
    esslfWrite : Result := 'Write';
    esslfShutdown : Result := 'Shutdown';
  else
    Result := 'Unknown';
  end;
end;

procedure TExtOpenSSLSocketHandler.SetSSLLastErrorString(AValue: string);
begin
  if FSSLLastErrorString=AValue then Exit;
  FSSLLastErrorString:=AValue;
end;

function TExtOpenSSLSocketHandler.Connect: Boolean;
begin
  Result := Inherited Connect;
  Result := Result and InitContext(False);
  if Result then
    begin
    Result:=CheckSSL(FSSL.SetFD(Socket.Handle));
    if Result then
     begin
       if SendHostAsSNI  and (Socket is TInetSocket) then
         FSSL.Ctrl(SSL_CTRL_SET_TLSEXT_HOSTNAME,TLSEXT_NAMETYPE_host_name,PAnsiChar(AnsiString((Socket as TInetSocket).Host)));
       FLocation := esslfConnect;
       Result:=CheckSSL(FSSL.Connect);
       if Result and VerifyPeerCert then
         Result:=(FSSL.VerifyResult<>0) or (not DoVerifyCert);
       if Result then
       begin
         SetSSLActive(True);
         TryToSaveMasterKey;
         if Assigned(FOnConnected) then FOnConnected(Self);
       end;
     end;
    end;
end;

function TExtOpenSSLSocketHandler.Close: Boolean;
begin
  Result:=Shutdown(False);
end;

function TExtOpenSSLSocketHandler.FetchErrorInfo: Boolean;
const LEN_OF_ERROR_STR = 256;
var
  S : AnsiString;
begin
  FSSLLastErrorString:='';
  Result:=(FSSLLastError <> SSL_ERROR_NONE);
  if Result then
  begin
    S:=StringOfChar(#0,LEN_OF_ERROR_STR);
    ErrErrorString(FSSLLastError, S, LEN_OF_ERROR_STR);
    FSSLLastErrorString := s;
  end;
  ErrClearError;
end;

procedure TExtOpenSSLSocketHandler.HandleSSLIOError(isSend : Boolean);
begin
  if (FSSLLastError <> SSL_ERROR_NONE) then begin
    if not IsSSLBlockError(FSSLLastError) then
    begin
      if not IsSSLNonFatalError(FSSLLastError, FLastResult, FLastError) then
      begin
        if IsSend and (IsPipeError(FLastError)) then
          raise ESSLIOError.CreateFmt('pipe error %d - %s', [FLastError, FSSLLastErrorString])
        else
          raise ESSLIOError.CreateFmt('at %s io_error %x, ssl_error %d, ssl_result %d %s',
                                                    [OpenSSLFuncToStr,
                                                     FLastError,
                                                     FSSLLastError,
                                                     FLastResult,
                                                     FSSLLastErrorString]);
      end;
    end;
  end;
end;

procedure TExtOpenSSLSocketHandler.HandleSSLLastError();
begin
  HandleSSLIOError(false);
end;

function TExtOpenSSLSocketHandler.CheckSSL(SSLResult : Integer) : Boolean;
begin
  FLastResult := SSLResult;
  Result := (SSLResult > 0);
  if Not Result then
  begin
    if Assigned(FSSL) then
      FSSLLastError := FSSL.GetError(SSLResult)
    else
      FSSLLastError := ErrPeekLastError;
    FetchErrorInfo;
  end;
end;

function TExtOpenSSLSocketHandler.CheckSSL(SSLResult: Pointer): Boolean;
begin
  FLastResult := 0;
  Result := Assigned(SSLResult);
  if not Result then
  begin
    FSSLLastError := ErrPeekLastError;
    Result:=FetchErrorInfo;
  end;
end;

function TExtOpenSSLSocketHandler.DoneContext: Boolean;
begin
  if Assigned(FSSL) then
    FreeAndNil(FSSL);
  if Assigned(FCTX) then
  begin
    if not FUseGlobalContext then
      FreeAndNil(FCTX) else
      FCTX := nil;
  end;
  ErrRemoveState(0);
  SetSSLActive(False);
  Result:=True;
end;

Function HandleSSLPwd(buf : PAnsiChar; len:Integer; {%H-}flags:Integer; UD : Pointer):Integer; cdecl;
var
  Pwd: AnsiString;
  H :  TExtOpenSSLSocketHandler;
begin
  if Not Assigned(UD) then
    PWD:=''
  else
    begin
    H:=TExtOpenSSLSocketHandler(UD);
    Pwd:=H.CertificateData.KeyPassword;
    end;
  if (len<Length(Pwd)+1) then
    SetLength(Pwd,len-1);
  pwd:=pwd+#0;
  Result:=Length(Pwd);
  Move(Pointer(Pwd)^,Buf^,Result);
end;

function TExtOpenSSLSocketHandler.InitSslKeys: boolean;
begin
  Result:=(FCTX<>Nil);
  if not Result then
    Exit;
  if not CertificateData.Certificate.Empty then
    Result:=CheckSSL(FCTX.UseCertificate(CertificateData.Certificate));
  if Result and not CertificateData.PrivateKey.Empty then
    Result:=CheckSSL(FCTX.UsePrivateKey(CertificateData.PrivateKey));
  if Result and (CertificateData.CertCA.FileName<>'') then
    Result:=CheckSSL(FCTX.LoadVerifyLocations(CertificateData.CertCA.FileName,''));
  if Result and not CertificateData.PFX.Empty then
    Result:=CheckSSL(FCTX.LoadPFX(CertificateData.PFX,CertificateData.KeyPassword));
end;

function TExtOpenSSLSocketHandler.AlpnSelect(outv: PPChar; outl: PChar;
  inv: PChar; inlen: Cardinal): integer;
var
  protlen : Byte;
  prot : PChar;
  i : integer;
  id : string;
begin
  protlen := 0;
  prot := inv;
  id := '';

  while (prot < pointer(inv + inlen)) do begin
      protlen := PByte(prot)^;
      Inc(prot);
      if (pointer(inv + inlen) < pointer(prot + protlen)) then
          Exit(SSL_TLSEXT_ERR_NOACK);

      SetLength(id, protlen);
      Move(PByteArray(prot)^, id[1], protlen);
      for i := 0 to FAlpnList.Count-1 do
      begin
        if SameText(id, FAlpnList[i]) then begin
            outv^ := prot;
            PByte(outl)^ := protlen;
            Exit(SSL_TLSEXT_ERR_OK);
        end;
      end;
      Inc(prot, protlen);
  end;

  Result := SSL_TLSEXT_ERR_NOACK;
end;

constructor TExtOpenSSLSocketHandler.create;
begin
  inherited create;
  FAlpnList := TStringList.Create;
  FOnConnected := nil;
  FSSL := nil;
  MaybeInitSSLInterface;
end;

destructor TExtOpenSSLSocketHandler.destroy;
begin
  if Assigned(FSSL) then
    FreeAndNil(FSSL);
  if Assigned(FCTX) then
  begin
    if not FUseGlobalContext then
      FreeAndNil(FCTX) else
      FCTX := nil;
  end;
  FAlpnList.Free;
  inherited destroy;
end;

function TExtOpenSSLSocketHandler.InitContext(NeedCertificate:Boolean): Boolean;

Const
  VO : Array[Boolean] of Integer = (SSL_VERIFY_NONE,SSL_VERIFY_PEER);

var
  s: AnsiString;

begin
  FLocation := esslfUnknown;

  if not FUseGlobalContext then
  begin
    Result:=DoneContext;
    if Not Result then
      Exit;
    try
      FCTX:=TExtSSLContext.Create(ExSSLType);
    Except
      CheckSSL(Nil);
      Result:=False;
      Exit;
    end;
    S:=CertificateData.CipherList;
    FCTX.SetCipherList(S);
    FCTX.SetVerify(VO[VerifypeerCert],Nil);
    FCTX.SetDefaultPasswdCb(@HandleSSLPwd);
    FCTX.SetDefaultPasswdCbUserdata(self);
    if Assigned(FAlpnList) and
       (FAlpnList.Count > 0) then
       FCTX.SetAlpnSelect(@AlpnSelect);
    If NeedCertificate and CertificateData.NeedCertificateData  then
      if Not CreateSelfSignedCertificate then
        begin
        DoneContext;
        Exit(False);
        end;
     if Not InitSSLKeys then
       begin
       DoneContext;
       Exit(False);
       end;
  end;
  try
    FSSL:=TExtSSL.Create(FCTX);
    Result:=True;
  Except
    Result:=False;
    CheckSSL(Nil);
    DoneContext;
  end;
end;

function TExtOpenSSLSocketHandler.Accept: Boolean;
const MAX_ACCEPT_TIMES = 3;
var
  acceptTimeOut : Integer;
begin
  Result:=InitContext(True);
  if Result then
  begin
    FLocation := esslfAccept;
    Result:=CheckSSL(FSSL.setfd(Socket.Handle));
    acceptTimeOut := 0;
    while Result do
    begin
      Result:=CheckSSL(FSSL.Accept);
      if (not Result) then
      begin
        case FSSLLastError of
         SSL_ERROR_WANT_READ, SSL_ERROR_WANT_WRITE:
         begin
           Inc(acceptTimeOut);
           Sleep(500);
           if (acceptTimeOut < MAX_ACCEPT_TIMES) then
             Result := True;
         end;
        else
          break;
        end;
      end else
        break;
    end;
  end;
  SetSSLActive(Result);
  if Result then
    TryToSaveMasterKey else
    HandleSSLLastError;
end;


function TExtOpenSSLSocketHandler.Shutdown(BiDirectional : Boolean): boolean;
var
  r : integer;
begin
  Result:=assigned(FSsl);
  if Result then
    If Not BiDirectional then
    begin
      FLocation := esslfShutdown;

      if not FSSL.GetShutdown then
      begin
        if FSSLLastError = SSL_ERROR_SSL then
          Result := true
        else begin
          FSSL.Shutdown;
          ErrClearError;
        end;
      end;
    end
    else
    begin
      r:=FSSL.Shutdown;
      if r<>0 then
        Result:=CheckSSL(r)
      else
      begin
        Result:=fpShutdown(Socket.Handle,1)=0;
        if Result then
          Result:=CheckSSL(FSsl.Shutdown);
      end;
    end;
  If Result then
    Result:=DoneContext;
end;

function TExtOpenSSLSocketHandler.Send(const Buffer; Count: Integer): Integer;
begin
  if (FSSLLastError = SSL_ERROR_NONE) or (IsSSLBlockError(FSSLLastError)) then
  begin
    // sentinel so we can tell if failure happened without any error code
    // (otherwise we might see ESysENoTTY)
    {$ifdef unix}
    FpSetErrNo(Low(SocketError));
    {$endif}
    FLocation := esslfWrite;
    Result:=FSsl.Write(@Buffer, Count);
    FLastResult := Result;
    FSSLLastError:= FSsl.GetError(Result);
    if (FSSLLastError=SSL_ERROR_WANT_WRITE) and (Socket.IOTimeout > 0) then
      FSSLLastError:=SSL_ERROR_ZERO_RETURN;
    if (FSSLLastError=SSL_ERROR_ZERO_RETURN) then
      Result:=0 else
    begin
      HandleSSLIOError(true);
      if Result < 0 then
      begin
        FSSLLastError := SSL_ERROR_ZERO_RETURN;
        Result := 0;
      end;
    end;
  end else
    Result := -1;
end;

function TExtOpenSSLSocketHandler.Recv(const Buffer; Count: Integer): Integer;
begin
  if (FSSLLastError = SSL_ERROR_NONE) or (IsSSLBlockError(FSSLLastError)) then
  begin
    FLocation := esslfRead;
    Result:=FSSL.Read(@Buffer, Count);
    FLastResult := Result;
    FSSLLastError:= FSSL.GetError(Result);
    if (FSSLLastError=SSL_ERROR_WANT_READ) and (Socket.IOTimeout > 0) then
      FSSLLastError:=SSL_ERROR_ZERO_RETURN;
    if (FSSLLastError=SSL_ERROR_ZERO_RETURN) then
      Result:=0 else
    begin
      HandleSSLIOError(false);
      if Result < 0 then
      begin
        FSSLLastError := SSL_ERROR_ZERO_RETURN;
        Result := 0;
      end;
    end;
  end else
    Result := -1;
end;

function TExtOpenSSLSocketHandler.BytesAvailable: Integer;
begin
  Result:= FSSL.Pending;
end;

function TExtOpenSSLSocketHandler.ContextToGlobal : TExtSSLContext;
begin
  Result := FCTX;
  FUseGlobalContext := true;
end;

function TExtOpenSSLSocketHandler.SSLLastError: integer;
begin
  Result:=FSSLLastError;
end;

initialization
  TSSLSocketHandler.SetDefaultHandlerClass(TExtOpenSSLSocketHandler);
end.

