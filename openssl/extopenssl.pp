{
 ExtOpenSSL:
   Wrappers for OpenSSL based on fpopenssl:
   Added Alpn wrapper

   Part of WCHTTP2Server project

   Copyright (c) 2021 by Ilya Medvedkov

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit extopenssl;

{$mode objfpc}{$H+}
{$DEFINE DUMPCERT}

interface

uses
  Classes, SysUtils, sslbase, openssl, ctypes;

{$IFDEF DUMPCERT}
Const
{$IFDEF UNIX}
  DumpCertFile = '/tmp/x509.txt';
{$ELSE}
  DumpCertFile = 'C:\temp\x509.txt';
{$ENDIF}
{$ENDIF}

Type
  { TExtSSLContext }

  TExtSSLContext = Class;
  TRTlsExtCtx = record
    CTX: TExtSSLContext;
    domains: array of string; // SSL Certificate with one or more alternative names (SAN)
  end;
  TTlsExtCtx = array of TRTlsExtCtx;

  TAlpnSelect = function (outv : PPChar; outl : PChar; inv : PChar; inlen : Cardinal) : integer of object;

  TExtSSLContext = Class(TObject)
  private
    FCTX: PSSL_CTX;
    FAlpnSelect : TAlpnSelect;
    function UsePrivateKey(pkey: SslPtr): cInt;
    function UsePrivateKeyASN1(pk: cInt; d: String; len: cLong): cInt;
    function UsePrivateKeyASN1(pk: cInt; d: TBytes; len: cLong): cInt;
    function UsePrivateKeyFile(const Afile: String; Atype: cInt): cInt;
  Public
    Constructor Create(AContext : PSSL_CTX = Nil); overload;
    Constructor Create(AType : TSSLType); overload;
    Destructor Destroy; override;
    Function SetCipherList(Var ACipherList : String) : Integer;
    procedure SetVerify(mode: Integer; arg2: TSSLCTXVerifyCallback);
    procedure SetDefaultPasswdCb(cb: PPasswdCb);
    procedure SetDefaultPasswdCbUserdata(u: SslPtr);
    Function UsePrivateKey(Data : TSSLData) : cint;
    // Use certificate.
    Function UseCertificate(Data : TSSLData) : cint;
    function UseCertificateASN1(len: cLong; d: String):cInt; overload; deprecated 'use TBytes overload';
    function UseCertificateASN1(len: cLong; buf: TBytes):cInt; overload;
    function UseCertificateFile(const Afile: String; Atype: cInt):cInt;
    function UseCertificateChainFile(const Afile: PChar):cInt;
    function UseCertificate(x: SslPtr):cInt;
    function LoadVerifyLocations(const CAfile: String; const CApath: String):cInt;
    function LoadPFX(Const S,APassword : AnsiString) : cint; deprecated 'use TBytes overload';
    function LoadPFX(Const Buf : TBytes;APassword : AnsiString) : cint;
    function LoadPFX(Data : TSSLData; Const APAssword : Ansistring) : cint;
    function SetOptions(AOptions: cLong): cLong;
    procedure SetTlsextServernameCallback(cb: PCallbackCb);
    procedure SetTlsextServernameArg(ATlsextcbp: SslPtr);
    procedure ActivateServerSNI(ATlsextcbp: TTlsExtCtx);
    procedure SetEcdhAuto(const onoff: boolean);
    procedure SetAlpnSelect(aAlpnSelect : TAlpnSelect);
    Property CTX: PSSL_CTX Read FCTX;
  end;

  TExtSSL = Class(TObject)
  Public
    FSSL : PSSL;
  Public
    Constructor Create(ASSL : PSSL = Nil);
    Constructor Create(AContext : TExtSSLContext);
    destructor Destroy; override;
    function SetFd(fd: cInt):cInt;
    function Accept : cInt;
    function Connect : cInt;
    function Shutdown : cInt;
    function Read(buf: SslPtr; num: cInt):cInt;
    function Peek(buf: SslPtr; num: cInt):cInt;
    function Write(buf: SslPtr; num: cInt):cInt;
    Function PeerCertificate : PX509;
    function Ctrl(cmd: cInt; larg: clong; parg: Pointer): cInt;
    function Pending:cInt;
    Function GetError(AResult :cint) : cint;
    function GetCurrentCipher :SslPtr;
    function Version: String;
    function PeerName: string;
    function PeerNameHash: cardinal;
    function PeerSubject : String;
    Function PeerIssuer : String;
    Function PeerSerialNo : Integer;
    Function PeerFingerprint : String;
    Function CertInfo : String;
    function CipherName: string;
    function CipherBits: integer;
    function CipherAlgBits: integer;
    Function VerifyResult : Integer;
    Property SSL: PSSL Read FSSL;
  end;


  TExtOpenSSLX509Certificate = Class (TX509Certificate)
  Protected
    function CreateKey: PEVP_PKEY; virtual;
    procedure SetNameData(x: PX509); virtual;
    procedure SetTimes(x: PX509); virtual;
  Public
    Function CreateCertificateAndKey : TCertAndKey; override;
  end;

  ESSL = Class(Exception);

Function BioToString(B : PBIO; FreeBIO : Boolean = False) : AnsiString;

implementation

uses dateutils;

Resourcestring
  SErrCountNotGetContext = 'Failed to create SSL Context';
  SErrFailedToCreateSSL = 'Failed to create SSL';


Function BioToString(B : PBIO; FreeBIO : Boolean = False) : AnsiString;

Var
  L,RL : Integer;
begin
  l:=bioctrlpending(B);
  Result:=StringOfChar(#0,l);
  RL:=BioRead(B,Result,L);
  if (RL>0) then
    SetLength(Result,RL)
  else
    SetLength(Result,0);
  if FreeBio then
    BioFreeAll(B);
end;

Function BioToTBytes(B : PBIO; FreeBIO : Boolean = False) : TBytes;

Var
  L,RL : Integer;
begin
  l:=bioctrlpending(B);
  SetLength(Result,l);
  FillChar(Result[0],L,0);
  RL:=BioRead(B,Result,L);
  if (RL>0) then
    SetLength(Result,RL)
  else
    SetLength(Result,0);
  if FreeBio then
    BioFreeAll(B);
end;

function SelectSNIContextCallback(ASSL: TExtSSL; {%H-}ad: integer; args: Pointer): integer; cdecl;
var
  arg :  TTlsExtCtx absolute args;
  sHostName: string;
  o, i, f: integer;
begin
  sHostName := SSLGetServername(ASSL, TLSEXT_NAMETYPE_host_name);
  if (sHostName <> '') and (length(arg) > 0) then
  begin
    f := -1;
    for o:=0 to length(arg)-1 do
    begin
      for i:=0 to length(arg[o].domains)-1 do
        if sHostName = arg[o].domains[i] then
        begin
          f := o;
          break;
        end;
      if f <> -1 then break
    end;
    if f = -1 then
      result := SSL_TLSEXT_ERR_NOACK
    else if f > 1 then // first one should be the main certificate
      SslSetSslCtx(ASSL, arg[f].CTX);
  end;
  result := SSL_TLSEXT_ERR_OK;
end;

function alpn_cb({%H-}s : PSSL; outv : PPChar; outlen : PChar;
                           inv : PChar; inlen : integer; arg : SslPtr) : integer; cdecl;
var alpn_ctx : TExtSSLContext;
begin
  alpn_ctx := TExtSSLContext(arg);

  Result := alpn_ctx.FAlpnSelect(outv, outlen, inv, inlen);
end;

{ TExtOpenSSLX509Certificate }


procedure TExtOpenSSLX509Certificate.SetNameData(x: PX509);

Var
  ND : PX509_NAME;
  S : AnsiString;

  Procedure SetEntry(aCode,aValue : AnsiString);

  begin
    if (AValue<>'') then
      X509NameAddEntryByTxt(ND, aCode, $1001, aValue, -1, -1, 0);
  end;

begin
  ND:=X509GetSubjectName(x);
  S:=Country;
  if S='' then
    S:='BE';
  SetEntry('C',S);
  S:=HostName;
  if S='' then
    S:='localhost';
  SetEntry('CN',S);
  SetEntry('O',Organization);
  x509SetIssuerName(x,ND);
end;

Procedure TExtOpenSSLX509Certificate.SetTimes(x : PX509);

var
  Utc : PASN1_UTCTIME;

begin
  Utc:=Asn1UtctimeNew;
  try
    ASN1UtcTimeSetString(Utc,PAnsiChar(FormatDateTime('YYMMDDHHNNSS',ValidFrom)));
    X509SetNotBefore(x, Utc);
    ASN1UtcTimeSetString(Utc,PAnsiChar(FormatDateTime('YYMMDDHHNNSS',ValidTo)));
    X509SetNotAfter(x,Utc);
  finally
    Asn1UtctimeFree(Utc);
  end;
end;


function TExtOpenSSLX509Certificate.CreateKey : PEVP_PKEY;

Var
  rsa: PRSA;

begin
  Result:=EvpPkeynew;
  rsa:=RsaGenerateKey(KeySize,$10001,nil,nil);
  EvpPkeyAssign(Result,EVP_PKEY_RSA,rsa);
end;

function TExtOpenSSLX509Certificate.CreateCertificateAndKey: TCertAndKey;

var
  pk: PEVP_PKEY;
  x: PX509;
  b: PBIO;
{$IFDEF DUMPCERT}
  s : string;
{$ENDIF}

begin
  SetLength(Result.Certificate,0);
  SetLength(Result.PrivateKey,0);
  pk := nil;
  x := X509New;
  try
    X509SetVersion(x, Version);
    Asn1IntegerSet(X509getSerialNumber(x), GetRealSerial);
    SetTimes(X);
    pk:=CreateKey;
    X509SetPubkey(x, pk);
    SetNameData(x);
    x509Sign(x,pk,EvpGetDigestByName('SHA1'));
    // Certificate
    b := BioNew(BioSMem);
    i2dX509Bio(b, x);
    Result.Certificate:=BioToTbytes(B,True);
    // Private key
    b := BioNew(BioSMem);
    i2dPrivatekeyBio(b, pk);
    Result.PrivateKey:=BioToTbytes(B,True);
{$IFDEF DUMPCERT}
    b := BioNew(BioSMem);
    PEM_write_bio_X509(b,x);
    S:=BioToString(B,True);
    With TStringList.Create do
      try
        Add(S);
        SaveToFile(DumpCertFile);
      finally
        Free;
      end;
{$ENDIF}
  finally
    X509free(x);
    EvpPkeyFree(pk);
  end;
end;

{ TExtSSLContext }

constructor TExtSSLContext.Create(AContext: PSSL_CTX);
begin
  FCTX:=AContext;
  FAlpnSelect := nil;
end;

constructor TExtSSLContext.Create(AType: TSSLType);

Var
  C : PSSL_CTX;

begin
  C := nil;
  Case AType of
    stAny:
      begin
        if Assigned(SslTLSMethod) then
          C := SslCtxNew(SslTLSMethod)
        else
          C := SslCtxNew(SslMethodV23);
      end;
    stSSLv2: C := SslCtxNew(SslMethodV2);
    stSSLv3: C := SslCtxNew(SslMethodV3);
    stTLSv1: C := SslCtxNew(SslMethodTLSV1);
    stTLSv1_1: C := SslCtxNew(SslMethodTLSV1_1);
    stTLSv1_2: C := SslCtxNew(SslMethodTLSV1_2);
  end;
  if (C=Nil) then
     Raise ESSL.Create(SErrCountNotGetContext);
  Create(C);
end;

destructor TExtSSLContext.Destroy;
begin
  SslCtxFree(FCTX);
  inherited Destroy;
end;

function TExtSSLContext.SetCipherList(var ACipherList: String): Integer;
begin
  Result:=SSLCTxSetCipherList(FCTX,ACipherList);
end;

procedure TExtSSLContext.SetVerify(mode: Integer; arg2: TSSLCTXVerifyCallback);
begin
  SslCtxSetVerify(FCtx,Mode,arg2);
end;

procedure TExtSSLContext.SetDefaultPasswdCb(cb: PPasswdCb);
begin
  SslCtxSetDefaultPasswdCb(Fctx,cb)
end;

procedure TExtSSLContext.SetDefaultPasswdCbUserdata(u: SslPtr);
begin
  SslCtxSetDefaultPasswdCbUserdata(FCTX,u);
end;

function TExtSSLContext.UsePrivateKey(pkey: SslPtr):cInt;
begin
  Result:=SslCtxUsePrivateKey(FCTX,pkey);
end;

function TExtSSLContext.UsePrivateKeyASN1(pk: cInt; d: String; len: cLong):cInt;
begin
  Result:=SslCtxUsePrivateKeyASN1(pk,FCtx,d,len);
end;

function TExtSSLContext.UsePrivateKeyASN1(pk: cInt; d: TBytes; len: cLong): cInt;
begin
  Result:=SslCtxUsePrivateKeyASN1(pk,FCtx,d,len);
end;

function TExtSSLContext.UsePrivateKeyFile(const Afile: String; Atype: cInt):cInt;
begin
  Result:=SslCtxUsePrivateKeyFile(FCTX,AFile,AType);
end;

function TExtSSLContext.UsePrivateKey(Data: TSSLData): cint;

Var
  FN : String;
  l : integer;

begin
  Result:=-1;
  L:=Length(Data.Value);
  If (l<>0) then
    Result:=UsePrivateKeyASN1(EVP_PKEY_RSA,Data.Value,L)
  else if (Data.FileName<>'') then
    begin
    FN:=Data.FileName;
    Result:=UsePrivateKeyFile(FN,SSL_FILETYPE_PEM);
    if (Result<>1) then
      Result:=UsePrivateKeyFile(FN,SSL_FILETYPE_ASN1);
    end;
end;

function TExtSSLContext.UseCertificate(Data: TSSLData): cint;

Var
  l : integer;
  FN : String;

begin
  Result:=-1;
  L:=Length(Data.Value);
  if (L<>0) then
    Result:=UseCertificateASN1(length(Data.Value),Data.Value)
  else if (Data.FileName<>'') then
    begin
    FN:=Data.FileName;
    Result:=UseCertificateChainFile(PChar(FN));
    if Result<>1 then
       begin
       Result:=UseCertificateFile(FN,SSL_FILETYPE_PEM);
       if (Result<>1) then
         Result:=UseCertificateFile(FN,SSL_FILETYPE_ASN1);
       end;
    end
end;

function TExtSSLContext.UseCertificateASN1(len: cLong; d: String): cInt;
begin
  Result:=sslctxUseCertificateASN1(FCTX,len,d);
end;

function TExtSSLContext.UseCertificateASN1(len: cLong; buf: TBytes): cInt;
begin
  Result:=sslctxUseCertificateASN1(FCTX,len,Buf);
end;

function TExtSSLContext.UseCertificateFile(const Afile: String; Atype: cInt): cInt;
begin
  Result:=sslctxUseCertificateFile(FCTX,Afile,Atype);
end;

function TExtSSLContext.UseCertificateChainFile(const Afile: PChar): cInt;
begin
  Result:=sslctxUseCertificateChainFile(FCTX,Afile);
end;

function TExtSSLContext.UseCertificate(x: SslPtr): cInt;
begin
  Result:=SSLCTXusecertificate(FCTX,X);
end;

function TExtSSLContext.LoadVerifyLocations(const CAfile: String; const CApath: String): cInt;
begin
  Result:=SslCtxLoadVerifyLocations(FCTX,CAfile,CApath);
end;

function TExtSSLContext.LoadPFX(const S, APassword: AnsiString): cint;

var
  Buf : TBytes;

begin
  SetLength(Buf,Length(S));
  Move(S[1],Buf[0],Length(S));
  Result:=LoadPFX(Buf,APAssword);
end;

function TExtSSLContext.LoadPFX(const Buf: TBytes; APassword: AnsiString): cint;

var
  b: PBIO;
  p12,c,pk,ca: SslPtr;

begin
  Result:=-1;
  c:=nil;
  pk:=nil;
  ca:=nil;
  p12:=Nil;
  b:=BioNew(BioSMem);
  try
    BioWrite(b,Buf,Length(Buf));
    p12:=d2iPKCS12bio(b,nil);
    if Assigned(p12) then
      if PKCS12parse(p12,APassword,pk,c,ca)>0 then
        begin
        Result:=UseCertificate(c);
        if (Result>0) then
          Result:=UsePrivateKey(pk);
        end;
  finally
    if pk<>Nil then
      EvpPkeyFree(pk);
    if c<>nil then
      X509free(c);
//  SkX509PopFree(ca,_X509Free);
    if p12<>Nil then
      PKCS12free(p12);
    BioFreeAll(b);
  end;
end;

function TExtSSLContext.LoadPFX(Data: TSSLData; const APAssword: Ansistring
  ): cint;

Var
  B : TBytes;

begin
  Result:=-1;
  try
    if (Length(Data.Value)<>0) then
      B:=Data.Value
    else
      With TFileStream.Create(Data.FileName,fmOpenRead or fmShareDenyNone) do
        Try
          SetLength(B,Size);
          ReadBuffer(B[0],Size);
        finally
          Free;
        end;
    Result:=LoadPFX(B,APassword);
  except
    // Silently ignore
    Exit;
  end;
end;

function TExtSSLContext.SetOptions(AOptions: cLong): cLong;
begin
  result := SslCtxCtrl(FCTX, SSL_CTRL_OPTIONS, AOptions, nil);
end;

procedure TExtSSLContext.SetTlsextServernameCallback(cb: PCallbackCb);
begin
  SslCtxCallbackCtrl(FCTX, SSL_CTRL_SET_TLSEXT_SERVERNAME_CB, cb);
end;

procedure TExtSSLContext.SetTlsextServernameArg(ATlsextcbp: SslPtr);
begin
  SslCtxCtrl(FCTX, SSL_CTRL_SET_TLSEXT_SERVERNAME_ARG, 0, ATlsextcbp);
end;

procedure TExtSSLContext.ActivateServerSNI(ATlsextcbp: TTlsExtCtx);
begin
  SetTlsextServernameCallback(@SelectSNIContextCallback);
  SetTlsextServernameArg(Pointer(ATlsextcbp));
end;

procedure TExtSSLContext.SetEcdhAuto(const onoff: boolean);
var larg: clong;
begin
  if onoff then
    larg := 1
  else
    larg := 0;
  SslCtxCtrl(FCTX, SSL_CTRL_SET_ECDH_AUTO, larg, nil);
end;

procedure TExtSSLContext.SetAlpnSelect(aAlpnSelect: TAlpnSelect);
begin
  FAlpnSelect:= aAlpnSelect;
  if Assigned(FAlpnSelect) then
     SSLCtxSetAlpnSelectCb(FCTX, @alpn_cb, Self);
end;

{ TExtSSL }

Constructor TExtSSL.Create(ASSL: PSSL);
begin
  FSSL:=ASSL;
end;

Constructor TExtSSL.Create(AContext: TExtSSLContext);
begin
  FSSL:=Nil;
  if Assigned(AContext) and Assigned(AContext.CTX) then
    FSSL:=sslNew(AContext.CTX);
  If (FSSL=Nil) then
    Raise ESSL.Create(SErrFailedToCreateSSL)
end;

destructor TExtSSL.Destroy;
begin
  sslfree(FSSL);
  inherited Destroy;
end;

function TExtSSL.Ctrl(cmd: cInt; larg: clong; parg: Pointer): cInt;

begin
  Result:=sslCtrl(fSSL,cmd,larg,parg);
end;

function TExtSSL.SetFd(fd: cInt): cInt;
begin
  Result:=sslSetFD(fSSL,fd);
end;

function TExtSSL.Accept: cInt;
begin
  Result:=sslAccept(fSSL);
end;

function TExtSSL.Connect: cInt;
begin
  Result:=sslConnect(fSSL);
end;

function TExtSSL.Shutdown: cInt;
begin
  try
    Result:=sslShutDown(fSSL);
  except
    // Sometimes, SSL gives an error when the connection is lost
  end;
end;

function TExtSSL.Read(buf: SslPtr; num: cInt): cInt;
begin
  Result:=sslRead(FSSL,buf,num);
end;

function TExtSSL.Peek(buf: SslPtr; num: cInt): cInt;
begin
  Result:=sslPeek(FSSL,buf,num);
end;

function TExtSSL.Write(buf: SslPtr; num: cInt): cInt;
begin
  Result:=sslWrite(FSSL,buf,num);
end;

Function TExtSSL.PeerCertificate: PX509;
begin
  Result:=sslGetPeercertificate(FSSL);
end;

function TExtSSL.Pending: cInt;
begin
  Result:=sslPending(FSSL);
end;

Function TExtSSL.GetError(AResult: cint): cint;
begin
  Result:=SslGetError(FSsl,AResult);
end;

function TExtSSL.GetCurrentCipher: SslPtr;
begin
  Result:=SSLGetCurrentCipher(FSSL);
end;

function TExtSSL.Version: String;
begin
  Result:=SSlGetVersion(FSsl);
end;

function TExtSSL.PeerName: string;
var
  s : ansistring;
  p : Integer;
begin
  Result:='';
  S:=PeerSubject;
  P:=Pos(S,'/CN=');
  if (P>0) then
    begin
    Delete(S,1,P+3);
    P:=Pos('/',S);
    if (P>0) then
      Result:=Copy(S,1,P-1);
    end;
end;

function TExtSSL.PeerNameHash: cardinal;
var
  C : PX509;
begin
  Result:=0;
  c:=PeerCertificate;
  if (C=Nil) then
    exit;
  try
    Result:=X509NameHash(X509GetSubjectName(C));
  finally
    X509Free(C);
  end;
end;

function TExtSSL.PeerSubject: String;
var
  c : PX509;
  s : ansistring;

begin
  Result:='';
  S:='';
  c:=PeerCertificate;
  if Assigned(c) then
    try
      setlength(s, 4096);
      Result:=X509NameOneline(X509GetSubjectName(c),s,Length(s));
    finally
      X509Free(c);
    end;
end;

Function TExtSSL.PeerIssuer: String;

var
  C: PX509;
  S: ansistring;

begin
  Result:='';
  C:=PeerCertificate;
  if (C=Nil) then
    Exit;
  try
    S:=StringOfChar(#0,4096);
    Result:=X509NameOneline(X509GetIssuerName(C),S,4096);
  finally
    X509Free(C);
  end;
end;

Function TExtSSL.PeerSerialNo: Integer;
var
  C : PX509;
  SN : PASN1_INTEGER;

begin
  Result:=-1;
  C:=PeerCertificate;
  if (C=Nil) then
    exit;
  try
    SN:=X509GetSerialNumber(C);
    Result:=Asn1IntegerGet(SN);
  finally
    X509Free(C);
  end;
end;

Function TExtSSL.PeerFingerprint: String;
var
  C : PX509;
  L : integer;

begin
  Result:='';
  C:=PeerCertificate;
  if (C=Nil) then
    Exit;
  try
    Result:=StringOfChar(#0,EVP_MAX_MD_SIZE);
    L:=0;
    X509Digest(C,EvpGetDigestByName('MD5'),Result,L);
    SetLength(Result,L);
  finally
    X509Free(C);
  end;
end;

Function TExtSSL.CertInfo: String;
var
  C : PX509;
  B : PBIO;

begin
  Result:='';
  C:=PeerCertificate;
  if (C=Nil)  then
    Exit;
  try
    B:=BioNew(BioSMem);
    try
      X509Print(B,C);
      Result:=BioToString(B);
    finally
      BioFreeAll(B);
    end;
  finally
    X509Free(C);
  end;
end;

function TExtSSL.CipherName: string;
begin
  Result:=SslCipherGetName(GetCurrentCipher);
end;

function TExtSSL.CipherBits: integer;

var
  x: integer;

begin
  x:=0;
  Result:=SSLCipherGetBits(GetCurrentCipher,x);
end;

function TExtSSL.CipherAlgBits: integer;

begin
  Result:=0;
  SSLCipherGetBits(GetCurrentCipher,Result);
end;

Function TExtSSL.VerifyResult: Integer;

begin
  Result:=SslGetVerifyResult(FSsl);
end;

end.

