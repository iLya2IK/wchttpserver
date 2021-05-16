{
 WCDecoders:
   Abstract classes for decoding client messages

   Part of WCHTTPServer project

   Copyright (c) 2021 by Ilya Medvedkov

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit wcdecoders;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StringHashList, ECommonObjs, BufferedStream, extmemorystream;

type

  { TWCClientDecoder }

  TWCClientDecoder = class
  protected
    class procedure Decode(Buffer: Pointer; Count: PtrInt;
                     out NewBuffer: PByte; out NewCount: PtrInt); virtual; abstract;
  public
    class function DecoderName : String; virtual;
    class procedure DecodeStream(InOutStream : TStream);
    class function DecodeString(const S : String) : RawByteString;
  end;

  TWCClientDecoderClass = class of TWCClientDecoder;

  { TThreadSafeDecoders }

  TThreadSafeDecoders = class(TNetCustomLockedObject)
  private
    FHash : TStringHashList;
    function GetDecoder(const index : String): TWCClientDecoderClass;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure AddNew(aData: TWCClientDecoderClass);
    procedure Remove(aData: TWCClientDecoderClass);
    property Decoder[const index : String] : TWCClientDecoderClass read GetDecoder; default;
    function GetListOfDecoders : String;
    procedure RebuildListOfDecoders(const aList : String);
  end;

procedure RegisterWCDecoder(aDecode : TWCClientDecoderClass);
procedure UnRegisterWCDecoder(aDecode : TWCClientDecoderClass);
function IsWCDecoderRegistered(const aDecoder : String) : Boolean;

implementation

const SErrStreamTypeNotSupported = 'Stream type not supported for decoding';

var WCRegisteredDecoders : TThreadSafeDecoders = nil;

procedure RegisterWCDecoder(aDecode : TWCClientDecoderClass);
begin
  if not Assigned(aDecode) then Exit;
  if not IsWCDecoderRegistered(aDecode.DecoderName) then
  begin
    WCRegisteredDecoders.AddNew(aDecode);
  end;
end;

procedure UnRegisterWCDecoder(aDecode : TWCClientDecoderClass);
begin
  if not Assigned(aDecode) then Exit;
  if IsWCDecoderRegistered(aDecode.DecoderName) then
  begin
    WCRegisteredDecoders.Remove(aDecode);
  end;
end;

function IsWCDecoderRegistered(const aDecoder : String) : Boolean;
begin
  Result := Assigned(WCRegisteredDecoders[aDecoder]);
end;

{ TThreadSafeDecoders }

constructor TThreadSafeDecoders.Create;
begin
  inherited Create;
  FHash := TStringHashList.Create(true);
end;

destructor TThreadSafeDecoders.Destroy;
begin
  FHash.Free;
  inherited Destroy;
end;

function TThreadSafeDecoders.GetDecoder(const index : String
  ) : TWCClientDecoderClass;
begin
  Result := TWCClientDecoderClass(FHash[index]);
end;

procedure TThreadSafeDecoders.Clear;
begin
  Lock;
  try
    FHash.Clear;
  finally
    Unlock;
  end;
end;

procedure TThreadSafeDecoders.AddNew(aData : TWCClientDecoderClass);
begin
  Lock;
  try
    FHash.Add(aData.DecoderName, aData);
  finally
    UnLock;
  end;
end;

procedure TThreadSafeDecoders.Remove(aData : TWCClientDecoderClass);
begin
  Lock;
  try
    FHash.Remove(aData.DecoderName);
  finally
    UnLock;
  end;
end;

function TThreadSafeDecoders.GetListOfDecoders : String;
var i : integer;
    D : String;
begin
  Lock;
  try
    Result := '';
    for i := 0 to FHash.Count-1 do
    begin
      D := TWCClientDecoderClass(FHash.List[i]^.Data).DecoderName;
      if Length(Result) > 0 then Result := Result + ',';
      Result := Result + D;
    end;
  finally
    UnLock;
  end;
end;

procedure TThreadSafeDecoders.RebuildListOfDecoders(const aList : String);
var i : integer;
    DList : TStringList;
    D : String;
begin
  Lock;
  try
    DList := TStringList.Create;
    try
      DList.Delimiter := ',';
      DList.DelimitedText := aList;
      for i := 0 to DList.Count-1 do
      begin
        if (not Assigned(Decoder[DList[i]])) and
           (IsWCDecoderRegistered(DList[i])) then
        begin
          AddNew(WCRegisteredDecoders.Decoder[DList[i]]);
        end;
      end;
      for i := FHash.Count-1 downto 0 do
      begin
        D :=  FHash.List[i]^.Key;
        if DList.IndexOf(D) < 0 then
        begin
          FHash.Remove(D);
        end;
      end;
    finally
      DList.Free;
    end;
  finally
    UnLock;
  end;
end;

{ TWCClientDecoder }

class function TWCClientDecoder.DecoderName : String;
begin
  Result := '';
end;

class procedure TWCClientDecoder.DecodeStream(InOutStream : TStream);
var outBuffer : PByte;
    Sz : PtrInt;
begin
  if not Assigned(InOutStream) then Exit;
  if InOutStream is TExtMemoryStream then
  begin
    Decode(TExtMemoryStream(InOutStream).Memory, InOutStream.Size, outBuffer, Sz);
    if Assigned(outBuffer) then
      TExtMemoryStream(InOutStream).SetPointer(outBuffer, Sz);
  end
  else
  if InOutStream is TMemoryStream then
  begin
    Decode(TMemoryStream(InOutStream).Memory, InOutStream.Size, outBuffer, Sz);
    if Assigned(outBuffer) then
    begin
      InOutStream.Position := 0;
      InOutStream.Size := Sz;
      InOutStream.WriteBuffer(outBuffer^, Sz);
      InOutStream.Position := 0;
      Freemem(outBuffer);
    end;
  end
  else
  if InOutStream is TBufferedStream then
  begin
    Decode(TBufferedStream(InOutStream).Memory, InOutStream.Size, outBuffer, Sz);
    if Assigned(outBuffer) then
    begin
      Freemem(TBufferedStream(InOutStream).Memory);
      TBufferedStream(InOutStream).SetPointer(outBuffer, Sz);
    end;
  end
  else
    raise Exception.Create(SErrStreamTypeNotSupported);
end;

class function TWCClientDecoder.DecodeString(const S : String) : RawByteString;
var outBuffer : PByte;
    Sz : PtrInt;
begin
  Decode(@(S[1]),  Length(S), outBuffer, Sz);
  SetLength(Result, Sz);
  Move(outBuffer^, Result[1], Sz);
  Freemem(outBuffer);
end;

initialization
  if not Assigned(WCRegisteredDecoders) then
    WCRegisteredDecoders := TThreadSafeDecoders.Create;

finalization
  if Assigned(WCRegisteredDecoders) then FreeAndNil(WCRegisteredDecoders);

end.

