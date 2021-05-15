unit wcdeflatedecoder;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, wcdecoders, zlib;

type
  { TWCDeflateClientDecoder }

  TWCDeflateClientDecoder = class(TWCClientDecoder)
  protected
    class procedure Decode(Buffer: Pointer; Count: PtrInt;
                     out NewBuffer: PByte; out NewCount: PtrInt); override;
  public
    class function DecoderName : String; override;
  end;

implementation

{ TWCDeflateClientDecoder }

class procedure TWCDeflateClientDecoder.Decode(Buffer : Pointer; Count : PtrInt;
  out NewBuffer : PByte; out NewCount : PtrInt);
const CHUNCK_START_SZ = 512;
      CHUNCK_MAX_SZ = 32768;
var ret, have : Integer;
    infstream : z_stream;
    decompressChunckSize : uInt;
begin
  infstream.zalloc := nil;
  infstream.zfree := nil;
  infstream.opaque := nil;
  infstream.avail_in := Count;
  infstream.next_in := Buffer; // input char array

  ret := inflateInit(infstream);
  if ret <> Z_OK then raise Exception.CreateFmt('inflateInit return code %d', [ret]);

  NewCount := 0;
  decompressChunckSize := CHUNCK_START_SZ;
  NewBuffer := AllocMem(decompressChunckSize);
  try
    while true do begin
      infstream.avail_out := decompressChunckSize;
      infstream.next_out := @(NewBuffer[NewCount]);
      ret := inflate(infstream,  Z_SYNC_FLUSH);
      if ret = Z_STREAM_ERROR then raise Exception.Create('inflate Z_STREAM_ERROR');
      if not (ret in [Z_OK, Z_STREAM_END]) then
      begin
        inflateEnd(infstream);
        raise Exception.CreateFmt('inflate error code %d', [ret]);
      end;
      have := decompressChunckSize - infstream.avail_out;
      Inc(NewCount, have);
      if infstream.avail_out = 0 then begin
        NewBuffer := ReAllocMem(NewBuffer, NewCount + decompressChunckSize);
        decompressChunckSize := decompressChunckSize shl 1;
        if decompressChunckSize > CHUNCK_MAX_SZ then
           decompressChunckSize := CHUNCK_MAX_SZ;
      end else begin
        ret := Z_OK;
        Break;
      end;
    end;
    inflateEnd(infstream);
  finally
    if ret <> Z_OK then
    begin
      NewCount := 0;
      FreeMemAndNil(NewBuffer);
    end;
  end;
end;

class function TWCDeflateClientDecoder.DecoderName : String;
begin
  Result := 'deflate';
       //according:
       //https://developer.mozilla.org/ru/docs/Web/HTTP/Headers/Content-Encoding
end;

initialization
  RegisterWCDecoder(TWCDeflateClientDecoder);

end.

