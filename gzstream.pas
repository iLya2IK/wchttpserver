{
 GzStream:
   Implementation for gzip and deflate compression
   based on zlib

   Part of WCHTTPServer project

   Copyright (c) 2021 by Ilya Medvedkov

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit gzstream;

{$mode objfpc}{$H+}

interface

uses
  Classes, zlib;

type
  Tcompressionlevel=(
    clnone,                     {Do not use compression, just copy data.}
    clfastest,                  {Use fast (but less) compression.}
    cldefault,                  {Use default compression}
    clmax                       {Use maximum compression}
  );

  Tcustomzlibstream=class(Townerstream)
  protected
    Fstream:z_stream;
    Fbuffer:pointer;
    Fonprogress:Tnotifyevent;
    procedure progress(sender:Tobject);
    property onprogress:Tnotifyevent read Fonprogress write Fonprogress;
  public
    constructor create(stream:Tstream);
    destructor destroy;override;
  end;

  { Tcompressionstream }

  TDefcompressionstream=class(Tcustomzlibstream)
  private
    procedure ClearOutBuffer;
  protected
    raw_written,compressed_written: int64;
  public
    constructor create(level:Tcompressionlevel;
                       dest:Tstream;
                       Askipheader:boolean=false);
    destructor destroy;override;
    function write(const buffer;count:longint):longint;override;
    procedure flush;
    function get_compressionrate:single;
    property OnProgress;
  end;

  {$IFDEF ALLOW_STREAM_GZIP}
  TGzCompressionstream = class(TDefcompressionstream)
  public
    constructor create(level:Tcompressionlevel; dest:Tstream);
  end;
  {$ENDIF}

  Ezliberror=class(Estreamerror)
  end;

  Ecompressionerror=class(Ezliberror)
  end;

implementation

const   bufsize=16384;     {Size of the buffer used for temporarily storing
                            data from the child stream.}


constructor Tcustomzlibstream.create(stream:Tstream);

begin
  assert(stream<>nil);
  inherited create(stream);
  getmem(Fbuffer,bufsize);
end;

procedure Tcustomzlibstream.progress(sender:Tobject);

begin
  if Fonprogress<>nil then
    Fonprogress(sender);
end;

destructor Tcustomzlibstream.destroy;

begin
  freemem(Fbuffer);
  inherited destroy;
end;

{***************************************************************************}

constructor TDefcompressionstream.create(level:Tcompressionlevel;
                                      dest:Tstream;
                                      Askipheader:boolean=false);

var err,l:smallint;

begin
  inherited create(dest);
  Fstream.next_out:=Fbuffer;
  Fstream.avail_out:=bufsize;

  case level of
    clnone:
      l:=Z_NO_COMPRESSION;
    clfastest:
      l:=Z_BEST_SPEED;
    cldefault:
      l:=Z_DEFAULT_COMPRESSION;
    clmax:
      l:=Z_BEST_COMPRESSION;
  end;

  if Askipheader then
    err:=deflateInit2(Fstream,l,Z_DEFLATED,-15,8,0)
  else
    err:=deflateInit(Fstream,l);
  if err<>Z_OK then
    raise Ecompressionerror.create(zerror(err));
end;

function TDefcompressionstream.write(const buffer;count:longint):longint;

var err:smallint;
    lastavail:longint;

begin
  Fstream.next_in:=@buffer;
  Fstream.avail_in:=count;
  lastavail:=count;
  while Fstream.avail_in<>0 do
    begin
      if Fstream.avail_out=0 then
        ClearOutBuffer;
      inc(raw_written,lastavail-Fstream.avail_in);
      lastavail:=Fstream.avail_in;
      err:=deflate(Fstream,Z_NO_FLUSH);
      if err<>Z_OK then
        raise Ecompressionerror.create(zerror(err));
    end;
  inc(raw_written,lastavail-Fstream.avail_in);
  write:=count;
end;

function TDefcompressionstream.get_compressionrate:single;
begin
  get_compressionrate:=100*compressed_written/raw_written;
end;

procedure TDefcompressionstream.ClearOutBuffer;

begin
  { Flush the buffer to the stream and update progress }
  source.writebuffer(Fbuffer^,bufsize-Fstream.avail_out);
  inc(compressed_written,bufsize-Fstream.avail_out);
  progress(self);
  { reset output buffer }
  Fstream.next_out:=Fbuffer;
  Fstream.avail_out:=bufsize;
end;

procedure TDefcompressionstream.flush;
var err:smallint;

begin
  {Compress remaining data still in internal zlib data buffers.}
  repeat
    if Fstream.avail_out=0 then
      ClearOutBuffer;
    err:=deflate(Fstream,Z_FINISH);
    if err=Z_STREAM_END then
      break;
    if (err<>Z_OK) then
      raise Ecompressionerror.create(zerror(err));
  until false;
  if Fstream.avail_out<bufsize then
    ClearOutBuffer;
end;


destructor TDefcompressionstream.destroy;
begin
  try
    Flush;
  finally
    deflateEnd(Fstream);
    inherited destroy;
  end;
end;

{$IFDEF ALLOW_STREAM_GZIP}

constructor TGzCompressionstream.create(level:Tcompressionlevel;
                                      dest:Tstream);
var err,l:smallint;

begin
  FSource := dest;
  getmem(Fbuffer,bufsize);

  Fstream.next_out:=Fbuffer;
  Fstream.avail_out:=bufsize;

  case level of
    clnone:
      l:=Z_NO_COMPRESSION;
    clfastest:
      l:=Z_BEST_SPEED;
    cldefault:
      l:=Z_DEFAULT_COMPRESSION;
    clmax:
      l:=Z_BEST_COMPRESSION;
  end;

  err:=deflateInit2(Fstream,l,Z_DEFLATED,15 or 16,8,0);
  if err<>Z_OK then
    raise Ecompressionerror.create(zerror(err));
end;

{$ENDIF}

end.

