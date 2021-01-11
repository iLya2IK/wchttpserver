{ BufferedStream
   stream class with non-owned memory buffer
   Copyright (c) 2018-2019 Ilya Medvedkov   }

unit BufferedStream;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

{ TBufferedStream }

TBufferedStream = class(TStream)
private
  FMemory: Pointer;
  FSize, FPosition: PtrInt;
protected
  Function GetSize : Int64; Override;
  function GetPosition: Int64; Override;
public
  procedure SetPointer(Ptr: Pointer; ASize: PtrInt);
  function Write(const Buffer; Count: Longint): Longint; override;
  function Read(var Buffer; Count: LongInt): LongInt; override;
  function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
  property Memory: Pointer read FMemory;
end;

implementation

{ TBufferedStream }

function TBufferedStream.GetSize: Int64;
begin
  Result:=FSize;
end;

function TBufferedStream.GetPosition: Int64;
begin
  Result:=FPosition;
end;

procedure TBufferedStream.SetPointer(Ptr: Pointer; ASize: PtrInt);
begin
  FMemory:=Ptr;
  FSize:=ASize;
  FPosition:=0;
end;

function TBufferedStream.Write(const Buffer; Count: Longint): Longint;
begin
 Result:=Count;
 If (Count>0) then
   begin
     If (Result>(FSize-FPosition)) then
       Result:=(FSize-FPosition);
     Move (Buffer,(FMemory+FPosition)^,Result);
     Inc(FPosition, Result);
   end;
end;

function TBufferedStream.Read(var Buffer; Count: LongInt): LongInt;
begin
 Result:=0;
 If (FSize>0) and (FPosition<Fsize) and (FPosition>=0) then
   begin
     Result:=Count;
     If (Result>(FSize-FPosition)) then
       Result:=(FSize-FPosition);
     Move ((FMemory+FPosition)^,Buffer,Result);
     Inc(FPosition, Result);
   end;
end;

function TBufferedStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
 Case Word(Origin) of
   soFromBeginning : FPosition:=Offset;
   soFromEnd       : FPosition:=FSize+Offset;
   soFromCurrent   : FPosition:=FPosition+Offset;
 end;
 Result:=FPosition;
end;

end.

