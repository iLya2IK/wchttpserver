{
 ExtSqlite3Funcs:
   Classes to realize additional sqlite functions.
   Example contains the Bitwise-Or aggregate function.
   
   Part of ESolver project
   Copyright (c) 2019-2020 by Ilya Medvedkov
   
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit extsqlite3funcs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtSqlite3DS, ctypes;

type

  { TBitwiseOrFunction }

  TBitwiseOrFunction = class(TSqlite3Function)
  public
    constructor Create;

    procedure StepFunc({%H-}argc : integer); override;
    procedure FinalFunc; override;
  end;

implementation

{ TBitwiseOrFunction }

constructor TBitwiseOrFunction.Create;
begin
  inherited Create('BITWISE_OR', 1, sqlteUtf8, sqlfAggregate);
end;

procedure TBitwiseOrFunction.StepFunc({%H-}argc: integer);
var buffer : PCInt;
  x : cint;
begin
    buffer := PCInt(GetAggregateContext(sizeof(cint)));
    x := AsInt(0);
    buffer^ := buffer^ or x;
end;

procedure TBitwiseOrFunction.FinalFunc;
var buffer : PCInt;
begin
    buffer := PCInt(GetAggregateContext(sizeof(cint)));
    SetResult(buffer^);
end;

end.

