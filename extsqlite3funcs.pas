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

