{
  This file is a part of example.
  look more in WCRESTJsonDemo.lpr
}

unit WCMainRESTJson;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes,
  wcApplication,
  HTTP1Utils;

type

  { TWCPreThread }

  TWCPreThread = class(TWCPreAnalizeClientNoSessionJob)
  public
    function GenerateClientJob: TWCMainClientJob; override;
  end;

procedure InitializeJobsTree;
procedure DisposeJobsTree;

implementation

uses WCRESTJsonJobs, AvgLvlTree;

var WCJobsTree : TStringToPointerTree;

procedure InitializeJobsTree;
begin
  WCJobsTree := TStringToPointerTree.Create(true);
  WCJobsTree.Values['/items.json'] := TWCMyItem;
end;

procedure DisposeJobsTree;
begin
  FreeAndNil(WCJobsTree);
end;

{ TWCPreThread }

function TWCPreThread.GenerateClientJob : TWCMainClientJob;
var ResultClass : TWCMainClientJobClass;
begin
  if CompareText(Request.Method, HTTPPOSTMethod)=0 then
  begin
    ResultClass := TWCMainClientJobClass(WCJobsTree.Values[Request.PathInfo]);
    if assigned(ResultClass) then
       Result := ResultClass.Create(Connection);
  end else
  begin
    Result := TWCSendServerFile.Create(Connection);
  end;
end;

end.
