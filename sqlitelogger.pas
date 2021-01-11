{
 SqliteLogger:
   Logging using sqlite database

   Part of WCHTTPServer project

   Copyright (c) 2021 by Ilya Medvedkov

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


unit sqlitelogger;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtSqlite3DS;

type

  { TSqliteLogger }

  TSqliteLogger = class
  private
    FSessionID: integer;
    FLogDB : TExtSqlite3Dataset;
    PREP_DOLOG : TSqlite3Prepared;
  public
    constructor Create(const DBName: String);
    destructor Destroy; override;
    procedure LogAdd(sec : Integer; const S : String); overload;
    procedure LogAdd(sec : Integer; const Fmt : String; const aParams : Array of Const); overload;
    property SessionID : integer read FSessionID;
  end;

const LOG_INFO = 1;
      LOG_ERROR = 2;

implementation

{ TSqliteLogger }

constructor TSqliteLogger.Create(const DBName: String);
var S : String;
begin
  FLogDB := TExtSqlite3Dataset.Create(nil);
  FLogDB.FileName := DBName;

  FLogDB.ExecSQL(
    'create table if not exists sessions'+
      '(id integer primary key autoincrement, '+
       'starttime timestamp default current_timestamp);');

  FLogDB.ExecSQL(
    'create table if not exists log'+
      '(id integer primary key autoincrement, '+
       't timestamp default current_timestamp, '+
       'sessId int, '+
       'kind int, '+
       'msg text, '+
       'foreign key(sessId) references sessions(id));');

  FLogDB.ExecuteDirect('INSERT INTO sessions default values;');
  S := FLogDB.QuickQuery('select id from sessions order by starttime desc limit 1;');
  FSessionID := StrToInt(S);
  PREP_DOLOG := FLogDB.AddNewPrep('INSERT INTO log (sessId,kind,msg) values('+S+',?1,?2);');
end;

destructor TSqliteLogger.Destroy;
begin
  FLogDB.Free;
  inherited Destroy;
end;

procedure TSqliteLogger.LogAdd(sec : Integer; const S: String);
begin
  PREP_DOLOG.Execute([sec, S]);
end;

procedure TSqliteLogger.LogAdd(sec : Integer; const Fmt: String;
  const aParams: array of const);
begin
  LogAdd(Sec, Format(Fmt, aParams));
end;

end.

