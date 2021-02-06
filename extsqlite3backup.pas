unit ExtSqlite3Backup;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtSqlite3DS,
  kcThreadPool,
  {$IFDEF LOAD_DYNAMICALLY}
  SQLite3Dyn
  {$ELSE}
  SQLite3
  {$ENDIF} ;

type
  { TBackupJob }

  TBackupJob = class(TJob)
  private
    FBackUpName : String;
    FOwner : TExtSqlite3Dataset;
  public
    constructor Create(aOwner: TExtSqlite3Dataset; const aBackupFile: String);
    procedure Execute; override;
  end;

implementation

{ TBackupJob }

constructor TBackupJob.Create(aOwner: TExtSqlite3Dataset; const aBackupFile : String);
begin
  FOwner := aOwner;
  FBackUpName:= aBackupFile;
end;

procedure TBackupJob.Execute;
var
  pBackup : psqlite3backup;
  rc : integer;
  pFile : psqlite3;
begin
  if not Assigned(FOwner) then Exit;
  if not Assigned(FOwner.SqliteHandle) then Exit;

  if FileExists(FBackUpName) then
    DeleteFile(FBackUpName);

  (* Open the database file identified by zFilename. *)
  rc := sqlite3_open(PAnsiChar(FBackUpName), @pFile);
  if( rc = SQLITE_OK ) then begin

    (* Open the sqlite3_backup object used to accomplish the transfer *)
    pBackup := sqlite3_backup_init(pFile, PAnsiChar('main'), FOwner.SqliteHandle, PAnsiChar('main'));
    if ( Assigned(pBackup) ) then begin

      { Each iteration of this loop copies 5 database pages from database
      ** pDb to the backup database. If the return value of backup_step()
      ** indicates that there are still further pages to copy, sleep for
      ** 250 ms before repeating. }
      repeat
        rc := sqlite3_backup_step(pBackup, 5);
        if (rc=SQLITE_OK) or (rc=SQLITE_BUSY) or (rc=SQLITE_LOCKED) then
          sqlite3_sleep(250);
      until not ( (rc=SQLITE_OK) or (rc=SQLITE_BUSY) or (rc=SQLITE_LOCKED) );

      { Release resources allocated by backup_init(). }
      sqlite3_backup_finish(pBackup);
    end;
    rc := sqlite3_errcode(pFile);
  end;

  { Close the database connection opened on database file zFilename
  ** and return the result of this function. }
  sqlite3_close(pFile);
end;

end.

