{ Thread Pool
   Модуль для асинхронного выполнения задач клиентов
   Copyright (C) 2011 Maciej Kaczkowski / keit.co
   Part of ESolver project
   Доработка в части разделения общего пула асинхронных
   задач на несколько независимых очередей: Медведков И.С. - 2018-2019 }

unit kcThreadPool;

{$mode objfpc}{$H+}

interface

uses
  OGLFastList, Classes, SysUtils, syncobjs;

type
  TParallelProc = procedure of object;
  TThreadPool = class;

  { TSemaphore }

  TSemaphore = class
  private
    FEnabled : Boolean;
  public
    constructor Create;
    procedure Activate;
    procedure Deactivate;
    function Active : Boolean;
  end;

  { TJob }

  TJob = class
  public
    procedure Execute; virtual; abstract;
  end;

  { TSemaphoreJob }

  TSemaphoreJob = class(TJob)
  private
    FParallelProc: TParallelProc;
    FOwner: TThreadPool;
    FSemaphore : TSemaphore;
  public
    constructor Create;
    procedure Execute; override;
    property ParallelProc: TParallelProc read FParallelProc write FParallelProc;
    property Owner: TThreadPool read FOwner write FOwner;
    property Semaphore: TSemaphore read FSemaphore write FSemaphore;
  end;

  { TCustomThread }

  TCustomThread = class(TThread)
  private
    FOwner: TThreadPool;
    FRunning: Boolean;
    FJob : TJob;
  public
    procedure Execute; override;
    constructor Create(AOwner: TThreadPool);
    destructor Destroy; override;
    property Running: Boolean read FRunning;
    property myJob : TJob read FJob write FJob;
  end;

  { TIDJobPair }

  TIDJobPair = class
  private
    FID : Cardinal;
    FJobSeq : TFastSeq;
    FRTI : TRTLCriticalSection;
    FBusy : Boolean;
  public
    constructor Create(aID : Cardinal);
    destructor Destroy; override;
    procedure Push(const J : TJob);
    function  Pop : TJob;
    procedure Lock;
    procedure UnLock;
    procedure Deactivate;
    procedure Activate;
    function isEmpty : Boolean;

    property ID : Cardinal read FID;
  end;

  { TIDJob }

  TIDJob = class(TJob)
  private
    FGuestJob : TJob;
    FPairOwner : TIDJobPair;
  public
    constructor Create(guestJob : TJob; aPairOwner : TIDJobPair);
    procedure Execute; override;
    procedure Activate;
    destructor Destroy; override;
  end;

  { TIDJobMap }

  TIDJobMap = class
  private
    FRTI : TRTLCriticalSection;
    FIds : TFastCollection;
    FLastLoc : Integer;
    function GetCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddID(aID : Cardinal);
    procedure RemoveID(aID : Cardinal);
    function GetJob : TJob;
    function GetPair(aID : Cardinal) : TIDJobPair;
    function Pop(aID : Cardinal) : TJob;
    procedure Push(aID : Cardinal; j : TJob);
    procedure Lock;
    procedure UnLock;
    property Count : Integer read GetCount;
  end;

  { TThreadPool }

  TThreadPool = class
  private
    FPool: TFastList;
    FList: TFastList;
    FJobMap : TIDJobMap;
    FCS: TCriticalSection;
    FThreadsCount : Integer;
    FRunning: Boolean;
    function GetJobAt(index : integer): TJob;
    function GetJobsCount: Integer;
    function GetRunningThreads: Integer;
    procedure ExcludeThread(aThread : TCustomThread);
    function GetJob: TJob;
  public
    class procedure Init;
    constructor Create(FThreads: Integer = 5);
    destructor Destroy; override;
    procedure AddMapID(aID : Cardinal);
    procedure RemoveMapID(aID : Cardinal);
    procedure Add(AJob: TParallelProc; Semaphore: TSemaphore); overload;
    procedure Add(AJob: TJob); overload;
    procedure GenerateJobMap;
    procedure AddID(PID : Cardinal; AJob: TJob);
    procedure Execute;
    procedure Terminate;
    procedure WaitForJob(J : TJob);
    property Running: Boolean read FRunning write FRunning;
    property CriticalSection: TCriticalSection read FCS;
    property JobsCount: Integer read GetJobsCount;
    property ThreadsCount: Integer read FThreadsCount;
    property Job[index : integer] : TJob read GetJobAt;
  end;

var
  ThreadPool: TThreadPool;

implementation

const
  SLEEP_TIME = 30;

{ TSemaphoreJob }

constructor TSemaphoreJob.Create;
begin
  FSemaphore := nil;
end;

procedure TSemaphoreJob.Execute;
begin
  if Assigned(FParallelProc) then
  begin
    FParallelProc;
    if Assigned(FSemaphore) then
       FSemaphore.Deactivate;
  end;
end;

{ TIDJobPair }

constructor TIDJobPair.Create(aID: Cardinal);
begin
  InitCriticalSection(FRTI);
  FJobSeq := TFastSeq.Create;
  FID:= aID;
  FBusy := false;
end;

destructor TIDJobPair.Destroy;
begin
  FJobSeq.Free;
  DoneCriticalsection(FRTI);
  inherited Destroy;
end;

function TIDJobPair.isEmpty: Boolean;
begin
  Result := FJobSeq.Count = 0;
end;

function TIDJobPair.Pop: TJob;
var i : TIteratorObject;
begin
  if FBusy then Exit(nil);

  Lock;
  try
    i := FJobSeq.Pop();
  finally
    UnLock;
  end;
  if assigned(i) then begin
    Result := TJob(i.Value);
    TIDJob(Result).Activate;
    i.Value := nil;
    i.free;
  end else Result := nil;
end;

procedure TIDJobPair.Push(const J: TJob);
begin
  Lock;
  try
    if assigned(j) then FJobSeq.Push_back(TIDJob.Create(j, Self));
  finally
    UnLock;
  end;
end;

procedure TIDJobPair.Deactivate;
begin
  Lock;
  try
    FBusy := false;
  finally
    UnLock;
  end;
end;

procedure TIDJobPair.Activate;
begin
  Lock;
  try
    FBusy := true;
  finally
    UnLock;
  end;
end;

procedure TIDJobPair.Lock;
begin
  EnterCriticalsection(FRTI);
end;

procedure TIDJobPair.UnLock;
begin
  LeaveCriticalsection(FRTI);
end;

{ TIDJob }

constructor TIDJob.Create(guestJob : TJob; aPairOwner : TIDJobPair);
begin
  inherited Create;
  FPairOwner := aPairOwner;
  FGuestJob := guestJob;
end;

procedure TIDJob.Execute;
begin
  try
    FGuestJob.Execute;
  finally
    FPairOwner.Deactivate;  // early call for boosting
  end;
end;

procedure TIDJob.Activate;
begin
  FPairOwner.Activate;
end;

destructor TIDJob.Destroy;
begin
  FPairOwner.Deactivate; // agian call
  if Assigned(FGuestJob) then FGuestJob.Free;
end;

{ TIDJobMap }

function TIDJobMap.GetCount: Integer;
begin
  Result := FIds.Count;
end;

constructor TIDJobMap.Create;
begin
  InitCriticalSection(FRTI);
  FIds := TFastCollection.Create;
  FLastLoc := 0;
end;

destructor TIDJobMap.Destroy;
begin
  FIds.Free;
  DoneCriticalsection(FRTI);
  inherited Destroy;
end;

procedure TIDJobMap.AddID(aID: Cardinal);
begin
  Push(aID, nil);
end;

procedure TIDJobMap.RemoveID(aID: Cardinal);
var p : TIDJobPair; i : integer;
begin
  Lock;
  try
    for i := fIds.Count-1 downto 0 do
    begin
      p := TIDJobPair(fIds[i]);
      if p.ID = aID then
      begin
         FIds.Delete(i);
      end;
    end;
  finally
    UnLock;
  end;
end;

function TIDJobMap.GetJob: TJob;
var p : TIDJobPair; i : integer;
begin
  Lock;
  try
    Result := nil;
    i := FLastLoc;
    while i < fIds.Count do
    if not assigned(Result) then
    begin
      p := TIDJobPair(fIds[i]);
      Result := p.Pop;
      inc(i);
    end else Break;
    if Assigned(Result) then
    begin
      FLastLoc := i;
    end else
      if FLastLoc > 0 then
      begin
        FLastLoc := 0;
        i := 0;
        while i < fIds.Count do
        if not assigned(Result) then
        begin
          p := TIDJobPair(fIds[i]);
	  Result := p.Pop;
	  inc(i);
	end else begin
	  FLastLoc := i;
	  Break;
	end;
      end;
  finally
    UnLock;
  end;
end;

function TIDJobMap.GetPair(aID: Cardinal): TIDJobPair;
var i : integer;
begin
  Result := nil;
  for i := 0 to FIds.Count-1 do
  begin
    if TIDJobPair(FIds[i]).ID = aID then
    begin
      Exit(TIDJobPair(FIds[i]));
    end;
  end;
end;

function TIDJobMap.Pop(aID: Cardinal): TJob;
var p : TIDJobPair;
begin
  result := nil;
  Lock;
  try
    p := GetPair(aID);
  finally
    UnLock;
  end;
  if assigned(p) then
     result := p.Pop;
end;

procedure TIDJobMap.Push(aID: Cardinal; j: TJob);
var p : TIDJobPair;
begin
  Lock;
  try
    p := GetPair(aID);
    if not assigned(p) then
    begin
      p := TIDJobPair.Create(aID);
      FIds.Add(p);
    end;
    p.Push(j);
  finally
    UnLock;
  end;
end;

procedure TIDJobMap.Lock;
begin
  EnterCriticalsection(FRTI);
end;

procedure TIDJobMap.UnLock;
begin
  LeaveCriticalsection(FRTI);
end;


{ TThreadPool }

function TThreadPool.GetJob: TJob;
begin
  FCS.Enter;
  try
    Result := nil;
    if assigned(FJobMap) then
       Result := FJobMap.GetJob();
    if not assigned(Result) then
    begin
      if FList.Count > 0 then
      begin
        Result := TJob(FList[0]);
        FList.Delete(0);
      end
    end;
  finally
    FCS.Leave;
  end;
end;

function TThreadPool.GetJobAt(index : integer): TJob;
begin
  FCS.Enter;
  try
    Result := TJob(FList[index]);
  finally
    FCS.Leave;
  end;
end;

function TThreadPool.GetJobsCount: Integer;
begin
  Result := FList.Count;
end;

function TThreadPool.GetRunningThreads: Integer;
var
  i: Integer;
begin
  FCS.Enter;
  try
    Result := 0;
    for i := 0 to FPool.Count - 1 do
    if assigned(FPool[i]) then
      if TCustomThread(FPool[i]).Running then
        Inc(Result);
  finally
    FCS.Leave;
  end;
end;

procedure TThreadPool.ExcludeThread(aThread: TCustomThread);
var i : integer;
begin
  FCS.Enter;
  try
    for i := 0 to FPool.Count - 1 do
    begin
      if (FPool[i] = aThread) then FPool[i] := nil;
    end;
  finally
    FCS.Leave;
  end;
end;

class procedure TThreadPool.Init;
begin
  if not Assigned(ThreadPool) then
    ThreadPool := TThreadPool.Create;
end;

constructor TThreadPool.Create(FThreads: Integer = 5);
var
  i: Integer;
begin
  FPool := TFastList.Create;
  FCS   := TCriticalSection.Create;
  FList := TFastList.Create;
  FJobMap := nil;
  FThreadsCount := FThreads;

  FCS.Enter;
  try
    for i := 1 to FThreads do begin
      FPool.Add(TCustomThread.Create(Self));
      Sleep(SLEEP_TIME div FThreads);
    end;
  finally
    FCS.Leave;
  end;
end;

destructor TThreadPool.Destroy;
var
  i, cnt: Integer;
  c: TCustomThread;
  o: TObject;
begin
  cnt := 0;
  FCS.Enter;
  try
    FRunning := False;
    for i := 0 to FPool.Count - 1 do
    begin
      c := TCustomThread(FPool[i]);
      if Assigned(c) then
      begin
        c.Terminate;
        inc(cnt);
      end;
    end;
  finally
    FCS.Leave;
  end;

  while cnt > 0 do
  begin
    cnt := 0;
    for i := 0 to FPool.Count - 1 do
    begin
      FCS.Enter;
      try
        c := TCustomThread(FPool[i]);
      finally
        FCS.Leave;
      end;
      if Assigned(c) then
      begin
        inc(cnt);
      end;
    end;
    Sleep(1);
  end;

  FPool.Free;

  for i := 0 to FList.Count - 1 do
  begin
    o := FList[i];
    if Assigned(o) then
      o.Free;
  end;

  FList.Free;
  FCS.Free;

  if assigned(FJobMap) then
     FJobMap.Free;
  inherited Destroy;
end;

procedure TThreadPool.AddMapID(aID: Cardinal);
begin
  if assigned(FJobMap) then
  begin
    FJobMap.AddID(aID);
  end;
end;

procedure TThreadPool.RemoveMapID(aID: Cardinal);
begin
  if assigned(FJobMap) then
  begin
    FJobMap.RemoveID(aID);
  end;
end;

procedure TThreadPool.Add(AJob: TParallelProc; Semaphore: TSemaphore);
var
  j: TSemaphoreJob;
begin
  j := TSemaphoreJob.Create;
  j.ParallelProc := AJob;
  j.Owner := Self;
  j.FSemaphore := Semaphore;
  Semaphore.Activate;
  FCS.Enter;
  try
    FList.Add(j);
  finally
    FCS.Leave;
  end;
end;

procedure TThreadPool.Add(AJob: TJob);
begin
  FCS.Enter;
  try
    FList.Add(AJob);
  finally
    FCS.Leave;
  end;
end;

procedure TThreadPool.GenerateJobMap;
begin
  FJobMap := TIDJobMap.Create;
end;

procedure TThreadPool.AddID(PID: Cardinal; AJob: TJob);
begin
  FJobMap.Push(PID, aJob);
end;

procedure TThreadPool.Execute;
var
  i: Integer;
begin
  Running := True;
  try
    while (FList.Count > 0) do
      Sleep(SLEEP_TIME);

    i := 0;
    while GetRunningThreads > 0 do
    begin
      Sleep(SLEEP_TIME);
      Inc(i);
      if i = 500 then
        Break;
    end;
  finally
    Running := False;
  end;
end;

procedure TThreadPool.Terminate;
var
  i: Integer;
begin
  for i := 0 to FPool.Count - 1 do
  if assigned(FPool[i]) then
    TCustomThread(FPool[i]).Terminate;
end;

procedure TThreadPool.WaitForJob(J: TJob);
var
  i: Integer;
  c: TCustomThread;
begin
  FCS.Enter;
  try
    c := nil;
    for i := 0 to FPool.Count - 1 do
    if assigned(FPool[i]) then
    begin
      c := TCustomThread(FPool[i]);
      if (c.myJob = J) then Break else c := nil;
    end;
  finally
    FCS.Leave;
  end;
  if assigned(c) then
  begin
    while c.myJob = J do
    begin
      Sleep(1);
    end;
  end;
end;

{ TCustomThead }

procedure TCustomThread.Execute;
var
  j: TJob;
begin
  while not Terminated do
  begin
    if FOwner.Running then
    begin
      j := FOwner.GetJob;
      if Assigned(j) then
      begin
        FRunning := True;
        try
          myJob := j;
          j.Execute;
        finally
          FRunning := False;
          myJob := nil;
          j.Free;
        end;
      end
      else
        FRunning := False;
    end;
    Sleep(SLEEP_TIME);
  end;
end;

constructor TCustomThread.Create(AOwner: TThreadPool);
begin
  FOwner := AOwner;
  inherited Create(False);
  FreeOnTerminate := true;
end;

destructor TCustomThread.Destroy;
begin
  FOwner.ExcludeThread(Self);
  inherited Destroy;
end;

{ TSemaphore }

constructor TSemaphore.Create;
begin
  FEnabled := false;
end;

procedure TSemaphore.Activate;
begin
  FEnabled := true;
end;

procedure TSemaphore.Deactivate;
begin
  FEnabled := false;
end;

function TSemaphore.Active: Boolean;
begin
  Result := FEnabled;
end;

finalization
  if Assigned(ThreadPool) then
  begin
     ThreadPool.Terminate;
     ThreadPool.Free;
  end;

end.

