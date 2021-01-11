{
 SortedThreadPool
   Modul fo async execution of clients jobs with ranking
   Based on
   Copyright (C) 2011 Maciej Kaczkowski / keit.co

   Added jobs ranking
   Copyright (C) 2020 Ilya Medvedkov

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


unit SortedThreadPool;

{$mode objfpc}{$H+}

interface

uses
  AvgLvlTree, OGLFastList, Classes, SysUtils, syncobjs;

type
  TSortedThreadPool = class;

  { TSortedJob }

  TSortedJob = class
  private
    FScore : Cardinal;
  public
    constructor Create(aScore : Cardinal); virtual;
    procedure Execute; virtual; abstract;
    property Score : Cardinal read FScore;
  end;

  { TSortedCustomThread }

  TSortedCustomThread = class(TThread)
  private
    FOwner: TSortedThreadPool;
    FRunning: Boolean;
    FJob : TSortedJob;
  public
    procedure Execute; override;
    constructor Create(AOwner: TSortedThreadPool);
    destructor Destroy; override;
    property Running: Boolean read FRunning;
    property myJob : TSortedJob read FJob write FJob;
  end;

  { TThreadPool }

  TSortedThreadPool = class
  private
    FPool: TFastList;
    FList: TAvgLvlTree;
    FCS: TCriticalSection;
    FRunning: Boolean;
    function GetJobsCount: Integer;
    function GetRunningThreads: Integer;
    procedure ExcludeThread(aThread : TSortedCustomThread);
    function GetJob: TSortedJob;
  public
    constructor Create(const OnCompareMethod: TAvgLvlObjectSortCompare; FThreads: Integer = 5);
    destructor Destroy; override;
    procedure Add(AJob: TSortedJob);
    procedure Execute;
    procedure Terminate;
    property Running: Boolean read FRunning write FRunning;
    property CriticalSection: TCriticalSection read FCS;
    property JobsCount: Integer read GetJobsCount;
  end;

implementation

const
  SORTED_SLEEP_TIME = 30;

{ TSortedJob }

constructor TSortedJob.Create(aScore: Cardinal);
begin
  FScore:= aScore;
end;

{ TSortedThreadPool }

function TSortedThreadPool.GetJob: TSortedJob;
var JN : TAvgLvlTreeNode;
begin
  FCS.Enter;
  try
    Result := nil;
    if not assigned(Result) then
    begin
      if FList.Count > 0 then
      begin
        JN := FList.FindLowest;
        Result := TSortedJob(JN.Data);
        FList.Delete(JN);
      end
    end;
  finally
    FCS.Leave;
  end;
end;

function TSortedThreadPool.GetJobsCount: Integer;
begin
  Result := FList.Count;
end;

function TSortedThreadPool.GetRunningThreads: Integer;
var
  i: Integer;
begin
  FCS.Enter;
  try
    Result := 0;
    for i := 0 to FPool.Count - 1 do
    if assigned(FPool[i]) then
      if TSortedCustomThread(FPool[i]).Running then
        Inc(Result);
  finally
    FCS.Leave;
  end;
end;

procedure TSortedThreadPool.ExcludeThread(aThread: TSortedCustomThread);
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

constructor TSortedThreadPool.Create(const OnCompareMethod: TAvgLvlObjectSortCompare;
                                           FThreads: Integer = 5);
var
  i: Integer;
begin
  FPool := TFastList.Create;
  FCS   := TCriticalSection.Create;
  FList := TAvgLvlTree.CreateObjectCompare(OnCompareMethod);
  FList.OwnsObjects:=false;

  FCS.Enter;
  try
    for i := 1 to FThreads do
      FPool.Add(TSortedCustomThread.Create(Self));
  finally
    FCS.Leave;
  end;
end;

destructor TSortedThreadPool.Destroy;
var
  i, cnt: Integer;
  c: TSortedCustomThread;
begin
  cnt := 0;
  FCS.Enter;
  try
    FRunning := False;
    for i := 0 to FPool.Count - 1 do
    begin
      c := TSortedCustomThread(FPool[i]);
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
        c := TSortedCustomThread(FPool[i]);
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

  FList.OwnsObjects:=true;
  FList.Free;
  FCS.Free;

  inherited Destroy;
end;

procedure TSortedThreadPool.Add(AJob: TSortedJob);
begin
  FCS.Enter;
  try
    FList.Add(AJob);
  finally
    FCS.Leave;
  end;
end;

procedure TSortedThreadPool.Execute;
var
  i: Integer;
begin
  Running := True;
  try
    while (FList.Count > 0) do
      Sleep(SORTED_SLEEP_TIME);

    i := 0;
    while GetRunningThreads > 0 do
    begin
      Sleep(SORTED_SLEEP_TIME);
      Inc(i);
      if i = 500 then
        Break;
    end;
  finally
    Running := False;
  end;
end;

procedure TSortedThreadPool.Terminate;
var
  i: Integer;
begin
  for i := 0 to FPool.Count - 1 do
  if assigned(FPool[i]) then
    TSortedCustomThread(FPool[i]).Terminate;
end;

{ TSortedCustomThread }

procedure TSortedCustomThread.Execute;
var
  j: TSortedJob;
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
          try
            j.Execute;
          except
            //Raise;
          end;
        finally
          FRunning := False;
          myJob := nil;
          j.Free;
        end;
      end
      else
        FRunning := False;
    end;
    Sleep(SORTED_SLEEP_TIME);
  end;
end;

constructor TSortedCustomThread.Create(AOwner: TSortedThreadPool);
begin
  FOwner := AOwner;
  inherited Create(False);
  FreeOnTerminate := true;
end;

destructor TSortedCustomThread.Destroy;
begin
  FOwner.ExcludeThread(Self);
  inherited Destroy;
end;

end.

