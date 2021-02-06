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
  AvgLvlTree, ECommonObjs, Classes, SysUtils;

type
  TSortedThreadPool = class;

  TLinearJob = class
  public
    procedure Execute; virtual; abstract;
  end;

  { TSortedJob }

  TSortedJob = class(TLinearJob)
  private
    FScore : Cardinal;
  public
    constructor Create(aScore : Cardinal); virtual;
    property Score : Cardinal read FScore;
  end;

  TPoolThreadKind = (ptkLinear, ptkSorted);

  { TSortedCustomThread }

  TSortedCustomThread = class(TThread)
  private
    FOwner: TSortedThreadPool;
    FRunning: TThreadBoolean;
    FJob : TLinearJob;
    FThreadKind : TPoolThreadKind;
    function GetRunning: Boolean;
  public
    procedure Execute; override;
    constructor Create(AOwner: TSortedThreadPool; AKind : TPoolThreadKind);
    destructor Destroy; override;
    property Running: Boolean read GetRunning;
    property myJob : TLinearJob read FJob write FJob;
  end;

  { TThreadPool }

  { TSortedThreadPool }

  TSortedThreadPool = class
  private
    FPool: TThreadSafeFastList;
    FSortedList: TAvgLvlTree;
    FSortedListLocker : TNetCustomLockedObject;
    FLinearList : TThreadSafeFastSeq;
    FSortedThreadsCount : TThreadInteger;
    FLinearThreadsCount : TThreadInteger;
    FRunning: TThreadBoolean;
    function GetLinearJobsCount: Integer;
    function GetLinearThreadsCount: integer;
    function GetRunning: Boolean;
    function GetRunningThreads: Integer;
    procedure ExcludeThread(aThread : TSortedCustomThread);
    function GetSortedJob: TSortedJob;
    function GetLinearJob : TLinearJob;
    function GetSortedJobsCount: Integer;
    function GetSortedThreadsCount: integer;
    function GetThreadsCount: integer;
    procedure ResizePool(aSortedThreads: Integer;
                         aLinearThreads: Integer);
    procedure SetLinearThreadsCount(AValue: integer);
    procedure SetRunning(AValue: Boolean);
    procedure SetSortedThreadsCount(AValue: integer);
  public
    constructor Create(const OnCompareMethod: TAvgLvlObjectSortCompare;
                                               aSortedThreads: Integer = 5;
                                               aLinearThreads: Integer = 5);
    destructor Destroy; override;
    procedure AddSorted(AJob : TSortedJob);
    procedure AddLinear(AJob : TLinearJob);
    procedure Terminate;
    property Running: Boolean read GetRunning write SetRunning;
    property SortedJobsCount: Integer read GetSortedJobsCount;
    property LinearJobsCount: Integer read GetLinearJobsCount;
    //
    property ThreadsCount : integer read GetThreadsCount;
    property SortedThreadsCount : integer read GetSortedThreadsCount
                                          write SetSortedThreadsCount;
    property LinearThreadsCount : integer read GetLinearThreadsCount
                                          write SetLinearThreadsCount;
  end;

implementation

const
  SORTED_SLEEP_TIME = 20;

{ TSortedJob }

constructor TSortedJob.Create(aScore: Cardinal);
begin
  FScore:= aScore;
end;

{ TSortedThreadPool }

function TSortedThreadPool.GetSortedJob: TSortedJob;
var JN : TAvgLvlTreeNode;
begin
  FSortedListLocker.Lock;
  try
    if FSortedList.Count > 0 then
    begin
      JN := FSortedList.FindLowest;
      Result := TSortedJob(JN.Data);
      FSortedList.Delete(JN);
    end else Result := nil;
  finally
    FSortedListLocker.UnLock;
  end;
end;

function TSortedThreadPool.GetLinearJob: TLinearJob;
begin
  Result := TLinearJob(FLinearList.PopValue);
end;

function TSortedThreadPool.GetSortedJobsCount: Integer;
begin
  FSortedListLocker.Lock;
  try
    Result := FSortedList.Count;
  finally
    FSortedListLocker.UnLock;
  end;
end;

function TSortedThreadPool.GetSortedThreadsCount: integer;
begin
  Result := FSortedThreadsCount.Value;
end;

function TSortedThreadPool.GetThreadsCount: integer;
begin
  Result := SortedThreadsCount + LinearThreadsCount;
end;

procedure TSortedThreadPool.ResizePool(aSortedThreads: Integer;
  aLinearThreads: Integer);
begin
  //TODO: resize pool here
end;

procedure TSortedThreadPool.SetLinearThreadsCount(AValue: integer);
begin
  ResizePool(FSortedThreadsCount.Value, AValue);
end;

procedure TSortedThreadPool.SetRunning(AValue: Boolean);
begin
  FRunning.Value:= AValue;
end;

procedure TSortedThreadPool.SetSortedThreadsCount(AValue: integer);
begin
  ResizePool(AValue, FLinearThreadsCount.Value);
end;

function TSortedThreadPool.GetLinearJobsCount: Integer;
begin
  Result := FLinearList.Count;
end;

function TSortedThreadPool.GetLinearThreadsCount: integer;
begin
  Result := FLinearThreadsCount.Value;
end;

function TSortedThreadPool.GetRunning: Boolean;
begin
  Result := FRunning.Value;
end;

function TSortedThreadPool.GetRunningThreads: Integer;
var
  i: Integer;
begin
  FPool.Lock;
  try
    Result := 0;
    for i := 0 to FPool.Count - 1 do
    if assigned(FPool[i]) then
      if TSortedCustomThread(FPool[i]).Running then
        Inc(Result);
  finally
    FPool.UnLock;
  end;
end;

procedure TSortedThreadPool.ExcludeThread(aThread: TSortedCustomThread);
var i : integer;
begin
  FPool.Lock;
  try
    for i := 0 to FPool.Count - 1 do
    begin
      if (FPool[i] = aThread) then FPool[i] := nil;
    end;
  finally
    FPool.UnLock;
  end;
end;

constructor TSortedThreadPool.Create(
  const OnCompareMethod: TAvgLvlObjectSortCompare; aSortedThreads: Integer;
  aLinearThreads: Integer);
var
  i, FThreads: Integer;
begin
  FPool:= TThreadSafeFastList.Create;
  FSortedList := TAvgLvlTree.CreateObjectCompare(OnCompareMethod);
  FSortedList.OwnsObjects:=false;
  FSortedListLocker := TNetCustomLockedObject.Create;
  FLinearList := TThreadSafeFastSeq.Create;
  FSortedThreadsCount := TThreadInteger.Create(aSortedThreads);
  FLinearThreadsCount := TThreadInteger.Create(aLinearThreads);
  FRunning:= TThreadBoolean.Create(false);
  FThreads := aSortedThreads + aLinearThreads;

  FPool.Lock;
  try
    for i := 1 to FThreads do begin
      if i <= aLinearThreads then
       FPool.Add(TSortedCustomThread.Create(Self, ptkLinear)) else
       FPool.Add(TSortedCustomThread.Create(Self, ptkSorted));
      Sleep(SORTED_SLEEP_TIME div FThreads);
    end;
  finally
    FPool.UnLock;
  end;
end;

destructor TSortedThreadPool.Destroy;
var
  i, cnt: Integer;
  c: TSortedCustomThread;
begin
  FRunning.Value := False;
  cnt := 0;

  FPool.Lock;
  try
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
    FPool.Unlock;
  end;

  while cnt > 0 do
  begin
    cnt := 0;
    for i := 0 to FPool.Count - 1 do
    begin
      FPool.Lock;
      try
        c := TSortedCustomThread(FPool[i]);
      finally
        FPool.UnLock;
      end;
      if Assigned(c) then
      begin
        inc(cnt);
      end;
    end;
    Sleep(1);
  end;

  FPool.Free;

  FSortedList.OwnsObjects:=true;
  FSortedList.Free;

  FLinearList.Free;

  FSortedListLocker.Free;
  FRunning.Free;
  FLinearThreadsCount.Free;
  FSortedThreadsCount.Free;

  inherited Destroy;
end;

procedure TSortedThreadPool.AddSorted(AJob: TSortedJob);
begin
  FSortedListLocker.Lock;
  try
    FSortedList.Add(AJob);
  finally
    FSortedListLocker.UnLock;
  end;
end;

procedure TSortedThreadPool.AddLinear(AJob: TLinearJob);
begin
  FLinearList.Push_back(AJob);
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

function TSortedCustomThread.GetRunning: Boolean;
begin
  if not assigned(FRunning) then Result := false else
  Result := FRunning.Value;
end;

procedure TSortedCustomThread.Execute;
var
  j: TLinearJob;
begin
  while not Terminated do
  begin
    if FOwner.Running then
    begin
      case FThreadKind of
        ptkLinear:  j := FOwner.GetLinearJob;
        ptkSorted:  j := FOwner.GetSortedJob;
      else
        j := nil;
      end;
      if Assigned(j) then
      begin
        FRunning.Value := True;
        try
          myJob := j;
          try
            j.Execute;
          except
            //Raise;
          end;
        finally
          FRunning.Value := False;
          myJob := nil;
          j.Free;
        end;
      end
      else
        FRunning.Value := False;
    end;
    Sleep(SORTED_SLEEP_TIME);
  end;
end;

constructor TSortedCustomThread.Create(AOwner: TSortedThreadPool;
  AKind: TPoolThreadKind);
begin
  FOwner := AOwner;
  FThreadKind:= AKind;
  FRunning := TThreadBoolean.Create(False);
  inherited Create(False);
  FreeOnTerminate := true;
end;

destructor TSortedCustomThread.Destroy;
begin
  FOwner.ExcludeThread(Self);
  FreeAndNil(FRunning);
  inherited Destroy;
end;

end.

