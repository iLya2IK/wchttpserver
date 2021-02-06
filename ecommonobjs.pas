{
 ECommonObjs:
   Thread safe helpers to access to objects and data

   Part of ESolver project

   Copyright (c) 2019-2021 by Ilya Medvedkov

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit ECommonObjs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, OGLFastList;

type
  { TNetCustomLockedObject }

  TNetCustomLockedObject = class
  private
    FRTI: TRTLCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Lock;
    procedure UnLock;
  end;

  { TNetReferencedObject }

  TNetReferencedObject = class(TNetCustomLockedObject)
  private
    mReferenceCount : Integer;
  public
    constructor Create;
    procedure IncReference;
    procedure DecReference;
    function HasReferences : Boolean;
    function HasExternReferences : Boolean;
  end;

  { TNetReferenceList }

  TNetReferenceList = class(TNetCustomLockedObject)
  private
    FList : TFastSeq;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(const O : TNetReferencedObject);
    procedure CleanDead;
  end;

  TNetReferenceOnDisconnect = procedure (v : TNetReferencedObject) of object;

  { TNetReferenceHolderList }

  TNetReferenceHolderList = class(TNetCustomLockedObject)
  private
    FList : TFastList;
    FOnDisconnect :  TNetReferenceOnDisconnect;
    function GetCount: integer;
    function GetItem(index : integer): TNetReferencedObject;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Add(Obj : TNetReferencedObject);
    procedure Remove(Obj : TNetReferencedObject);
    procedure Delete(i : integer);
    function IndexOf(Obj : TNetReferencedObject) : integer;
    property Count : integer read GetCount;
    property OnDisconnect : TNetReferenceOnDisconnect read FOnDisconnect write FOnDisconnect;
    property Item[index : integer] : TNetReferencedObject read GetItem; default;
  end;

  THandlesList = class;

  { TNetHandle }

  TNetHandle = class(TNetReferencedObject)
  private
   fHandle : Cardinal;
   fOwner  : THandlesList;
  public
   procedure Disconnect;
   property Handle : Cardinal read FHandle write FHandle;
   property Owner : THandlesList read fOwner write fOwner;
  end;

  { THandlesList }

  THandlesList = class(TNetCustomLockedObject)
  private
    FList : TNetReferenceHolderList;
    function GetCount: integer;
    function GetItem(index : integer): TNetHandle;
  public
    constructor Create;
    destructor Destroy; override;

    function  GetByHandle(aHandle : Cardinal) : TObject;
    procedure RemoveHandle(obj : TNetHandle); virtual;
    procedure AddHandle(Obj : TNetHandle); virtual;
    procedure Clear; virtual;

    property Count : integer read GetCount;
    property Item[index : integer] : TNetHandle read GetItem; default;
  end;

  { TThreadSafeObject }

  TThreadSafeObject = class(TNetCustomLockedObject)
  end;

  { TThreadInteger }

  TThreadInteger = class(TThreadSafeObject)
  private
    FValue : Integer;
    function GetValue: Integer;
    procedure SetValue(AValue: Integer);
  public
    constructor Create(AValue : Integer);
    procedure IncValue;
    procedure DecValue;
    property Value : Integer read GetValue write SetValue;
  end;

  { TThreadQWord }

  TThreadQWord = class(TThreadSafeObject)
  private
    FValue : QWord;
    function GetValue: QWord;
    procedure SetValue(AValue: QWord);
  public
    constructor Create(AValue : QWord);
    property Value : QWord read GetValue write SetValue;
  end;

  { TThreadPointer }

  TThreadPointer = class(TThreadSafeObject)
  private
    FValue : Pointer;
    function GetValue: Pointer;
  public
    constructor Create(ASize : QWord);
    destructor Destroy; override;
    property Value : Pointer read GetValue;
  end;

  { TThreadUtf8String }

  TThreadUtf8String = class(TThreadSafeObject)
  private
    FValue : Utf8String;
    function GetValue: UTF8String;
    procedure SetValue(AValue: UTF8String);
  public
    constructor Create(AValue : Utf8String);
    property Value : UTF8String read GetValue write SetValue;
  end;

  { TThreadBoolean }

  TThreadBoolean = class(TThreadSafeObject)
  private
    FValue : Boolean;
    function GetValue: Boolean;
    procedure SetValue(AValue: Boolean);
  public
    constructor Create(AValue : Boolean);
    property Value : Boolean read GetValue write SetValue;
  end;

  { TThreadWideString }

  TThreadWideString = class(TThreadSafeObject)
  private
    FValue : WideString;
    function GetValue: WideString;
    procedure SetValue(AValue: WideString);
  public
    constructor Create(AValue : WideString);
    property Value : WideString read GetValue write SetValue;
  end;

  { TThreadSafeAutoIncrementCardinal }

  TThreadSafeAutoIncrementCardinal = class(TNetCustomLockedObject)
  private
    FValue : Cardinal;
    function GetID: Cardinal;
  public
    constructor Create;
    procedure Reset;
    procedure SetValue(v : Cardinal);
    property ID : Cardinal read GetID;
  end;

  { TThreadStringList }

  TThreadStringList = class(TThreadSafeObject)
  private
    FStringList: TStringList;
    FOnChange : TNotifyEvent;
    FUpdates  : Integer;

    function GetCount: Integer;
    function GetStr(index : integer): String;
    function GetText: String;
    procedure SetStr(index : integer; AValue: String);
    procedure SetText(AValue: String);
    procedure DoChange;
    function GetOnChange: TNotifyEvent;
    procedure SetOnChange(AValue: TNotifyEvent);
  public
    constructor Create;
    destructor Destroy; override;
    function Add(const S: string): Integer;
    function IndexOf(const S: string): Integer;
    procedure AddStrings(S : TStringList);
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure Clear;
    property Item[index : integer] : String read GetStr write SetStr; default;
    property Count : Integer read GetCount;
    property Text : String read GetText write SetText;
    property OnChange : TNotifyEvent read GetOnChange write SetOnChange;
  end;

  { TThreadSafeFastCollection }

  TThreadSafeFastCollection = class(TThreadSafeObject)
  private
    FCollection :  TFastCollection;

    function GetCount: integer;
    function GetObject(index : integer): TObject;
    procedure SetObject(index : integer; AValue: TObject);
  protected
    procedure SetCount(AValue: integer); virtual;
  public
    constructor Create;
    function Add(const Obj : TObject) : Integer; virtual;
    function IndexOf(const Obj : TObject) : Integer;
    function Remove(const obj: TObject) : integer; virtual;
    procedure Delete(Ind : integer); virtual;
    procedure Clear; virtual;
    procedure Extract(Ind : integer); virtual;
    procedure Pack; virtual;
    destructor Destroy; override;
    procedure SortList(func : TObjectSortFunction);

    property Count : integer read GetCount write SetCount;
    property Item[index : integer] : TObject read GetObject write SetObject; default;
  end;

  { TThreadSafeFastList }

  TThreadSafeFastList = class(TThreadSafeObject)
  private
    FList :  TFastList;

    function GetCount: integer;
    function GetObject(index : integer): TObject;
    procedure SetObject(index : integer; AValue: TObject);
  protected
    procedure SetCount(AValue: integer); virtual;
  public
    constructor Create;
    function Add(const Obj : TObject) : Integer; virtual;
    function IndexOf(const Obj : TObject) : Integer;
    function Remove(const obj: TObject) : integer; virtual;
    procedure Delete(Ind : integer); virtual;
    procedure Clear; virtual;
    procedure Extract(Ind : integer); virtual;
    procedure Pack; virtual;
    destructor Destroy; override;
    procedure SortList(func : TObjectSortFunction);

    property Count : integer read GetCount write SetCount;
    property Item[index : integer] : TObject read GetObject write SetObject; default;
  end;

  TThreadSafeCriteria = function (obj : TObject; data : pointer) : Boolean of object;
  TThreadSafeAction   = procedure (obj : TObject) of object;

  { TThreadSafeFastSeq }

  TThreadSafeFastSeq = class(TThreadSafeObject)
  private
    FSeq : TFastSeq;
    function GetCount: integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clean;
    procedure ExtractAll;
    procedure Push_back(const O : TObject);
    function Pop : TIteratorObject;
    function PopValue : TObject;
    procedure Erase(const loc : TIteratorObject);
    procedure EraseObject(const obj : TObject);
    function  EraseObjectsByCriteria(criteria: TThreadSafeCriteria;
                                               data : pointer): Boolean;
    procedure DoForAll(action: TThreadSafeAction);
    procedure Extract(const loc: TIteratorObject);
    procedure ExtractObject(const obj: TObject);
    function  ExtractObjectsByCriteria(criteria: TThreadSafeCriteria;
                                       afterextract : TThreadSafeAction;
                                               data : pointer): Boolean;
    function ListBegin : TIteratorObject;
    function IteratorBegin : TIterator;
    class function ListEnd   : TIteratorObject;

    property Count : integer read GetCount;
  end;

  { TReferencedStream }

  TReferencedStream = class(TNetReferencedObject)
  private
    FStream : TStream;
  public
    constructor Create(aStrm : TStream);
    destructor Destroy; override;
    procedure WriteTo(Strm : TStream; from, Sz : Int64); virtual;
    property Stream : TStream read FStream;
  end;

  { TRefMemoryStream }

  TRefMemoryStream = class(TReferencedStream)
  public
    constructor Create;
    procedure WriteTo(Strm : TStream; from, Sz : Int64); override;
  end;

implementation

{ TThreadSafeFastList }

function TThreadSafeFastList.GetCount: integer;
begin
  Lock;
  try
    Result := FList.Count;
  finally
    UnLock;
  end;
end;

function TThreadSafeFastList.GetObject(index: integer): TObject;
begin
  Lock;
  try
    Result := FList[index];
  finally
    UnLock;
  end;
end;

procedure TThreadSafeFastList.SetObject(index: integer; AValue: TObject);
begin
  Lock;
  try
    FList[index] := AValue;
  finally
    UnLock;
  end;
end;

procedure TThreadSafeFastList.SetCount(AValue: integer);
begin
  Lock;
  try
    FList.Count := AValue;
  finally
    UnLock;
  end;
end;

constructor TThreadSafeFastList.Create;
begin
  Inherited Create;
  FList := TFastList.Create;
end;

function TThreadSafeFastList.Add(const Obj: TObject): Integer;
begin
  Lock;
  try
    FList.Add(Obj);
  finally
    UnLock;
  end;
end;

function TThreadSafeFastList.IndexOf(const Obj: TObject): Integer;
begin
  Lock;
  try
    Result := FList.IndexOf(Obj);
  finally
    UnLock;
  end;
end;

function TThreadSafeFastList.Remove(const obj: TObject): integer;
begin
  Lock;
  try
    Result := FList.Remove(Obj);
  finally
    UnLock;
  end;
end;

procedure TThreadSafeFastList.Delete(Ind: integer);
begin
  Lock;
  try
    FList.Delete(Ind);
  finally
    UnLock;
  end;
end;

procedure TThreadSafeFastList.Clear;
begin
  Lock;
  try
    FList.Clear;
  finally
    UnLock;
  end;
end;

procedure TThreadSafeFastList.Extract(Ind: integer);
begin
  Lock;
  try
    FList.Extract(Ind);
  finally
    UnLock;
  end;
end;

procedure TThreadSafeFastList.Pack;
begin
  Lock;
  try
    FList.Pack;
  finally
    UnLock;
  end;
end;

destructor TThreadSafeFastList.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

procedure TThreadSafeFastList.SortList(func: TObjectSortFunction);
begin
  Lock;
  try
    FList.SortList(func);
  finally
    UnLock;
  end;
end;

{ TRefMemoryStream }

constructor TRefMemoryStream.Create;
begin
  inherited Create(TMemoryStream.Create);
end;

procedure TRefMemoryStream.WriteTo(Strm: TStream; from, Sz: Int64);
begin
  Strm.Write(PByte(TMemoryStream(Stream).Memory)[from], Sz);
end;

{ TReferencedStream }

constructor TReferencedStream.Create(aStrm: TStream);
begin
  inherited Create;
  FStream := aStrm;
end;

destructor TReferencedStream.Destroy;
begin
  FStream.Free;
  inherited Destroy;
end;

procedure TReferencedStream.WriteTo(Strm: TStream; from, Sz: Int64);
begin
  Lock;
  try
    FStream.Position := from;
    Strm.CopyFrom(FStream, Sz);
  finally
    UnLock;
  end;
end;

{ TThreadPointer }

function TThreadPointer.GetValue: Pointer;
begin
  Lock;
  try
    Result := FValue;
  finally
    UnLock;
  end;
end;

constructor TThreadPointer.Create(ASize: QWord);
begin
  Inherited Create;
  FValue:= GetMem(ASize);
end;

destructor TThreadPointer.Destroy;
begin
  FreeMem(FValue);
  inherited Destroy;
end;

{ TThreadQWord }

function TThreadQWord.GetValue: QWord;
begin
  Lock;
  try
    Result := FValue;
  finally
    UnLock;
  end;
end;

procedure TThreadQWord.SetValue(AValue: QWord);
begin
  Lock;
  try
    FValue := AValue;
  finally
    UnLock;
  end;
end;

constructor TThreadQWord.Create(AValue: QWord);
begin
  inherited Create;
  FValue:= AValue;
end;

{ TNetHandle }

procedure TNetHandle.Disconnect;
begin
  DecReference;
end;

{ THandlesList }

function THandlesList.GetCount: integer;
begin
  Result := FList.Count;
end;

function THandlesList.GetItem(index : integer): TNetHandle;
begin
  Result := TNetHandle(FList[index]);
end;

constructor THandlesList.Create;
begin
  inherited Create;
  FList := TNetReferenceHolderList.Create;
end;

destructor THandlesList.Destroy;
begin
  Clear;
  FList.Free;
  inherited Destroy;
end;

function THandlesList.GetByHandle(aHandle: Cardinal): TObject;
var
  i : integer;
begin
  Result := nil;
  FList.Lock;
  try
    for i := 0 to FList.Count-1 do
    begin
      if TNetHandle(FList[i]).Handle = aHandle then Exit(FList[i]);
    end;
  finally
    FList.UnLock;
  end;
end;

procedure THandlesList.RemoveHandle(obj: TNetHandle);
begin
  FList.Remove(obj);
end;

procedure THandlesList.AddHandle(Obj: TNetHandle);
begin
  FList.Add(obj);
end;

procedure THandlesList.Clear;
begin
  FList.Clear;
end;

{ TThreadWideString }

function TThreadWideString.GetValue: WideString;
begin
  Lock;
  try
    Result := FValue;
  finally
    UnLock;
  end;
end;

procedure TThreadWideString.SetValue(AValue: WideString);
begin
  Lock;
  try
    FValue := AValue;
  finally
    UnLock;
  end;
end;

constructor TThreadWideString.Create(AValue: WideString);
begin
  inherited Create;
  FValue := AValue;
end;

{ TThreadSafeFastCollection }

function TThreadSafeFastCollection.GetCount: integer;
begin
  Lock;
  try
    Result := FCollection.Count;
  finally
    UnLock;
  end;
end;

function TThreadSafeFastCollection.GetObject(index : integer): TObject;
begin
  Lock;
  try
    Result := FCollection[index];
  finally
    UnLock;
  end;
end;

procedure TThreadSafeFastCollection.SetCount(AValue: integer);
begin
  Lock;
  try
    FCollection.Count:=AValue;
  finally
    UnLock;
  end;
end;

procedure TThreadSafeFastCollection.SetObject(index: integer; AValue: TObject);
begin
  Lock;
  try
    FCollection[index] := AValue;
  finally
    UnLock;
  end;
end;

constructor TThreadSafeFastCollection.Create;
begin
  inherited Create;
  FCollection := TFastCollection.Create;
end;

function TThreadSafeFastCollection.Add(const Obj: TObject): Integer;
begin
  Lock;
  try
    Result := FCollection.Add(Obj);
  finally
    UnLock;
  end;
end;

function TThreadSafeFastCollection.IndexOf(const Obj: TObject): Integer;
begin
  Lock;
  try
    Result := FCollection.IndexOf(Obj);
  finally
    UnLock;
  end;
end;

function TThreadSafeFastCollection.Remove(const obj: TObject): integer;
begin
  Lock;
  try
    Result := FCollection.Remove(obj);
  finally
    UnLock;
  end;
end;

procedure TThreadSafeFastCollection.Delete(Ind: integer);
begin
  Lock;
  try
    FCollection.Delete(Ind);
  finally
    UnLock;
  end;
end;

procedure TThreadSafeFastCollection.Clear;
begin
  Lock;
  try
    FCollection.Clear;
  finally
    UnLock;
  end;
end;

procedure TThreadSafeFastCollection.Extract(Ind: integer);
begin
  Lock;
  try
    FCollection.Extract(ind);
  finally
    UnLock;
  end;
end;

procedure TThreadSafeFastCollection.Pack;
begin
  Lock;
  try
    FCollection.Pack;
  finally
    UnLock;
  end;
end;

destructor TThreadSafeFastCollection.Destroy;
begin
  FCollection.Free;
  inherited Destroy;
end;

procedure TThreadSafeFastCollection.SortList(func: TObjectSortFunction);
begin
  Lock;
  try
    FCollection.SortList(func);
  finally
    UnLock;
  end;
end;

{ TNetReferenceHolderList }

function TNetReferenceHolderList.GetItem(index : integer): TNetReferencedObject;
begin
  Lock;
  try
    Result := TNetReferencedObject(FList[index]);
  finally
    UnLock;
  end;
end;

function TNetReferenceHolderList.GetCount: integer;
begin
  Lock;
  try
    Result := FList.Count;
  finally
    UnLock;
  end;
end;

constructor TNetReferenceHolderList.Create;
begin
  inherited Create;
  FList := TFastList.Create;
end;

destructor TNetReferenceHolderList.Destroy;
begin
  Clear;
  FList.Free;
  inherited Destroy;
end;

procedure TNetReferenceHolderList.Clear;
var
  i : integer;
begin
  Lock;
  try
    for i := 0 to FList.Count-1 do
    begin
      if assigned(FOnDisconnect) then FOnDisconnect(TNetReferencedObject(FList[i]));
      TNetReferencedObject(FList[i]).DecReference;
      FList[i] := nil;
    end;
    FList.Clear;
  finally
    UnLock;
  end;
end;

procedure TNetReferenceHolderList.Add(Obj: TNetReferencedObject);
begin
  Lock;
  try
    FList.Add(Obj);
  finally
    UnLock;
  end;
end;

procedure TNetReferenceHolderList.Remove(Obj: TNetReferencedObject);
var i : integer;
begin
  Lock;
  try
    i := FList.IndexOf(Obj);
    if i >= 0 then
    begin
      if assigned(FOnDisconnect) then FOnDisconnect(TNetReferencedObject(FList[i]));
      TNetReferencedObject(FList[i]).DecReference;
      FList.Delete(i);
    end;
  finally
    UnLock;
  end;
end;

procedure TNetReferenceHolderList.Delete(i: integer);
begin
  Lock;
  try
    if assigned(FOnDisconnect) then FOnDisconnect(TNetReferencedObject(FList[i]));
    TNetReferencedObject(FList[i]).DecReference;
    FList.Delete(i);
  finally
    UnLock;
  end;
end;

function TNetReferenceHolderList.IndexOf(Obj: TNetReferencedObject): integer;
begin
  Lock;
  try
    Result := Flist.IndexOf(Obj);
  finally
    UnLock;
  end;
end;

{ TThreadSafeFastSeq }

function TThreadSafeFastSeq.GetCount: integer;
begin
  Lock;
  try
    Result := FSeq.Count;
  finally
    UnLock;
  end;
end;

constructor TThreadSafeFastSeq.Create;
begin
  inherited Create;
  fSeq := TFastSeq.Create;
end;

destructor TThreadSafeFastSeq.Destroy;
begin
  fseq.free;
  inherited Destroy;
end;

procedure TThreadSafeFastSeq.Clean;
begin
  Lock;
  try
    fSeq.Clean;
  finally
    UnLock;
  end;
end;

procedure TThreadSafeFastSeq.ExtractAll;
begin
  Lock;
  try
    FSeq.ExtractAll;
  finally
    UnLock;
  end;
end;

procedure TThreadSafeFastSeq.Push_back(const O: TObject);
begin
  Lock;
  try
    FSeq.Push_back(O);
  finally
    UnLock;
  end;
end;

function TThreadSafeFastSeq.Pop: TIteratorObject;
begin
  Lock;
  try
    Result := FSeq.Pop;
  finally
    UnLock;
  end;
end;

function TThreadSafeFastSeq.PopValue: TObject;
begin
  Lock;
  try
    Result := FSeq.PopValue;
  finally
    UnLock;
  end;
end;

procedure TThreadSafeFastSeq.Erase(const loc: TIteratorObject);
begin
  Lock;
  try
    FSeq.Erase(loc);
  finally
    UnLock;
  end;
end;

procedure TThreadSafeFastSeq.EraseObject(const obj: TObject);
begin
  Lock;
  try
    FSeq.EraseObject(obj);
  finally
    UnLock;
  end;
end;

function TThreadSafeFastSeq.EraseObjectsByCriteria(
  criteria: TThreadSafeCriteria; data: pointer): Boolean;
var P, NP : TIteratorObject;
begin
  Result := false;

  if not assigned(criteria) then Exit;

  Lock;
  try
    P := ListBegin;
    while P <> nil do
    begin
      NP := P.Next;
      if criteria(P.Value, data) then
      begin
        Erase(P);
        Result := True;
      end;
      P := NP;
    end;
  finally
    UnLock;
  end;
end;

procedure TThreadSafeFastSeq.DoForAll(action: TThreadSafeAction);
var P : TIteratorObject;
begin
  if not Assigned(action) then exit;

  Lock;
  try
    P := ListBegin;
    while assigned(P) do
    begin
      action(P.Value);
      P := P.Next;
    end;
  finally
    UnLock;
  end;
end;

procedure TThreadSafeFastSeq.Extract(const loc: TIteratorObject);
begin
  Lock;
  try
    FSeq.Extract(loc);
  finally
    UnLock;
  end;
end;

procedure TThreadSafeFastSeq.ExtractObject(const obj: TObject);
begin
  Lock;
  try
    FSeq.ExtractObject(obj);
  finally
    UnLock;
  end;
end;

function TThreadSafeFastSeq.ExtractObjectsByCriteria(
  criteria: TThreadSafeCriteria; afterextract: TThreadSafeAction; data: pointer
  ): Boolean;
var P, NP : TIteratorObject;
begin
  Result := false;

  if not assigned(criteria) then Exit;

  Lock;
  try
    P := ListBegin;
    while P <> nil do
    begin
      NP := P.Next;
      if criteria(P.Value, data) then
      begin
        afterextract(P.Value);
        Extract(P);
        Result := True;
      end;
      P := NP;
    end;
  finally
    UnLock;
  end;
end;

function TThreadSafeFastSeq.ListBegin: TIteratorObject;
begin
  Lock;
  try
    Result := FSeq.ListBegin;
  finally
    UnLock;
  end;
end;

function TThreadSafeFastSeq.IteratorBegin: TIterator;
begin
  Lock;
  try
    Result := FSeq.IteratorBegin;
  finally
    UnLock;
  end;
end;

class function TThreadSafeFastSeq.ListEnd: TIteratorObject;
begin
  Result := nil;
end;

{ TNetReferenceList }

constructor TNetReferenceList.Create;
begin
  inherited Create;
  FList := TFastSeq.Create;
end;

destructor TNetReferenceList.Destroy;
begin
  while FList.Count > 0 do
  begin
    CleanDead;
    Sleep(0);
  end;
  FList.Free;
  inherited Destroy;
end;

procedure TNetReferenceList.Add(const O: TNetReferencedObject);
begin
  Lock;
  try
    FList.Push_back(O);
  finally
    UnLock;
  end;
end;

procedure TNetReferenceList.CleanDead;
var v, v2 : TIteratorObject;
begin
  Lock;
  try
    v := FList.ListBegin;
    while (v <> FList.ListEnd) do
    begin
      if TNetReferencedObject(v.Value).mReferenceCount <= 0 then
      begin
        v2 := v.Next;
        FList.Erase(v);
        v := v2;
      end else
        v := v.Next;
    end;
  finally
    UnLock;
  end;
end;

{ TNetReferencedObject }

constructor TNetReferencedObject.Create;
begin
  inherited Create;
  mReferenceCount:= 1;
end;

procedure TNetReferencedObject.IncReference;
begin
  Lock;
  try
    Inc(mReferenceCount);
  finally
    UnLock;
  end;
end;

procedure TNetReferencedObject.DecReference;
begin
  Lock;
  try
    Dec(mReferenceCount);
  finally
    UnLock;
  end;
end;

function TNetReferencedObject.HasReferences: Boolean;
begin
  Lock;
  try
    Result := mReferenceCount > 0;
  finally
    UnLock;
  end;
end;

function TNetReferencedObject.HasExternReferences: Boolean;
begin
  Lock;
  try
    Result := mReferenceCount > 1;
  finally
    UnLock;
  end;
end;

{ TNetCustomLockedObject }

constructor TNetCustomLockedObject.Create;
begin
  InitCriticalSection(FRTI);
end;

destructor TNetCustomLockedObject.Destroy;
begin
  DoneCriticalsection(FRTI);
  inherited Destroy;
end;

procedure TNetCustomLockedObject.Lock;
begin
  EnterCriticalsection(FRTI);
end;

procedure TNetCustomLockedObject.UnLock;
begin
  LeaveCriticalsection(FRTI);
end;

{ TThreadSafeAutoIncrementCardinal }

function TThreadSafeAutoIncrementCardinal.GetID: Cardinal;
begin
  Lock;
  try
    Result := FValue;
    Inc(FValue);
  finally
    UnLock;
  end;
end;

constructor TThreadSafeAutoIncrementCardinal.Create;
begin
  inherited Create;
  Reset;
end;

procedure TThreadSafeAutoIncrementCardinal.Reset;
begin
  Lock;
  try
    FValue:=1;
  finally
    UnLock;
  end;
end;

procedure TThreadSafeAutoIncrementCardinal.SetValue(v: Cardinal);
begin
  Lock;
  try
    FValue := v;
  finally
    UnLock;
  end;
end;

{ TThreadBoolean }

function TThreadBoolean.GetValue: Boolean;
begin
  Lock;
  try
    Result := FValue;
  finally
    UnLock;
  end;
end;

procedure TThreadBoolean.SetValue(AValue: Boolean);
begin
  Lock;
  try
    FValue := AValue;
  finally
    UnLock;
  end;
end;

constructor TThreadBoolean.Create(AValue: Boolean);
begin
  inherited Create;
  FValue := AValue;
end;


{ TThreadUtf8String }

function TThreadUtf8String.GetValue: UTF8String;
begin
  Lock;
  try
    Result := FValue;
  finally
    UnLock;
  end;
end;

procedure TThreadUtf8String.SetValue(AValue: UTF8String);
begin
  Lock;
  try
    FValue := AValue;
  finally
    UnLock;
  end;
end;

constructor TThreadUtf8String.Create(AValue: Utf8String);
begin
  inherited Create;
  FValue:= AValue;
end;

{ TThreadInteger }

function TThreadInteger.GetValue: Integer;
begin
  Lock;
  try
    Result := FValue;
  finally
    UnLock;
  end;
end;

procedure TThreadInteger.SetValue(AValue: Integer);
begin
  Lock;
  try
    FValue := AValue;
  finally
    UnLock;
  end;
end;

constructor TThreadInteger.Create(AValue: Integer);
begin
  inherited Create;
  FValue:= AValue;
end;

procedure TThreadInteger.IncValue;
begin
  Lock;
  try
    Inc(FValue);
  finally
    UnLock;
  end;
end;

procedure TThreadInteger.DecValue;
begin
  Lock;
  try
    Dec(FValue);
  finally
    UnLock;
  end;
end;

{ TThreadStringList }

function TThreadStringList.GetCount: Integer;
begin
  Lock;
  try
    Result := FStringList.Count;
  finally
    UnLock;
  end;
end;

function TThreadStringList.GetStr(index : integer): String;
begin
  Lock;
  try
    Result := FStringList[index];
  finally
    UnLock;
  end;
end;

function TThreadStringList.GetText: String;
begin
  Lock;
  try
    Result := FStringList.Text;
  finally
    UnLock;
  end;
end;

procedure TThreadStringList.SetStr(index : integer; AValue: String);
begin
  Lock;
  try
    FStringList[index] := AValue;
  finally
    UnLock;
  end;
  DoChange;
end;

procedure TThreadStringList.SetText(AValue: String);
begin
  Lock;
  try
    FStringList.Text:= AValue;
  finally
    UnLock;
  end;
  DoChange;
end;

procedure TThreadStringList.DoChange;
begin
  if FUpdates = 0 then
   if assigned(FOnChange) then
     FOnChange(Self);
end;

function TThreadStringList.GetOnChange: TNotifyEvent;
begin
  Lock;
  try
    Result := FOnChange;
  finally
    UnLock;
  end;
end;

procedure TThreadStringList.SetOnChange(AValue: TNotifyEvent);
begin
  Lock;
  try
    FOnChange:=AValue;
  finally
    UnLock;
  end;
end;

constructor TThreadStringList.Create;
begin
  inherited Create;
  FStringList := TStringList.Create;
  FUpdates := 0;
end;

destructor TThreadStringList.Destroy;
begin
  FStringList.Free;
  inherited Destroy;
end;

function TThreadStringList.Add(const S: string): Integer;
begin
  Lock;
  try
    Result := FStringList.Add(S);
  finally
    UnLock;
  end;
  DoChange;
end;

function TThreadStringList.IndexOf(const S: string): Integer;
begin
  Lock;
  try
    Result := FStringList.IndexOf(S);
  finally
    UnLock;
  end;
end;

procedure TThreadStringList.AddStrings(S: TStringList);
begin
  Lock;
  try
    FStringList.AddStrings(S);
  finally
    UnLock;
  end;
  DoChange;
end;

procedure TThreadStringList.BeginUpdate;
begin
  Lock;
  FStringList.BeginUpdate;
  Inc(FUpdates);
end;

procedure TThreadStringList.EndUpdate;
begin
  FStringList.EndUpdate;
  Dec(FUpdates);
  UnLock;
  DoChange;
end;

procedure TThreadStringList.Clear;
begin
  if FStringList.Count > 0 then
  begin
    Lock;
    try
      FStringList.Clear;
    finally
      UnLock;
    end;
    DoChange;
  end;
end;

end.

