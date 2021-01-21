{ OGLFastList
   lightweighted lists, collections and seqs
   Copyright (c) 2018-2019 Ilya Medvedkov   }

unit OGLFastList;

{$ifdef fpc}
{$mode delphi}
{$endif}

interface

type
  PObjectList = ^TObjectList;
  TObjectList = array[0..MaxInt shr 3] of TObject;
  PFPointerList = ^TFPointerList;
  TFPointerList = array[0..MaxInt shr 3] of Pointer;

  TPointerSortFunction = function (obj1: Pointer; obj2 : Pointer) : Integer;
  TObjectSortFunction = function (obj1: TObject; obj2 : TObject) : Integer;

  { TFastList }

  TFastList = class
  private
    FList : PObjectList;
    FCount, FCapacity, fLstSP : Integer;
    function GetObject(index: integer): TObject;
    procedure SetObject(index: integer; const Value: TObject);
    procedure SetCapacity(const Value: integer);
    procedure Grow;
    procedure SetCount(const Value: integer);
  public
    constructor Create; virtual;
    function Last : TObject;
    function First : TObject;
    procedure SortList(func : TObjectSortFunction);
    function Add(const Obj : TObject) : Integer;
    procedure Assign(const src : TFastList);
    procedure Merge(const src : TFastList);
    procedure Insert(Index: Integer; const Item: TObject);
    function IndexOf(const Obj : TObject) : Integer;
    function Remove(const obj: TObject) : integer;
    procedure Delete(Ind : integer); virtual;
    procedure Clear; virtual;
    procedure Extract(Ind : integer); virtual;
    procedure Pack;
    destructor Destroy; override;

    property List : PObjectList read FList;
    property Capacity : integer read FCapacity write SetCapacity;
    property Count : integer read FCount write SetCount;
    property Item[index : integer] : TObject read GetObject write SetObject; default;
  end;

  TIteratorObject = class
  public
    Value : TObject;
    Next, Prev  : TIteratorObject;
    constructor Create(const O : TObject);
    destructor Destroy; override;
  end;

  { TIterator }

  TIterator = object
    Loc :  TIteratorObject;
    function Inc : TIteratorObject;
  end;

  PIterator = ^TIterator;

  { TFastSeq }

  TFastSeq = class
  private
    FFirst, FLast : TIteratorObject;
    FCount : Integer;
    procedure swap(const SrcL : TFastSeq);
    procedure merge(const SrcL : TFastSeq; const func : TObjectSortFunction);
    procedure makeempty;
    procedure Push_iterator_obj(const aIO : TIteratorObject);
  public
    constructor Create;
    constructor CreateEmpty;
    destructor Destroy; override;

    procedure Clean;
    procedure ExtractAll;
    procedure Push_back(const O : TObject);
    function Pop : TIteratorObject;
    function PopValue : TObject;
    procedure IteratorErase(const loc : PIterator);
    procedure Erase(const loc : TIteratorObject);
    procedure EraseObject(const obj : TObject);
    procedure Extract(const loc: TIteratorObject);
    procedure ExtractObject(const obj: TObject);
    function ListBegin : TIteratorObject;
    function IteratorBegin : TIterator;
    class function ListEnd   : TIteratorObject;
    procedure SortList(func : TObjectSortFunction);

    property Count : integer read FCount;
  end;

  PFastSeq = ^TFastSeq;

  TFastPointerList = class
  private
    FList : PFPointerList;
    FCount, FCapacity, fLstSP : Integer;
    function GetPointer(index: integer): Pointer;
    procedure SetPointer(index: integer; const Value: Pointer);
    procedure SetCapacity(const Value: integer);
    procedure Grow;
    procedure SetCount(const Value: integer);
  public
    constructor Create; virtual;
    function Last : Pointer;
    function First : Pointer;
    function Add(const Obj : Pointer) : Integer;
    procedure Assign(const src : TFastPointerList);
    procedure Insert(Index: Integer; const Item: Pointer);
    function IndexOf(const Obj : Pointer) : Integer;
    function Remove(const obj: Pointer) : integer;
    procedure Delete(Ind : integer); virtual;
    procedure Clear; virtual;
    procedure Extract(Ind : integer); virtual;
    procedure Pack;
    destructor Destroy; override;

    property List : PFPointerList read FList;
    property Capacity : integer read FCapacity write SetCapacity;
    property Count : integer read FCount write SetCount;
    property Item[index : integer] : Pointer read GetPointer write SetPointer; default;
  end;

  TFastCollection = class(TFastList)
  public
    procedure Extract(Ind : integer); override;
    procedure Delete(Ind : integer); override;
    procedure Clear; override;
    destructor Destroy; override;
  end;

  { TFastPointerCollection }

  TFastPointerCollection = class(TFastPointerList)
  public
    procedure Extract(Ind : integer); override;
    procedure Delete(Ind : integer); override;
    procedure Clear; override;
    destructor Destroy; override;
  end;

implementation

{$IFDEF VER150}
uses SysUtils;
{$ENDIF}

var cGrowOffset : Integer = 8;

Procedure QSort(numbers : PObjectList; L : Integer; R : Integer; func : TObjectSortFunction);
var
  I, J, Pivot: Integer;
  PivotObj, O1 : TObject;
begin
  repeat
    I := L;
    J := R;
    Pivot := (L + R) div 2;
    PivotObj := numbers^[Pivot];
    repeat
      while func(numbers^[i], PivotObj) < 0 do Inc(I);
      while func(numbers^[j], PivotObj) > 0 do Dec(J);
      if I <= J then
      begin
        O1 := numbers^[J];
        numbers^[J] := numbers^[i];
        numbers^[i] := O1;
        if Pivot = I then
        begin
          Pivot := J;
          PivotObj := numbers^[Pivot];
        end
        else if Pivot = J then
        begin
          Pivot := I;
          PivotObj := numbers^[Pivot];
        end;
        Inc(I);
        Dec(j);
      end;
    until I > J;
    if L < J then
      QSort(numbers, L,J, func);
    L := I;
  until I >= R;
end;

{Var
	l_ptr, r_ptr, pv : Integer;
  pivot : TObject;
Begin
	l_ptr := left;
	r_ptr := right;
	pivot := numbers^[left];

	While (left < right) do
	Begin
		While ((
            func(pivot, numbers^[right]) <= 0) AND (left < right)) do
			right := right - 1;

		If (left <> right) Then
		Begin
			numbers^[left] := numbers^[right];
			left := left + 1;
		End;

		While ((
           func(numbers^[left], pivot) <= 0) AND (left < right)) do
			left := left + 1;

		If (left <> right) Then
		Begin
			numbers^[right] := numbers^[left];
			right := right - 1;
		End;
	End;

	numbers^[left] := pivot;
	pv := left;
	left := l_ptr;
	right := r_ptr;

	If (left < pv) Then
		QSort(numbers, left, pv-1, func);

	If (right > pv) Then
		QSort(numbers, pv+1, right, func);
End;                    }

Procedure QuickSort(numbers : PObjectList; size : Integer; func : TObjectSortFunction);
Begin
	QSort(numbers, 0, size-1, func);
End;

{ TFastPointerCollection }

procedure TFastPointerCollection.Extract(Ind: integer);
begin
  FreeMem(Item[ind]);
  inherited Extract(Ind);
end;

procedure TFastPointerCollection.Delete(Ind: integer);
begin
  FreeMem(Item[ind]);
  inherited Delete(Ind);
end;

procedure TFastPointerCollection.Clear;
var i, C : integer;
begin
  C := FCount;
  FCount := 0;
  fLstSP := 0;
  for i := 0 to C - 1 do
  if Assigned(FList^[I]) then
     FreeMem(FList^[i]);
end;

destructor TFastPointerCollection.Destroy;
begin
  Clear;
  inherited Destroy;
end;

{ TFastPointerList }

procedure TFastPointerList.Grow;
begin
  ReallocMem(FList, FCapacity * SizeOf(Pointer));
end;

function TFastPointerList.IndexOf(const Obj: Pointer): Integer;
var i : integer;
begin
  Result := -1;

  i := fLstSP;

  while I < fCount do
  begin
    if Obj = FList^[i] then
    begin
      Result := I;
      fLstSP := I;
      Exit;
    end;
    Inc(I);
  end;
  if fLstSP > 0 then
  begin
    if fLstSP > FCount then fLstSP := FCount;
    I := 0;
    while I < fLstSP do
    begin
      if Obj = FList^[i] then
      begin
        Result := I;
        fLstSP := I;
        Exit;
      end;
      Inc(I);
    end;
  end;
  fLstSP := 0;
end;

procedure TFastPointerList.Insert(Index: Integer; const Item: Pointer);
begin
  if FCount >= FCapacity then SetCapacity(FCount + cGrowOffset);

  if Index < FCount then
    System.Move(FList^[Index], FList^[Index + 1],
      (FCount - Index) * SizeOf(Pointer));
  FList^[Index] := Item;
  Inc(FCount);
end;

function TFastPointerList.Last: Pointer;
begin
  if FCount = 0 then Result := nil else
  Result:=FList^[FCount-1];
end;

procedure TFastPointerList.Pack;
var
  RC, i, Start : Integer;
begin
  if FCount = 0 then Exit;

  RC := FCount;
  Start := 0;
  for I := FCount - 1 downto 0 do
  begin
    if Item[i] = nil then
    begin
      if Start = 0 then
         Start := I;
    end else
    if Start > 0 then
    begin
      if Start < (RC - 1) then
      begin
        System.Move(FList^[Start + 1], FList^[I + 1],
          (RC - Start - 1) * SizeOf(Pointer));
      end;
      DEC(RC, Start - I);
      Start := 0;
    end;
  end;

  if RC = FCount then RC := 0;

  FCount := RC;

  fLstSP := 0;
end;

function TFastPointerList.Remove(const obj: Pointer) : integer;
begin
  result := IndexOf(obj);
  if result >= 0 then Delete(result);
end;

function TFastPointerList.Add(const Obj: Pointer) : Integer;
begin
  if FCount >= FCapacity then  SetCapacity(FCount + cGrowOffset);

  FList^[FCount] := Obj;

  Result := FCount;
  Inc(FCount);
end;

procedure TFastPointerList.Assign(const src: TFastPointerList);
var i : integer;
begin
  if src <> nil then
  begin
    SetCapacity(src.FCapacity);
    FCount := src.FCount;
    fLstSP := src.fLstSP;
    for I := 0 to FCount - 1 do
        FList^[i] := src.FList^[i];
  end;
end;

procedure TFastPointerList.Clear;
begin
  FCount := 0;
  fLstSP := 0;
end;

constructor TFastPointerList.Create;
begin
  FCapacity := cGrowOffset;
  FCount := 0;
  fLstSP := 0;
  FList := AllocMem( FCapacity * SizeOf(Pointer) )
end;

procedure TFastPointerList.Delete(Ind: integer);
begin
  Dec(FCount);
  if Ind < FCount then
    System.Move(FList^[Ind + 1], FList^[Ind],
      (FCount - Ind) * SizeOf(Pointer));
end;

destructor TFastPointerList.Destroy;
begin
  FreeMem(FList);
  inherited;
end;

procedure TFastPointerList.Extract(Ind: integer);
begin
  FList^[ind] := nil;
end;

function TFastPointerList.First: Pointer;
begin
  if FCount = 0 then Result := nil else
  Result:=FList^[0];
end;

function TFastPointerList.GetPointer(index: integer): Pointer;
begin
  Result := FList^[index];
end;

procedure TFastPointerList.SetCapacity(const Value: integer);
begin
  if FCapacity < Value then
  begin
    FCapacity := Value;
    Grow;
  end;
end;

procedure TFastPointerList.SetCount(const Value: integer);
var i  :integer;
begin
  if FCount = Value then Exit;

  I := FCapacity;
  if FCount < Value then
     I := Value + cGrowOffset;
  SetCapacity(I);

  for I := FCount to Value - 1 do
      Item[i] := nil;

  FCount := Value;
end;

procedure TFastPointerList.SetPointer(index: integer; const Value: Pointer);
begin
  FList^[index] := Value;
end;

{ TFastList }

procedure TFastList.Grow;
begin
  FList := ReallocMem(FList, FCapacity * SizeOf(TObject));
end;

function TFastList.IndexOf(const Obj: TObject): Integer;
var i : integer;
begin
  Result := -1;

  i := fLstSP;

  while I < fCount do
  begin
    if Obj = FList^[i] then
    begin
      Result := I;
      fLstSP := I;
      Exit;
    end;
    Inc(I);
  end;
  if fLstSP > 0 then
  begin
    if fLstSP > FCount then fLstSP := FCount;
    I := 0;
    while I < fLstSP do
    begin
      if Obj = FList^[i] then
      begin
        Result := I;
        fLstSP := I;
        Exit;
      end;
      Inc(I);
    end;
  end;
  fLstSP := 0;
end;

procedure TFastList.Insert(Index: Integer; const Item: TObject);
begin
  if FCount >= FCapacity then SetCapacity(FCount + cGrowOffset);

  if Index < FCount then
    System.Move(FList^[Index], FList^[Index + 1],
      (FCount - Index) * SizeOf(TObject));
  FList^[Index] := Item;
  Inc(FCount);
end;

function TFastList.Last: TObject;
begin
  if FCount = 0 then Result := nil else
  Result:=FList^[FCount-1];
end;

procedure TFastList.Pack;
var
  RC, i, Start : Integer;
begin
  if FCount = 0 then Exit;

  RC := FCount;
  Start := 0;
  for I := FCount - 1 downto 0 do
  begin
    if Item[i] = nil then
    begin
      if Start = 0 then
         Start := I;
    end else
    if Start > 0 then
    begin
      if Start < (RC - 1) then
      begin
        System.Move(FList^[Start + 1], FList^[I + 1],
          (RC - Start - 1) * SizeOf(TObject));
      end;
      DEC(RC, Start - I);
      Start := 0;
    end;
  end;

  if RC = FCount then RC := 0;

  FCount := RC;

  fLstSP := 0;
end;

function TFastList.Remove(const obj: TObject) : integer;
begin
  result := IndexOf(obj);
  if result >= 0 then Delete(result);
end;

function TFastList.Add(const Obj: TObject) : Integer;
begin
  if FCount >= FCapacity then
    SetCapacity(FCount + cGrowOffset);

  FList^[FCount] := Obj;

  Result := FCount;
  Inc(FCount);
end;

procedure TFastList.Assign(const src: TFastList);
begin
  if src <> nil then
  begin
    SetCapacity(src.FCapacity);
    FCount := src.FCount;
    fLstSP := src.fLstSP;
    System.Move(src.FList^[0], FList^[0], (FCount) * SizeOf(TObject));
{    for I := 0 to FCount - 1 do
        FList^[i] := src.FList^[i];    }
  end;
end;

procedure TFastList.Merge(const src: TFastList);
begin
  if src.Count = 0 then Exit;
  if FCapacity < (src.FCapacity + FCount) then
    SetCapacity(src.FCapacity + FCount);
  System.Move(src.FList^[0], FList^[FCount], src.Count * SizeOf(TObject));
  Inc(FCount, src.FCount);
end;

procedure TFastList.Clear;
begin
  FCount := 0;
  fLstSP := 0;
end;

constructor TFastList.Create;
begin
  inherited;
  FCapacity := cGrowOffset;
  FCount := 0;
  fLstSP := 0;
  GetMem( FList, FCapacity * SizeOf(TObject) )
end;

procedure TFastList.Delete(Ind: integer);
begin
  Dec(FCount);
  if Ind < FCount then
    System.Move(FList^[Ind + 1], FList^[Ind],
      (FCount - Ind) * SizeOf(TObject));
end;

destructor TFastList.Destroy;
begin
  FreeMem(FList);
  inherited;
end;

procedure TFastList.Extract(Ind: integer);
begin
  FList^[ind] := nil;
end;

function TFastList.First: TObject;
begin
  if FCount = 0 then Result := nil else
  Result:=FList^[0];
end;

function TFastList.GetObject(index: integer): TObject;
begin
  Result := FList^[index];
end;

procedure TFastList.SetCapacity(const Value: integer);
begin
  if FCapacity < Value then
  begin
    FCapacity := Value;
    Grow;
  end;
end;

procedure TFastList.SetCount(const Value: integer);
var i  :integer;
begin
  if FCount = Value then Exit;

  I := FCapacity;
  if FCount < Value then
     I := Value + cGrowOffset;
  SetCapacity(I);

  for I := FCount to Value - 1 do
      Item[i] := nil;

  FCount := Value;
end;

procedure TFastList.SetObject(index: integer; const Value: TObject);
begin
  FList^[index] := Value;
end;

procedure TFastList.SortList(func: TObjectSortFunction);
begin
  QuickSort(FList, Count, func);
end;

{ TFastCollection }

procedure TFastCollection.Clear;
var i, C : integer;
begin
  C := FCount;
  FCount := 0;
  fLstSP := 0;
  for i := 0 to C - 1 do
  if Assigned(FList^[I]) then
     FList^[I].Free;
end;

procedure TFastCollection.Delete(Ind: integer);
begin
  FList^[Ind].Free;
  inherited Delete(ind);
end;

destructor TFastCollection.Destroy;
begin
  Clear;
  inherited;
end;

procedure TFastCollection.Extract(Ind: integer);
begin
  FList^[Ind].Free;
  inherited;
end;

{ TFastSeq }

constructor TFastSeq.Create;
begin
  makeempty;
end;

constructor TFastSeq.CreateEmpty;
begin
  makeempty;
end;

destructor TFastSeq.Destroy;
begin
  Clean;
  inherited;
end;

procedure TFastSeq.Clean;
begin
  while assigned(FFirst) do
       Erase(FFirst);
  makeempty;
end;

procedure TFastSeq.ExtractAll;
begin
  while assigned(FFirst) do
       Extract(FFirst);
  makeempty;
end;

procedure TFastSeq.IteratorErase(const loc: PIterator);
var NL : TIteratorObject;
begin
  NL := loc^.Loc;
  //if not Assigned(NL) then Exit;
  loc^.Inc;

  Erase(NL);
end;

procedure TFastSeq.Erase(const loc: TIteratorObject);
begin
  if not Assigned(loc) then
     Exit;


  if assigned(loc.Prev) then
     loc.Prev.Next := loc.Next else
  begin
     FFirst := loc.Next;
     if assigned(FFirst) then
       FFirst.Prev := nil;
  end;
  if assigned(loc.Next) then
     loc.Next.Prev := loc.Prev else
  begin
     FLast := loc.Prev;
     if assigned(FLast) then
       FLast.Next := nil;
  end;

  loc.Free;
  Dec(FCount);
end;

procedure TFastSeq.EraseObject(const obj: TObject);
var P : TIteratorObject;
begin
  P := ListBegin;
  while P <> nil do
  begin
    if P.Value = obj then
    begin
      Erase(P);
      Exit;
    end;
    P := P.Next;
  end;
end;

procedure TFastSeq.Extract(const loc: TIteratorObject);
begin
  if not Assigned(loc) then
     Exit;

  if assigned(loc.Prev) then
     loc.Prev.Next := loc.Next else
  begin
     FFirst := loc.Next;
     if assigned(FFirst) then
       FFirst.Prev := nil;
  end;
  if assigned(loc.Next) then
     loc.Next.Prev := loc.Prev else
  begin
     FLast := loc.Prev;
     if assigned(FLast) then
       FLast.Next := nil;
  end;

  loc.Value := nil;
  loc.Free;
  Dec(FCount);
end;

procedure TFastSeq.ExtractObject(const obj: TObject);
var P : TIteratorObject;
begin
  P := ListBegin;
  while P <> nil do
  begin
    if P.Value = obj then
    begin
      Extract(P);
      Exit;
    end;
    P := P.Next;
  end;
end;

function TFastSeq.IteratorBegin: TIterator;
begin
  Result.Loc := ListBegin;
end;

function TFastSeq.ListBegin: TIteratorObject;
begin
  Result := FFirst;
end;

class function TFastSeq.ListEnd: TIteratorObject;
begin
  Result := nil;
end;

procedure TFastSeq.makeempty;
begin
  FFirst := nil;
  FLast := nil;
  FCount := 0;
end;

procedure TFastSeq.merge(const SrcL: TFastSeq; const func: TObjectSortFunction);
var it1, it2 : TIterator; merged : TFastSeq;
    O : TIteratorObject;
begin
  it1 := IteratorBegin;
  it2 := SrcL.IteratorBegin;

  merged := TFastSeq.CreateEmpty;

  while Assigned(it1.loc) and Assigned(it2.loc) do
  begin
    if func(it1.loc.Value, it2.loc.Value) <= 0 then
    begin
      O := it1.Loc;
      it1.Loc := O.Next;
      merged.Push_iterator_obj(O);
    end else
    begin
      O := it2.Loc;
      it2.Loc := O.Next;
      merged.Push_iterator_obj(O);
    end;
  end;
  if Assigned(it1.Loc) then
  begin
      O := it1.Loc;
      if Assigned(merged.FLast) then
      begin
        merged.FLast.Next := O;
        O.Prev := merged.FLast;
      end else
      begin
        merged.FFirst := O;
        merged.FLast := O;
        O.Prev := nil;
        O.Next := nil;
      end;
      merged.FLast := FLast;
  end;
  if Assigned(it2.Loc) then
  begin
      O := it2.Loc;
      if Assigned(merged.FLast) then
      begin
        merged.FLast.Next := O;
        O.Prev := merged.FLast;
      end else
      begin
        merged.FFirst := O;
        merged.FLast := O;
        O.Prev := nil;
        O.Next := nil;
      end;
      merged.FLast := SrcL.FLast;
  end;


  FFirst := merged.FFirst;
  FLast := merged.FLast;
  FCount := FCount + SrcL.FCount;
  merged.FFirst := nil;
  SrcL.makeempty;
  merged.Free;
end;

procedure TFastSeq.Push_back(const O: TObject);
var NL : TIteratorObject;
begin
  NL := TIteratorObject.Create(O);
  NL.Prev := FLast;
  if not assigned(FFirst) then
     FFirst := NL else
     FLast.Next := NL;
  FLast := NL;
  Inc(FCount);
end;

function TFastSeq.Pop: TIteratorObject;
begin
  if assigned(FFirst) then
  begin
     Result := FFirst;
     FFirst := FFirst.Next;
     if assigned(FFirst) then
        FFirst.Prev := nil else FLast := nil;
     Dec(FCount);
  end else Result := nil;
end;

function TFastSeq.PopValue: TObject;
var IOb : TIteratorObject;
begin
  IOb := Pop;
  if Assigned(IOb) then begin
    Result := IOb.Value;
    IOb.Value := nil;
    IOb.Free;
  end else Result := nil;
end;

procedure TFastSeq.Push_iterator_obj(const aIO: TIteratorObject);
begin
  aIO.Prev := FLast;
  if not assigned(FFirst) then
     FFirst := aIO else
     FLast.Next := aIO;
  FLast := aIO;
  aIO.Next := nil;
  Inc(FCount);
end;

procedure TFastSeq.SortList(func: TObjectSortFunction);
var carry, m : TFastSeq;
    tmp : Array [0..15] of TFastSeq;
    fill, counter, loc : PFastSeq;
    i:Integer;
begin
   // Do nothing if the list has length 0 or 1.
   if (FLast <> FFirst) and Assigned(FFirst) then
   begin

     carry := TFastSeq.CreateEmpty;
     for I := 0 to 15 do tmp[i] := TFastSeq.CreateEmpty;


     fill := @(tmp[0]);
     counter := nil;

     repeat
       carry.FFirst := FFirst;
       FFirst := FFirst.Next;
       carry.FFirst.Next := nil;
       carry.FLast := carry.FFirst;
       Dec(FCount);
       carry.FCount := 1;

       counter := @(tmp[0]);
       while (counter <> fill) and (counter^.FCount > 0) do
       begin
         counter^.merge(carry, func);
         m := carry;
         carry := counter^;
         counter^ := m;
         Inc(counter);
       end;
       m := carry;
       carry := counter^;
       counter^ := m;
       if (counter = fill) then
          Inc(fill);
     until ( FCount = 0 );

     counter := @(tmp[1]);
     loc :=  @(tmp[0]);
     while (counter <> fill) do
     begin
       counter^.merge(loc^, func);
       Inc(counter);
       Inc(loc);
     end;
     swap( loc^ );

     carry.Free;
     for I := 0 to 15 do tmp[i].Free;
   end;
end;

procedure TFastSeq.swap(const SrcL: TFastSeq);
var i : TIteratorObject; C : Integer;
begin
  i := SrcL.FFirst;
  SrcL.FFirst := FFirst;
  FFirst := i;

  i := SrcL.FLast;
  SrcL.FLast := FLast;
  FLast := i;

  C := SrcL.FCount;
  SrcL.FCount := FCount;
  FCount := C;
end;

{ TIterator }

function TIterator.Inc: TIteratorObject;
begin
  Loc := Loc.Next;
  Result := Loc;
end;

{ TIteratorObject }

constructor TIteratorObject.Create(const O: TObject);
begin
  Next := nil;
  Value := O;
end;

destructor TIteratorObject.Destroy;
begin
  if assigned(Value) then Value.Free;
  inherited;
end;

end.
