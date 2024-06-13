unit wcMPUtils;

{$mode objfpc}{$H+}

interface

uses
  PasMP;

type

  { TAtomicBoolean }

  TAtomicBoolean = class(TPasMPAtomic)
  private
    FValue : TPasMPBool32;
    function GetValue: Boolean;
    procedure SetValue(AValue: Boolean);
  public
    constructor Create(aValue : Boolean);

    property Value : Boolean read GetValue write SetValue;
  end;

  { TAtomicInteger }

  TAtomicInteger = class(TPasMPAtomic)
  private
    FValue : TPasMPInt32;
    function GetValue: Integer;
    procedure SetValue(AValue: Integer);
  public
    constructor Create(aValue : Integer);

    procedure IncValue;
    procedure DecValue;

    property Value : Integer read GetValue write SetValue;
  end;

  { TAtomicQWord }

  TAtomicQWord = class(TPasMPAtomic)
  private
    FValue : TPasMPUInt64;
    function GetValue: QWord;
    procedure SetValue(AValue: QWord);
  public
    constructor Create(aValue : QWord);

    procedure IncValue;
    procedure DecValue;

    property Value : QWord read GetValue write SetValue;
  end;

implementation

{ TAtomicQWord }

function TAtomicQWord.GetValue: QWord;
begin
  Result := QWord(Read(FValue));
end;

procedure TAtomicQWord.SetValue(AValue: QWord);
begin
  Write(FValue, TPasMPUInt64(AValue));
end;

constructor TAtomicQWord.Create(aValue: QWord);
begin
  FValue:= TPasMPUInt64(aValue);
end;

procedure TAtomicQWord.IncValue;
begin
  Increment(FValue);
end;

procedure TAtomicQWord.DecValue;
begin
  Decrement(FValue);
end;

{ TAtomicInteger }

function TAtomicInteger.GetValue: Integer;
begin
  Result := Integer(Read(FValue));
end;

procedure TAtomicInteger.SetValue(AValue: Integer);
begin
  Write(FValue, TPasMPInt32(AValue));
end;

constructor TAtomicInteger.Create(aValue: Integer);
begin
  FValue:= TPasMPInt32(aValue);
end;

procedure TAtomicInteger.IncValue;
begin
  Increment(FValue);
end;

procedure TAtomicInteger.DecValue;
begin
  Decrement(FValue);
end;

{ TAtomicBoolean }

function TAtomicBoolean.GetValue: Boolean;
begin
  Result := Read(FValue);
end;

procedure TAtomicBoolean.SetValue(AValue: Boolean);
begin
  Write(FValue, TPasMPBool32(AValue));
end;

constructor TAtomicBoolean.Create(aValue: Boolean);
begin
  FValue:= TPasMPBool32(aValue);
end;

end.

