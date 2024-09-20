(*
    Delphi Components Library

    StellaSOFT objektumok gyüjteménye
    ---------------------------------
    TUndoRedo:      UndoRedo component: stores datas in array of MemoryStreams;
    THRTimer        High resolution timer object

    Copyright: Agócs László, Hungary
               Free szoftware for experience


*)
unit StObjects;

interface

uses Windows, Classes;


Type

  T2DPoint = class(TPersistent)
  private
    FOnChange: TNotifyEvent;
    Fx : extended;
    Fy : extended;
    procedure Setx(Value:extended);
    procedure Sety(Value:extended);
    procedure Changed; dynamic;
  public
    constructor Create(AOwner:TObject; px,py: extended);
  published
    property x: extended read Fx write Setx;
    property y: extended read Fy write Sety;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TUndoRedoChangeEvent = procedure(Sender:TObject; Undo,Redo:boolean) of object;
  TUndoSaveEvent = procedure(Sender:TObject; MemSt:TMemoryStream) of object;
  TUndoSaveProcedure = procedure(var MemSt:TMemoryStream) of object;
  TUndoRedoProcedure = procedure(MemSt:TMemoryStream) of object;

  {---- UndoRedo objektum -----}
  TUndoRedo = class
  private
    fEnable: boolean;
    fUndoLimit: integer;
    FUndoRedo:TUndoRedoChangeEvent;
    FUndoSave: TUndoSaveEvent;
    fUndoSaveProcedure: TUndoSaveProcedure;
    fUndoRedoProcedure: TUndoRedoProcedure;
    procedure SetUndoLimit(const Value: integer);
  protected
    UndoSaveCount : integer;
    UndoCount     : integer;
    UndoStart     : integer;
    UndoPointer   : integer;
    UndoEnable,RedoEnable : boolean;
    function GetIndex(us:integer): integer;
  public
    TempStream    : TMemoryStream;  // Stream az aktuális mentésekhez
    UndoStreams   : array[0..999] of TMemoryStream;
    constructor Create; virtual;
    destructor Destroy; override;
    procedure UndoInit;
    procedure UndoSave;
    procedure Undo;
    procedure Redo;
    property Enable : boolean read fEnable write fEnable;
    property UndoLimit : integer read fUndoLimit write SetUndoLimit;
    property UndoSaveProcedure: TUndoSaveProcedure read fUndoSaveProcedure write fUndoSaveProcedure;
    property UndoRedoProcedure: TUndoRedoProcedure read fUndoRedoProcedure write fUndoRedoProcedure;
    property OnUndoRedo : TUndoRedoChangeEvent read FUndoRedo write FUndoRedo;
    property OnUndoSave : TUndoSaveEvent read FUndoSave write FUndoSave;
  end;

  {---- High Resolution Timer objektum -----}
  // Real precision is about min. 0.001 sec = 1 milisec
   THRTimer = Class(TObject)
     Constructor Create;
     Function StartTimer : Boolean;
     Function ReadTimer : Double;
   private
   public
     Exists    : Boolean;
     StartTime : Double;
     ClockRate : Double;
     PROCEDURE Delay(ms: double);
   End;

implementation

{ -----------  TUndoRedo --------- }

constructor TUndoRedo.Create;
var i: integer;
begin
  Inherited Create;
  UndoLimit := 1000;
  Enable    := True;
  UndoInit;
end;

destructor TUndoRedo.Destroy;
var i: integer;
begin
  for i:=0 to fUndoLimit-1 do
      if  UndoStreams[i]<>nil then UndoStreams[i].Destroy;
  Inherited Destroy;
end;

{Az Undo stream-eket alapra hozza}
procedure TUndoRedo.UndoInit;
var i: integer;
begin
  UndoSaveCount := 0;
  UndoCount     := 0;
  UndoStart     := 0;
  UndoPointer   := 0;
  UndoEnable    := False;
  RedoEnable    := False;
  for i:=0 to fUndoLimit-1 do
      if UndoStreams[i]=nil then UndoStreams[i].Create
      else UndoStreams[i].Clear;
  If Assigned(FUndoRedo) then FUndoRedo(Self,False,False);
end;

{Undo mentés az sbl stream tartalmát menti az UndoStreams n. streamjére;
 az undopointer és undoCount értékét 1-el növeli }
// Saveing to the actual Memorystream
procedure TUndoRedo.UndoSave;
begin
If Enable then begin
  UndoStart := UndoPointer;
  UndoStreams[UndoPointer].Clear;
  If Assigned(fUndoSaveProcedure) then
     fUndoSaveProcedure(UndoStreams[UndoPointer]);
  Inc(UndoPointer);
  UndoPointer := UndoPointer mod UndoLimit;
  Inc(UndoSaveCount);
  UndoCount := 0;
  If Assigned(FUndoRedo) then FUndoRedo(Self,True,False);
end;
end;

function TUndoRedo.GetIndex(us:integer): integer;
begin
  Result := us;
  If us>(Undolimit-1) then Result:=us mod Undolimit;
  if us<0 then Result:=UndoLimit-(Trunc(Abs(us)) mod Undolimit)
end;

// Previous stream activating
procedure TUndoRedo.Undo;
var UC,IDX: integer;
begin
If Enable then begin
   UC := UndoPointer-1;
   If UndoSaveCount>=UndoLimit then UC:=UndoLimit-1;
   UndoEnable := UndoCount<UC;
   if UndoEnable then begin
        Dec(UndoStart);
        IDX := GetIndex(UndoStart);
        UndoStreams[IDX].Seek(0,0);
        If Assigned(fUndoRedoProcedure) then
           fUndoRedoProcedure(UndoStreams[IDX]);
        Inc(UndoCount);
        UndoEnable := UndoCount<UC;
        RedoEnable := UndoCount>0;
   end;
   If Assigned(FUndoRedo) then FUndoRedo(Self,UndoEnable,RedoEnable);
end;
end;

// Next stream activating
procedure TUndoRedo.Redo;
var UC,IDX: integer;
begin
If Enable then begin
   RedoEnable := UndoCount>0;
   if RedoEnable then begin
        Inc(UndoStart);
        IDX := GetIndex(UndoStart);
        UndoStreams[IDX].Seek(0,0);
        If Assigned(fUndoRedoProcedure) then
           fUndoRedoProcedure(UndoStreams[IDX]);
        Dec(UndoCount);
        RedoEnable := UndoCount>0;
        UndoEnable := True;
   end;
   If Assigned(FUndoRedo) then FUndoRedo(Self,UndoEnable,RedoEnable);
end;
end;

// 0..Value count memorystream deffiniation
procedure TUndoRedo.SetUndoLimit(const Value: integer);
var i: integer;
begin
  If fUndoLimit <> Value then begin
     fUndoLimit := Value;
     If fUndoLimit>High(UndoStreams) then fUndoLimit:=High(UndoStreams);
     for i:=0 to fUndoLimit-1 do
       if UndoStreams[i]=nil then UndoStreams[i]:=TMemoryStream.Create;
     for i:=fUndoLimit to High(UndoStreams) do
       if UndoStreams[i]<>nil then UndoStreams[i].Destroy;
  end;
end;

{ -----------  T2DPoint --------- }

procedure T2DPoint.Changed;
begin
  if Assigned(FOnChange) then
     FOnChange(Self);
end;

constructor T2DPoint.Create(AOwner:TObject; px,py: extended);
begin
  inherited Create;
  x := px; y := py;
end;

procedure T2DPoint.Setx(Value:extended);
begin
  If Fx<>Value then begin
     Fx:=Value;
     Changed;
  end;
end;

procedure T2DPoint.Sety(Value:extended);
begin
  If Fy<>Value then begin
     Fy:=Value;
     Changed;
  end;
end;

//-----------THRTimer-----------------

Constructor THRTimer.Create;
Var  QW : _Large_Integer;
BEGIN
   Inherited Create;
   Exists := QueryPerformanceFrequency(TLargeInteger(QW));
   ClockRate := QW.QuadPart;
END;

// Clear the timer, and starts with 0 time
Function THRTimer.StartTimer : Boolean;
Var
  QW : _Large_Integer;
BEGIN
   Result := QueryPerformanceCounter(TLargeInteger(QW));
   StartTime := QW.QuadPart;
END;

// Timer's time in second since the start time
Function THRTimer.ReadTimer : Double;
Var
  ET : _Large_Integer;
BEGIN
   QueryPerformanceCounter(TLargeInteger(ET));
   Result := 1000.0*(ET.QuadPart - StartTime)/ClockRate;
END;

// Delay ms miliseconds
PROCEDURE THRTimer.Delay(ms: double);
Var
  QW,ET : _Large_Integer;
  Start_Time, dt : double;
BEGIN
   QueryPerformanceCounter(TLargeInteger(QW));
   Start_Time := QW.QuadPart;
   repeat
         QueryPerformanceCounter(TLargeInteger(ET));
         dt := 1000.0*(ET.QuadPart - Start_Time)/ClockRate;
   Until dt>=ms;
END;


end.
