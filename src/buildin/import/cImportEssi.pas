unit cImportEssi;

interface

uses
  Forms, SysUtils, Classes, ComCtrls,  Math, Graphics, IniFiles,
  CADSys4, CS4BaseTypes, CS4Tasks, CS4Shapes, CS4DXFModule,
  applicationh,
  camh,
  CommonUtils;

const RapidMoveONPenStyle  = psDOT;
      RapidMoveOFFPenStyle = psSOLID;
      CuttingONPenColor    = clRED;
      CuttingOFFPenColor   = clBLUE;

      DefaultLayer         = 0;
      CuttingOFFLayer      = 250;

type

  TPTEssiImportTraceRec = ^TEssiImportTraceRec;
  TEssiImportTraceRec = record
    //SourceLine: string;
    SourceLineNum: integer;
    GeneratedObjectID: integer;
  end;

  TImportEssi = class
  private
    { Private declarations }
    fLastKerfInfo: integer;
    fTraceInfoList: TList;
    fCurrLineNum: integer;
    fCADCmp2D: TCADCmp2D;
    fCADViewport2D: TCADViewport2D;
    fProgressBar: TProgressBar;

    InputList: TStringList;

    LayerNr    : integer;
    PenWidth   : integer;
    PenStyle   : integer;
    PenColor   : Integer;
    BrushStyle : integer;
    BrushColor : integer;
    RowStyle   : integer;
    Selected   : boolean;
    ArcStyle   : integer;

          //commands
      fESSICmdKerfInsRight,
      fESSICmdKerfInsLeft,
      fESSICmdKerfOutsRight,
      fESSICmdKerfOutsLeft : string;
      fESSICmdRapidMoveON: string;
      fESSICmdRapidMoveOFF: string;
      fESSICmdToolDown: string;
      fESSICmdToolUp: string;
      fESSICmdKerfOFF: string;
      fESSICmdCommentON: string;
      fESSICmdCommentOFF: string;
      fESSICmdProgramEnd: string;
      fESSICmdProgramStart: string;
      fESSICmdAsoulutON: string;
      fESSICmdRelativeON: string;


      fIniFile: TIniFile;
      fMachineName,
      fIniFileName: string; //machine


  //Essi
    fUnitFactor: single;
    CommentON  : boolean;
    fRelativeProgram: boolean;
    RapidMoveON: boolean;
    CuttingON:   boolean;
    fImportMoves: boolean;
    fShowRapidMoves: boolean;
    fShowDirection: boolean;
    lastposx,
    lastposy: single;

    procedure DrawLine(AX1, AY1, AX2, AY2: single);
    procedure DrawArc(AX1, AY1, AX2, AY2, AStartAngle, AEndAngle: single; ADirection: string);

    function  SignIdx(AStr: string): integer;
    function  SubString(AStr : string; startPos, endPos: integer): string;
    function  CharAnzahl(AStr: string; AChar: char): integer;
    function  isLine(AStr: string) : boolean;
    procedure FillLine(AStr: string);
    function  isArc(AStr: string): boolean;
    procedure FillArc(AStr: string);

    procedure SwapPoints(var Ax1:double; var Ay1:double; var Ax2:double; var Ay2: double);
    procedure SwapAngle(var AAngle1:double; var AAngle2:double);
    function  GetAngleFromPoints(Ax1, Ay1, Ax2, Ay2: double): double;
    procedure AddTraceInfo(AObject2DID: integer);

    procedure Init;

    procedure SetPenPropertys(AObject2D: TObject2D);
    procedure SetRapidMoveVisibility(AValue: boolean);
    procedure SetDirectionVisibility(AValue: boolean);
  public
    { Public declarations }
    constructor create(ACADCmp2D: TCADCmp2D; ACADViewport2D: TCADViewport2D ;AProgressBar: TProgressBar);
    procedure  LoadFromFile(AFileName: string);
    procedure  LoadFromList(AStrList: TStringList);
    procedure  SaveTraceInfoToList(AList: TList);
    procedure  Import;
    procedure  Clear;
    destructor destroy; override;

    procedure ReadIniFile;

    property ImportMoves: boolean   read   fImportMoves     write fImportMoves;
    property ShowMoves: boolean     read   fShowRapidMoves  write SetRapidMoveVisibility;
    property ShowDirection: boolean read   fShowDirection   write SetDirectionVisibility;
  end;

implementation

procedure TImportEssi.ReadIniFile;
begin
  fMachineName               := fIniFile.ReadString('ESSI', 'MACHINE_NAME', fMachineName);
  fUnitFactor                := fIniFile.ReadFloat ('ESSI', 'UNIT_FACTOR', 0.1);
  fESSICmdKerfInsRight       := fIniFile.ReadString('ESSI', 'KERF_INS_RIGHT', '30');
  fESSICmdKerfInsLeft        := fIniFile.ReadString('ESSI', 'KERF_INS_LEFT', '29');
  fESSICmdKerfOutsRight      := fIniFile.ReadString('ESSI', 'KERF_OUTS_RIGHT', '29');
  fESSICmdKerfOutsLeft       := fIniFile.ReadString('ESSI', 'KERF_OUTS_LEFT', '30');
  fESSICmdKerfOFF            := fIniFile.ReadString('ESSI', 'KERF_OFF', '38');

  fESSICmdRapidMoveON        := fIniFile.ReadString('ESSI', 'RAPID_MOVE_ON', '5');
  fESSICmdRapidMoveOFF       := fIniFile.ReadString('ESSI', 'RAPID_MOVE_OFF', '6');
  fESSICmdToolDown           := fIniFile.ReadString('ESSI', 'TOOL_DOWN', '7');
  fESSICmdToolUp             := fIniFile.ReadString('ESSI', 'TOOL_UP', '8');
  fESSICmdCommentON          := fIniFile.ReadString('ESSI', 'COMMENT_ON', '3');
  fESSICmdCommentOFF         := fIniFile.ReadString('ESSI', 'COMMENT_OFF', '4');
  fESSICmdProgramStart       := fIniFile.ReadString('ESSI', 'PROGRAM_START', '%');
  fESSICmdProgramEnd         := fIniFile.ReadString('ESSI', 'PROGRAM_END', '0');
  fESSICmdAsoulutON          := fIniFile.ReadString('ESSI', 'ABSOULUT_ON ', '90');
  fESSICmdRelativeON         := fIniFile.ReadString('ESSI', 'RELATIVE_ON ', '91');
  fRelativeProgram           := fIniFile.ReadBool('ESSI',   'RELATIVPROGRAM', true);
  lastposx                   := fIniFile.ReadFloat('ESSI', 'START_X', 0) / fUnitFactor;
  lastposy                   := fIniFile.ReadFloat('ESSI', 'START_Y', 0) / fUnitFactor;
end;


constructor TImportEssi.create(ACADCmp2D: TCADCmp2D; ACADViewport2D: TCADViewport2D; AProgressBar: TProgressBar);
begin
  fCADCmp2D              := ACADCmp2D;
  fCADViewport2D         := ACADViewport2D;
  fProgressBar           := AProgressBar;
  fCADCmp2D.CurrentLayer := DefaultLayer;
  InputList              := TStringList.Create;
  fTraceInfoList         := TList.Create;
  fIniFile := TIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'));
  Init;
  //fCADCmp2D.Layers.Layers[CuttingOFFLayer].Visible := false;
end;

procedure  TImportEssi.Clear;
begin
  InputList.clear;
  fTraceInfoList.Clear;
  lastposx := fIniFile.ReadFloat('SETTINGS', 'START_X', 0) / fUnitFactor;
  lastposy := fIniFile.ReadFloat('SETTINGS', 'START_Y', 0) / fUnitFactor;
end;

destructor TImportEssi.destroy;
begin
  InputList.clear;
  InputList.Free;
  fTraceInfoList.Clear;
  fTraceInfoList.Free;
  fIniFile.Free;
  inherited;
end;

procedure TImportEssi.Init;
begin
  ReadIniFile;

  fImportMoves    := true;
  fShowRapidMoves := true;
  fShowDirection  := false;
  fCurrLineNum := 0;
  LayerNr    := 0;
  PenWidth   := 1;
  //PenStyle   := integer(psSolid);
  //PenColor   := clWhite;
  //RowStyle   := 1;
  //BrushStyle := 0;
  //BrushColor := clWhite;
  //Selected   := true;
  //ArcStyle   := 0;
  CommentOn   := false;
  RapidMoveON  := false;
  CuttingON   := false;
  fLastKerfInfo := 0;
end;

procedure TImportEssi.LoadFromFile(AFileName: string);
begin
  if ((AFileName <> '')  and (fCADCmp2D <> nil) )then
  begin
    fTraceInfoList.Clear;
    InputList.Clear;
    InputList.LoadFromFile(AFileName);
  end;
end;

procedure TImportEssi.LoadFromList(AStrList: TStringList);
var i: integer;
begin
  if ((AStrList <> nil)  and (fCADCmp2D <> nil) )then
  begin
    InputList.Clear;
    for i := 0 to AStrList.Count - 1 do
      InputList.Add(AStrList[i]);
  end;
end;

procedure   TImportEssi.Import;
var i, hHF: integer; hStr: string;
begin
  //lastposx := 0;
  //lastposy := 0;
  fProgressBar.Position := 0;
  fProgressBar.Min      := 0;
  fProgressBar.Max      := InputList.Count -1;
  fProgressBar.Step     := 1;
  for i:= 0 to (InputList.Count -1) do
  begin
    fCurrLineNum := i + 1;
    hStr := InputList[i];
    if (hStr <> '') then
    begin
      if (hStr = fESSICmdKerfOFF) then
        fLastKerfInfo := ord(None)
      else if (hStr = fESSICmdKerfInsRight) then
        fLastKerfInfo := ord(InnerContourCW)
      else if (hStr = fESSICmdKerfInsLeft) then
        fLastKerfInfo := ord(InnerContourCCW)
      else if (hStr = fESSICmdKerfOutsRight) then
        fLastKerfInfo := ord(OuterContourCW)
      else if (hStr = fESSICmdKerfOutsLeft) then
        fLastKerfInfo := ord(OuterContourCCW)
      else if (hStr = fESSICmdRapidMoveON) then
        RapidMoveON := true
      else if (hStr = fESSICmdRapidMoveOFF) then
        RapidMoveON := false
      else if (hStr = fESSICmdToolDown) then
        CuttingOn := true
      else if (hStr = fESSICmdToolUp) then
        CuttingOn := false
      else if  (hStr = fESSICmdAsoulutON) then
        fRelativeProgram := false
      else if  (hStr = fESSICmdRelativeON) then
        fRelativeProgram := true
      else if isLine(hStr) then
        FillLine(hStr)
      else  if isArc(hStr) then
        FillArc(hStr)
      //else FillErrRec(hStr);
    end;
    fProgressBar.StepIt;
    hStr := '';
  end;
  fProgressBar.Position := 0;
  //CheckRapidMoveVisibility;
  //CheckDirectionVisibility;
  //fCADViewport2D.ZoomToExtension;
  //fCADCmp2D.RepaintViewports;
end;


function  TImportEssi.SignIdx(AStr: string): integer;
var i, hRet, AStrLen: integer;
begin
  i := 0;
  AStrLen := StrLen(PChar(AStr));
  hRet := -1;
  if AStrLen > 0 then
  begin
    repeat
      i := i + 1;
    until  (AStr[i] = '+') or (AStr[i] = '-') or (i > AStrLen) ;
    if ((AStr[i] = '+') or (AStr[i] = '-')) then hRet := i
  end;
  result := hRet;
end;

function TImportEssi.CharAnzahl(AStr: string; AChar: char): integer;
var i, AStrLen, hRet: integer;
begin
  hRet        := 0;
  AStrLen     := StrLen(PChar(AStr));
  if AStrLen > 0 then
    for i:= 1 to AStrLen do
      if AStr[i] = AChar then
        hRet := hRet + 1;
  result := hRet;
end;

procedure TImportEssi.DrawLine(AX1, AY1, AX2, AY2: single);
var Line2D: TLine2D;
begin
  Line2D := TLine2D.Create( -1, Point2D(AX1, AY1) , Point2D(AX2, AY2));
  //Line2D.ReserveInt1 := fLastKerfInfo;
  SetPenPropertys(Line2D);
  if   fShowDirection then Line2D.ShowDirection := true
  else                     Line2D.ShowDirection := false;
  if (not CuttingON) then
    fCADCmp2D.CurrentLayer := 250
  else
    fCADCmp2D.CurrentLayer := DefaultLayer;
  fCADCmp2D.AddObject(-1, Line2D);
  AddTraceInfo(Line2D.ID);
end;

procedure TImportEssi.AddTraceInfo(AObject2DID: integer);
var PTEssiImportTraceRec: TPTEssiImportTraceRec;
begin
  new(PTEssiImportTraceRec);
  //PTEssiImportTraceRec^.SourceLine        := InputList[fCurrLineNum - 1];
  PTEssiImportTraceRec^.SourceLineNum     := fCurrLineNum;
  PTEssiImportTraceRec^.GeneratedObjectID := AObject2DID;
  fTraceInfoList.Add(PTEssiImportTraceRec);
end;

procedure TImportEssi.SaveTraceInfoToList(AList: TList);
var i: integer; var PTEssiImportTraceRec: TPTEssiImportTraceRec;
begin
  if (AList <> nil) then
  begin
    for i := 0 to fTraceInfoList.Count - 1 do
    begin
      new(PTEssiImportTraceRec);
      //PTEssiImportTraceRec^.SourceLine        := TEssiImportTraceRec(fTraceInfoList.Items[i]^).SourceLine;
      PTEssiImportTraceRec^.SourceLineNum     := TEssiImportTraceRec(fTraceInfoList.Items[i]^).SourceLineNum;
      PTEssiImportTraceRec^.GeneratedObjectID := TEssiImportTraceRec(fTraceInfoList.Items[i]^).GeneratedObjectID;
      AList.Add(PTEssiImportTraceRec);
    end;
  end;
end;

function TImportEssi.SubString(AStr : string; startPos, endPos: integer):    string;
var i, AStrLen: integer; hStr, hRet: string; gueltig: boolean;
begin
  hRet := '';
  gueltig := true;
  AStrLen := StrLen(PChar(AStr));
  if (startPos <= 0)         then startPos := 1;
  if (endPos   > AStrLen)    then endPos   := AStrLen;
  if (startPos > endPos)     then gueltig  := false;
  if (startPos > AStrLen)    then gueltig  := false;
  if (gueltig) then
  begin
    for i := startPos to endPos do hStr := hStr + AStr[i];
    hRet := hStr;
  end else hRet := '-';
  result := hStr;
end;

function TImportEssi.isLine(AStr: string) : boolean;
var idx1, idx2, SignCount, AStrLen: integer; val1, val2 : string;
begin
  result := false;
  AStrLen     := StrLen(PChar(AStr));
  SignCount   := cuCharCount(AStr, '+') + cuCharCount(AStr, '-');
  if SignCount <> 2 then exit;
  idx1        := 1;
  idx2        := SignIdx(SubString(AStr, 2, AStrLen)) + idx1;
  val1        := SubString(AStr, idx1, (idx2 - 1));
  val2        := SubString(AStr, idx2, AStrLen);
  if ((val1 = '+') or (val1 = '-')) then val1 := val1 + '0';
  if ((val2 = '+') or (val2 = '-')) then val2 := val2 + '0';

  result :=  (   
                 ( (AStr[1] = '+') or (AStr[1] = '-' ))                              and
                 ((val1='+') or (val1='-') or (StrToIntDef(val1, -9999999) <> -9999999))  and
                 ((val2='+') or (val2='-') or (StrToIntDef(val2, -9999999) <> -9999999))
              );
end;

function TImportEssi.isArc(AStr: string): boolean;
var idx1, idx2, idx3, idx4, idx5, SignCount, AStrLen: integer;
    val1, val2, val3, val4: string;
begin
  result      := false;
  AStrLen     := StrLen(PChar(AStr));
  SignCount   := cuCharCount(AStr, '+') + cuCharCount(AStr, '-');
  if SignCount <> 5 then exit;
  idx1        := 1;
  idx2        := SignIdx(SubString(AStr, 2, AStrLen)) + idx1;
  idx3        := SignIdx(SubString(AStr, idx2 + 1, AStrLen)) + idx2;
  idx4        := SignIdx(SubString(AStr, idx3 + 1, AStrLen)) + idx3;
  idx5        := AStrLen;
  val1        := SubString(AStr, idx1, (idx2 - 1));
  val2        := SubString(AStr, idx2, (idx3 - 1));
  val3        := SubString(AStr, idx3, (idx4 - 1));
  val4        := SubString(AStr, idx4, (idx5 - 1));
  if ((val1 = '+') or (val1 = '-')) then val1 := val1 + '0';
  if ((val2 = '+') or (val2 = '-')) then val2 := val2 + '0';
  if ((val3 = '+') or (val3 = '-')) then val3 := val3 + '0';
  if ((val4 = '+') or (val4 = '-')) then val4 := val4 + '0';

  result :=  (   ((AStr[1] = '+') or (AStr[1] = '-' ))       and
                 ((val1='+') or (val1='-') or (StrToIntDef(val1, -9999999) <> -9999999))  and
                 ((val2='+') or (val2='-') or (StrToIntDef(val2, -9999999) <> -9999999))  and
                 ((val3='+') or (val3='-') or (StrToIntDef(val3, -9999999) <> -9999999))  and
                 ((val4='+') or (val4='-') or (StrToIntDef(val4, -9999999) <> -9999999))
              );
end;

procedure TImportEssi.FillArc(AStr: string);
var idx1, idx2, idx3, idx4, idx5, SignAnzahl, AStrLen: integer;
    val1, val2, val3, val4, hDirection: string;
    A,B,C,D: TPoint2D;
    a1, a2, x1, y1, x2, y2, cx, cy, radius: single;
begin
  AStrLen     := StrLen(PChar(AStr));
  idx1        := 1;
  idx2        := SignIdx(SubString(AStr, idx1 + 1, AStrLen)) + idx1;
  idx3        := SignIdx(SubString(AStr, idx2 + 1, AStrLen)) + idx2;
  idx4        := SignIdx(SubString(AStr, idx3 + 1, AStrLen)) + idx3;
  idx5        := AStrLen;
  hDirection  := AStr[idx5];
  val1        := SubString(AStr, idx1, (idx2 - 1));
  val2        := SubString(AStr, idx2, (idx3 - 1));
  val3        := SubString(AStr, idx3, (idx4 - 1));    
  val4        := SubString(AStr, idx4, (idx5 - 1));
  if ((val1 = '+') or (val1 = '-')) then val1 := '0';
  if ((val2 = '+') or (val2 = '-')) then val2 := '0';
  if ((val3 = '+') or (val3 = '-')) then val3 := '0';
  if ((val4 = '+') or (val4 = '-')) then val4 := '0';
  x1           := LastPosX; //StartPunktX
  y1           := LastPosY; //StartPunktY
  cx           := StrToFloat(val3);  //MittelPunktX
  cy           := StrToFloat(val4);  //MittelPunktY
  x2           := StrToFloat(val1);  //+ x1; //EndPunktX
  y2           := StrToFloat(val2);  // + y1; //EndPunktY

  radius := sqrt(sqr(cx) + sqr(cy));

  //Koordinaten von Rechteck ermitteln.
  A.x :=  cx - radius; //round(cx - radius);
  A.y :=  cy - radius;
  B.x :=  cx + radius;
  B.y :=  cy + radius;

  C.x := 0;  //bogen Startpunkt.x
  C.y := 0;  //bogen Startpunkt.y
  D.x :=  x2;         //bogen EndPunkt.x
  D.y :=  y2;         //bogen EndPunkt.y

  if fRelativeProgram then
  begin
    cx  :=  cx  + LastPosX;
    cy  :=  cy  + LastPosY;
    A.x :=  A.x + LastPosX;
    A.y :=  A.y + LastPosY;
    B.x :=  B.x + LastPosX;
    B.y :=  B.y + LastPosY;
    C.x :=  C.x + LastPosX;
    C.y :=  C.y + LastPosY;
    D.x :=  D.x + LastPosX;
    D.y :=  D.y + LastPosY;
  end;
  a1 := GetAngleFromPoints(cx, cy, C.X, C.Y);
  a2 := GetAngleFromPoints(cx, cy, D.X, D.Y);
  if (CuttingON or fImportMoves) then
    DrawArc((fUnitFactor * A.x), (fUnitFactor * A.y), (fUnitFactor * B.x), (fUnitFactor * B.y), a1, a2, hDirection);
  LastPosX := D.X;
  LastPosY := D.Y;
end;


procedure TImportEssi.DrawArc(AX1, AY1, AX2, AY2, AStartAngle, AEndAngle: single; ADirection: string);
var Arc2D: TEllipticalArc2D;
begin
  if (AStartAngle = 0) then  AStartAngle := 0.001; //?????????
  if (AEndAngle = 0)   then  AEndAngle   := 0.001; //?????????
  Arc2D := TEllipticalArc2D.Create(-1, Point2D(AX1, AY1),  Point2D(AX2, AY2), AStartAngle, AEndAngle);
  //Arc2D.ReserveInt1 := fLastKerfInfo;
  if (ADirection = '-') then
    Arc2D.Direction := CLOCKWISE
  else
    Arc2D.Direction := COUNTERCLOCKWISE;
  SetPenPropertys(Arc2D);
  if   fShowDirection then Arc2D.ShowDirection := true
  else                     Arc2D.ShowDirection := false;
  //if (not CuttingON) then Arc2D.Layer := 250;
  if (not CuttingON) then
    fCADCmp2D.CurrentLayer := CuttingOFFLayer
  else
    fCADCmp2D.CurrentLayer := DefaultLayer;
  fCADCmp2D.AddObject(-1, Arc2D);
  AddTraceInfo(Arc2D.ID);
end;

procedure TImportEssi.FillLine(AStr: string);
var idx1, idx2, SignAnzahl, AStrLen: integer; val1, val2 : string;
    x1, y1, x2, y2: single;
begin
  AStrLen     := StrLen(PChar(AStr));
  idx1        := 1;
  idx2        := SignIdx(SubString(AStr, idx1 + 1, AStrLen)) + idx1;
  val1        := SubString(AStr, idx1, (idx2 - 1));
  val2        := SubString(AStr, idx2, AStrLen);
  if ((val1 = '+') or (val1 = '-')) then val1 := '0';
  if ((val2 = '+') or (val2 = '-')) then val2 := '0';

  x1           := LastPosX;
  y1           := LastPosY;

  if fRelativeProgram then
  begin
    x2           := StrToFloat(val1) + x1;
    y2           := StrToFloat(val2) + y1;
  end else
  begin
    x2           := StrToFloat(val1);
    y2           := StrToFloat(val2);
  end;

  if (CuttingON or fImportMoves) then
    DrawLine((x1*fUnitFactor),(y1*fUnitFactor),(x2*fUnitFactor),(y2*fUnitFactor));
  LastPosX := x2;
  LastPosY := y2;
end;

procedure TImportEssi.SetPenPropertys(AObject2D: TObject2D);
begin
  {if RapidMoveON then
    TPrimitive2D(AObject2D).PenStyle := RapidMoveONPenStyle
  else
    TPrimitive2D(AObject2D).PenStyle := RapidMoveOFFPenStyle;
  if CuttingON then
    TPrimitive2D(AObject2D).color := CuttingONPenColor
  else
    TPrimitive2D(AObject2D).color := CuttingOFFPenColor; }
end;

procedure TImportEssi.SetRapidMoveVisibility(AValue: boolean);
var i: integer; TmpPrimitive2D: TPrimitive2D;
begin
  fShowRapidMoves := AValue;
   fCADCmp2D.Layers.Layers[CuttingOFFLayer].Visible := fShowRapidMoves;
   //fCADCmp2D.RepaintViewports;
   //fCADCmp2D.RepaintViewports;
  {for i := 0 to fCADCmp2D.ObjectsCount - 1 do
  begin
    TObject2D(TmpPrimitive2D) := fCADCmp2D.GetObject(i);
    if (TmpPrimitive2D.Color = CuttingOFFPenColor) then
       TmpPrimitive2D.Visible := fShowRapidMoves;
  end;}
end;

procedure TImportEssi.SetDirectionVisibility(AValue: boolean);
var i: integer; TmpPrimitive2D: TPrimitive2D;
begin
  {fShowDirection := AValue;
  for i := 0 to fCADCmp2D.ObjectsCount - 1 do
  begin
    TObject2D(TmpPrimitive2D) := fCADCmp2D.GetObject(i);
    if (TmpPrimitive2D.Color = CuttingONPenColor) then
      if fShowDirection then
        TmpPrimitive2D.ArrowStyle := 1
      else
        TmpPrimitive2D.ArrowStyle := 0;
  end; }
end;

procedure TImportEssi.SwapPoints(var Ax1:double; var Ay1:double; var Ax2:double; var Ay2: double);
var tempX, tempY: double;
begin
  tempX := Ax1;
  tempY := Ay1;
  Ax1   := Ax2;
  Ay1   := Ay2;
  Ax2   := tempX;
  Ay2   := tempY;
end;

procedure TImportEssi.SwapAngle(var AAngle1:double; var AAngle2:double);
var tempAngle: double;
begin
  tempAngle := AAngle1;
  AAngle1   := AAngle2;
  AAngle2   := tempAngle;
end;

function TImportEssi.GetAngleFromPoints(Ax1, Ay1, Ax2, Ay2: double): double;
var x,y, angle: double;
begin
  x := Ax2 - Ax1;
  y := Ay2 - Ay1;
  angle := ArcTan2(y, x);
  
  if angle < 0 then
    angle := angle + (2 * pi);
  result := angle;
end;


end.
