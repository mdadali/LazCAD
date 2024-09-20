unit cImportGCODE2;

interface

uses  Classes, SysUtils, ComCtrls, Math, IniFiles,
      CS4BaseTypes,
      CS4Shapes,
      CADSys4,
      applicationh,
      camh;



const
  X_VALUE = 1;
  Y_VALUE = 2;
  I_VALUE = 3;
  J_VALUE = 4;

  COORDS_SEPARATOR  = ' ';


type

  TImportGCode2 = class
  private
    fLastKerfInfo: integer;
    fCADCmp2D: TCADCmp2D;
    fCADViewport2D: TCADViewport2D;
    fProgressBar: TProgressBar;

    fLastCommand: string;
    fInputList: TStringList;
    fUnitFactor: single;
    CommentOn  : boolean;
    fImportsRapidMove: boolean;
    fRelativeProgram: boolean;
    fArcCenterREL:  boolean;
    RapidMoveON: boolean;

    fStartX, fStartY: single;
    fDecseparator: string;

    LastX, LastY, LastZ: Double;

      fMachineName: string;
      fCommentTextFileName: string;
      fOutPutCommentText:boolean;
      fFooterTextFileName: string;
      fOutPutFooterText:boolean;
      fFooterTextAsComment: boolean;
      fOutputProgramStartCmd,
      fOutputProgramEndCmd: boolean;
      //commands
      fGCODECmdRapidMoveOFF,
      fGCODECmdRapidMoveON,
      fGCODECmdLine,
      fGCODECmdArcCW,
      fGCODECmdArcCCW,
      fGCODECmdKerfInsRight,
      fGCODECmdKerfInsLeft,
      fGCODECmdKerfOutsRight,
      fGCODECmdKerfOutsLeft : string;
      fGCODECmdToolDown: string;
      fGCODECmdToolUp: string;
      fGCODECmdKerfOFF: string;
      fGCODECmdCommentON: string;
      fGCODECmdCommentOFF: string;
      fGCODECmdProgramStart: string;
      fGCODECmdProgramEnd: string;
      fGCODECmdAsoulutON: string;
      fGCODECmdRelativeON: string;
      fGCODECmdSpeedCommand: string;


      fIniFile: TIniFile;
      fIniFileName: string; //machine


  public
    constructor create(ACADCmp2D: TCADCmp2D; ACADViewport2D: TCADViewport2D ;AProgressBar: TProgressBar);
    destructor  destroy;

    procedure ReadIniFile;
    procedure WriteIniFile;


    procedure LoadFromFile(AFile: string);
    procedure Import;
    function  AddLineToCAD(AX, AY, AZ: single): integer;


    procedure ProcessCommands(AStr: string);

    function EliminateComment(AStr: string): string;
    
    function ProcessKerf(AStr: string): boolean;
    function ProgressIncrementalON(AStr: string): boolean;
    function ProgressIncrementalOFF(AStr: string): boolean;

    function    ProgressRapidMove(AStr: string): boolean;
    function    ProgressLinearMove(AStr: string): boolean;
    function    ProgressCircularMoveCW(AStr: string): boolean;
    function    ProgressCircularMoveCCW(AStr: string): boolean;

    function  GetValue(AStr: string; ACoord: word): single;

    function  isRapidMove(AStr: string): boolean;
    function  isLinearMove(AStr: string): boolean;
    function  isCircularMoveCW(AStr: string): boolean;
    function  isCircularMoveCCW(AStr: string): boolean;

    function  GetAngleFromPoints(Ax1, Ay1, Ax2, Ay2: single): single;

  end;


implementation

procedure TImportGCode2.ReadIniFile;
begin
  fIniFile := TIniFile.Create(fIniFileName);
  //fMachineName               := fIniFile.ReadString('SETTINGS', 'MACHINE_NAME', fMachineName);
  //fCommentTextFileName       := fIniFile.ReadString('SETTINGS', 'COMMENT_TEXT_FILENAME', GetCommentTexFileName);
  //fOutPutCommentText         := fIniFile.ReadBool  ('SETTINGS', 'OUTPUT_COMMENT_TEXT', true);
  //fFooterTextFileName        := fIniFile.ReadString('SETTINGS', 'FOOTER_TEXT_FILENAME', GetFooterTexFileName);
  //fOutPutFooterText          := fIniFile.ReadBool  ('SETTINGS', 'OUTPUT_FOOTER_TEXT', true);
  //fFooterTextAsComment       := fIniFile.ReadBool  ('SETTINGS', 'FOOTER_TEXT_AS_COMMENT', true);

  //fOutputKerfCommand          := fIniFile.ReadBool  ('SETTINGS', 'OUTPUT_KERF_COMMAND', true);
  //fOutputKerfValue          := fIniFile.ReadBool  ('SETTINGS', 'OUTPUT_KERF_VALUE', true);
  //fKerfValue                := fIniFile.ReadFloat ('SETTINGS', 'KERF_VALUE', 1.2);
  fUnitFactor                 := fIniFile.ReadFloat ('SETTINGS', 'UNIT_FACTOR', 1);
  //fOutputProgramStartCmd      := fIniFile.ReadBool  ('SETTINGS', 'OUTPUT_PROGRAM_START', true);
  //fOutputProgramEndCmd        := fIniFile.ReadBool  ('SETTINGS', 'OUTPUT_PROGRAM_END', true);
  //fOutputSpeedCommand         := fIniFile.ReadBool  ('SETTINGS', 'OUTPUT_SPEED_COMMAND', true);
  fGCODECmdKerfInsRight       := fIniFile.ReadString('COMMANDS', 'KERF_INS_RIGHT', 'G42');
  fGCODECmdKerfInsLeft        := fIniFile.ReadString('COMMANDS', 'KERF_INS_LEFT', 'G41');
  fGCODECmdKerfOutsRight      := fIniFile.ReadString('COMMANDS', 'KERF_OUTS_RIGHT', 'G41');
  fGCODECmdKerfOutsLeft       := fIniFile.ReadString('COMMANDS', 'KERF_OUTS_LEFT', 'G42');
  fGCODECmdKerfOFF            := fIniFile.ReadString('COMMANDS', 'KERF_OFF', 'G40');

  fGCODECmdRapidMoveON        := fIniFile.ReadString('COMMANDS', 'RAPID_MOVE', 'G00');
  fGCODECmdRapidMoveOFF       := fIniFile.ReadString('COMMANDS', 'RAPID_MOVE_OFF', '');
  fGCODECmdLine               := fIniFile.ReadString('COMMANDS', 'LINE', 'G01');
  fGCODECmdArcCW              := fIniFile.ReadString('COMMANDS', 'ARC_CW', 'G02');
  fGCODECmdArcCCW             := fIniFile.ReadString('COMMANDS', 'ARC_CCW', 'G03');

  fGCODECmdRapidMoveOFF       := fIniFile.ReadString('COMMANDS', 'RAPID_MOVE_OFF', '');
  fGCODECmdToolDown           := fIniFile.ReadString('COMMANDS', 'TOOL_DOWN', 'M10');
  fGCODECmdToolUp             := fIniFile.ReadString('COMMANDS', 'TOOL_UP', 'M11');

  fGCODECmdCommentON          := fIniFile.ReadString('COMMANDS', 'COMMENT_ON', '{');
  fGCODECmdCommentOFF         := fIniFile.ReadString('COMMANDS', 'COMMENT_OFF', '}');
  fGCODECmdProgramStart       := fIniFile.ReadString('COMMANDS', 'PROGRAM_START', '%');
  fGCODECmdProgramEnd         := fIniFile.ReadString('COMMANDS', 'PROGRAM_END', 'M30');
  fGCODECmdAsoulutON          := fIniFile.ReadString('COMMANDS', 'ABSOULUT_ON ', 'G90');
  fGCODECmdRelativeON         := fIniFile.ReadString('COMMANDS', 'RELATIVE_ON ', 'G91');
  fGCODECmdSpeedCommand       := fIniFile.ReadString('COMMANDS', 'SPEED_COMMAND', 'F');
  //fSpeedValue                 := fIniFile.ReadString('SETTINGS', 'SPEED_VALUE', '200');
  //fOutputSpeedCommand         := fIniFile.ReadBool('SETTINGS',   'OUTPUT_SPEED_COMMAND', true);
  fRelativeProgram            := fIniFile.ReadBool('SETTINGS',   'RELATIVPROGRAM', true);

  fStartX := fIniFile.ReadFloat ('SETTINGS', 'START_X', 0);
  fStartY := fIniFile.ReadFloat ('SETTINGS', 'START_Y', 0);

  //BlockFigures   := fIniFile.ReadBool('SETTINGS',   'BLOCK_FIGURES', true);
  //LineNumON      := fIniFile.ReadBool('SETTINGS',   'LINE_NUM_ON', true);
  //LineNumStart   := fIniFile.ReadInteger('SETTINGS',   'LINE_NUM_START', 100);
  //LineNumStep    := fIniFile.ReadInteger('SETTINGS',   'LINE_NUM_STEP', 10);
  //LineNumPrefix  := fIniFile.ReadString('SETTINGS',   'LINE_NUM_PREFIX', 'N');
  fDecseparator  := fIniFile.ReadString('SETTINGS',   'DECIMAL_SEPERATOR', ',');
  fIniFile.Free;
end;

procedure TImportGCode2.WriteIniFile;
begin

end;

constructor  TImportGCode2.create(ACADCmp2D: TCADCmp2D; ACADViewport2D: TCADViewport2D ;AProgressBar: TProgressBar);
begin
  inherited create;
  fCADCmp2D              := ACADCmp2D;
  fCADViewport2D         := ACADViewport2D;
  fProgressBar           := AProgressBar;
  fInputList             := TStringList.Create;
  fRelativeProgram       := true;
  fArcCenterREL          := false;
  fUnitFactor            := 1;
  fImportsRapidMove      := false;
  CommentOn              := false;
  LastX                  := 0;
  LastY                  := 0;
  fDecseparator          := ',';
  fLastKerfInfo := 0;
  ReadIniFile;
end;

destructor  TImportGCode2.destroy;
begin
  fInputList.Free;
  inherited free;
end;

procedure TImportGCode2.LoadFromFile(AFile: string);
begin
  fInputList.LoadFromFile(AFile);
end;

function TImportGCode2.AddLineToCAD(AX, AY, AZ: single): integer;
var TmpLine2D: TLine2D; P0, P1: TPoint2D;
begin
  P0.X := LastX;
  P0.Y := LastY;
  P0.W := 1;
  P1.X := AX;
  P1.Y := AY;
  P1.W := 1;
  TmpLine2D := TLine2D.Create(-1, P0, P1);
  //TmpLine2D.ReserveInt1 := fLastKerfInfo;
  fCADCmp2D.AddObject(-1, TmpLine2D);
  fCADCmp2D.RepaintViewports;
  result :=  TmpLine2D.ID;
end;

function TImportGCode2.EliminateComment(AStr: string): string;
begin
  result := AStr;
end;

procedure TImportGCode2.Import;
var i: Integer;  hStr: string;   TmpDecseparator: string;
begin
  TmpDecseparator := ' ';
  TmpDecseparator[1] := Decimalseparator;
  Decimalseparator := fDecseparator[1];

  fProgressBar.Position := 0;
  fProgressBar.Min      := 0;
  fProgressBar.Max      := fInputList.Count -1;
  fProgressBar.Step     := 1;
  for i := 0 to fInputList.Count - 1 do
  begin
    if not CommentOn then
      ProcessCommands(UpperCase(fInputList[i]));
    fProgressBar.StepIt;
  end;
  fProgressBar.Position := 0;
  Decimalseparator := TmpDecseparator[1];
end;

procedure TImportGCode2.ProcessCommands(AStr: string);
begin
  if not ProcessKerf(AStr) then
    if not ProgressIncrementalON(AStr) then
      if not ProgressIncrementalOFF(AStr) then
        if not ProgressLinearMove(AStr) then
          if not ProgressCircularMoveCW(AStr) then
            if not ProgressCircularMoveCCW(AStr) then
              if not ProgressRapidMove(AStr) then;
end;

function TImportGCode2.ProcessKerf(AStr: string): boolean;
begin
  result := false;
  if (Pos(UpperCase(fGCODECmdKerfOFF), UpperCase(AStr)) > 0) then
  begin
    fLastKerfInfo := ord(None);
    fLastCommand  := fGCODECmdKerfOFF;
    result := true;
    exit;
  end;
  if (Pos(UpperCase(fGCODECmdKerfInsRight), UpperCase(AStr)) > 0) then
  begin
    fLastKerfInfo := ord(InnerContourCW);
    fLastCommand  := fGCODECmdKerfInsRight;
    result := true;
    exit;
  end;
  if (Pos(UpperCase(fGCODECmdKerfInsLeft), UpperCase(AStr)) > 0) then
  begin
    fLastKerfInfo := ord(InnerContourCCW);
    fLastCommand  := fGCODECmdKerfInsLeft;
    result := true;
    exit;
  end;
  if (Pos(UpperCase(fGCODECmdKerfOutsRight), UpperCase(AStr)) > 0) then
  begin
    fLastKerfInfo := ord(OuterContourCW);
    fLastCommand  := fGCODECmdKerfOutsRight;
    result := true;
    exit;
  end;
  if (Pos(UpperCase(fGCODECmdKerfOutsLeft), UpperCase(AStr)) > 0) then
  begin
    fLastKerfInfo := ord(OuterContourCCW);
    fLastCommand  := fGCODECmdKerfOutsLeft;
    result := true;
    exit;
  end;
end;

function TImportGCode2.ProgressIncrementalON(AStr: string): boolean;
begin
  if Pos(UpperCase(fGCODECmdRelativeON), AStr) > 0 then
  begin
    fRelativeProgram := true;
    fLastCommand := fGCODECmdRelativeON;
    result := true;
  end else
    result := false;
end;

function TImportGCode2.ProgressIncrementalOFF(AStr: string): boolean;
begin
  if Pos(UpperCase(fGCODECmdAsoulutON), AStr) > 0 then
  begin
    fRelativeProgram := false;
    fLastCommand := fGCODECmdAsoulutON;
    result := true;
  end else
    result := false;
end;

function  TImportGCode2.ProgressRapidMove(AStr: string): boolean;
var XValue, YValue: single;
begin
  result := false;
  if isRapidMove(AStr) then
  begin
    XValue := GetValue(AStr, X_VALUE);
    YValue := GetValue(AStr, Y_VALUE);
    if (XValue = INVALID_FLOAT_VALUE) and (YValue = INVALID_FLOAT_VALUE)  then exit;
    if fRelativeProgram then
    begin
      if (XValue = INVALID_FLOAT_VALUE) then  XValue := LastX
      else XValue := XValue + LastX;
      if (YValue = INVALID_FLOAT_VALUE) then  YValue := LastY
      else YValue := YValue + LastY;
    end else
    begin
      if (XValue = INVALID_FLOAT_VALUE) then  XValue := LastX;
      if (YValue = INVALID_FLOAT_VALUE) then  YValue := LastY;
    end;
    if (XValue = LastX) and (YValue = LastY) then exit;
    //I_DrawLine(LastX, LastY, XValue, YValue);
    LastX := XValue; LastY := YValue;
    fLastCommand := fGCODECmdRapidMoveON;
    result := true;
  end;
end;

function  TImportGCode2.ProgressLinearMove(AStr: string): boolean;
var XValue, YValue: single; Line2D: TLine2D;
begin
  result := false;
  if isLinearMove(AStr) then
  begin
    XValue := GetValue(AStr, X_VALUE);
    YValue := GetValue(AStr, Y_VALUE);
    if (XValue = INVALID_FLOAT_VALUE) and (YValue = INVALID_FLOAT_VALUE)  then exit;
    if fRelativeProgram then
    begin
      if (XValue = INVALID_FLOAT_VALUE) then  XValue := LastX
      else XValue := XValue + LastX;
      if (YValue = INVALID_FLOAT_VALUE) then  YValue := LastY
      else YValue := YValue + LastY;
    end else
    begin
      if (XValue = INVALID_FLOAT_VALUE) then  XValue := LastX;
      if (YValue = INVALID_FLOAT_VALUE) then  YValue := LastY;
    end;
    if (XValue = LastX) and (YValue = LastY) then exit;
    Line2D := TLine2D.Create(-1, Point2D(LastX, LastY), Point2D(XValue, YValue));
    fCADCmp2D.AddObject(-1, Line2D);
    //I_DrawLine(LastX, LastY, XValue, YValue);
    LastX := XValue; LastY := YValue;
    fLastCommand := fGCODECmdLine;
    result := true;
  end;
end;

function  TImportGCode2.ProgressCircularMoveCW(AStr: string): boolean;
var sx, sy, ex, ey, cx, cy, a1, a2, radius: single;
A, B: TPoint2D;  Arc2D: TEllipticalArc2D;
begin
  result := false;
  if isCircularMoveCW(AStr) then
  begin
    sx := LastX;
    sy := LastY;
    ex := GetValue(AStr, X_VALUE);
    ey := GetValue(AStr, Y_VALUE);
    cx := GetValue(AStr, I_VALUE);
    cy := GetValue(AStr, J_VALUE);
    
    if ((ex = INVALID_FLOAT_VALUE) and (ey = INVALID_FLOAT_VALUE) and
        (cx = INVALID_FLOAT_VALUE) and (cy = INVALID_FLOAT_VALUE)) then exit;

    if (ex = INVALID_FLOAT_VALUE) then  ex := LastX;
    if (ey = INVALID_FLOAT_VALUE) then  ey := LastY;
    if (cx = INVALID_FLOAT_VALUE) then  cx := LastX;
    if (cy = INVALID_FLOAT_VALUE) then  cy := LastY;

    radius := sqrt(sqr(cx) + sqr(cy));
    //Koordinaten von Rechteck ermitteln.
    A.x :=  cx - radius; //round(cx - radius);
    A.y :=  cy - radius;
    A.W :=  1;
    B.x :=  cx + radius;
    B.y :=  cy + radius;
    B.W :=  1;
    
    if fRelativeProgram then
    begin
      A.x := A.x + LastX;
      A.y := A.y + LastY;
      B.x := B.x + LastX;
      B.y := B.y + LastY;

      if (ex <> LastX) then ex := ex + LastX;
      if (ey <> LastY) then ey := ey + LastY;

      if (cx <> LastX) then cx := cx + LastX;
      if (cy <> LastY) then cy := cy + LastY;
    end else
    if fArcCenterREL then
    begin
      if (cx <> LastX) then cx := cx + LastX;
      if (cy <> LastY) then cy := cy + LastY;
    end;

    a1 := GetAngleFromPoints(cx, cy, sx, sy);
    a2 := GetAngleFromPoints(cx, cy, ex, ey);
    if (a1 = 0) then  a1 := 0.001; //?????????
    if (a2 = 0) then  a2 := 0.001; //?????????

    Arc2D := TEllipticalArc2D.Create(-1, A, B, a1, a2);
    //Arc2D.ReserveInt1 := fLastKerfInfo;
    Arc2D.Direction := CLOCKWISE;
    fCADCmp2D.AddObject(-1, Arc2D);

    LastX := ex; LastY := ey;
    fLastCommand := fGCODECmdArcCW;
    result := true;
  end;
end;

function  TImportGCode2.ProgressCircularMoveCCW(AStr: string): boolean;
var sx, sy, ex, ey, cx, cy, a1, a2, radius: single;
A, B: TPoint2D;  Arc2D: TEllipticalArc2D;
begin
  result := false;
  if isCircularMoveCCW(AStr) then
  begin
    sx := LastX;
    sy := LastY;
    ex := GetValue(AStr, X_VALUE);
    ey := GetValue(AStr, Y_VALUE);
    cx := GetValue(AStr, I_VALUE);
    cy := GetValue(AStr, J_VALUE);

    if ((ex = INVALID_FLOAT_VALUE) and (ey = INVALID_FLOAT_VALUE) and
        (cx = INVALID_FLOAT_VALUE) and (cy = INVALID_FLOAT_VALUE)) then exit;

    if (ex = INVALID_FLOAT_VALUE) then  ex := LastX;
    if (ey = INVALID_FLOAT_VALUE) then  ey := LastY;
    if (cx = INVALID_FLOAT_VALUE) then  cx := LastX;
    if (cy = INVALID_FLOAT_VALUE) then  cy := LastY;

    radius := sqrt(sqr(cx) + sqr(cy));
    //Koordinaten von Rechteck ermitteln.
    A.x :=  cx - radius; //round(cx - radius);
    A.y :=  cy - radius;
    A.W :=  1;
    B.x :=  cx + radius;
    B.y :=  cy + radius;
    B.W :=  1;
    
    if fRelativeProgram then
    begin
      A.x := A.x + LastX;
      A.y := A.y + LastY;
      B.x := B.x + LastX;
      B.y := B.y + LastY;

      if (ex <> LastX) then ex := ex + LastX;
      if (ey <> LastY) then ey := ey + LastY;

      if (cx <> LastX) then cx := cx + LastX;
      if (cy <> LastY) then cy := cy + LastY;
    end else
    if fArcCenterREL then
    begin
      if (cx <> LastX) then cx := cx + LastX;
      if (cy <> LastY) then cy := cy + LastY;
    end;

    a1 := GetAngleFromPoints(cx, cy, sx, sy);
    a2 := GetAngleFromPoints(cx, cy, ex, ey);
    if (a1 = 0) then  a1 := 0.001; //?????????
    if (a2 = 0) then  a2 := 0.001; //?????????


    Arc2D := TEllipticalArc2D.Create(-1, A, B, a1, a2);
    Arc2D.Direction := CounterClockwise;
    fCADCmp2D.AddObject(-1, Arc2D);

    LastX := ex; LastY := ey;
    fLastCommand := fGCODECmdArcCCW;
    result := true;
  end;
end;

function  TImportGCode2.GetValue(AStr: string; ACoord: word): single;
var i, CoordIdx, ValueStartIdx, ValueEndIdx: integer; hChar: char;
hStr: string;
begin
  hStr := '';
  result := INVALID_FLOAT_VALUE;
  case ACoord of
    X_VALUE: CoordIdx := Pos('X', AStr);
    Y_VALUE: CoordIdx := Pos('Y', AStr);
    I_VALUE: CoordIdx := Pos('I', AStr);
    J_VALUE: CoordIdx := Pos('J', AStr);
  end;
  if (CoordIdx = 0) then exit;
  ValueStartIdx := CoordIdx + 1;
  i := ValueStartIdx;
  hChar := AStr[i];
  while ((i < Length(AStr)) and (hChar <> COORDS_SEPARATOR))  do
  begin
    hStr := hStr +  hChar;
    inc(i);
    hChar := AStr[i];
  end;
  result := StrToFloatDef(hStr, INVALID_FLOAT_VALUE);
end;

function  TImportGCode2.isRapidMove(AStr: string): boolean;
begin
  result := (Pos(UpperCase(fGCODECmdRapidMoveON), AStr) > 0) or

            (
                (fLastCommand = fGCODECmdRapidMoveON)           and
                (Pos(UpperCase(fGCODECmdLine), AStr)      = 0)  and
                (Pos(UpperCase(fGCODECmdArcCW), AStr)     = 0)  and
                (Pos(UpperCase(fGCODECmdArcCCW), AStr)    = 0)
            );
end;

function  TImportGCode2.isLinearMove(AStr: string): boolean;
begin
  result := (Pos(UpperCase(fGCODECmdLine), AStr) > 0) or

            (
                (fLastCommand = fGCODECmdLine)                   and
                (Pos(UpperCase(fGCODECmdRapidMoveON), AStr) = 0) and
                (Pos(UpperCase(fGCODECmdArcCW), AStr)   = 0)     and
                (Pos(UpperCase(fGCODECmdArcCCW), AStr)  = 0)
            );
end;

function  TImportGCode2.isCircularMoveCW(AStr: string): boolean;
begin
  result := (Pos(UpperCase(fGCODECmdArcCW), AStr) > 0) or

            (
                (fLastCommand = fGCODECmdArcCW)                  and
                (Pos(UpperCase(fGCODECmdRapidMoveON), AStr) = 0) and
                (Pos(UpperCase(fGCODECmdLine), AStr)        = 0) and
                (Pos(UpperCase(fGCODECmdArcCCW), AStr)      = 0)
            );
end;


function  TImportGCode2.isCircularMoveCCW(AStr: string): boolean;
begin
  result := (Pos(UpperCase(fGCODECmdArcCCW), AStr) > 0) or

            (
                (fLastCommand = fGCODECmdArcCCW)                  and
                (Pos(UpperCase(fGCODECmdRapidMoveON), AStr) = 0)  and
                (Pos(UpperCase(fGCODECmdLine), AStr)        = 0)  and
                (Pos(UpperCase(fGCODECmdArcCW), AStr)       = 0)
            );
end;

function TImportGCode2.GetAngleFromPoints(Ax1, Ay1, Ax2, Ay2: single): single;
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
