unit mEssiExport;

interface

uses Classes, SysUtils, Dialogs, fCadIntrf2D, tools;

var
  Output: TStringList;
  DestFileName,
  tmpOutputFileName: string;
  LastKerfInfo: string;
  KerfON,
  ProgramKerf: boolean;
  UnitFactor: single;
  Kerf : single;
  NewLine: string;
  CurrLineCount,
  //DIN
  LineNumStart,
  LineNumStep : integer;
  LineNumPrefix: string;
  RelativeProgram,
  Comment: boolean;
  
  const MIN_FIGURE_DISTANCE = 0.09;

        CAM_KERF_INS_RIGHT_STR  = 'KERF_INS_RIGHT';
        CAM_KERF_INS_LEFT_STR   = 'KERF_INS_LEFT';
        CAM_KERF_OUTS_RIGHT_STR = 'KERF_OUTS_RIGHT';
        CAM_KERF_OUTS_LEFT_STR  = 'KERF_OUTS_LEFT';

        CAM_LEFTDIRECTION   = 0;
        CAM_RIGHTDIRECTION  = 1;

        
implementation

procedure GenerateGlobalData;
var i, FiguresCount: integer; hKerfInfo: string;
begin
  LastKerfInfo := ''; //CAM_DEFAULT_KERF_STR;
  ProgramStart;
  FiguresCount := I_GetFiguresCount;
  I_ProgressBarReset(0, FiguresCount, 1);
  for i := 0 to FiguresCount - 1  do
  begin
    if IsCounturStart(i) then
    begin
      CreateRapidMove(i);
      LastKerfInfo := ''; //CAM_DEFAULT_KERF_STR;
      hKerfInfo := I_GetFigureKerfInfo(i);
      ProcessCounturStart(hKerfInfo, LastKerfInfo);
      LastKerfInfo :=  hKerfInfo;
    end;
    ProcessFigure(i);
    if IsCounturEnd(i) then
      ProcessCounturEnd;
    I_ProgressBarStep;
  end;
  I_ProgressBarReset(0, FiguresCount, 1);
  ProgramEnd;
end;

procedure CreateRapidMove(AIndex: integer);
var spx, spy, epx, epy: single; hDirection: word; hNextDirection, hSide:word;
begin
  epx := I_GetFigureSP_X(AIndex);
  epy := I_GetFigureSP_Y(AIndex);
  if (AIndex = 0) then
  begin
     spx := 0;
     spy := 0;
  end else
  begin
    spx := I_GetFigureEP_X(AIndex - 1);
    spy := I_GetFigureEP_Y(AIndex - 1);
  end;
  if   ((abs(spx - epx) > MIN_FIGURE_DISTANCE) or (abs(spy - epy) > MIN_FIGURE_DISTANCE)) then
    RapidMove(spx, spy, epx, epy);
end;

function ProcessCounturStart(AKerfInfo, ALastKerfInfo: string): string;
var hRapidMoveOFF, hRapidMove, hKerf, hToolDown: string;
begin
  hRapidMoveOFF :=  RapidMoveOFF;
  if (hRapidMoveOFF <> '') then
  begin
    CurrLineCount := CurrLineCount + 1;
    output.Add(hRapidMoveOFF);
    WriteLn(hRapidMoveOFF);
  end;
  ProcessKerf(AKerfInfo, ALastKerfInfo);
  hToolDown := ToolDown;
  if (hToolDown <> '') then
  begin
    CurrLineCount := CurrLineCount + 1;
    output.Add(hToolDown);
    WriteLn(hToolDown);
    //output.Add(GetNewLineNum  + ' ' + hToolDown);
  end;
end;

procedure ProcessFigure(AIndex: integer);
var hKerfInfo, hClassName: string;  hArcDirection: word;
spx, spy, epx, epy, cpx, cpy: single;
begin
  spx  := I_GetFigureSP_X(Aindex);
  spy  := I_GetFigureSP_Y(Aindex);
  epx  := I_GetFigureEP_X(Aindex);
  epy  := I_GetFigureEP_Y(Aindex);

  hClassName  :=  I_GetFigureClassName(AIndex);
  hKerfInfo   :=  I_GetFigureKerfInfo(AIndex);
  if (hClassName = 'TLine2D') then
  begin
    ProcessKerf(hKerfInfo, LastKerfInfo);
    ProcessLine(spx, spy, epx, epy);
    LastKerfInfo :=  hKerfInfo;
  end;
  if (hClassName = 'TArc2D') then
  begin
    ProcessKerf(hKerfInfo, LastKerfInfo);
    LastKerfInfo :=  hKerfInfo;
    cpx  := I_GetFigureCP_X(Aindex);
    cpy  := I_GetFigureCP_Y(Aindex);
    hArcDirection := I_GetFigureDirection(Aindex);
    ProcessArc(cpx, cpy, spx, spy, epx, epy, hArcDirection);
  end;
  if (hClassName = 'TCircle2D') then
  begin
    {ProcessKerf(hKerfInfo, LastKerfInfo);
    LastKerfInfo :=  hKerfInfo;
    cpx  := CAD.FigureCP_X[Aindex];
    cpy  := CAD.FigureCP_Y[Aindex];
    ProcessKerf(hKerfInfo, LastKerfInfo);
    //ProcessCircle(cpx, cpy, spx, spy, epx, epy);}
  end;
end;

procedure ProcessCircle;
begin

end;

function IsCounturStart(AFigureIndex: integer): boolean;
var hRes: boolean;
begin
  hRes := false;
  if (AFigureIndex = 0) then
     hRes := true
  else
    if (not I_CombinedFigures(AFigureIndex, AFigureIndex-1)) then
      hRes := true;
  result := hRes;
end;

function IsCounturEnd(AFigureIndex: integer): boolean;
var hRes: boolean; FiguresCount: integer;
begin
  hRes := false;
  FiguresCount := I_GetFiguresCount;
  if (AFigureIndex = FiguresCount - 1) then
    hRes := true
  else
    if (not I_CombinedFigures(AFigureIndex+1, AFigureIndex)) then
      hRes := true;
  result := hRes;
end;

function GetDecimalSeparator: string;
begin
  result := '.';
end;

function GetNewLineNum: string;
var hStr: string;
begin
  hStr := 'N' +  IntToStr(((CurrLineCount - 1) *  LineNumStep) + LineNumStart);
  result := hStr;
end;

function CommentON: string;
begin
  result := '3';
end;

function CommentOFF: string;
begin
  result := '4';
end;

function CommentText: string;
var hStr: string;
begin
  {CurrLineCount := CurrLineCount + 1;
  hStr   := GetNewLineNum        + ' ' + CommentON + 'PostProcessor-Name:     '  + PPDesc   + CommentOFF + NewLine;
  CurrLineCount := CurrLineCount + 1;
  hStr   := hStr + GetNewLineNum + ' ' + CommentON + 'PostProcessor-FileName: '  + PPFileName + CommentOFF + NewLine;
  CurrLineCount := CurrLineCount + 1;
  hStr   := hStr + GetNewLineNum + ' ' + CommentON + 'Source-FileName:        '  + SourceFileName + CommentOFF + NewLine;
  CurrLineCount := CurrLineCount + 1;
  hStr   := hStr + GetNewLineNum + ' ' + CommentON + 'Output-FileName:        '  + DestFileName+ CommentOFF + NewLine;
  CurrLineCount := CurrLineCount + 1;
  hStr   := hStr + GetNewLineNum + ' ' + CommentON + 'PostProcessor-Author:   '  + PPAuthor + CommentOFF + NewLine;
  CurrLineCount := CurrLineCount + 1;
  hStr   := hStr + GetNewLineNum + ' ' + CommentON + 'Date:                   '  + DateTimeToStr(now)  + CommentOFF;
  result := hStr; }
end;


function ProgramStart: string;
var hStr: string;
begin
  if (Comment = true) then
  begin
    hStr  := CommentText;
    output.Add(hStr);
    WriteLn(hStr);
  end;
end;

function RapidMoveON: string;
begin
  result := '5';
end;

function LineON: string;
begin
  result := '';
end;

function CWArcON: string;
begin
  result := '';
end;

function CCWArcON: string;
begin
  result := '';
end;

function RapidMoveOFF: string;
begin
  result := '6';
end;

function ToolDown: string;
begin
  result := '7';
end;

function ToolUp: string;
begin
  result := '8';
end;

function KerfOFF: string;
begin
  result := '38';
end;

function InsRightKerf: string;
begin
  result:= 'InsRight';
end;

function InsLeftKerf: string;
begin
  result:= 'InsLeft';
end;

function OutsRightKerf: string;
begin
  result:= 'OutsRight';
end;

function OutsLeftKerf: string;
begin
  result:= 'OutsLeft';
end;

procedure RapidMove(AStartPointX, AStartPointY, AEndPointX, AEndPointY: single);
var hEndPointX, hEndPointY: single;  hStr: string; hLineNum: string;
begin
 output.Add(RapidMoveON);
 WriteLn(RapidMoveON);
 ProcessLine(AStartPointX, AStartPointY, AEndPointX, AEndPointY);
end;

procedure ProcessKerf(AKerfInfo, ALastKerfInfo: word);
var hStr: string;
begin
  hStr := '';
  if  (AKerfInfo <> ALastKerfInfo) then
  begin
    if KerfON then
    begin
      if (AKerfInfo = CAM_KERF_INS_RIGHT_STR)       then
        hStr := InsRightKerf
      else if (AKerfInfo = CAM_KERF_INS_LEFT_STR)   then
         hStr := InsLeftKerf
      else if (AKerfInfo = CAM_KERF_OUTS_RIGHT_STR) then
        hStr := OutsRightKerf
      else if (AKerfInfo = CAM_KERF_OUTS_LEFT_STR)  then
        hStr := OutsLeftKerf
      else
        hStr := OutsLeftKerf;
      if ProgramKerf then
        hStr := hStr + '+' + FloatToStr(UnitFactor * Kerf);
    end;
  end;
  if hStr <> '' then
  begin
    output.Add(hStr);
    WriteLn(hStr);
  end;
end;

function ProcessCounturEnd: string;
var hToolUp, hKerfOFF, hLineNum: string;
begin
  hToolUp  := ToolUp;
  hKerfOFF := KerfOFF;

  if (hToolUp <> '') then
  begin
    CurrLineCount := CurrLineCount + 1;
    //output.Add(GetNewLineNum + ' ' + hToolUp);
    output.Add(hToolUp);
    WriteLn(hToolUp);
  end;
  if (hKerfOFF <> '') then
  begin
    CurrLineCount := CurrLineCount + 1;
    //output.Add(GetNewLineNum + ' ' + hKerfOFF);
    output.Add(hKerfOFF);
    WriteLn(hKerfOFF);
  end;
end;

function ProcessLine(AStartPointX, AStartPointY, AEndPointX, AEndPointY: single): string;
var xstr, ystr, hLine: string; x, y: single;
begin
  x := round(UnitFactor  * (AEndPointX - AStartPointX));
  if (x = 0) then xStr := '+'
  else begin
    xstr := FloatTostr(x);
    if (x > 0) then
      xstr := '+' + xStr;
  end;
  y := round(UnitFactor * (AEndPointY - AStartPointY));
  if (y = 0) then yStr := '+'
  else begin
    ystr := FloatTostr(y);
    if (y > 0) then
      ystr := '+' + yStr;
  end;
  hLine  := xStr+yStr;
  output.Add(hLine);
  WriteLn(hLine);
end;

function ProcessArc(ACenterPointX, ACenterPointY, AStartPointX, AStartPointY, AEndPointX, AEndPointY: single; ADirection: word): string;
var Ex, Ey, Cx, Cy: single;  ExStr, EyStr, CxStr, CyStr, hStr: string; hDirection: string;
begin
   if (ADirection = CAM_LEFTDIRECTION) then  //CCW
     hDirection := '-'
   else
     hDirection := '+';      //CCW
   Ex := round(UnitFactor * (AEndPointX - AStartPointX));
   Ey := round(UnitFactor * (AEndPointY - AStartPointY));
   Cx := round(UnitFactor * (ACenterPointX - AStartPointX));
   Cy := round(UnitFactor * (ACenterPointY - AStartPointY));

  if (Ex = 0) then ExStr := '+'
  else begin
    ExStr := FloatTostr(Ex);
    if (Ex > 0) then
      ExStr := '+' + ExStr;
  end;

  //Ey :=  Ey * -1;   //spiegeln
  if (Ey = 0) then EyStr := '+'
  else begin
    EyStr := FloatTostr(Ey);
    if (Ey > 0) then
      EyStr := '+' + EyStr;
  end;

  if (Cx = 0) then CxStr := '+'
  else begin
    CxStr := FloatTostr(Cx);
    if (Cx > 0) then
      CxStr := '+' + CxStr;
  end;

  //Cy :=  Cy * -1;   //spiegeln
  if (Cy = 0) then CyStr := '+'
  else begin
    CyStr := FloatTostr(Cy);
    if (Cy > 0) then
      CyStr := '+' + CyStr;
  end;
  hStr := ExStr +  EyStr + CxStr +  CyStr + hDirection;
  output.Add(hStr);
  WriteLn(hStr);
end;

function ProgramEnd: string;
begin
  CurrLineCount := CurrLineCount + 1;
  //output.Add(GetNewLineNum + ' ' + '0');
  output.Add('0');
  WriteLn('0');
end;

procedure init(AOtputFileName: string);
begin
    output         := TStringList.Create;
    Comment        := false;
    UnitFactor     := 10;
    KerfON         := true;
    ProgramKerf    := false;
    Kerf           := 1.2;
    NewLine        := chr(13) + chr(10);
    CurrLineCount  := 0;
  //DIN
    LineNumStart  := 100;
    LineNumStep   := 10;
    LineNumPrefix := 'N';
    RelativeProgram := true;
    GenerateGlobalData;
    output.SaveToFile(AOtputFileName);
    output.free;
end;

var SaveDialog1: TSaveDialog;

begin
  try
    SaveDialog1    := TSaveDialog.Create(nil);
    SaveDialog1.Title := 'Export As Essi';
    if SaveDialog1.Execute then
      init(SaveDialog1.FileName);
  finally
     SaveDialog1.Free;
  end;
end.
