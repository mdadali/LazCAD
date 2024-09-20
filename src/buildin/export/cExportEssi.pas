unit cExportEssi;

interface

uses Classes, SysUtils, IniFiles,
     CS4BaseTypes,
     CS4Shapes,
     CADSys4,
     fCiArc_CSE,
     fCircle2D_CPR,
     fCadIntrf2D,
     tools,
     applicationh,
     camh;

type

  TExportEssi = class
    private
      //settings
      fMachineName: string;
      fCommentTextFileName: string;
      fOutPutCommentText:boolean;
      fFooterTextFileName: string;
      fOutPutFooterText:boolean;
      fFooterTextAsComment: boolean;
      fOutputKerfCommand: boolean;
      fOutputKerfValue: boolean;
      fKerfValue : single;
      fUnitFactor: single;
      fOutputProgramStartCmd,
      fOutputProgramEndCmd: boolean;
      fStartX,
      fStartY: single;
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
      fESSICmdProgramStart: string;
      fESSICmdProgramEnd: string;
      fESSICmdAsoulutON: string;
      fESSICmdRelativeON: string;

      fIniFile: TIniFile;
      fIniFileName: string; //machine
      Output: TStringList;
      fOutputFileName,
      tmpOutputFileName: string;
      LastKerfInfo: TKerfInfo;
      fKerfON: boolean;
      NewLine: string;
      CurrLineCount,
      //DIN
      LineNumStart,
      LineNumStep : integer;
      LineNumPrefix: string;
      fRelativeProgram,
      Comment: boolean;
      procedure GenerateGlobalData;
      procedure CreateRapidMove(AIndex: integer);
      function  ProcessCounturStart(AKerfInfo, ALastKerfInfo: TKerfInfo): string;
      procedure ProcessFigure(AIndex: integer);
      procedure ProcessCircle;
      function IsCounturStart(AFigureIndex: integer): boolean;
      function IsCounturEnd(AFigureIndex: integer): boolean;
      function GetDecimalSeparator: string;
      function GetNewLineNum: string;
      function LineON: string;
      function GetStdCommentTexFileName: string;
      function GetCommentTexFileName: string;
      function GetStdFooterTexFileName: string;
      function GetFooterTexFileName: string;
      procedure CheckStdCommentTexFile;
      procedure CheckCommentTexFile;
      procedure CheckStdFooterTexFile;
      procedure CheckFooterTexFile;
      function ProgramStart: string;
      function CWArcON: string;
      function CCWArcON: string;
      procedure RapidMove(AStartPointX, AStartPointY, AEndPointX, AEndPointY: single);
      procedure ProcessKerf(AKerfInfo, ALastKerfInfo: TKerfInfo);
      function ProcessCounturEnd: string;
      function ProcessLine(AStartPointX, AStartPointY, AEndPointX, AEndPointY: single): string;
      function ProcessArc(ACenterPointX, ACenterPointY, AStartPointX, AStartPointY, AEndPointX, AEndPointY: single; ADirection: word): string;
      function ProgramEnd: string;
      procedure init;
      procedure ReadIniFile;
      procedure WriteIniFile;
    public
      constructor create(AMachineFileName: string);
      procedure   SaveToFile(AFileName: string);
      procedure   SaveToStringList(AStringList: TStringList);
      destructor  destroy; override;
  end;

implementation

constructor TExportEssi.create(AMachineFileName: string);
begin
  inherited create;
  if ((AMachineFileName = '') or (AMachineFileName = ' ')) then
    fIniFileName  := GetAppStdESSIOutputMachineFileName
  else
    fIniFileName := AMachineFileName;
  init;
end;

destructor TExportEssi.destroy;
begin
  WriteIniFile;
  fIniFile.Free;
  output.free;
  inherited;
end;

procedure TExportEssi.init;
begin
  fMachineName := GetAppOnlyFileName(fIniFileName);
  fIniFile := TIniFile.Create(fIniFileName);
  ReadIniFile;
  CheckStdCommentTexFile;
  CheckCommentTexFile;
  CheckStdFooterTexFile;
  CheckFooterTexFile;
  output         := TStringList.Create;
  fKerfOn        := true;
  Comment        := false;
  NewLine        := chr(13) + chr(10);
  CurrLineCount  := 0;
  //DIN
  LineNumStart  := 100;
  LineNumStep   := 10;
  LineNumPrefix := 'N';
  //fRelativeProgram := false;
  I_Regen;
  GenerateGlobalData;
end;

procedure TExportEssi.ReadIniFile;
begin
  fMachineName               := fIniFile.ReadString('SETTINGS', 'MACHINE_NAME', fMachineName);
  fCommentTextFileName       := fIniFile.ReadString('SETTINGS', 'COMMENT_TEXT_FILENAME', GetCommentTexFileName);
  fOutPutCommentText         := fIniFile.ReadBool  ('SETTINGS', 'OUTPUT_COMMENT_TEXT', true);
  fFooterTextFileName        := fIniFile.ReadString('SETTINGS', 'FOOTER_TEXT_FILENAME', GetFooterTexFileName);
  fOutPutFooterText          := fIniFile.ReadBool  ('SETTINGS', 'OUTPUT_FOOTER_TEXT', true);
  fFooterTextAsComment       := fIniFile.ReadBool  ('SETTINGS', 'FOOTER_TEXT_AS_COMMENT', true);

  fOutputKerfCommand         := fIniFile.ReadBool  ('SETTINGS', 'OUTPUT_KERF_COMMAND', true);
  fOutputKerfValue           := fIniFile.ReadBool  ('SETTINGS', 'OUTPUT_KERF_VALUE', true);
  fKerfValue                 := fIniFile.ReadFloat ('SETTINGS', 'KERF_VALUE', 1.2);
  fUnitFactor                := fIniFile.ReadFloat ('SETTINGS', 'UNIT_FACTOR', 10.0);
  fOutputProgramStartCmd     := fIniFile.ReadBool  ('SETTINGS', 'OUTPUT_PROGRAM_START', true);
  fOutputProgramEndCmd       := fIniFile.ReadBool  ('SETTINGS', 'OUTPUT_PROGRAM_END', true);
  fESSICmdKerfInsRight       := fIniFile.ReadString('COMMANDS', 'KERF_INS_RIGHT', '30');
  fESSICmdKerfInsLeft        := fIniFile.ReadString('COMMANDS', 'KERF_INS_LEFT', '29');
  fESSICmdKerfOutsRight      := fIniFile.ReadString('COMMANDS', 'KERF_OUTS_RIGHT', '29');
  fESSICmdKerfOutsLeft       := fIniFile.ReadString('COMMANDS', 'KERF_OUTS_LEFT', '30');

  fESSICmdRapidMoveON        := fIniFile.ReadString('COMMANDS', 'RAPID_MOVE_ON', '5');
  fESSICmdRapidMoveOFF       := fIniFile.ReadString('COMMANDS', 'RAPID_MOVE_OFF', '6');
  fESSICmdToolDown           := fIniFile.ReadString('COMMANDS', 'TOOL_DOWN', '7');
  fESSICmdToolUp             := fIniFile.ReadString('COMMANDS', 'TOOL_UP', '8');
  fESSICmdKerfOFF            := fIniFile.ReadString('COMMANDS', 'KERF_OFF', '38');
  fESSICmdCommentON          := fIniFile.ReadString('COMMANDS', 'COMMENT_ON', '3');
  fESSICmdCommentOFF         := fIniFile.ReadString('COMMANDS', 'COMMENT_OFF', '4');
  fESSICmdProgramStart       := fIniFile.ReadString('COMMANDS', 'PROGRAM_START', '%');
  fESSICmdProgramEnd         := fIniFile.ReadString('COMMANDS', 'PROGRAM_END', '0');
  fESSICmdAsoulutON          := fIniFile.ReadString('COMMANDS', 'ABSOULUT_ON ', '90');
  fESSICmdRelativeON         := fIniFile.ReadString('COMMANDS', 'RELATIVE_ON ', '91');
  fRelativeProgram           := fIniFile.ReadBool('SETTINGS',   'RELATIVPROGRAM', true);

  fStartX := fIniFile.ReadFloat ('SETTINGS', 'START_X', 0);
  fStartY := fIniFile.ReadFloat ('SETTINGS', 'START_Y', 0);
end;


procedure TExportEssi.WriteIniFile;
begin
  fIniFile.WriteString('SETTINGS', 'MACHINE_NAME', fMachineName);
  fIniFile.WriteString('SETTINGS', 'COMMENT_TEXT_FILENAME', fCommentTextFileName);
  fIniFile.WriteBool  ('SETTINGS', 'OUTPUT_COMMENT_TEXT',   fOutPutCommentText);
  fIniFile.WriteString('SETTINGS', 'FOOTER_TEXT_FILENAME', fFooterTextFileName);
  fIniFile.WriteBool  ('SETTINGS', 'OUTPUT_FOOTER_TEXT', fOutPutFooterText);
  fIniFile.WriteBool  ('SETTINGS', 'FOOTER_TEXT_AS_COMMENT', fFooterTextAsComment);
  fIniFile.WriteBool  ('SETTINGS', 'OUTPUT_KERF_COMMAND',   fOutputKerfCommand);

  fIniFile.WriteBool ('SETTINGS',   'OUTPUT_KERF_COMMAND', fOutputKerfCommand);
  fIniFile.WriteBool('SETTINGS',    'OUTPUT_KERF_VALUE', fOutputKerfValue);
  fIniFile.WriteFloat('SETTINGS',   'KERF_VALUE', fKerfValue);
  fIniFile.WriteFloat('SETTINGS',   'UNIT_FACTOR', fUnitFactor);
  fIniFile.WriteBool  ('SETTINGS',  'OUTPUT_PROGRAM_END', fOutputProgramEndCmd);

  fIniFile.WriteString('COMMANDS',   'KERF_INS_RIGHT',  fESSICmdKerfInsRight);
  fIniFile.WriteString('COMMANDS',   'KERF_INS_LEFT',   fESSICmdKerfInsLeft);
  fIniFile.WriteString('COMMANDS',   'KERF_OUTS_RIGHT', fESSICmdKerfOutsRight);
  fIniFile.WriteString('COMMANDS',   'KERF_OUTS_LEFT',  fESSICmdKerfOutsLeft);

  fIniFile.WriteString('COMMANDS',   'RAPID_MOVE_ON',  fESSICmdRapidMoveON);
  fIniFile.WriteString('COMMANDS',   'RAPID_MOVE_OFF', fESSICmdRapidMoveOFF);
  fIniFile.WriteString('COMMANDS',   'TOOL_DOWN',      fESSICmdToolDown);
  fIniFile.WriteString('COMMANDS',   'TOOL_UP',        fESSICmdToolUp);
  fIniFile.WriteString('COMMANDS',   'KERF_OFF',       fESSICmdKerfOFF);
  fIniFile.WriteString('COMMANDS',   'COMMENT_ON',     fESSICmdCommentON);
  fIniFile.WriteString('COMMANDS',   'COMMENT_OFF',    fESSICmdCommentOFF);
  fIniFile.WriteString('COMMANDS',   'PROGRAM_START',  fESSICmdProgramStart);
  fIniFile.WriteString('COMMANDS',   'PROGRAM_END',    fESSICmdProgramEnd);
  fIniFile.WriteString('COMMANDS',   'ABSOULUT_ON',    fESSICmdAsoulutON);
  fIniFile.WriteString('COMMANDS',   'RELATIVE_ON',    fESSICmdRelativeON);
  fIniFile.WriteBool('SETTINGS',   'RELATIVPROGRAM', fRelativeProgram);
end;

procedure TExportEssi.SaveToFile(AFileName: string);
begin
  output.SaveToFile(AFileName);
end;

procedure   TExportEssi.SaveToStringList(AStringList: TStringList);
var i: longInt;
begin
  if  (AStringList <> nil) then
  begin
    for i := 0 to  output.Count - 1 do
      AStringList.Add(output[i]);
  end;
end;

procedure TExportEssi.GenerateGlobalData;
var i, FiguresCount: integer; hKerfInfo: TKerfInfo;
begin
  LastKerfInfo := CAM_KERF_NONE;
  FiguresCount := I_GetFiguresCount;
  if FiguresCount > 0 then
    ProgramStart;
  I_ProgressBarReset(0, FiguresCount, 1);
  for i := 0 to FiguresCount - 1  do
  begin
    if IsCounturStart(i) then
    begin
      CreateRapidMove(i);
      LastKerfInfo := CAM_KERF_NONE;
      hKerfInfo := TKerfInfo(I_GetFigureKerfInfo(i));
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

procedure TExportEssi.CreateRapidMove(AIndex: integer);
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

function TExportEssi.ProcessCounturStart(AKerfInfo, ALastKerfInfo: TKerfInfo): string;
var hRapidMoveOFF, hRapidMove, hKerf, hToolDown: string;
begin
  hRapidMoveOFF :=  fESSICmdRapidMoveOFF;
  if (hRapidMoveOFF <> '') then
  begin
    CurrLineCount := CurrLineCount + 1;
    output.Add(hRapidMoveOFF);
  end;
  ProcessKerf(AKerfInfo, ALastKerfInfo);
  hToolDown := fESSICmdToolDown;
  if (hToolDown <> '') then
  begin
    CurrLineCount := CurrLineCount + 1;
    output.Add(hToolDown);
    //output.Add(GetNewLineNum  + ' ' + hToolDown);
  end;
end;

procedure TExportEssi.ProcessFigure(AIndex: integer);
var hClassName: string;  hKerfInfo:TKerfInfo; hArcDirection: word;
spx, spy, epx, epy, cpx, cpy: single; TmpObj2D: TObject2D;
PointCount, idx1, idx2: integer; TmpPoint2D: TPoint2D;
begin
  TmpObj2D := I_GetObject(AIndex);
  if (TmpObj2D is TOutline2D) then
    TOutline2D(TmpObj2D).BeginUseProfilePoints;

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
  end else if (hClassName = 'TCircle2D_CPR') then
  begin
    ProcessKerf(hKerfInfo, LastKerfInfo);
    LastKerfInfo :=  hKerfInfo;
    cpx := I_GetFigureCP_X(Aindex);
    cpy := I_GetFigureCP_Y(Aindex);
    spx := I_GetFigureSP_X(Aindex);
    spy := I_GetFigureSP_Y(Aindex);
    epx := I_GetFigureEP_X(Aindex);
    epy := I_GetFigureEP_Y(Aindex);
    hArcDirection := I_GetFigureDirection(Aindex);
    ProcessArc(cpx, cpy, spx, spy, epx, epy, hArcDirection);
  end else if (hClassName = 'TCIArc2D_CSE') then
  begin
    ProcessKerf(hKerfInfo, LastKerfInfo);
    LastKerfInfo :=  hKerfInfo;
    cpx := I_GetFigureCP_X(Aindex);
    cpy := I_GetFigureCP_Y(Aindex);
    spx := I_GetFigureSP_X(Aindex);
    spy := I_GetFigureSP_Y(Aindex);
    epx := I_GetFigureEP_X(Aindex);
    epy := I_GetFigureEP_Y(Aindex);
    hArcDirection := I_GetFigureDirection(Aindex);
    ProcessArc(cpx, cpy, spx, spy, epx, epy, hArcDirection);
  end else if (hClassName = 'TArc2D') then
  begin
    ProcessKerf(hKerfInfo, LastKerfInfo);
    LastKerfInfo :=  hKerfInfo;
    cpx := I_GetFigureCP_X(Aindex);
    cpy := I_GetFigureCP_Y(Aindex);
    spx := I_GetFigureSP_X(Aindex);
    spy := I_GetFigureSP_Y(Aindex);
    epx := I_GetFigureEP_X(Aindex);
    epy := I_GetFigureEP_Y(Aindex);
    hArcDirection := I_GetFigureDirection(Aindex);
    ProcessArc(cpx, cpy, spx, spy, epx, epy, hArcDirection);
  end else if (TmpObj2D is  TOutline2D) then
  begin
    ProcessKerf(hKerfInfo, LastKerfInfo);
    idx2 := I_GetFigureProfilePointCount(AIndex);
    for idx1 := 0 to  idx2 - 2 do
    begin
      TmpPoint2D := I_GetFigureProfilePoint(AIndex, idx1);
      spx  :=  TmpPoint2D.X;
      spy  :=  TmpPoint2D.Y;
      TmpPoint2D := I_GetFigureProfilePoint(AIndex, idx1 + 1);
      epx  := TmpPoint2D.X;
      epy  := TmpPoint2D.Y;
      ProcessLine(spx, spy, epx, epy);
    end;
    LastKerfInfo :=  hKerfInfo;
    if (TmpObj2D is TOutline2D) then
    TOutline2D(TmpObj2D).EndUseProfilePoints;
  end;
end;

procedure TExportEssi.ProcessCircle;
begin

end;

function TExportEssi.IsCounturStart(AFigureIndex: integer): boolean;
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

function TExportEssi.IsCounturEnd(AFigureIndex: integer): boolean;
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

function TExportEssi.GetDecimalSeparator: string;
begin
  result := '.';
end;

function TExportEssi.GetNewLineNum: string;
var hStr: string;
begin
  hStr := 'N' +  IntToStr(((CurrLineCount - 1) *  LineNumStep) + LineNumStart);
  result := hStr;
end;

function TExportEssi.GetStdCommentTexFileName: string;
begin
  result := ExtractFilePath(fIniFileName) + STD_COMMENT_TEXT_FILE;
end;

function TExportEssi.GetCommentTexFileName: string;
begin
  result := ChangeFileExt(fIniFileName, '.cmt');
end;

function TExportEssi.GetStdFooterTexFileName: string;
begin
  result := ExtractFilePath(fIniFileName) + STD_FOOTER_TEXT_FILE;
end;

function TExportEssi.GetFooterTexFileName: string;
begin
  result := ChangeFileExt(fIniFileName, '.ftr');
end;

procedure TExportEssi.CheckStdCommentTexFile;
var hFileName: string; hStrList: TStringList;
begin
  hFileName := GetStdCommentTexFileName;
  if (not FileExists(hFileName)) then
  begin
    try
      hStrList := TStringList.Create;
      hStrList.Add('Start of Comment');
      hStrList.Add('Standart ESSI Filegenerator');
      hStrList.Add('End of Comment');
      hStrList.SaveToFile(hFileName);
    finally
      hStrList.Free;
    end;
  end;
end;

procedure TExportEssi.CheckCommentTexFile;
var hFileName: string; hStrList: TStringList;
begin
  hFileName := fCommentTextFileName;
  if (not FileExists(hFileName)) then
  begin
    try
      hStrList := TStringList.Create;
      hStrList.LoadFromFile(GetStdCommentTexFileName);
      hStrList.SaveToFile(GetCommentTexFileName);
    finally
      hStrList.Free;
    end;
  end;
end;

procedure TExportEssi.CheckStdFooterTexFile;
var hFileName: string; hStrList: TStringList;
begin
  hFileName := GetStdFooterTexFileName;
  if (not FileExists(hFileName)) then
  begin
    try
      hStrList := TStringList.Create;
      hStrList.Add('Start of Footertext');
      hStrList.Add('Standart ESSI Filegenerator');
      hStrList.Add('End of Footertext');
      hStrList.SaveToFile(hFileName);
    finally
      hStrList.Free;
    end;
  end;
end;

procedure TExportEssi.CheckFooterTexFile;
var hFileName: string; hStrList: TStringList;
begin
  hFileName := fFooterTextFileName;
  if (not FileExists(hFileName)) then
  begin
    try
      hStrList := TStringList.Create;
      hStrList.LoadFromFile(GetStdFooterTexFileName);
      hStrList.SaveToFile(GetFooterTexFileName);
    finally
      hStrList.Free;
    end;
  end;
end;

function TExportEssi.ProgramStart: string;
var hStrList: TStringList;
begin
  if fOutputProgramStartCmd then
    output.Add(fESSICmdProgramStart);
  if fOutPutCommentText then
  begin
    hStrList := TStringList.Create;
    try
      output.Add(fESSICmdCommentON);
      hStrList.LoadFromFile(fCommentTextFileName);
      output.AddStrings(hStrList);
      output.Add(fESSICmdCommentOFF);
    finally
      hStrList.Free;
    end;
  end;
  if fRelativeProgram then output.Add(fESSICmdRelativeON)
  else output.Add(fESSICmdAsoulutON);
end;

function TExportEssi.LineON: string;
begin
  result := '';
end;

function TExportEssi.CWArcON: string;
begin
  result := '';
end;

function TExportEssi.CCWArcON: string;
begin
  result := '';
end;

procedure TExportEssi.RapidMove(AStartPointX, AStartPointY, AEndPointX, AEndPointY: single);
var hEndPointX, hEndPointY: single;  hStr: string; hLineNum: string;
begin
 //Old
  output.Add(fESSICmdRapidMoveON);
 // new
 //output.Add(ppEssiStandart.RapidMoveON);
 ProcessLine(AStartPointX, AStartPointY, AEndPointX, AEndPointY);
end;

procedure TExportEssi.ProcessKerf(AKerfInfo, ALastKerfInfo: TKerfInfo);
var KerfStr: string;
begin
  if not fOutputKerfCommand  then exit;
  if  (AKerfInfo <> ALastKerfInfo) then
  begin
    if fKerfON then
    begin
      if (AKerfInfo = CAM_KERF_INS_RIGHT)       then
        KerfStr := fESSICmdKerfInsRight
      else if (AKerfInfo = CAM_KERF_INS_LEFT)   then
         KerfStr := fESSICmdKerfInsLeft
      else if (AKerfInfo = CAM_KERF_OUTS_RIGHT) then
        KerfStr := fESSICmdKerfOutsRight
      else if (AKerfInfo = CAM_KERF_OUTS_LEFT)  then
        KerfStr := fESSICmdKerfOutsLeft
      else
        KerfStr := fESSICmdKerfOFF;
      if ((AKerfInfo <> CAM_KERF_NONE) and fOutputKerfValue) then
        KerfStr := KerfStr + '+' + FloatToStr(round(fUnitFactor * fKerfValue));
      output.Add(KerfStr);
    end;
  end;
end;

function TExportEssi.ProcessCounturEnd: string;
var hToolUp, hKerfOFF, hLineNum: string;
begin
  hToolUp  := fESSICmdToolUp;
  hKerfOFF := fESSICmdKerfOFF;
  if (hToolUp <> '') then
  begin
    CurrLineCount := CurrLineCount + 1;
    //output.Add(GetNewLineNum + ' ' + hToolUp);
    output.Add(hToolUp);
  end;
  if fOutputKerfCommand then
    if (LastKerfInfo <> CAM_KERF_NONE) then
    begin
      CurrLineCount := CurrLineCount + 1;
      //output.Add(GetNewLineNum + ' ' + hKerfOFF);
      output.Add(hKerfOFF);
    end;
end;

function TExportEssi.ProcessLine(AStartPointX, AStartPointY, AEndPointX, AEndPointY: single): string;
var xstr, ystr, hLine: string; x, y: single;
begin
  if fRelativeProgram  then
  begin
    x := round(fUnitFactor  * (AEndPointX - AStartPointX));
    y := round(fUnitFactor  * (AEndPointY - AStartPointY));
  end else
  begin
    x := round(fUnitFactor  * AEndPointX);
    y := round(fUnitFactor  * AEndPointY);
  end;

  if (x = 0) then xStr := '+'
  else begin
    xstr := FloatTostr(x);
    if (x > 0) then
      xstr := '+' + xStr;
  end;
  
  if (y = 0) then yStr := '+'
  else begin
    ystr := FloatTostr(y);
    if (y > 0) then
      ystr := '+' + yStr;
  end;
  hLine  := xStr+yStr;
  //hLine := ppEssiStandart.Line(AStartPointX, AStartPointY, AEndPointX, AEndPointY);
  if (hLine <> '') then
    output.Add(hLine);
end;

function TExportEssi.ProcessArc(ACenterPointX, ACenterPointY, AStartPointX, AStartPointY, AEndPointX, AEndPointY: single; ADirection: word): string;
var Ex, Ey, Cx, Cy: single;  ExStr, EyStr, CxStr, CyStr, hStr: string; hDirection: string;
begin
   if (ADirection = CAM_LEFTDIRECTION) then  //CCW
     hDirection := '+'
   else
     hDirection := '-';      //CCW
   if fRelativeProgram then
   begin
     Ex := round(fUnitFactor * (AEndPointX - AStartPointX));
     Ey := round(fUnitFactor * (AEndPointY - AStartPointY));
     Cx := round(fUnitFactor * (ACenterPointX - AStartPointX));
     Cy := round(fUnitFactor * (ACenterPointY - AStartPointY));
   end else
   begin
     Ex := round(fUnitFactor * AEndPointX);
     Ey := round(fUnitFactor * AEndPointY);
     Cx := round(fUnitFactor * ACenterPointX);
     Cy := round(fUnitFactor * ACenterPointY);
   end;

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
  //output.Add(hStr);
  //hStr := ppEssiStandart.Arc(ACenterPointX, ACenterPointY, AStartPointX, AStartPointY, AEndPointX, AEndPointY, ADirection);
  if (hStr <> '') then
    output.Add(hStr);
end;

function TExportEssi.ProgramEnd: string;
var hStrList: TStringList;
begin
  if (fOutputProgramEndCmd and (output.Count > 0))  then
  begin
    CurrLineCount := CurrLineCount + 1;
    output.Add(fESSICmdProgramEnd);
  end;
  if fOutPutFooterText then
  begin
    hStrList := TStringList.Create;
    try
      if fFooterTextAsComment then
      begin
        output.Add(fESSICmdCommentON);
        CurrLineCount := CurrLineCount + 1;
      end;
      hStrList.LoadFromFile(fFooterTextFileName);
      output.AddStrings(hStrList);
      if fFooterTextAsComment then
      begin
        output.Add(fESSICmdCommentOFF);
        CurrLineCount := CurrLineCount + 1;
      end;
      CurrLineCount := CurrLineCount + hStrList.Count;
    finally
      hStrList.Free;
    end;
  end;
end;

end.

