unit cExportGCODE;

interface

uses Classes, SysUtils, IniFiles,
     CS4BaseTypes,
     CS4Shapes,
     CADSys4,
     fCadIntrf2D,
     tools,
     fCircle2D_CPR,
     applicationh,
     camh;

type

  TExportGCODE = class
    private
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

      Output: TStringList;
      DestFileName,
      tmpOutputFileName: string;
      LastKerfInfo: TKerfInfo;
      KerfON,
      fOutputKerfCommand,
      fOutputSpeedCommand,
      ProgramKerf: boolean;
      fUnitFactor: double;
      Kerf : double;
      NewLine: string;
      CurrLineCount: integer;
      //DIN
      BlockFigures,
      LineNumON: boolean;
      LineNumStart,
      LineNumStep : integer;
      fSpeedValue,
      LineNumPrefix: string;
      fDecseparator: string;
      ReturnReferencePoint,
      fRelativeProgram,
      Comment: boolean;
      LastElementID: integer;

      fStartX,
      fStartY: single;

      procedure ProcessProgramStart;
      function IsCounturStart(AFigureIndex: integer): boolean;
      procedure CreateRapidMove(AIndex: integer);
      function ProcessCounturStart(AKerfInfo, ALastKerfInfo: TKerfInfo): string;
      procedure ProcessFigure(AIndex: integer);
      function IsCounturEnd(AFigureIndex: integer): boolean;
      procedure ProcessCounturEnd;
      procedure ProcessProgramEnd;
      procedure OutputRapidMoveData(AStartPointX, AStartPointY, AEndPointX, AEndPointY: double);
      procedure OutputhRapidMoveOFFData;
      procedure ProcessKerf(AKerfInfo, ALastKerfInfo: TKerfInfo);
      procedure OutputToolDownData;
      function RapidMoveOFF: string;
      function FormatOutput(AStr: string): string;
      function ToolDown: string;
      function ToolUp: string;
      function ProcessLine(AStartPointX, AStartPointY, AEndPointX, AEndPointY: double): string;
      function RapidMoveON: string;
      function LineON: string;
      function CWArcON: string;
      function CCWArcON: string;
      procedure OutputLineData(ALineData: string);
      function  ProcessArc(ACenterPointX, ACenterPointY, AStartPointX, AStartPointY, AEndPointX, AEndPointY: double; ADirection: word): string;
      procedure OutputCWArcData(AArcData: string);
      procedure OutputCCWArcData(AArcData: string);
      procedure OutputRetToRefPointData;
      procedure OutputRelAbsCoordinates;
      function  ReferencePointReturn: string;
      function  IncrementalCoordinates: string;
      function  AbsoluteCoordinates: string;
      procedure OutputToolUpData;
      procedure OutPutKerfOFFData;
      procedure MainLoop;
      procedure ProcessCircle;
      function  GetDecimalSeparator: string;
      function  GetNewLineNum: string;
      function  CommentON: string;
      function  CommentText: string;
      function  CommentOFF: string;
      function  KerfOFF: string;
      function  InsRightKerf: string;
      function  InsLeftKerf: string;
      function  OutsRightKerf: string;
      function  OutsLeftKerf: string;
      function  ProgramEnd: string;

      function GetStdCommentTexFileName: string;
      function GetCommentTexFileName: string;
      function GetStdFooterTexFileName: string;
      function GetFooterTexFileName: string;
      procedure CheckStdCommentTexFile;
      procedure CheckCommentTexFile;
      procedure CheckStdFooterTexFile;
      procedure CheckFooterTexFile;

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

constructor TExportGCODE.create(AMachineFileName: string);
begin
  inherited create;
  if ((AMachineFileName = '') or (AMachineFileName = ' ')) then
    fIniFileName  := GetAppStdGCODEOutputMachineFileName
  else
    fIniFileName := AMachineFileName;
  init;
end;

destructor  TExportGCODE.destroy;
begin
  WriteIniFile;
  fIniFile.Free;
  output.free;
  inherited;
end;

procedure TExportGCODE.init;
begin
  fIniFile := TIniFile.Create(fIniFileName);
  ReadIniFile;
  CheckStdCommentTexFile;
  CheckCommentTexFile;
  CheckStdFooterTexFile;
  CheckFooterTexFile;
  output         := TStringList.Create;
  Comment        := false;
  KerfON         := true;
  NewLine        := chr(13) + chr(10);
  CurrLineCount  := 0;
  //DIN
  ReturnReferencePoint :=  false;
  I_Regen;
  MainLoop;
end;

procedure TExportGCODE.ReadIniFile;
begin
  fMachineName               := fIniFile.ReadString('SETTINGS', 'MACHINE_NAME', fMachineName);
  fCommentTextFileName       := fIniFile.ReadString('SETTINGS', 'COMMENT_TEXT_FILENAME', GetCommentTexFileName);
  fOutPutCommentText         := fIniFile.ReadBool  ('SETTINGS', 'OUTPUT_COMMENT_TEXT', true);
  fFooterTextFileName        := fIniFile.ReadString('SETTINGS', 'FOOTER_TEXT_FILENAME', GetFooterTexFileName);
  fOutPutFooterText          := fIniFile.ReadBool  ('SETTINGS', 'OUTPUT_FOOTER_TEXT', true);
  fFooterTextAsComment       := fIniFile.ReadBool  ('SETTINGS', 'FOOTER_TEXT_AS_COMMENT', true);

  fOutputKerfCommand          := fIniFile.ReadBool  ('SETTINGS', 'OUTPUT_KERF_COMMAND', true);
  //fOutputKerfValue          := fIniFile.ReadBool  ('SETTINGS', 'OUTPUT_KERF_VALUE', true);
  //fKerfValue                := fIniFile.ReadFloat ('SETTINGS', 'KERF_VALUE', 1.2);
  fUnitFactor                 := fIniFile.ReadFloat ('SETTINGS', 'UNIT_FACTOR', 1);
  fOutputProgramStartCmd      := fIniFile.ReadBool  ('SETTINGS', 'OUTPUT_PROGRAM_START', true);
  fOutputProgramEndCmd        := fIniFile.ReadBool  ('SETTINGS', 'OUTPUT_PROGRAM_END', true);
  fOutputSpeedCommand         := fIniFile.ReadBool  ('SETTINGS', 'OUTPUT_SPEED_COMMAND', true);
  fGCODECmdKerfInsRight       := fIniFile.ReadString('COMMANDS', 'KERF_INS_RIGHT', 'G42');
  fGCODECmdKerfInsLeft        := fIniFile.ReadString('COMMANDS', 'KERF_INS_LEFT', 'G41');
  fGCODECmdKerfOutsRight      := fIniFile.ReadString('COMMANDS', 'KERF_OUTS_RIGHT', 'G41');
  fGCODECmdKerfOutsLeft       := fIniFile.ReadString('COMMANDS', 'KERF_OUTS_LEFT', 'G42');

  fGCODECmdRapidMoveON        := fIniFile.ReadString('COMMANDS', 'RAPID_MOVE', 'G00');
  fGCODECmdLine               := fIniFile.ReadString('COMMANDS', 'LINE', 'G01');
  fGCODECmdArcCW              := fIniFile.ReadString('COMMANDS', 'ARC_CW', 'G02');
  fGCODECmdArcCCW             := fIniFile.ReadString('COMMANDS', 'ARC_CCW', 'G03');

  fGCODECmdRapidMoveOFF       := fIniFile.ReadString('COMMANDS', 'RAPID_MOVE_OFF', '');
  fGCODECmdToolDown           := fIniFile.ReadString('COMMANDS', 'TOOL_DOWN', 'M10');
  fGCODECmdToolUp             := fIniFile.ReadString('COMMANDS', 'TOOL_UP', 'M11');
  fGCODECmdKerfOFF            := fIniFile.ReadString('COMMANDS', 'KERF_OFF', 'G40');
  fGCODECmdCommentON          := fIniFile.ReadString('COMMANDS', 'COMMENT_ON', '{');
  fGCODECmdCommentOFF         := fIniFile.ReadString('COMMANDS', 'COMMENT_OFF', '}');
  fGCODECmdProgramStart       := fIniFile.ReadString('COMMANDS', 'PROGRAM_START', '%');
  fGCODECmdProgramEnd         := fIniFile.ReadString('COMMANDS', 'PROGRAM_END', 'M30');
  fGCODECmdAsoulutON          := fIniFile.ReadString('COMMANDS', 'ABSOULUT_ON ', 'G90');
  fGCODECmdRelativeON         := fIniFile.ReadString('COMMANDS', 'RELATIVE_ON ', 'G91');
  fGCODECmdSpeedCommand       := fIniFile.ReadString('COMMANDS', 'SPEED_COMMAND', 'F');
  fSpeedValue                 := fIniFile.ReadString('SETTINGS', 'SPEED_VALUE', '200');
  fOutputSpeedCommand         := fIniFile.ReadBool('SETTINGS',   'OUTPUT_SPEED_COMMAND', true);
  fRelativeProgram            := fIniFile.ReadBool('SETTINGS',   'RELATIVPROGRAM', true);

  fStartX := fIniFile.ReadFloat ('SETTINGS', 'START_X', 0);
  fStartY := fIniFile.ReadFloat ('SETTINGS', 'START_Y', 0);

  BlockFigures   := fIniFile.ReadBool('SETTINGS',   'BLOCK_FIGURES', true);
  LineNumON      := fIniFile.ReadBool('SETTINGS',   'LINE_NUM_ON', true);
  LineNumStart   := fIniFile.ReadInteger('SETTINGS',   'LINE_NUM_START', 100);
  LineNumStep    := fIniFile.ReadInteger('SETTINGS',   'LINE_NUM_STEP', 10);
  LineNumPrefix  := fIniFile.ReadString('SETTINGS',   'LINE_NUM_PREFIX', 'N');
  fDecseparator  := fIniFile.ReadString('SETTINGS',   'DECIMAL_SEPERATOR', ',');
end;


procedure TExportGCODE.WriteIniFile;
begin
  fIniFile.WriteString('SETTINGS', 'MACHINE_NAME', fMachineName);
  fIniFile.WriteString('SETTINGS', 'COMMENT_TEXT_FILENAME', fCommentTextFileName);
  fIniFile.WriteBool  ('SETTINGS', 'OUTPUT_COMMENT_TEXT',   fOutPutCommentText);
  fIniFile.WriteString('SETTINGS', 'FOOTER_TEXT_FILENAME', fFooterTextFileName);
  fIniFile.WriteBool  ('SETTINGS', 'OUTPUT_FOOTER_TEXT', fOutPutFooterText);
  fIniFile.WriteBool  ('SETTINGS', 'FOOTER_TEXT_AS_COMMENT', fFooterTextAsComment);
  fIniFile.WriteBool  ('SETTINGS', 'OUTPUT_KERF_COMMAND',   fOutputKerfCommand);

  fIniFile.WriteBool ('SETTINGS',   'OUTPUT_KERF_COMMAND', fOutputKerfCommand);
  //fIniFile.WriteBool('SETTINGS',    'OUTPUT_KERF_VALUE', fOutputKerfValue);
  //fIniFile.WriteFloat('SETTINGS',   'KERF_VALUE', fKerfValue);
  fIniFile.WriteFloat('SETTINGS',   'UNIT_FACTOR', fUnitFactor);
  fIniFile.WriteBool  ('SETTINGS',  'OUTPUT_PROGRAM_END', fOutputProgramEndCmd);

  fIniFile.WriteString('COMMANDS',   'KERF_INS_RIGHT',  fGCODECmdKerfInsRight);
  fIniFile.WriteString('COMMANDS',   'KERF_INS_LEFT',   fGCODECmdKerfInsLeft);
  fIniFile.WriteString('COMMANDS',   'KERF_OUTS_RIGHT', fGCODECmdKerfOutsRight);
  fIniFile.WriteString('COMMANDS',   'KERF_OUTS_LEFT',  fGCODECmdKerfOutsLeft);

  fIniFile.WriteString('COMMANDS',   'RAPID_MOVE',  fGCODECmdRapidMoveON);
  fIniFile.WriteString('COMMANDS',   'LINE',  fGCODECmdLine);
  fIniFile.WriteString('COMMANDS',   'ARC_CW',  fGCODECmdArcCW);
  fIniFile.WriteString('COMMANDS',   'ARC_CCW',  fGCODECmdArcCCW);

  fIniFile.WriteString('COMMANDS',   'RAPID_MOVE_OFF', fGCODECmdRapidMoveOFF);
  fIniFile.WriteString('COMMANDS',   'TOOL_DOWN',      fGCODECmdToolDown);
  fIniFile.WriteString('COMMANDS',   'TOOL_UP',        fGCODECmdToolUp);
  fIniFile.WriteString('COMMANDS',   'KERF_OFF',       fGCODECmdKerfOFF);
  fIniFile.WriteString('COMMANDS',   'COMMENT_ON',     fGCODECmdCommentON);
  fIniFile.WriteString('COMMANDS',   'COMMENT_OFF',    fGCODECmdCommentOFF);
  fIniFile.WriteString('COMMANDS',   'PROGRAM_START',  fGCODECmdProgramStart);
  fIniFile.WriteString('COMMANDS',   'PROGRAM_END',    fGCODECmdProgramEnd);
  fIniFile.WriteString('COMMANDS',   'ABSOULUT_ON',    fGCODECmdAsoulutON);
  fIniFile.WriteString('COMMANDS',   'RELATIVE_ON',    fGCODECmdRelativeON);
  fIniFile.WriteString('COMMANDS',   'SPEED_COMMAND',  fGCODECmdSpeedCommand);
  fIniFile.WriteString('SETTINGS',   'SPEED_VALUE',    fSpeedValue);
  fIniFile.WriteBool('SETTINGS',   'OUTPUT_SPEED_COMMAND', fOutputSpeedCommand);
  fIniFile.WriteBool('SETTINGS',     'RELATIVPROGRAM', fRelativeProgram);

  fIniFile.WriteBool('SETTINGS',   'BLOCK_FIGURES', BlockFigures);
  fIniFile.WriteBool('SETTINGS',   'LINE_NUM_ON', LineNumON);
  fIniFile.WriteInteger('SETTINGS',   'LINE_NUM_START', LineNumStart);
  fIniFile.WriteInteger('SETTINGS',   'LINE_NUM_STEP', LineNumStep);
  fIniFile.WriteString('SETTINGS',   'LINE_NUM_PREFIX', LineNumPrefix);
  fIniFile.WriteString('SETTINGS',    'DECIMAL_SEPERATOR',  fDecseparator);
end;

procedure   TExportGCODE.SaveToFile(AFileName: string);
begin
  output.SaveToFile(AFileName);
end;

procedure   TExportGCODE.SaveToStringList(AStringList: TStringList);
var i: longInt;
begin
  if  (AStringList <> nil) then
  begin
    for i := 0 to  output.Count - 1 do
      AStringList.Add(output[i]);
  end;
end;

procedure TExportGCODE.MainLoop;
var i, FiguresCount: integer; hKerfInfo: TKerfInfo;  TmpDecseparator: string;
begin
  TmpDecseparator := ' ';
  TmpDecseparator[1] := Decimalseparator;
  Decimalseparator := fDecseparator[1];
  LastKerfInfo  := CAM_KERF_NONE;
  LastElementID := -1; // Ungültig
  ProcessProgramStart;
  FiguresCount := I_GetFiguresCount;
  I_ProgressBarReset(0, FiguresCount, 1);
  for i := 0 to FiguresCount - 1  do
  begin
    if IsCounturStart(i) then
    begin
      CreateRapidMove(i);
      LastKerfInfo := CAM_KERF_NONE;
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
  ProcessProgramEnd;
  Decimalseparator := TmpDecseparator[1];
end;

procedure TExportGCODE.CreateRapidMove(AIndex: integer);
var spx, spy, epx, epy: double; hDirection, hNextDirection: word;
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
    OutputRapidMoveData(spx, spy, epx, epy);
end;

function TExportGCODE.ProcessCounturStart(AKerfInfo, ALastKerfInfo: TKerfInfo): string;
begin
  OutputhRapidMoveOFFData;
  if fOutputKerfCommand then ProcessKerf(AKerfInfo, ALastKerfInfo);
  OutputToolDownData;
end;

procedure TExportGCODE.OutputhRapidMoveOFFData;
var hStr: string;
begin
    hStr :=  RapidMoveOFF;
    if (hStr <> '') then
      output.Add(FormatOutput(hStr));
end;

procedure TExportGCODE.OutputToolDownData;
var hStr: string;
begin
  hStr := ToolDown;
  if (hStr <> '') then
    output.Add(FormatOutput(hStr));
end;

procedure TExportGCODE.OutputRapidMoveData(AStartPointX, AStartPointY, AEndPointX, AEndPointY: double);
var hEndPointX, hEndPointY: double;  hStr: string; hLineNum: string;
begin
  hStr := ProcessLine(AStartPointX, AStartPointY, AEndPointX, AEndPointY);
  hStr :=  RapidMoveON + ' ' + hStr;
  if (hStr <> '') then
  begin
    output.Add(FormatOutput(hStr));
    LastElementID := 1;
  end;
end;

procedure TExportGCODE.ProcessFigure(AIndex: integer);
var hKerfInfo: TKerfInfo; hClassName: string;  hArcDirection: word;
spx, spy, epx, epy, cpx, cpy: double;  hLineData, hArcData: string;
TmpObj2D: TObject2D;
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
  if (TmpObj2D is TLine2D) then
  begin
    if fOutputKerfCommand then ProcessKerf(hKerfInfo, LastKerfInfo);
    hLineData := ProcessLine(spx, spy, epx, epy);
    OutputLineData(hLineData);
    LastKerfInfo :=  hKerfInfo;
  end else
  if (TmpObj2D is TArc2D) then
  begin
    if fOutputKerfCommand then ProcessKerf(hKerfInfo, LastKerfInfo);
    LastKerfInfo :=  hKerfInfo;
    cpx  := I_GetFigureCP_X(Aindex);
    cpy  := I_GetFigureCP_Y(Aindex);
    hArcDirection := I_GetFigureDirection(Aindex);
    hArcData := ProcessArc(cpx, cpy, spx, spy, epx, epy, hArcDirection);
    case  hArcDirection of
      CAM_RIGHTDIRECTION:  OutputCWArcData(hArcData);
      CAM_LEFTDIRECTION:   OutputCCWArcData(hArcData);
    end;
  end else
  if (TmpObj2D is TCircle2D_CPR) then
  begin
    if fOutputKerfCommand then ProcessKerf(hKerfInfo, LastKerfInfo);
    LastKerfInfo :=  hKerfInfo;
    //cpx  := I_GetFigureCP_X(Aindex);
    cpx := TCircle2D_CPR(TmpObj2D).CenterPoint.X;
    cpy := TCircle2D_CPR(TmpObj2D).CenterPoint.Y;
    spx := TCircle2D_CPR(TmpObj2D).EndPoint.X;
    spy := TCircle2D_CPR(TmpObj2D).EndPoint.Y;
    epx := TCircle2D_CPR(TmpObj2D).EndPoint.X;
    epy := TCircle2D_CPR(TmpObj2D).EndPoint.Y;
    hArcDirection := I_GetFigureDirection(Aindex);
    hArcData := ProcessArc(cpx, cpy, spx, spy, epx, epy, hArcDirection);
    case  hArcDirection of
      CAM_RIGHTDIRECTION:  OutputCWArcData(hArcData); 
      CAM_LEFTDIRECTION:   OutputCCWArcData(hArcData);
    end;
  end else
  if (TmpObj2D is TOutline2D) then
  begin
    if fOutputKerfCommand then ProcessKerf(hKerfInfo, LastKerfInfo);
    idx2 := I_GetFigureProfilePointCount(AIndex);
    for idx1 := 0 to  idx2 - 2 do
    begin
      TmpPoint2D := I_GetFigureProfilePoint(AIndex, idx1);
      spx  :=  TmpPoint2D.X;
      spy  :=  TmpPoint2D.Y;
      TmpPoint2D := I_GetFigureProfilePoint(AIndex, idx1 + 1);
      epx  := TmpPoint2D.X;
      epy  := TmpPoint2D.Y;
      hLineData := ProcessLine(spx, spy, epx, epy);
      OutputLineData(hLineData);
    end;
    LastKerfInfo :=  hKerfInfo;
    if (TmpObj2D is TOutline2D) then
    TOutline2D(TmpObj2D).EndUseProfilePoints;
  end;
end;

procedure TExportGCODE.OutputLineData(ALineData: string);
var hStr: string;
begin
  hStr := ALineData;
  if (not BlockFigures) then
    hStr :=  LineON + '  ' + hStr
  else
    if (LastElementID <> 2) then
       hStr :=  LineON + ' ' + hStr
    else hStr := '    ' + hStr;  //Block wird eigerückt.
  if (hStr <> '') then
  begin
    output.Add(FormatOutput(hStr));
    LastElementID := 2;
  end;
end;

procedure TExportGCODE.OutputCCWArcData(AArcData: string);
var hStr: string;
begin
  hStr := AArcData;
  if (not BlockFigures) then
    hStr := CCWArcON  + ' ' + hStr
  else
    if (LastElementID <> 3) then
       hStr := CCWArcON  + ' ' + hStr
    else hStr := '    ' + hStr;  //Block wird eigerückt.
  if (hStr <> '') then
  begin
    output.Add(FormatOutput(hStr));
    LastElementID := 3;
  end;
end;

procedure TExportGCODE.OutputCWArcData(AArcData: string);
var hStr: string;
begin
  hStr := AArcData;
  if (not BlockFigures) then
    hStr := CWArcON  + '  ' + hStr
  else
    if (LastElementID <> 4) then
         hStr := CWArcON  + ' ' + hStr
    else hStr := '    ' + hStr;  //Block wird eigerückt.
  if (hStr <> '') then
  begin
    output.Add(FormatOutput(hStr));
    LastElementID := 4;
  end;
end;

procedure TExportGCODE.ProcessCircle;
begin

end;

function TExportGCODE.IsCounturStart(AFigureIndex: integer): boolean;
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

function TExportGCODE.IsCounturEnd(AFigureIndex: integer): boolean;
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

function TExportGCODE.GetDecimalSeparator: string;
begin
  result := '.';
end;

function TExportGCODE.GetNewLineNum: string;
var hStr: string;
begin
  if (CurrLineCount = 0) then
    hStr := LineNumPrefix + IntToStr(LineNumStart)
  else
    hStr := LineNumPrefix +  IntToStr(((CurrLineCount) *  LineNumStep) + LineNumStart);
  result := hStr;
end;

function TExportGCODE.CommentText: string;
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


procedure TExportGCODE.ProcessProgramStart;
var hStr: string; hStrList: TStringList; i: integer;
begin
  if fOutPutCommentText then
  begin
    hStrList := TStringList.Create;
    try
      hStrList.LoadFromFile(fCommentTextFileName);
      for i := 0 to  hStrList.Count -1 do
        output.Add(FormatOutput(fGCODECmdCommentON + hStrList[i] + fGCODECmdCommentOFF));
    finally
      hStrList.Free;
    end;
  end;
  if fOutputProgramStartCmd then
    output.Add(FormatOutput(fGCODECmdProgramStart));
  if fOutputSpeedCommand then
    output.Add(FormatOutput(fGCODECmdSpeedCommand + fSpeedValue));
  OutputRelAbsCoordinates;
end;

procedure TExportGCODE.OutputRetToRefPointData;
var hStr: string;
begin
  if ReturnReferencePoint then
  begin
    hStr := ReferencePointReturn;
    if (hStr <> '') then
      output.Add(FormatOutput(hStr));
  end;
end;

procedure TExportGCODE.OutputRelAbsCoordinates;
var hStr: string;
begin
  if fRelativeProgram then
  begin
    hStr := IncrementalCoordinates;
    if (hStr <> '') then
      output.Add(FormatOutput(hStr));
  end
  else begin
    hStr := AbsoluteCoordinates;
    if (hStr <> '') then
      output.Add(FormatOutput(hStr));
  end;
end;

function TExportGCODE.CommentON: string;
begin
  result := fGCODECmdCommentON;
end;

function TExportGCODE.CommentOFF: string;
begin
  result := fGCODECmdCommentOFF;
end;

function TExportGCODE.RapidMoveON: string;
begin
  result := fGCODECmdRapidMoveON;  //
end;

function TExportGCODE.RapidMoveOFF: string;
begin
  result := fGCODECmdRapidMoveOFF;
end;

function TExportGCODE.LineON: string;
begin
  result := fGCODECmdLine; //
end;

function TExportGCODE.CWArcON: string;
begin
  result := fGCODECmdArcCW; //
end;

function TExportGCODE.CCWArcON: string;
begin
  result := fGCODECmdArcCCW; //
end;

function TExportGCODE.ToolDown: string;
begin
  result := fGCODECmdToolDown; //
end;

function TExportGCODE.ToolUp: string;
begin
  result := fGCODECmdToolUp; //
end;

function TExportGCODE.KerfOFF: string;
begin
  result := fGCODECmdKerfOFF; //
end;

function TExportGCODE.InsRightKerf: string;
begin
  result:= fGCODECmdKerfInsRight; //
end;

function TExportGCODE.InsLeftKerf: string;
begin
  result:= fGCODECmdKerfInsLeft; //
end;

function TExportGCODE.OutsRightKerf: string;
begin
  result:= fGCODECmdKerfOutsRight; //
end;

function TExportGCODE.OutsLeftKerf: string;
begin
  result:= fGCODECmdKerfOutsLeft;  //
end;

function TExportGCODE.AbsoluteCoordinates: string;
begin
  result := fGCODECmdAsoulutON; //
end;

function TExportGCODE.IncrementalCoordinates: string;
begin
  result := fGCODECmdRelativeON;  //
end;

function TExportGCODE.ReferencePointReturn: string;
begin
  result := 'G28'; //
end;

function TExportGCODE.ProgramEnd: string;
begin
  result := fGCODECmdProgramEnd; //
end;

procedure TExportGCODE.ProcessKerf(AKerfInfo, ALastKerfInfo: TKerfInfo);
var KerfStr, KerfOFFStr : string;
begin
  if not fOutputKerfCommand  then exit;
  if  (AKerfInfo <> ALastKerfInfo) then
  begin
    if KerfON then
    begin
      if (AKerfInfo = CAM_KERF_INS_RIGHT)       then
        KerfStr := InsRightKerf
      else if (AKerfInfo = CAM_KERF_INS_LEFT)   then
         KerfStr := InsLeftKerf
      else if (AKerfInfo = CAM_KERF_OUTS_RIGHT) then
        KerfStr := OutsRightKerf
      else if (AKerfInfo = CAM_KERF_OUTS_LEFT)  then
        KerfStr := OutsLeftKerf
      else
        KerfStr := KerfOFF;
      if ProgramKerf then
        KerfStr := KerfStr + '+' + FloatToStr(fUnitFactor * Kerf);
      output.Add(FormatOutput(KerfStr));
    end;
  end;
end;

procedure TExportGCODE.ProcessCounturEnd;
begin
  OutputToolUpData;
  if fOutputKerfCommand  then OutPutKerfOFFData;
end;

procedure TExportGCODE.OutputToolUpData;
var hStr: string;
begin
  hStr := ToolUp;
  if (hStr <> '') then
    output.Add(FormatOutput(hStr));
end;

procedure TExportGCODE.OutPutKerfOFFData;
var hStr: string;
begin
  hStr := KerfOFF;
  if (hStr <> '') then
    output.Add(FormatOutput(hStr));
end;

function TExportGCODE.ProcessLine(AStartPointX, AStartPointY, AEndPointX, AEndPointY: double): string;
var hEndPointX, hEndPointY: double;  hStr: string; hLineNum: string;
begin
  hEndPointX :=  fUnitFactor * AEndPointX;
  hEndPointY :=  fUnitFactor * AEndPointY;
  if fRelativeProgram  then
  begin
    hEndPointX := hEndPointX - AStartPointX;
    hEndPointY := hEndPointY - AStartPointY;
  end;
  hStr := 'X' +  FormatFloat('0.00', hEndPointX) + ' ' +
          'Y' +  FormatFloat('0.00', hEndPointY);
  result := hStr;
end;

function TExportGCODE.ProcessArc(ACenterPointX, ACenterPointY, AStartPointX, AStartPointY, AEndPointX, AEndPointY: double; ADirection: word): string;
var Ex, Ey, Cx, Cy: double;  ExStr, EyStr, CxStr, CyStr, hStr: string; hDirection: string;
begin
   //if (ADirection = LEFTDIRECTION)  then hDirection := CCWArcON;
   //if (ADirection = RIGHTDIRECTION) then hDirection := CWArcON;

   if (fRelativeProgram) then
   begin
     Ex := fUnitFactor * (AEndPointX - AStartPointX);
     Ey := fUnitFactor * (AEndPointY - AStartPointY);
   end else
   begin
     Ex := fUnitFactor * AEndPointX;
     Ey := fUnitFactor * AEndPointY;
   end;
   //CenterPunk ist immer Relativ.  ??
   Cx := fUnitFactor * (ACenterPointX - AStartPointX);
   Cy := fUnitFactor * (ACenterPointY - AStartPointY);

  //if (Ex = 0) then ExStr := ''
  {else}             ExStr := 'X' +  FormatFloat('0.00', Ex);

  //Ey :=  Ey * -1;   //spiegeln
  //if (Ey = 0) then EyStr := ''
  {else}             EyStr := 'Y' +  FormatFloat('0.00', Ey);

  //if (Cx = 0) then CxStr := ''
  {else}             CxStr := 'I' +  FormatFloat('0.00', Cx);

  //Cy :=  Cy * -1;   //spiegeln
  //if (Cy = 0) then CyStr := ''
  {else}             CyStr := 'J' +  FormatFloat('0.00', Cy);

  hStr := '';
  if  (ExStr <> '') then  hStr := hStr + ExStr + ' ';
  if  (EyStr <> '') then  hStr := hStr + EyStr + ' ';
  if  (CxStr <> '') then  hStr := hStr + CxStr + ' ';
  if  (CyStr <> '') then  hStr := hStr + CyStr;
  result := hStr;
end;

procedure TExportGCODE.ProcessProgramEnd;
var hStr: string;
begin
  if fOutputProgramEndCmd then
    if (fGCODECmdProgramEnd  <> '') then
      output.Add(FormatOutput(fGCODECmdProgramEnd));
end;

function TExportGCODE.FormatOutput(AStr: string): string;
var hStr: string;
begin
  hStr := AStr;
  if LineNumON then
  begin
    hStr := GetNewLineNum  + ' ' + hStr;
    CurrLineCount := CurrLineCount + 1;
  end;
  result := hStr;
end;

function TExportGCODE.GetStdCommentTexFileName: string;
begin
  result := ExtractFilePath(fIniFileName) + STD_COMMENT_TEXT_FILE;
end;

function TExportGCODE.GetCommentTexFileName: string;
begin
  result := ChangeFileExt(fIniFileName, '.cmt');
end;

function TExportGCODE.GetStdFooterTexFileName: string;
begin
  result := ExtractFilePath(fIniFileName) + STD_FOOTER_TEXT_FILE;
end;

function TExportGCODE.GetFooterTexFileName: string;
begin
  result := ChangeFileExt(fIniFileName, '.ftr');
end;

procedure TExportGCODE.CheckStdCommentTexFile;
var hFileName: string; hStrList: TStringList;
begin
  hFileName := GetStdCommentTexFileName;
  if (not FileExists(hFileName)) then
  begin
    try
      hStrList := TStringList.Create;
      hStrList.Add('Start of Comment');
      hStrList.Add('Standart GCODE Filegenerator');
      hStrList.Add('End of Comment');
      hStrList.SaveToFile(hFileName);
    finally
      hStrList.Free;
    end;
  end;
end;

procedure TExportGCODE.CheckCommentTexFile;
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

procedure TExportGCODE.CheckStdFooterTexFile;
var hFileName: string; hStrList: TStringList;
begin
  hFileName := GetStdFooterTexFileName;
  if (not FileExists(hFileName)) then
  begin
    try
      hStrList := TStringList.Create;
      hStrList.Add('Start of Footertext');
      hStrList.Add('Standart GCODE Filegenerator');
      hStrList.Add('End of Footertext');
      hStrList.SaveToFile(hFileName);
    finally
      hStrList.Free;
    end;
  end;
end;

procedure TExportGCODE.CheckFooterTexFile;
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

end.


