unit cExportEssiExt;

interface

uses Classes, SysUtils, IniFiles,  ComCtrls,
     //CS4BaseTypes,
     //CS4Shapes,
     CADSys4,
     //fCadIntrf2D,
     //tools,
     //applicationh;
      cBaseCNCWriter;

  const MIN_FIGURE_DISTANCE = 0.09;

        ESSI_LEFTDIRECTION   = 0;
        ESSI_RIGHTDIRECTION  = 1;
        STD_COMMENT_TEXT_FILE = 'stdcomment.cmt';
        STD_FOOTER_TEXT_FILE  = 'stdfooter.ftr';
type

  TExportEssiExt = class(TBaseCNCWriter)
    private
      function  CreateRapidMove(AIndex: integer): string;
      function  ProcessCounturStart(AKerfInfo, ALastKerfInfo: word): string;
      function ProcessCircle: string;
      function GetDecimalSeparator: string;
      function LineON: string;
      function ProgramStart: string;
      function CWArcON: string;
      function CCWArcON: string;
      function ProcessKerf(AKerfInfo, ALastKerfInfo: word): string;
      function ProcessCounturEnd: string;
      function ProcessLine(AStartPointX, AStartPointY, AEndPointX, AEndPointY: single): string;
      function ProcessArc(ACenterPointX, ACenterPointY, AStartPointX, AStartPointY, AEndPointX, AEndPointY: single; ADirection: word): string;
      function ProgramEnd: string;
    public
      constructor create(AMachineFileName: string; ASourceCAD: TCADCmp2D; AProgressBar: TProgressBar);
      destructor  destroy; override;
  end;

implementation

constructor TExportEssiExt.create(AMachineFileName: string; ASourceCAD: TCADCmp2D; AProgressBar: TProgressBar);
begin
  inherited create(AMachineFileName, ASourceCAD, AProgressBar);
end;

destructor TExportEssiExt.destroy;
begin
  inherited;
end;

function TExportEssiExt.CreateRapidMove(AIndex: integer): string;
var spx, spy, epx, epy: single;
begin
  result := '';
  epx := GetFigureSP_X(AIndex);
  epy := GetFigureSP_Y(AIndex);
  if (AIndex = 0) then
  begin
     spx := 0;
     spy := 0;
  end else
  begin
    spx := GetFigureEP_X(AIndex - 1);
    spy := GetFigureEP_Y(AIndex - 1);
  end;
  if   ((abs(spx - epx) > MIN_FIGURE_DISTANCE) or (abs(spy - epy) > MIN_FIGURE_DISTANCE)) then
    result := CmdRapidMoveON + NewLine + ProcessLine(spx, spy, epx, epy);
end;

function TExportEssiExt.ProcessCounturStart(AKerfInfo, ALastKerfInfo: word): string;
begin
  result := CmdRapidMoveOFF + NewLine + CmdToolDown;
end;

function TExportEssiExt.ProcessCircle: string;
begin
  result := '';
end;


function TExportEssiExt.GetDecimalSeparator: string;
begin
  result := '.';
end;

function TExportEssiExt.ProgramStart: string;
var hStrList: TStringList;
begin
  result := '';
end;

function TExportEssiExt.LineON: string;
begin
  result := '';
end;

function TExportEssiExt.CWArcON: string;
begin
  result := '';
end;

function TExportEssiExt.CCWArcON: string;
begin
  result := '';
end;

function TExportEssiExt.ProcessKerf(AKerfInfo, ALastKerfInfo: word): string;
var KerfType, hStr: string;
begin
  if not OutputKerfCommand  then exit;
  hStr := ''; KerfType := '';  //KerfValue := '';
  if  (AKerfInfo <> ALastKerfInfo) then
  begin
    if KerfON then
    begin
      if (AKerfInfo = 0 {ESSI_KERF_INS_RIGHT_STR})       then
        KerfType := CmdKerfInsRight
      else if (AKerfInfo = 1 {ESSI_KERF_INS_LEFT_STR})   then
         KerfType := CmdKerfInsLeft
      else if (AKerfInfo = 2 {ESSI_KERF_OUTS_RIGHT_STR}) then
        KerfType := CmdKerfOutsRight
      else if (AKerfInfo = 3 {ESSI_KERF_OUTS_LEFT_STR})  then
        KerfType := CmdKerfOutsLeft
      else
        KerfType := CmdKerfOutsLeft;
      hStr := KerfType;
      if OutputKerfValue then
      begin
        hStr := hStr + '+' + FloatToStr(round(UnitFactor * KerfValue));
        result := hStr
      end else
        result := '';
    end;
  end;
end;

function TExportEssiExt.ProcessCounturEnd: string;
begin
  result := CmdToolUp + NewLine + CmdKerfOFF;
end;

function TExportEssiExt.ProcessLine(AStartPointX, AStartPointY, AEndPointX, AEndPointY: single): string;
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
  result := xStr+yStr;
end;

function TExportEssiExt.ProcessArc(ACenterPointX, ACenterPointY, AStartPointX, AStartPointY, AEndPointX, AEndPointY: single; ADirection: word): string;
var Ex, Ey, Cx, Cy: single;  ExStr, EyStr, CxStr, CyStr, hStr: string; hDirection: string;
begin
   if (ADirection = ESSI_LEFTDIRECTION) then  //CCW
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
  result := hStr;
end;

function TExportEssiExt.ProgramEnd: string;
var hStrList: TStringList;
begin
  if (OutputProgramEndCmd and (OutputList.Count > 0))  then
  begin
    //CurrLineCount := CurrLineCount + 1;
    result := CmdProgramEnd;
  end;
end;

end.

