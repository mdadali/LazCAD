unit cBaseCNCWriter;

interface

uses Classes, ComCtrls, IniFiles,
     CADSys4, CS4BaseTypes, CS4Tasks, CS4Shapes, CS4DXFModule,
     CS4UserShapes, //Circle
     fCircle2D_CPR,
     ciarc2d_bbox;

const
  INVALID_FLOAT_VALUE = 0.999999999;
  INVALID_STRING_VALUE = 'INVALID_STRING_VALUE';
  INVALID_KERF_VALUE = 4;
  INVALID_DIRECTION_VALUE = 2;
  MIN_FIGURE_DISTANCE = 0.09;

type

      TGeometryType =  (gtLine, gtArc, gtCircle, gtRectangle);
      TCommandType  =  (cmdWithParam, cmdWithoutParam);
      TRecType      =  (rtCommand, rtGeometry, rtComment, rtUnknow);
      PTTRec    = ^TOutputRec;
      TOutputRec     = record
                        id     : integer;      //Datensatznummer
                        SourceObjectID: integer;
                        GeneratedCAMLine: string[255];
                        GeneratedCAMLineIDX: integer;
                        GeneratedCodeLine: string[255];
                        GeneratedCodeLineIDX: integer;
                        Description: string[255];
                        case RecType: TRecType of       //Datensatztyp
                          rtCommand: (
                                      Command : string[5];
                                      case CommandType: TCommandType of
                                         cmdWithParam:    (param: string[10]);
                                      );
                          rtUnknow:   (eMessage      :string[100]);
                          rtGeometry:(
                                      gPenStyle: integer; // WinApi Definition.
                                      gPenWidth: integer;
                                      case GeometryType: TGeometryType of
                                        gtLine: (x1,y1,x2,y2   :string[6]);
                                        gtArc:  (ax, ay, ex, ey, mx, my   :string[6];
                                                 direction  :char;
                                                );
                                    );

                     end;  

     TPExportTraceRec = ^TExportTraceRec;
     TExportTraceRec = record
       SourceObjectID: integer;
       GeneratedLineIDX: integer;
       Description: string;
     end;

     TBaseCNCWriter = class
     private
       fSourceCAD: TCADCmp2D;
       fPExportTraceRec: TList;
       fProgressBar: TProgressBar;
       fOutputList: TStringList;
       fTraceInfoList: TList;
       fCurrObj2D: TObject2D;

       fMachineFileName: string;  //IniFile
       fIniFile: TIniFile;

       fOutputKerfCommand: boolean;
       fOutputKerfValue: boolean;
       fKerfValue : single;
       fUnitFactor: single;
       fOutputProgramEndCmd: boolean;
       //commands
       fRelativeCoordsON: string;
       fAbsCoordsON: string;
       fCmdKerfInsRight,
       fCmdKerfInsLeft,
       fCmdKerfOutsRight,
       fCmdKerfOutsLeft : string;
       fCmdRapidMoveON: string;
       fCmdRapidMoveOFF: string;
       fCmdToolDown: string;
       fCmdToolUp: string;
       fCmdKerfOFF: string;
       fCmdCommentON: string;
       fCmdCommentOFF: string;
       fCmdProgramEnd: string;

       fLastKerfInfo: word;
       fKerfON: boolean;
       fNewLine: string;

      //DIN
       LineNumStart,
       LineNumStep : integer;
       LineNumPrefix: string;

       fCurrLineIDX: integer;
       RelativeProgram,
       Comment: boolean;

       procedure ProgressBarReset(AMin, AMax, AStep: integer);
       procedure ProgressBarStep;

       function  CombinedFigures(AIndex1, AIndex2: integer): boolean;

       procedure Init;
       procedure GenerateGlobalData;
       function  IsCounturStart(AFigureIndex: integer): boolean;
       function  IsCounturEnd(AFigureIndex: integer): boolean;
       procedure ProcessFigure(AIndex: integer);

       function  GetCurrLineIDX: integer;
       function  AddNewLine(ALine: string): integer;
     protected
       function  GetFiguresCount: integer;

       function  GetFigureSP_X(AIndex: integer): single;
       function  GetFigureSP_Y(AIndex: integer): single;

       function  GetFigureEP_X(AIndex: integer): single;
       function  GetFigureEP_Y(AIndex: integer): single;

       function  GetFigureCP_X(AIndex: integer): single;
       function  GetFigureCP_Y(AIndex: integer): single;

       function  GetFigureClassName(AIndex: integer): string;
       function  GetFigureKerfInfo(AIndex: integer): integer;
       function  GetFigureDirection(AIndex: integer): word;
       function  GetFigureProfilePointCount(AIndex: integer): integer;
       function  GetFigureProfilePoint(AFigureIndex, APointIndex: integer): TPoint2D;
       function  GetObject(AIndex: integer): TObject2D;

       procedure CreateRapidMove(AIndex: integer);  virtual; abstract;
       function  ProcessCounturStart(AKerfInfo, ALastKerfInfo: word): string; virtual; abstract;
       function  ProcessCounturEnd: string; virtual; abstract;
       function  ProgramEnd: string; virtual; abstract;
       procedure ProcessKerf(AKerfInfo, ALastKerfInfo: word); virtual; abstract;
       function  ProcessLine(AStartPointX, AStartPointY, AEndPointX, AEndPointY: single): string; virtual; abstract;
       function  ProcessArc(ACenterPointX, ACenterPointY, AStartPointX, AStartPointY,
                             AEndPointX, AEndPointY: single; ADirection: word): string; virtual; abstract;
     public
       function  GetSourceObjIDFromTraceList(AGeneratedLineIDX: integer): integer;
       function  GetGeneratedLineIDXFromTraceList(ASourceObjID: integer): integer;

       constructor create(AMachineFileName: string; ASourceCAD: TCADCmp2D; AProgressBar: TProgressBar);
       destructor  destroy; override;
       procedure   Reset;
       procedure   SaveToFile(AFileName: string);
       procedure   SaveToStringList(AStringList: TStringList);
       procedure   SaveTraceInfoToList(AList: TList);

       property   SourceCAD: TCADCmp2D read  fSourceCAD;
       property   ProgressBar: TProgressBar read fProgressBar write fProgressBar;
       property   OutputList : TStringList read fOutputList;
       property   TraceInfoList: TList read fTraceInfoList;
       property   CurrObj2D: TObject2D read  fCurrObj2D;
       property   MachineFileName : string   read fMachineFileName;
       property   IniFile: TIniFile read fIniFile;
       property   OutputKerfCommand: boolean read fOutputKerfCommand;
       property   OutputKerfValue: boolean read fOutputKerfValue;
       property   KerfValue : single read fKerfValue;
       property   UnitFactor: single read fUnitFactor;
       property   OutputProgramEndCmd: boolean read fOutputProgramEndCmd;
       //commands
       //property   CmdRelativeCoords: string; boolean;
       property   CmdKerfInsRight: string  read fCmdKerfInsRight;
       property   CmdKerfInsLeft: string   read fCmdKerfInsLeft;
       property   CmdKerfOutsRight: string read fCmdKerfOutsRight;
       property   CmdKerfOutsLeft : string read fCmdKerfOutsLeft;
       property   CmdRapidMoveON: string   read fCmdRapidMoveON;
       property   CmdRapidMoveOFF: string  read fCmdRapidMoveOFF;
       property   CmdToolDown: string      read fCmdToolDown;
       property   CmdToolUp: string        read  fCmdToolUp;
       property   CmdKerfOFF: string       read  fCmdKerfOFF;
       property   CmdCommentON: string     read  fCmdCommentON;
       property   CmdCommentOFF: string    read fCmdCommentOFF;
       property   CmdProgramEnd: string    read fCmdProgramEnd;
       property   LastKerfInfo: word       read fLastKerfInfo;
       property   KerfON: boolean          read fKerfON;
       property   CurrLineIDX: integer     read GetCurrLineIDX;
       property   NewLine: string          read fNewLine;

       {NewLine: string;

      //DIN
       LineNumStart,
       LineNumStep : integer;
       LineNumPrefix: string;

       CurrLineIDX: integer;
       RelativeProgram,
       Comment: boolean;}

     end;

implementation

function  TBaseCNCWriter.AddNewLine(ALine: string): integer;
begin
  if (ALine <> '') then
  begin
    OutputList.Add(ALine);
    result := GetCurrLineIDX;
  end
end;

function  TBaseCNCWriter.GetCurrLineIDX: integer;
begin
  if OutputList <> nil then
    result := OutputList.Count - 1
  else
    result := - 1;
end;

constructor TBaseCNCWriter.create(AMachineFileName: string; ASourceCAD: TCADCmp2D; AProgressBar: TProgressBar);
begin
  inherited create;
  fMachineFileName := AMachineFileName;
  fSourceCAD       := ASourceCAD;
  fProgressBar     := AProgressBar;
  fOutputList      := TStringList.Create;
  fTraceInfoList   := TList.Create;
  Init;
end;

procedure TBaseCNCWriter.Init;
begin
  fOutputList.Clear;
  fTraceInfoList.Clear;
  fCurrObj2D      := nil;
  Comment         := false;
  fUnitFactor     := 1;
  fKerfON         := true;
  //fProgramKerf  := false;
  fKerfValue      := 1.2;
  fNewLine        := chr(13) + chr(10);
  fCurrLineIDX    := 0;
  RelativeProgram := true;
  GenerateGlobalData;
end;

procedure TBaseCNCWriter.GenerateGlobalData;
var i, FiguresCount: integer; hKerfInfo: word;
begin
  //ProgramStart;
  fLastKerfInfo := INVALID_KERF_VALUE;
  FiguresCount := GetFiguresCount;
  ProgressBarReset(0, FiguresCount, 1);
  for i := 0 to FiguresCount - 1  do
  begin
    if IsCounturStart(i) then
    begin
      CreateRapidMove(i);
      fLastKerfInfo := INVALID_KERF_VALUE;
      hKerfInfo := GetFigureKerfInfo(i);
      ProcessCounturStart(hKerfInfo, fLastKerfInfo);
      fLastKerfInfo :=  hKerfInfo;
    end;
    ProcessFigure(i);
    if IsCounturEnd(i) then
      ProcessCounturEnd;
    ProgressBarStep;
  end;
  ProgressBarReset(0, FiguresCount, 1);
  ProgramEnd;
end;

procedure TBaseCNCWriter.ProcessFigure(AIndex: integer);
var hClassName: string;  hKerfInfo, hArcDirection: word;
spx, spy, epx, epy, cpx, cpy: single; TmpObj2D: TObject2D;
PointCount, idx1, idx2: integer; TmpPoint2D: TPoint2D;
begin
  TmpObj2D := GetObject(AIndex);
  if (TmpObj2D is TOutline2D) then
    TOutline2D(TmpObj2D).BeginUseProfilePoints;
  spx  := GetFigureSP_X(Aindex);
  spy  := GetFigureSP_Y(Aindex);
  epx  := GetFigureEP_X(Aindex);
  epy  := GetFigureEP_Y(Aindex);

  hClassName  :=  GetFigureClassName(AIndex);
  hKerfInfo   :=  GetFigureKerfInfo(AIndex);
  if (hClassName = 'TLine2D') then
  begin
    ProcessKerf(hKerfInfo, fLastKerfInfo);
    ProcessLine(spx, spy, epx, epy);
    fLastKerfInfo :=  hKerfInfo;
  end else if (hClassName = 'TArc2D') then
  begin
    ProcessKerf(hKerfInfo, fLastKerfInfo);
    fLastKerfInfo :=  hKerfInfo;
    cpx  := GetFigureCP_X(Aindex);
    cpy  := GetFigureCP_Y(Aindex);
    hArcDirection := GetFigureDirection(Aindex);
    ProcessArc(cpx, cpy, spx, spy, epx, epy, hArcDirection);
  end else if (TmpObj2D is  TOutline2D) then
  begin
    ProcessKerf(hKerfInfo, fLastKerfInfo);
    idx2 := GetFigureProfilePointCount(AIndex);
    for idx1 := 0 to  idx2 - 2 do
    begin
      TmpPoint2D := GetFigureProfilePoint(AIndex, idx1);
      spx  :=  TmpPoint2D.X;
      spy  :=  TmpPoint2D.Y;
      TmpPoint2D := GetFigureProfilePoint(AIndex, idx1 + 1);
      epx  := TmpPoint2D.X;
      epy  := TmpPoint2D.Y;
      ProcessLine(spx, spy, epx, epy);
    end;
    fLastKerfInfo :=  hKerfInfo;
  end; {else if (hClassName = 'TCircle2D_CPR') then
  begin
    ProcessKerf(hKerfInfo, LastKerfInfo);
    LastKerfInfo :=  hKerfInfo;
    cpx  := I_GetFigureCP_X(Aindex);
    cpy  := I_GetFigureCP_Y(Aindex);
    hArcDirection := I_GetFigureDirection(Aindex);
    ProcessArc(cpx, cpy, spx, spy, epx, epy, hArcDirection);
  end; }
    if (TmpObj2D is TOutline2D) then
    TOutline2D(TmpObj2D).EndUseProfilePoints;
end;

function TBaseCNCWriter.IsCounturStart(AFigureIndex: integer): boolean;
var hRes: boolean;
begin
  hRes := false;
  if (AFigureIndex = 0) then
     hRes := true
  else
    if (not CombinedFigures(AFigureIndex, AFigureIndex-1)) then
      hRes := true;
  result := hRes;
end;

function TBaseCNCWriter.IsCounturEnd(AFigureIndex: integer): boolean;
var hRes: boolean; FiguresCount: integer;
begin
  hRes := false;
  FiguresCount := GetFiguresCount;
  if (AFigureIndex = FiguresCount - 1) then
    hRes := true
  else
    if (not CombinedFigures(AFigureIndex+1, AFigureIndex)) then
      hRes := true;
  result := hRes;
end;

procedure TBaseCNCWriter.Reset;
begin
  fOutputList.Clear;
  fTraceInfoList.Clear;
  fCurrObj2D := nil;
end;

destructor TBaseCNCWriter.destroy;
begin
  fOutputList.Clear;
  fOutputList.Free;
  fTraceInfoList.Clear;
  fTraceInfoList.Free;
end;

procedure TBaseCNCWriter.SaveToFile(AFileName: string);
begin
  fOutputList.SaveToFile(AFileName);
end;

procedure   TBaseCNCWriter.SaveToStringList(AStringList: TStringList);
var i: integer;
begin
  if  (AStringList <> nil) then
  begin
    for i := 0 to  fOutputList.Count - 1 do
      AStringList.Add(fOutputList[i]);
  end;
end;

function  TBaseCNCWriter.GetSourceObjIDFromTraceList(AGeneratedLineIDX: integer): integer;
var i, TmpID: integer;
begin
  result := - 1;
  for i := 0 to fTraceInfoList.Count - 1 do
  begin
    if TExportTraceRec(fTraceInfoList.Items[i]^).GeneratedLineIDX = AGeneratedLineIDX then
    begin
      result :=  TExportTraceRec(fTraceInfoList.Items[i]^).SourceObjectID;
      exit;
    end;
  end;
end;

function TBaseCNCWriter.GetGeneratedLineIDXFromTraceList(ASourceObjID: integer): integer;
var i, TmpLineNum: integer;
begin
  result := - 1;
  for i := 0 to fTraceInfoList.Count - 1 do
  begin
    if TExportTraceRec(fTraceInfoList.Items[i]^).SourceObjectID = ASourceObjID then
    begin
      result := TExportTraceRec(fTraceInfoList.Items[i]^).GeneratedLineIDX;
      exit;
    end;
  end;
end;

procedure TBaseCNCWriter.SaveTraceInfoToList(AList: TList);
var i: integer; var PExportTraceRec: TPExportTraceRec;
begin
  if (AList <> nil) then
  begin
    for i := 0 to fTraceInfoList.Count - 1 do
    begin
      new(PExportTraceRec);
      PExportTraceRec^.GeneratedLineIDX  := TExportTraceRec(fTraceInfoList.Items[i]^).GeneratedLineIDX;
      PExportTraceRec^.SourceObjectID    := TExportTraceRec(fTraceInfoList.Items[i]^).SourceObjectID;
      PExportTraceRec^.Description       := TExportTraceRec(fTraceInfoList.Items[i]^).Description;
      AList.Add(PExportTraceRec);
    end;
  end;
end;

function  TBaseCNCWriter.GetFiguresCount: integer;
begin
  result := 0;
  if (fSourceCAD <> nil) then
    result := fSourceCAD.ObjectsCount;
end;

function  TBaseCNCWriter.GetFigureKerfInfo(AIndex: integer): integer;
var TmpObj2D: TObject2D;
begin
  result := INVALID_KERF_VALUE;
  if (fSourceCAD <> nil) then
  begin
    TmpObj2D := fSourceCAD.GetObject(AIndex);
    if (TmpObj2D <> nil) then
      result := TmpObj2D.ReserveInt1;
  end;
end;
                               
function  TBaseCNCWriter.GetFigureSP_X(AIndex: integer): single;
var TmpObj2D: TObject2D;  i: integer;
begin
  result := INVALID_FLOAT_VALUE;
  if (fSourceCAD <> nil) then
  begin
    TmpObj2D := fSourceCAD.GetObject(AIndex);
    if (TmpObj2D <> nil) then
    begin
      if (TmpObj2D is TLine2D) then
        result := TransformPoint2D(TLine2D(TmpObj2D).StartPoint, TLine2D(TmpObj2D).ModelTransform).X
      else if (TmpObj2D is TOutline2D) then
      begin
        TOutline2D(TmpObj2D).BeginUseProfilePoints;
        result := TransformPoint2D(TOutline2D(TmpObj2D).StartPoint, TOutline2D(TmpObj2D).ModelTransform).X;
        //result := TOutline2D(TmpObj2D).StartPoint.X;
        TOutline2D(TmpObj2D).EndUseProfilePoints;
      end;
    end;
  end;
end;

function  TBaseCNCWriter.GetFigureSP_Y(AIndex: integer): single;
var TmpObj2D: TObject2D;
begin
  result := INVALID_FLOAT_VALUE;
  if (fSourceCAD <> nil) then
  begin
    TmpObj2D := fSourceCAD.GetObject(AIndex);
    if (TmpObj2D <> nil) then
    begin
      if (TmpObj2D is TLine2D) then
        result := TransformPoint2D(TLine2D(TmpObj2D).StartPoint, TLine2D(TmpObj2D).ModelTransform).Y
      else if (TmpObj2D is TOutline2D) then
      begin
        TOutline2D(TmpObj2D).BeginUseProfilePoints;
        result := TransformPoint2D(TOutline2D(TmpObj2D).StartPoint, TOutline2D(TmpObj2D).ModelTransform).Y;
        //result := TOutline2D(TmpObj2D).StartPoint.Y;
        TOutline2D(TmpObj2D).EndUseProfilePoints;
      end;
    end;
  end;
end;

function  TBaseCNCWriter.GetFigureEP_X(AIndex: integer): single;
var TmpObj2D: TObject2D; TmpPointIdx: integer;
begin
  result := INVALID_FLOAT_VALUE;
  if (fSourceCAD <> nil) then
  begin
    TmpObj2D := fSourceCAD.GetObject(AIndex);
    if (TmpObj2D <> nil) then
    begin
      if (TmpObj2D is TLine2D) then
        result := TransformPoint2D(TLine2D(TmpObj2D).EndPoint, TLine2D(TmpObj2D).ModelTransform).X
      else if (TmpObj2D is TOutline2D) then
      begin
        TOutline2D(TmpObj2D).BeginUseProfilePoints;
        result := TransformPoint2D(TOutline2D(TmpObj2D).EndPoint, TOutline2D(TmpObj2D).ModelTransform).X;
        //result := TOutline2D(TmpObj2D).EndPoint.X;
        TOutline2D(TmpObj2D).EndUseProfilePoints;
      end;
    end;
  end;
end;

function  TBaseCNCWriter.GetFigureEP_Y(AIndex: integer): single;
var TmpObj2D: TObject2D;
begin
  result := INVALID_FLOAT_VALUE;
  if (fSourceCAD <> nil) then
  begin
    TmpObj2D := fSourceCAD.GetObject(AIndex);
    if (TmpObj2D <> nil) then
    begin
      if (TmpObj2D is TLine2D) then
        result := TransformPoint2D(TLine2D(TmpObj2D).EndPoint, TLine2D(TmpObj2D).ModelTransform).Y
      else if (TmpObj2D is TOutline2D) then
      begin
        TOutline2D(TmpObj2D).BeginUseProfilePoints;
        result := TransformPoint2D(TOutline2D(TmpObj2D).EndPoint, TOutline2D(TmpObj2D).ModelTransform).Y;
        //result := TOutline2D(TmpObj2D).EndPoint.Y;
        TOutline2D(TmpObj2D).EndUseProfilePoints;
      end;
    end;
  end;
end;

function  TBaseCNCWriter.GetFigureCP_X(AIndex: integer): single;
var TmpObj2D: TObject2D;
begin
  result := INVALID_FLOAT_VALUE;
  if (fSourceCAD <> nil) then
  begin
    TmpObj2D := fSourceCAD.GetObject(AIndex);
    if (TmpObj2D <> nil) then
      if (TmpObj2D is TOutline2D) then
      begin
        TOutline2D(TmpObj2D).BeginUseProfilePoints;
        result := TransformPoint2D(TOutline2D(TmpObj2D).CenterPoint, TOutline2D(TmpObj2D).ModelTransform).X;
        //result :=  TOutline2D(TmpObj2D).CenterPoint.X;
        TOutline2D(TmpObj2D).EndUseProfilePoints;
      end;
  end;
end;

function  TBaseCNCWriter.GetFigureCP_Y(AIndex: integer): single;
var TmpObj2D: TObject2D;
begin
  result := INVALID_FLOAT_VALUE;
  if (fSourceCAD <> nil) then
  begin
    TmpObj2D := fSourceCAD.GetObject(AIndex);
    if (TmpObj2D <> nil) then
      if (TmpObj2D is TOutline2D) then
      begin
        TOutline2D(TmpObj2D).BeginUseProfilePoints;
        result := TransformPoint2D(TOutline2D(TmpObj2D).CenterPoint, TOutline2D(TmpObj2D).ModelTransform).Y;
        //result :=  TOutline2D(TmpObj2D).CenterPoint.Y;
        TOutline2D(TmpObj2D).EndUseProfilePoints;
      end;
  end;
end;

function  TBaseCNCWriter.GetFigureDirection(AIndex: integer): word;
var TmpObj2D: TObject2D;
begin
  result := INVALID_DIRECTION_VALUE;
  if (fSourceCAD <> nil) then
  begin
    TmpObj2D := fSourceCAD.GetObject(AIndex);
    if (TmpObj2D <> nil) then
      if (TmpObj2D is TArc2D)              then result := word(TArc2D(TmpObj2D).Direction)
      else if (TmpObj2D is TCircle2D)      then result := word(TCircle2D(TmpObj2D).Direction)
      else if (TmpObj2D is TCircle2D_CPR)  then result := word(TCircle2D_CPR(TmpObj2D).Direction)
      else if (TmpObj2D is TCiArc2D_BBOX)  then result := word(TCiArc2D_BBOX(TmpObj2D).Direction);
  end;
end;

function  TBaseCNCWriter.GetFigureProfilePointCount(AIndex: integer): integer;
var TmpObj2D: TObject2D;
begin
  if (fSourceCAD <> nil) then
  begin
    TmpObj2D := fSourceCAD.GetObject(AIndex);
    if (TmpObj2D is TLine2D) then
      result := TLine2D(TmpObj2D).Points.Count
    else if (TmpObj2D is TOutline2D) then
    begin
      TOutline2D(TmpObj2D).BeginUseProfilePoints;
      result := TOutline2D(TmpObj2D).ProfilePoints.Count;
      TOutline2D(TmpObj2D).EndUseProfilePoints;
    end
  end;
end;

function  TBaseCNCWriter.GetFigureProfilePoint(AFigureIndex, APointIndex: integer): TPoint2D;
var TmpObj2D: TObject2D; TmpPoint2D: TPoint2D;
begin
  if (fSourceCAD <> nil) then
  begin
     TmpObj2D := fSourceCAD.GetObject(AFigureIndex);
    if (TmpObj2D is TOutline2D) then
    begin
      TOutline2D(TmpObj2D).BeginUseProfilePoints;
      result := TransformPoint2D(TOutline2D(TmpObj2D).ProfilePoints[APointIndex], TOutline2D(TmpObj2D).ModelTransform);
      //result := TOutline2D(TmpObj2D).ProfilePoints[APointIndex];
      TOutline2D(TmpObj2D).EndUseProfilePoints;
    end else
    begin
      //
    end;
  end;
end;

function  TBaseCNCWriter.GetObject(AIndex: integer): TObject2D;
begin
  if (fSourceCAD <> nil) then
    result := fSourceCAD.GetObject(AIndex)
  else
    result := nil;
end;

function  TBaseCNCWriter.GetFigureClassName(AIndex: integer): string;
var TmpObj2D: TObject2D;
begin
  result := INVALID_STRING_VALUE;
  if (fSourceCAD <> nil) then
  begin
    TmpObj2D := fSourceCAD.GetObject(AIndex);
    if (TmpObj2D <> nil) then
      result := TmpObj2D.ClassName;
  end;
end;

function  TBaseCNCWriter.CombinedFigures(AIndex1, AIndex2: integer): boolean;
var x1, y1, x2, y2: single; hRes: boolean;  Point1, Point2: TPoint2D;
begin
  Point1 := Point2D(GetFigureSP_X(AIndex1), GetFigureSP_Y(AIndex1));
  Point2 := Point2D(GetFigureEP_X(AIndex2), GetFigureEP_Y(AIndex2));
  hRes := ( (abs(Point1.x - Point2.x) <= MIN_FIGURE_DISTANCE) and
              (abs(Point1.y - Point2.y) <= MIN_FIGURE_DISTANCE)
            );

  result := hRes;
end;

procedure TBaseCNCWriter.ProgressBarReset(AMin, AMax, AStep: integer);
begin
  if fProgressBar <> nil then
  begin
    fProgressBar.Min := AMin;
    fProgressBar.Max := AMax;
    fProgressBar.Step := AStep;
    fProgressBar.Position := 0;
  end;
end;

procedure TBaseCNCWriter.ProgressBarStep;
begin
  if fProgressBar <> nil then
  begin
    fProgressBar.StepIt;
  end;
end;

end.

