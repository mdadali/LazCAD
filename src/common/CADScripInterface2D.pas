unit CADScripInterface2D;

interface

uses

  RTTIGrids,
  PropEdits,
  ObjectInspector,

  classes, ComCtrls,
  CS4BaseTypes,
  CS4Tasks,
  CS4Shapes,
  CADSys4,
  CS4DXFModule,

  cImportESSI,

  CADDocument,
  camh,
  applicationh;
  

var ObjectsIterator: TExclusiveGraphicObjIterator;

    CADPrg2D:  TCADPrg2D;
    CADCmp2D:  TCADCmp2D;
    CADViewport2D: TCADViewport2D;
    ProgressBar: TProgressBar;
    PropertyGrid: TTIPropertyGrid;
  

procedure GetActiveDocument;

procedure I_Regen;

procedure I_DrawLine(AX, AY, BX, BY: TRealType);
procedure I_DrawArc(AX, AY, BX, BY, SA, EA: TRealType; ADirection: word);
procedure I_Refresh;
procedure I_DeleteObjects;
procedure I_ProgressBarReset(AMin, AMax, AStep: integer);
procedure I_ProgressBarStep; 

function  I_GetFiguresCount: integer;

function  I_GetFigureSP_X(AIndex: integer): single;
function  I_GetFigureSP_Y(AIndex: integer): single;

function  I_GetFigureEP_X(AIndex: integer): single;
function  I_GetFigureEP_Y(AIndex: integer): single;

//function  I_GetFigureCP_X(AIndex: integer): single;
//function  I_GetFigureCP_Y(AIndex: integer): single;


function  I_GetFigureClassName(AIndex: integer): string;
function  I_GetFigureKerfInfo(AIndex: integer): TKerfType;
//function  I_GetFigureDirection(AIndex: integer): word;
function  I_GetFigureProfilePointCount(AIndex: integer): integer;


function  I_GetFigureProfilePoint(AFigureIndex, APointIndex: integer): TPoint2D;
function  I_GetFigureProfilePointX(AFigureIndex, APointIndex: integer): TRealType;
function  I_GetFigureProfilePointY(AFigureIndex, APointIndex: integer): TRealType;


function  I_GetObject(AIndex: integer): TObject2D;

//function  I_CombinedFigures(AIndex1, AIndex2: integer): boolean;

function CreateLine(AX, AY, BX, BY: single): TLine2D;

function I_OpenIterator: TExclusiveGraphicObjIterator;

function I_CloseIterator: word;

procedure I_ImportDXF;
//procedure I_ImportGCODE;
procedure I_ImportESSI;

procedure I_ExportDXF;
//procedure I_ExportGCODE;
//procedure I_ExportESSI;


var ExportFileName: string;

implementation

procedure GetActiveDocument;
begin
  CADDocument.GetDocument(CADPrg2D, CADCmp2D, CADViewport2D, ProgressBar, PropertyGrid);
end;

procedure I_DrawLine(AX, AY, BX, BY: TRealType);
var TmpLine2D: TLine2D;
begin
  GetActiveDocument;
  if CADCmp2D = nil then exit;
  TmpLine2D := TLine2D.Create(-1, Point2D(AX, AY), Point2D(BX, BY));
  CADCmp2D.AddObject(TmpLine2D.ID, TmpLine2D);
end;

procedure I_ImportDXF;
var DXF2DImport: TDXF2DImport;
begin
  GetActiveDocument;
  if CADCmp2D = nil then exit;
  try
    DXF2DImport := TDXF2DImport.Create(ImportFileName, CADCmp2D);
    DXF2DImport.SetTextFont(CADSysFindFontByIndex(0));
    DXF2DImport.ReadDXF;
  finally
    DXF2DImport.Free;
  end;
  if (CADCmp2D.Viewports[0] <> nil) then
    CADCmp2D.Viewports[0].ZoomToExtension;
end;

procedure I_ImportESSI;
var ImportEssi: TImportEssi;
begin
  GetActiveDocument;
  if CADCmp2D = nil then exit;
  try
    ImportEssi := TImportEssi.create(CADCmp2D, CADViewport2D, ProgressBar);
    ImportEssi.LoadFromFile(ImportFileName);
    ImportEssi.ImportMoves := false;
    ImportEssi.Import;
  finally
    ImportEssi.Free;
  end;
  if (CADCmp2D.Viewports[0] <> nil) then
    CADCmp2D.Viewports[0].ZoomToExtension;
end;

procedure I_ExportDXF;
var DXF2DExport: TDXF2DExport;
begin
  GetActiveDocument;
  if CADCmp2D = nil then exit;
  DXF2DExport := DXF2DExport.Create(ExportFileName, CADCmp2D);
  try
    DXF2DExport.WriteDXF;
  finally
    DXF2DExport.Free;
  end;
end;

{procedure I_ExportESSI;
var ExportESSI: TExportESSI;
begin
  ExportESSI := TExportESSI.create(GetAppStdESSIOutputMachineFileName);
  try
    ExportESSI.SaveToFile(ExportFileName);
  finally
    ExportESSI.Free;
  end;
end;}

procedure I_Regen;
var TmpFileName: string;
begin
  GetActiveDocument;
  if CADCmp2D = nil then exit;
  try
    TmpFileName := GetAppTempPath + 'temp.tmp';
    CADCmp2D.SaveToFile(TmpFileName);
    CADCmp2D.LoadFromFile(TmpFileName);
  finally
    //
    //TmpStream.Free;
  end;
end;

function I_OpenIterator: TExclusiveGraphicObjIterator;
begin
  GetActiveDocument;
  if CADCmp2D = nil then exit;
  try
    ObjectsIterator := CADCmp2D.ObjectsExclusiveIterator;
  except
    ObjectsIterator := nil;
  end;
  result := ObjectsIterator;
end;

function I_CloseIterator: word;
begin
  GetActiveDocument;
  if CADCmp2D = nil then exit;
  if ObjectsIterator <> nil then
  begin
    ObjectsIterator.Free;
    ObjectsIterator := nil;
  end;
  result := 0;
end;

function CreateLine(AX, AY, BX, BY: single): TLine2D;
begin
  result := nil;
  GetActiveDocument;
  if CADCmp2D = nil then exit;
  result := TLine2D.Create(-1, Point2D(AX, AY), Point2D(BX, BY));
end;

function  I_GetFiguresCount: integer;
begin
  result := 0;
  GetActiveDocument;
  if CADCmp2D = nil then exit;
  result := CADCmp2D.ObjectsCount;
end;

function  I_GetFigureKerfInfo(AIndex: integer): TKerfType;
var TmpObj2D: TObject2D;
begin
  result := None;
  GetActiveDocument;
  if CADCmp2D = nil then exit;

  TmpObj2D := CADCmp2D.GetObject(AIndex);
  if (TmpObj2D <> nil) then
    result := TKerfType(TmpObj2D.fReserveInt1);
end;

function  I_GetFigureSP_X(AIndex: integer): single;
var TmpObj2D: TObject2D;
begin
  result := INVALID_FLOAT_VALUE;
  GetActiveDocument;
  if CADCmp2D = nil then exit;

  TmpObj2D := CADCmp2D.GetObject(AIndex);
  if (TmpObj2D <> nil) then
    result :=  TPrimitive2D(TmpObj2D).StartPoint.X;
end;

function  I_GetFigureSP_Y(AIndex: integer): single;
var TmpObj2D: TObject2D;
begin
  result := INVALID_FLOAT_VALUE;
  GetActiveDocument;
  if CADCmp2D = nil then exit;

  TmpObj2D := CADCmp2D.GetObject(AIndex);
  if (TmpObj2D <> nil) then
    result :=  TPrimitive2D(TmpObj2D).StartPoint.Y;
end;

function  I_GetFigureEP_X(AIndex: integer): single;
var TmpObj2D: TObject2D;
begin
  result := INVALID_FLOAT_VALUE;
  GetActiveDocument;
  if CADCmp2D = nil then exit;

  TmpObj2D := CADCmp2D.GetObject(AIndex);
  if (TmpObj2D <> nil) then
    result :=  TPrimitive2D(TmpObj2D).EndPoint.X;
end;

function  I_GetFigureEP_Y(AIndex: integer): single;
var TmpObj2D: TObject2D;
begin
  result := INVALID_FLOAT_VALUE;
  GetActiveDocument;
  if CADCmp2D = nil then exit;

  TmpObj2D := CADCmp2D.GetObject(AIndex);
  if (TmpObj2D <> nil) then
    result :=  TPrimitive2D(TmpObj2D).EndPoint.Y;
end;

{function  I_GetFigureCP_X(AIndex: integer): single;
var TmpObj2D: TObject2D;
begin
  result := INVALID_FLOAT_VALUE;
  if (CADCmp2D <> nil) then
  begin
    TmpObj2D := CADCmp2D.GetObject(AIndex);
    if (TmpObj2D <> nil) then
      if (TmpObj2D is TCircle2D) then
     result :=  TmpObj2D.CenterPoint.X;
  end;
end;

function  I_GetFigureCP_Y(AIndex: integer): single;
var TmpObj2D: TObject2D;
begin
  result := INVALID_FLOAT_VALUE;
  if (CADCmp2D <> nil) then
  begin
    TmpObj2D := CADCmp2D.GetObject(AIndex);
    if (TmpObj2D <> nil) then
     result :=  TPrimitive2D(TmpObj2D).CenterPoint.Y;
  end;
end;

function  I_GetFigureDirection(AIndex: integer): word;
var TmpObj2D: TObject2D;
begin
  result := INVALID_DIRECTION_VALUE;
  if (CADCmp2D <> nil) then
  begin
    TmpObj2D := CADCmp2D.GetObject(AIndex);
    if (TmpObj2D <> nil) then

      if (TmpObj2D is TCIArc2D_CSE) then
        if TCIArc2D_CSE(TmpObj2D).Direction = adCounterClockwise then
          result := 1
        else
          result := 0

      else if (TmpObj2D is TArc2D) then
        if TArc2D(TmpObj2D).Direction = adCounterClockwise then
          result := 1
        else
          result := 0

      else if (TmpObj2D is TCircle2D)  then
        if TCircle2D(TmpObj2D).Direction = adCounterClockwise then
          result := 1
        else
          result := 0

      else if (TmpObj2D is TCircle2D_CPR)  then
        if TCircle2D_CPR(TmpObj2D).Direction = adCounterClockwise then
          result := 1
        else
          result := 0
  end;
end;
 }
function  I_GetFigureProfilePointCount(AIndex: integer): integer;
var TmpObj2D: TObject2D;
begin
  result := 0;
  GetActiveDocument;
  if CADCmp2D = nil then exit;

  TmpObj2D := CADCmp2D.GetObject(AIndex);
  if (TmpObj2D is TLine2D) then
    result := TLine2D(TmpObj2D).Points.Count
  else if (TmpObj2D is TOutline2D) then
  begin
    TOutline2D(TmpObj2D).BeginUseProfilePoints;
    result := TOutline2D(TmpObj2D).ProfilePoints.Count;
    TOutline2D(TmpObj2D).EndUseProfilePoints;
  end
end;

function  I_GetFigureProfilePoint(AFigureIndex, APointIndex: integer): TPoint2D;
var TmpObj2D: TObject2D; TmpPoint2D: TPoint2D;
begin
  GetActiveDocument;
  if CADCmp2D = nil then exit;

  TmpObj2D := CADCmp2D.GetObject(AFigureIndex);
  if (TmpObj2D is TOutline2D) then
  begin
    TOutline2D(TmpObj2D).BeginUseProfilePoints;
    //result := TransformPoint2D(TOutline2D(TmpObj2D).ProfilePoints[APointIndex], TOutline2D(TmpObj2D).ModelTransform);
    result := TOutline2D(TmpObj2D).ProfilePoints[APointIndex];
    TOutline2D(TmpObj2D).EndUseProfilePoints;
  end else
  begin
    result := TPrimitive2D(TmpObj2D).Points[APointIndex];
  end;
end;

function  I_GetFigureProfilePointX(AFigureIndex, APointIndex: integer): TRealType;
begin
  GetActiveDocument;
  if CADCmp2D = nil then exit;
  result := I_GetFigureProfilePoint(AFigureIndex, APointIndex).X;
end;

function  I_GetFigureProfilePointY(AFigureIndex, APointIndex: integer): TRealType;
begin
  GetActiveDocument;
  if CADCmp2D = nil then exit;
  result := I_GetFigureProfilePoint(AFigureIndex, APointIndex).Y;
end;

function  I_GetObject(AIndex: integer): TObject2D;
begin
  result := nil;
  GetActiveDocument;
  if CADCmp2D = nil then exit;
  result := CADCmp2D.GetObject(AIndex)
end;

function  I_GetFigureClassName(AIndex: integer): string;
var TmpObj2D: TObject2D;
begin
  //result := INVALID_STRING_VALUE;
  GetActiveDocument;
  if CADCmp2D = nil then exit;
  TmpObj2D := CADCmp2D.GetObject(AIndex);
  if (TmpObj2D <> nil) then
    result := TmpObj2D.ClassName;
end;

{function  I_CombinedFigures(AIndex1, AIndex2: integer): boolean;
var x1, y1, x2, y2: single; hRes: boolean;  Point1, Point2: TPoint2D;
begin
  Point1 := Point2D(I_GetFigureSP_X(AIndex1), I_GetFigureSP_Y(AIndex1));
  Point2 := Point2D(I_GetFigureEP_X(AIndex2), I_GetFigureEP_Y(AIndex2));
  hRes := ( (abs(Point1.x - Point2.x) <= MIN_FIGURE_DISTANCE) and
              (abs(Point1.y - Point2.y) <= MIN_FIGURE_DISTANCE)
            );

  result := hRes;
end;
}

procedure I_ProgressBarReset(AMin, AMax, AStep: integer);
begin
  if ProgressBar <> nil then
  begin
    ProgressBar.Min      := AMin;
    ProgressBar.Max      := AMax;
    ProgressBar.Step     := AStep;
    ProgressBar.Position := 0;
  end;
end;

procedure I_ProgressBarStep;
begin
  if ProgressBar <> nil then
  begin
    ProgressBar.StepIt;
  end;
end;

procedure I_DeleteObjects;
begin
  GetActiveDocument;
  if CADCmp2D = nil then exit;

  CADCmp2D.DeleteSavedSourceBlocks;
  CADCmp2D.DeleteAllObjects;
  CADCmp2D.DeleteAllSourceBlocks;
end;

procedure I_Refresh;
begin
  if CADViewport2D <> nil then
    CADViewport2D.ZoomToExtension;
end;

procedure I_DrawArc(AX, AY, BX, BY, SA, EA: TRealType; ADirection: word);
var TmpArc2D: TEllipticalArc2D;
begin
  GetActiveDocument;
  if CADCmp2D = nil then exit;

  if SA = 0 then SA := 0.001;
  if EA = 0 then EA := 0.001;
  TmpArc2D := TEllipticalArc2D.Create(-1, Point2D(AX, AY), Point2D(BX, BY), SA, EA);
  if ADirection = 0 then
    TmpArc2D.Direction := adCounterClockwise
  else
    TmpArc2D.Direction := adClockwise;
  CADCmp2D.AddObject(TmpArc2D.ID, TmpArc2D);
end;

initialization
  CADCmp2D := nil;
  CADViewport2D   := nil;
  ProgressBar     := nil;
  PropertyGrid    := nil;

  ObjectsIterator := nil;
finalization

end.

