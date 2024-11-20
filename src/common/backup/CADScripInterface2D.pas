unit CADScripInterface2D;

interface

uses
  Dialogs,
  Graphics,
  RTTIGrids,
  PropEdits,
  ObjectInspector,

  SysUtils,
  classes,
  ComCtrls,
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

function CAD_Rect2D(var Left, Bottom, Right, Top: TRealType): TRect2D;

procedure CAD_Regen;
procedure CAD_Repaint;
procedure CAD_Clear;
procedure CAD_ZoomToExtentions;

function CAD_DrawLine2D(AID: longint;  var P0, P1: TPoint2D): integer;
function CAD_DrawEllipticalArc2D(AID: longint; var P0, P1: TPoint2D; SA, EA: TRealType; ADirection: TArcDirection): longint;
function CAD_DrawCircularArc2D(AID: longint; var CP: TPoint2D; R, SA, EA: TRealType; ADirection: TArcDirection): longint;
function CAD_DrawCircle2D(AID: longint; var CP: TPoint2D; R, SA: TRealType; ADirection: TArcDirection): longint;
function CAD_DrawPolyline2D(AID: longint): longint;
function CAD_DrawFrame2D(AID: longint; var P0, P1: TPoint2D; ADirection: TArcDirection): longint;
function CAD_DrawPolygon2D(AID: longint; var ADirection: TArcDirection): longint;
function CAD_DrawBSPline2D(AID: longint): longInt;
function CAD_DrawEllipse2D(AID: longint; var P0, P1: TPoint2D; ACurvePrecision: word; ADirection: TArcDirection): longint;
function CAD_DrawSector2D(AID:  longint; var CP: TPoint2D; R, SA, EA: TRealType; ACurvePrecision: word; ADirection: TArcDirection): longint;
function CAD_DrawSegment2D(AID: longint; var CP: TPoint2D; R, SA, EA: TRealType; ACurvePrecision: word; ADirection: TArcDirection): longint;
function CAD_DrawSymetricSymbol2D(AID:  longint; var CP: TPoint2D; R, SA: TRealType; ACurvePrecision: word; ADirection: TArcDirection): longint;
function CAD_DrawASymetricSymbol2D(AID: longint; var P0, P1: TPoint2D; R, SA: TRealType; ACurvePrecision: word; ADirection: TArcDirection): longint;
function CAD_DrawText2D(AID: longint; var ARect2D: TRect2D; AHeight: TrealType; AStr: String): longint;
function CAD_DrawJustifiedVectText2D(AID: longint; AFontIndex: word; var ARect2D: TRect2D; AHeight: TrealType; AStr: string): longint;
function CAD_DrawBitmap2D(AID: longint; var P0, P1: TPoint2D; AFileName: string): longInt;


function CAD_Polyline2DAddPoint2D(AIDX: longint; var APoint2D: TPoint2D): longint;
function CAD_Polygon2DAddPoint2D(AIDX: longint; var APoint2D: TPoint2D): longint;
function CAD_BSpline2DAddPoint2D(AIDX: longint; var APoint2D: TPoint2D): longint;

//MakeBlock2D

function CAD_SetColor(AID:integer; AColor: TColor): integer;

function CAD_IntToStr(AValue: integer): string;

procedure CAD_Refresh;
procedure CAD_DeleteObjects;
procedure CAD_ProgressBarReset(AMin, AMax, AStep: integer);
procedure CAD_ProgressBarStep; 

function  CAD_GetFiguresCount: integer;

function  CAD_GetFigureSP_X(AIndex: integer): TRealType;
function  CAD_GetFigureSP_Y(AIndex: integer): TRealType;

function  CAD_GetFigureEP_X(AIndex: integer): TRealType;
function  CAD_GetFigureEP_Y(AIndex: integer): TRealType;

//function  CAD_GetFigureCP_X(AIndex: integer): TRealType;
//function  CAD_GetFigureCP_Y(AIndex: integer): TRealType;


function  CAD_GetFigureClassName(AIndex: integer): string;
function  CAD_GetFigureKerfInfo(AIndex: integer): TKerfType;
//function  CAD_GetFigureDirection(AIndex: integer): word;
function  CAD_GetFigureProfilePointCount(AIndex: integer): integer;


function  CAD_GetFigureProfilePoint(AFigureIndex, APointIndex: integer): TPoint2D;
function  CAD_GetFigureProfilePointX(AFigureIndex, APointIndex: integer): TRealType;
function  CAD_GetFigureProfilePointY(AFigureIndex, APointIndex: integer): TRealType;


function  CAD_GetObject(AIndex: integer): TObject2D;

//function  CAD_CombinedFigures(AIndex1, AIndex2: integer): boolean;

function CreateLine(AX, AY, BX, BY: TRealType): TLine2D;

function CAD_OpenIterator: TExclusiveGraphicObjIterator;

function CAD_CloseIterator: word;

procedure CAD_ImportDXF;
//procedure CAD_ImportGCODE;
procedure CAD_ImportESSI;

procedure CAD_ExportDXF;
//procedure CAD_ExportGCODE;
//procedure CAD_ExportESSI;


var ExportFileName: string;

implementation

procedure GetActiveDocument;
begin
  CADDocument.GetDocument(CADPrg2D, CADCmp2D, CADViewport2D, ProgressBar, PropertyGrid);
end;

function CAD_Rect2D(var Left, Bottom, Right, Top: TRealType): TRect2D;
begin
  Result := Rect2D(Left, Bottom, Right, Top);
end;

function CAD_IntToStr(AValue: integer): string;
begin
  result := IntToStr(AValue);
end;

function CAD_DrawLine2D(AID: longint;  var P0, P1: TPoint2D): integer;
var TmpLine2D: TLine2D;
begin
  result := -1;
  GetActiveDocument;
  if CADCmp2D = nil then exit;
  TmpLine2D := TLine2D.Create(AID, P0, P1);
  //TmpLine2D.Points[0].W := P0_W;
  //TmpLine2D.Points[1].W := P1_W;
  CADCmp2D.AddObject(AID, TmpLine2D);
  result := TmpLine2D.ID;
end;

function CAD_DrawEllipticalArc2D(AID: longint; var P0, P1: TPoint2D; SA, EA: TRealType; ADirection: TArcDirection): longint;
var TmpArc2D: TEllipticalArc2D;
begin
  GetActiveDocument;
  if CADCmp2D = nil then exit;

  if SA = 0 then SA := 0.001;
  if EA = 0 then EA := 0.001;
  TmpArc2D := TEllipticalArc2D.Create(-1, P0, P1, SA, EA);
  TmpArc2D.Direction := ADirection;
  CADCmp2D.AddObject(AID, TmpArc2D);
  result := TmpArc2D.ID;
end;

function CAD_DrawCircularArc2D(AID: longint; var CP: TPoint2D; R, SA, EA: TRealType; ADirection: TArcDirection): longint;
var TmpArc2D: TCircularArc2D;
begin
  GetActiveDocument;
  if CADCmp2D = nil then exit;

  if SA = 0 then SA := 0.001;
  if EA = 0 then EA := 0.001;
  TmpArc2D := TCircularArc2D.Create(-1, CP, R, DegToRad(SA), DegToRad(EA));
  TmpArc2D.Direction := ADirection;
  CADCmp2D.AddObject(AID, TmpArc2D);
  result := TmpArc2D.ID;
end;

function CAD_DrawCircle2D(AID: longint; var CP: TPoint2D; R, SA: TRealType; ADirection: TArcDirection): longint;
var TmpCircle2D: TCircle2D;
begin
  result := -1;
  GetActiveDocument;
  TmpCircle2D := TCircle2D.Create(-1, CP, R);
  TmpCircle2D.StartAngle := SA;
  TmpCircle2D.Direction := ADirection;
  CADCmp2D.AddObject(AID, TmpCircle2D);
  result := TmpCircle2D.ID;
end;

function CAD_DrawFrame2D(AID: longint; var P0, P1: TPoint2D; ADirection: TArcDirection): longint;
var TmpFrame2D: TFrame2D;
begin
  result := -1;
  GetActiveDocument;
  if CADCmp2D = nil then exit;
  TmpFrame2D := TFrame2D.Create(AID, P0, P1);
  TmpFrame2D.Direction := ADirection;
  CADCmp2D.AddObject(AID, TmpFrame2D);
  result := TmpFrame2D.ID;
end;

function CAD_DrawPolyline2D(AID: longint): longint;
var TmpPolyline2D: TPolyline2D;
begin
  result := -1;
  GetActiveDocument;
  if CADCmp2D = nil then exit;
  TmpPolyline2D := TPolyline2D.Create(AID, []);
  CADCmp2D.AddObject(AID, TmpPolyline2D);
  result := TmpPolyline2D.ID;
end;

function CAD_Polyline2DAddPoint2D(AIDX: longint; var APoint2D: TPoint2D): longint;
var TmpObject2D: TObject2D;
begin
  result := -1;
  GetActiveDocument;
  if CADCmp2D = nil then exit;
  TmpObject2D := CAD_GetObject(AIDX);
  if (TmpObject2D is TPolyline2D) then
  begin
    TPolyline2D(TmpObject2D).Points.Add(APoint2D);
    result := 0;
  end;
end;

function CAD_DrawPolygon2D(AID: longint; var ADirection: TArcDirection): longint;
var TmpPolygon2D: TPolygon2D;
begin
  result := -1;
  GetActiveDocument;
  if CADCmp2D = nil then exit;
  TmpPolygon2D := TPolygon2D.Create(AID, []);
  TmpPolygon2D.Direction := ADirection;
  CADCmp2D.AddObject(AID, TmpPolygon2D);
  result := TmpPolygon2D.ID;
end;

function CAD_Polygon2DAddPoint2D(AIDX: longint; var APoint2D: TPoint2D): longint;
var TmpObject2D: TObject2D;
begin
  result := -1;
  GetActiveDocument;
  if CADCmp2D = nil then exit;
  TmpObject2D := CAD_GetObject(AIDX);
  if (TmpObject2D is TPolygon2D) then
  begin
    TPolygon2D(TmpObject2D).Points.Add(APoint2D);
    result := 0;
  end;
end;

function CAD_DrawBSPline2D(AID: longint): longInt;
var TmpBSpline2D: TBSpline2D;
begin
  result := -1;
  GetActiveDocument;
  if CADCmp2D = nil then exit;
  TmpBSpline2D := TBSpline2D.Create(AID, []);
  CADCmp2D.AddObject(TmpBSpline2D.ID, TmpBSpline2D);
  result := TmpBSpline2D.ID;
end;

function CAD_BSpline2DAddPoint2D(AIDX: longint; var APoint2D: TPoint2D): longint;
var TmpObject2D: TObject2D;
begin
  result := -1;
  GetActiveDocument;
  if CADCmp2D = nil then exit;
  TmpObject2D := CAD_GetObject(AIDX);
  if (TmpObject2D is TBSpline2D) then
  begin
    TBSpline2D(TmpObject2D).Points.Add(APoint2D);
    result := 0;
  end;
end;


function CAD_DrawEllipse2D(AID: longint; var P0, P1: TPoint2D; ACurvePrecision: word; ADirection: TArcDirection): longint;
var TmpEllipse2D: TEllipse2D;
begin
  result := -1;
  GetActiveDocument;
  if CADCmp2D = nil then exit;
  TmpEllipse2D := TEllipse2D.Create(AID, P0, P1);
  TmpEllipse2D.CurvePrecision := ACurvePrecision;
  TmpEllipse2D.Direction := ADirection;
  CADCmp2D.AddObject(AID, TmpEllipse2D);
  result := TmpEllipse2D.ID;
end;

function CAD_DrawSector2D(AID:  longint; var CP: TPoint2D; R, SA, EA: TRealType; ACurvePrecision: word; ADirection: TArcDirection): longint;
var TmpSector2D: TSector2D;
begin
  result := -1;
  GetActiveDocument;
  if CADCmp2D = nil then exit;
  TmpSector2D := TSector2D.Create(AID, CP, R, DegToRad(SA), DegToRad(EA));
  TmpSector2D.CurvePrecision := ACurvePrecision;
  TmpSector2D.Direction := ADirection;
  CADCmp2D.AddObject(AID, TmpSector2D);
  result := TmpSector2D.ID;
end;

function CAD_DrawSegment2D(AID: longint; var CP: TPoint2D; R, SA, EA: TRealType; ACurvePrecision: word; ADirection: TArcDirection): longint;
var TmpSegment2D: TSegment2D;
begin
  result := -1;
  GetActiveDocument;
  if CADCmp2D = nil then exit;
  TmpSegment2D := TSegment2D.Create(AID, CP, R, DegToRad(SA), DegToRad(EA));
  TmpSegment2D.CurvePrecision := ACurvePrecision;
  TmpSegment2D.Direction := ADirection;
  CADCmp2D.AddObject(AID, TmpSegment2D);
  result := TmpSegment2D.ID;
end;

function CAD_DrawSymetricSymbol2D(AID:  longint; var CP: TPoint2D; R, SA: TRealType; ACurvePrecision: word; ADirection: TArcDirection): longint;
var TmpSymetricSymbol2D: TSymetricSymbol2D;
begin
  result := -1;
  GetActiveDocument;
  if CADCmp2D = nil then exit;
  TmpSymetricSymbol2D := TSymetricSymbol2D.Create(AID, CP, R);
  TmpSymetricSymbol2D.StartAngle := DegToRad(SA);
  TmpSymetricSymbol2D.CurvePrecision := ACurvePrecision;
  TmpSymetricSymbol2D.Direction := ADirection;
  CADCmp2D.AddObject(AID, TmpSymetricSymbol2D);
  result := TmpSymetricSymbol2D.ID;
end;

function CAD_DrawASymetricSymbol2D(AID: longint; var P0, P1: TPoint2D; R, SA: TRealType; ACurvePrecision: word; ADirection: TArcDirection): longint;
var TmpASymetricSymbol2D: TASymetricSymbol2D;
begin
  result := -1;
  GetActiveDocument;
  if CADCmp2D = nil then exit;
  TmpASymetricSymbol2D := TASymetricSymbol2D.Create(AID, P0, P1);
  TmpASymetricSymbol2D.CurvePrecision := ACurvePrecision;
  TmpASymetricSymbol2D.Direction := ADirection;
  CADCmp2D.AddObject(AID, TmpASymetricSymbol2D);
  result := TmpASymetricSymbol2D.ID;
end;

function CAD_DrawText2D(AID: longint; var ARect2D: TRect2D; AHeight: TrealType; AStr: String): longint;
var TmpText2D: TText2D;
begin
  result := -1;
  GetActiveDocument;
  if CADCmp2D = nil then exit;
  TmpText2D := TText2D.Create(AID, ARect2D, AHeight, AStr);
  CADCmp2D.AddObject(AID, TmpText2D);
  result := TmpText2D.ID;
end;

function CAD_DrawJustifiedVectText2D(AID: longint; AFontIndex: word; var ARect2D: TRect2D; AHeight: TRealType; AStr: string): longint;
var TmpJustifiedVectText2D: TJustifiedVectText2D;
begin
  result := -1;
  GetActiveDocument;
  if CADCmp2D = nil then exit;
  TmpJustifiedVectText2D := TJustifiedVectText2D.Create(AID, CADSysFindFontByIndex(AFontIndex), ARect2D, AHeight, AStr);
  CADCmp2D.AddObject(AID, TmpJustifiedVectText2D);
  result := TmpJustifiedVectText2D.ID;
end;

function CAD_DrawBitmap2D(AID: longint; var P0, P1: TPoint2D; AFileName: string): longInt;
var TmpBitmap2D: TBitmap2D;  TmpBmp: TBitmap;
begin
  result := -1;
  GetActiveDocument;
  if CADCmp2D = nil then exit;
  try
    TmpBmp := TBitmap.Create;
    TmpBmp.LoadFromFile(AFileName);
    TmpBitmap2D := TBitmap2D.Create(AID, P0, P1, TmpBmp);
    CADCmp2D.AddObject(AID, TmpBitmap2D);
    result := TmpBitmap2D.ID;
  except
    raise;
  end;
end;

function CAD_SetColor(AID:integer; AColor: TColor): integer;
var TmpObject2D: TObject2D;
begin
  result := -1;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if TmpObject2D <> nil then
  begin
    if TPrimitive2D(TmpObject2D).PenSource <> psCustom then
      TPrimitive2D(TmpObject2D).PenSource := psCustom;
    TPrimitive2D(TmpObject2D).PenColor := AColor;
    result := 0;
  end;
end;

procedure CAD_ImportDXF;
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

procedure CAD_ImportESSI;
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

procedure CAD_ExportDXF;
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

{procedure CAD_ExportESSI;
var ExportESSI: TExportESSI;
begin
  ExportESSI := TExportESSI.create(GetAppStdESSIOutputMachineFileName);
  try
    ExportESSI.SaveToFile(ExportFileName);
  finally
    ExportESSI.Free;
  end;
end;}

procedure CAD_Regen;
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

procedure CAD_Repaint;
begin
  GetActiveDocument;
  if CADViewport2D = nil then exit;
    CADViewport2D.Repaint;
end;

procedure CAD_Clear;
begin
  GetActiveDocument;
  if CADCmp2D = nil then exit;
  CADCmp2D.DeleteAllObjects;
end;

procedure CAD_ZoomToExtentions;
begin
  GetActiveDocument;
  if CADViewport2D = nil then exit;
   CADViewport2D.ZoomToExtension;
end;

function CAD_OpenIterator: TExclusiveGraphicObjIterator;
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

function CAD_CloseIterator: word;
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

function CreateLine(AX, AY, BX, BY: TRealType): TLine2D;
begin
  result := nil;
  GetActiveDocument;
  if CADCmp2D = nil then exit;
  result := TLine2D.Create(-1, Point2D(AX, AY), Point2D(BX, BY));
end;

function  CAD_GetFiguresCount: integer;
begin
  result := 0;
  GetActiveDocument;
  if CADCmp2D = nil then exit;
  result := CADCmp2D.ObjectsCount;
end;

function  CAD_GetFigureKerfInfo(AIndex: integer): TKerfType;
var TmpObj2D: TObject2D;
begin
  result := None;
  GetActiveDocument;
  if CADCmp2D = nil then exit;

  TmpObj2D := CADCmp2D.GetObject(AIndex);
  if (TmpObj2D <> nil) then
    result := TKerfType(TmpObj2D.fReserveInt1);
end;

function  CAD_GetFigureSP_X(AIndex: integer): TRealType;
var TmpObj2D: TObject2D;
begin
  result := INVALID_FLOAT_VALUE;
  GetActiveDocument;
  if CADCmp2D = nil then exit;

  TmpObj2D := CADCmp2D.GetObject(AIndex);
  if (TmpObj2D <> nil) then
    result :=  TPrimitive2D(TmpObj2D).StartPoint.X;
end;

function  CAD_GetFigureSP_Y(AIndex: integer): TRealType;
var TmpObj2D: TObject2D;
begin
  result := INVALID_FLOAT_VALUE;
  GetActiveDocument;
  if CADCmp2D = nil then exit;

  TmpObj2D := CADCmp2D.GetObject(AIndex);
  if (TmpObj2D <> nil) then
    result :=  TPrimitive2D(TmpObj2D).StartPoint.Y;
end;

function  CAD_GetFigureEP_X(AIndex: integer): TRealType;
var TmpObj2D: TObject2D;
begin
  result := INVALID_FLOAT_VALUE;
  GetActiveDocument;
  if CADCmp2D = nil then exit;

  TmpObj2D := CADCmp2D.GetObject(AIndex);
  if (TmpObj2D <> nil) then
    result :=  TPrimitive2D(TmpObj2D).EndPoint.X;
end;

function  CAD_GetFigureEP_Y(AIndex: integer): TRealType;
var TmpObj2D: TObject2D;
begin
  result := INVALID_FLOAT_VALUE;
  GetActiveDocument;
  if CADCmp2D = nil then exit;

  TmpObj2D := CADCmp2D.GetObject(AIndex);
  if (TmpObj2D <> nil) then
    result :=  TPrimitive2D(TmpObj2D).EndPoint.Y;
end;

{function  CAD_GetFigureCP_X(AIndex: integer): TRealType;
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

function  CAD_GetFigureCP_Y(AIndex: integer): TRealType;
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

function  CAD_GetFigureDirection(AIndex: integer): word;
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
function  CAD_GetFigureProfilePointCount(AIndex: integer): integer;
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

function  CAD_GetFigureProfilePoint(AFigureIndex, APointIndex: integer): TPoint2D;
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

function  CAD_GetFigureProfilePointX(AFigureIndex, APointIndex: integer): TRealType;
begin
  GetActiveDocument;
  if CADCmp2D = nil then exit;
  result := CAD_GetFigureProfilePoint(AFigureIndex, APointIndex).X;
end;

function  CAD_GetFigureProfilePointY(AFigureIndex, APointIndex: integer): TRealType;
begin
  GetActiveDocument;
  if CADCmp2D = nil then exit;
  result := CAD_GetFigureProfilePoint(AFigureIndex, APointIndex).Y;
end;

function  CAD_GetObject(AIndex: integer): TObject2D;
begin
  result := nil;
  GetActiveDocument;
  if CADCmp2D = nil then exit;
  result := CADCmp2D.GetObject(AIndex)
end;

function  CAD_GetFigureClassName(AIndex: integer): string;
var TmpObj2D: TObject2D;
begin
  //result := INVALID_STRING_VALUE;
  GetActiveDocument;
  if CADCmp2D = nil then exit;
  TmpObj2D := CADCmp2D.GetObject(AIndex);
  if (TmpObj2D <> nil) then
    result := TmpObj2D.ClassName;
end;

{function  CAD_CombinedFigures(AIndex1, AIndex2: integer): boolean;
var x1, y1, x2, y2: TRealType; hRes: boolean;  Point1, Point2: TPoint2D;
begin
  Point1 := Point2D(CAD_GetFigureSP_X(AIndex1), CAD_GetFigureSP_Y(AIndex1));
  Point2 := Point2D(CAD_GetFigureEP_X(AIndex2), CAD_GetFigureEP_Y(AIndex2));
  hRes := ( (abs(Point1.x - Point2.x) <= MIN_FIGURE_DISTANCE) and
              (abs(Point1.y - Point2.y) <= MIN_FIGURE_DISTANCE)
            );

  result := hRes;
end;
}

procedure CAD_ProgressBarReset(AMin, AMax, AStep: integer);
begin
  if ProgressBar <> nil then
  begin
    ProgressBar.Min      := AMin;
    ProgressBar.Max      := AMax;
    ProgressBar.Step     := AStep;
    ProgressBar.Position := 0;
  end;
end;

procedure CAD_ProgressBarStep;
begin
  if ProgressBar <> nil then
  begin
    ProgressBar.StepIt;
  end;
end;

procedure CAD_DeleteObjects;
begin
  GetActiveDocument;
  if CADCmp2D = nil then exit;

  CADCmp2D.DeleteSavedSourceBlocks;
  CADCmp2D.DeleteAllObjects;
  CADCmp2D.DeleteAllSourceBlocks;
end;

procedure CAD_Refresh;
begin
  if CADViewport2D <> nil then
    CADViewport2D.ZoomToExtension;
end;

initialization
  CADCmp2D := nil;
  CADViewport2D   := nil;
  ProgressBar     := nil;
  PropertyGrid    := nil;

  ObjectsIterator := nil;
finalization

end.

