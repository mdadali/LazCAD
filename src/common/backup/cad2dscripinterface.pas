unit cad2dscripinterface;

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



//Drawing
procedure GetActiveDocument;

//Drawing-Grid

{function  CAD_Drw_GetShowGrid: boolean;
procedure CAD_Drw_SetShowGrid(AValue: boolean);
function  CAD_Drw_GetShowGridMainAxes: boolean;
procedure CAD_Drw_SetShowGridMainAxes(AValue: boolean);
function  CAD_Drw_GetGridDeltaX: TRealType;
procedure CAD_Drw_SetGridDeltaX(AValue: TRealType);
function  CAD_Drw_GetGridDeltaY: TRealType;
procedure CAD_Drw_SetGridDeltaY(AValue: TRealType);

function  CAD_Drw_GetGridColor: TColor;
procedure CAD_Drw_SetGridColor(AValue: TColor);

function  CAD_Drw_GetBGColor: TColor;
procedure CAD_Drw_SetBGColor(AValue: TColor);

//Drawing-Commands
function  CAD_Drw_GetEnable_DragDrop: boolean;
procedure CAD_Drw_SetEnable_DragDrop(AValue: boolean);
function  CAD_Drw_GetPolarTracking: boolean;
procedure CAD_Drw_SetPolarTracking(AValue: boolean);

function  CAD_Drw_GetPolarTrackingValue: TRealType;
procedure CAD_Drw_SetPolarTrackingValue(AValue: TRealType);

function  CAD_Drw_GetShowControlPoints: boolean;
procedure CAD_Drw_SetShowControlPoints(AValue: boolean);

function  CAD_Drw_GetShowDirection: boolean;
procedure CAD_Drw_SetShowDirection(AValue: boolean);

function  CAD_Drw_GetShowRulerMarker: boolean;
procedure CAD_Drw_SetShowRulerMarker(AValue: boolean);

function  CAD_Drw_GetUseOrto: boolean;
procedure CAD_Drw_SetUseOrto(AValue: boolean);

function  CAD_Drw_GetUseSnap: boolean;
procedure CAD_Drw_SetUseSnap(AValue: boolean);

function  CAD_Drw_GetXSnap: TRealType;
procedure CAD_Drw_SetXSnap(AValue: TRealType);

function  CAD_Drw_GetYSnap: TRealType;
procedure CAD_Drw_SetYSnap(AValue: TRealType);

}

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

//MakeBlock2D

function CAD_Polyline2DAddPoint2D(AIDX: longint; var APoint2D: TPoint2D): longint;
function CAD_Polygon2DAddPoint2D(AIDX: longint; var APoint2D: TPoint2D): longint;
function CAD_BSpline2DAddPoint2D(AIDX: longint; var APoint2D: TPoint2D): longint;

//Brush
function CAD_GetBrushSource(AID: longint): TBrushSource;
function CAD_SetBrushSource(AID: longint; AValue: TBrushSource): boolean;
function CAD_GetBrushColor(AID: longint): TColor;
function CAD_SetBrushColor(AID: longint; AValue: TColor): boolean;
function CAD_GetBrushStyle(AID: longint): TBrushStyle;
function CAD_SetBrushStyle(AID: longint; AValue: TBrushStyle): boolean;

//TGraphicObject////////////////////////////////////////////////////////////////
function CAD_GrObjGetClassName(AID: longint): string;
function CAD_GrObjGetEnabled(AID: longint): boolean;
function CAD_GrObjSetEnabled(AID: longint; AValue: boolean): boolean;
function CAD_GrObjGetVisible(AID: longint): boolean;
function CAD_GrObjSetVisible(AID: longint; AValue: boolean): boolean;
function CAD_GrObjGetLayerIndex(AID: longint): word;
function CAD_GrObjSetLayerIndex(AID: longint; AValue: word): boolean;
function CAD_GrObjGetLayerName(AID: longint): string;
function CAD_GrObjSetLayerName(AID: longint; AValue: string): boolean;
function CAD_GrObjGetTag(AID: longint): integer;
function CAD_GrObjSetTag(AID: longint; AValue: integer): boolean;

//TObject2//////////////////////////////////////////////////////////////////////
function CAD_Obj2DGetAngle(AID: longint): TRealType;
function CAD_Obj2DSetAngle(AID: longint; AValue: TRealType): boolean;

function CAD_Obj2DGetBottom(AID: longint): TRealType;
function CAD_Obj2DSetBottom(AID: longint; var AValue: TRealType): boolean;
function CAD_Obj2DGetLeft(AID: longint): TRealType;
function CAD_Obj2DSetLeft(AID: longint; var AValue: TRealType): boolean;
function CAD_Obj2DGetRight(AID: longint): TRealType;
function CAD_Obj2DSetRight(AID: longint; var AValue: TRealType): boolean;
function CAD_Obj2DGetTop(AID: longint): TRealType;
function CAD_Obj2DSetTop(AID: longint; var AValue: TRealType): boolean;

function CAD_Obj2DGetMiddlePoint2D(AID: longint): TPoint2D;
function CAD_Obj2DSetMiddlePoint2D(AID: longint;  var APoint2D: TPoint2D): boolean;
function CAD_Obj2DGetMiddlePoint2DX(AID: longint): TRealType;
function CAD_Obj2DSetMiddlePoint2DX(AID: longint; var AValue: TRealType): boolean;
function CAD_Obj2DGetMiddlePoint2DY(AID: longint): TRealType;
function CAD_Obj2DSetMiddlePoint2DY(AID: longint; var AValue: TRealType): boolean;
function CAD_Obj2DGetMiddlePoint2DW(AID: longint): TRealType;
function CAD_Obj2DSetMiddlePoint2DW(AID: longint; var AValue: TRealType): boolean;


//TPrimitive2D//////////////////////////////////////////////////////////////////
function CAD_Prim2DGetPenSource(AID: longint): TPenSource;
function CAD_Prim2DSetPenSource(AID: longint; AValue: TPenSource): boolean;
function CAD_Prim2DSetPenColor(AID:integer; AColor: TColor): boolean;
function CAD_Prim2DGetPenColor(AID: longint): TColor;
function CAD_Prim2DSetPenWidth(AID:integer; AValue: word): boolean;
function CAD_Prim2DGetPenWidth(AID: longint): word;
function CAD_Prim2DSetPenStyle(AID:integer; AValue: TPenStyle): boolean;
function CAD_Prim2DGetPenStyle(AID: longint): TPenStyle;

//TSimplePrimitive2D////////////////////////////////////////////////////////////
function CAD_SimplePrim2DGetStartPoint2D(AID: longint): TPoint2D;
function CAD_SimplePrim2DSetStartPoint2D(AID: longint;  var APoint2D: TPoint2D): boolean;
function CAD_SimplePrim2DGetStartPoint2DX(AID: longint): TRealType;
function CAD_SimplePrim2DSetStartPoint2DX(AID: longint; var AValue: TRealType): boolean;
function CAD_SimplePrim2DGetStartPoint2DY(AID: longint): TRealType;
function CAD_SimplePrim2DSetStartPoint2DY(AID: longint; var AValue: TRealType): boolean;
function CAD_SimplePrim2DGetStartPoint2DW(AID: longint): TRealType;
function CAD_SimplePrim2DSetStartPoint2DW(AID: longint; var AValue: TRealType): boolean;

function CAD_SimplePrim2DGetEndPoint2D(AID: longint): TPoint2D;
function CAD_SimplePrim2DSetEndPoint2D(AID: longint;  var APoint2D: TPoint2D): boolean;
function CAD_SimplePrim2DGetEndPoint2DX(AID: longint): TRealType;
function CAD_SimplePrim2DSetEndPoint2DX(AID: longint; var AValue: TRealType): boolean;
function CAD_SimplePrim2DGetEndPoint2DY(AID: longint): TRealType;
function CAD_SimplePrim2DSetEndPoint2DY(AID: longint; var AValue: TRealType): boolean;
function CAD_SimplePrim2DGetEndPoint2DW(AID: longint): TRealType;
function CAD_SimplePrim2DSetEndPoint2DW(AID: longint; var AValue: TRealType): boolean;

function CAD_SimplePrim2DGetShowDirection(AID: longint): boolean;
function CAD_SimplePrim2DSetShowDirection(AID: longint; var AValue: boolean): boolean;

function CAD_SimplePrim2DGetObjectLength(AID: longint): TRealType;


//TCircularArc2D////////////////////////////////////////////////////////////////
function CAD_CircularArc2DGetStartAngle(AID: longint): TRealType;
function CAD_CircularArc2DSetStartAngle(AID: longint; var AValue: TRealType): boolean;
function CAD_CircularArc2DGetEndAngle(AID: longint): TRealType;
function CAD_CircularArc2DSetEndAngle(AID: longint; var AValue: TRealType): boolean;
function CAD_CircularArc2DGetArcAngle(AID: longint): TRealType;
function CAD_CircularArc2DSetArcAngle(AID: longint; var AValue: TRealType): boolean;

function CAD_CircularArc2DGetCenterPoint2D(AID: longint): TPoint2D;
function CAD_CircularArc2DSetCenterPoint2D(AID: longint;  var APoint2D: TPoint2D): boolean;
function CAD_CircularArc2DGetCenterPoint2DX(AID: longint): TRealType;
function CAD_CircularArc2DSetCenterPoint2DX(AID: longint; var AValue: TRealType): boolean;
function CAD_CircularArc2DGetCenterPoint2DY(AID: longint): TRealType;
function CAD_CircularArc2DSetCenterPoint2DY(AID: longint; var AValue: TRealType): boolean;
function CAD_CircularArc2DGetCenterPoint2DW(AID: longint): TRealType;
function CAD_CircularArc2DSetCenterPoint2DW(AID: longint; var AValue: TRealType): boolean;

function CAD_CircularArc2DGetRadius(AID: longint): TRealType;
function CAD_CircularArc2DSetRadius(AID: longint; var AValue: TRealType): boolean;

function CAD_CircularArc2DGetDirection(AID: longint): TArcDirection;
function CAD_CircularArc2DSetDirection(AID: longint; var AValue: TArcDirection): boolean;

function CAD_CircularArc2DGetCurvePrecision(AID: longint): word;
function CAD_CircularArc2DSetCurvePrecision(AID: longint; var AValue: word): boolean;


//TEllipticalArc2D//////////////////////////////////////////////////////////////
function CAD_ElliArc2DGetStartAngle(AID: longint): TRealType;
function CAD_ElliArc2DSetStartAngle(AID: longint; var AValue: TRealType): boolean;
function CAD_ElliArc2DGetEndAngle(AID: longint): TRealType;
function CAD_ElliArc2DSetEndAngle(AID: longint; var AValue: TRealType): boolean;
function CAD_ElliArc2DGetArcAngle(AID: longint): TRealType;
function CAD_ElliArc2DSetArcAngle(AID: longint; var AValue: TRealType): boolean;

function CAD_ElliArc2DGetCurvePrecision(AID: longint): word;
function CAD_ElliArc2DSetCurvePrecision(AID: longint; var AValue: word): boolean;

function CAD_ElliArc2DGetDirection(AID: longint): TArcDirection;
function CAD_ElliArc2DSetDirection(AID: longint; var ADirection: TArcDirection): boolean;


//TClosedCurve2D & TClosedPolyline2D////////////////////////////////////////////
//Brush implementiert
function CAD_ClosedCurve2DGetArea(AID: longint): TRealType;
function CAD_ClosedPolyline2DGetArea(AID: longint): TRealType;

//TCircle2D/////////////////////////////////////////////////////////////////////
function CAD_Circle2DGetStartAngle(AID: longint): TRealType;
function CAD_Circle2DSetStartAngle(AID: longint; var AValue: TRealType): boolean;

function CAD_Circle2DGetCenterPoint2D(AID: longint): TPoint2D;
function CAD_Circle2DSetCenterPoint2D(AID: longint;  var APoint2D: TPoint2D): boolean;
function CAD_Circle2DGetCenterPoint2DX(AID: longint): TRealType;
function CAD_Circle2DSetCenterPoint2DX(AID: longint; var AValue: TRealType): boolean;
function CAD_Circle2DGetCenterPoint2DY(AID: longint): TRealType;
function CAD_Circle2DSetCenterPoint2DY(AID: longint; var AValue: TRealType): boolean;
function CAD_Circle2DGetCenterPoint2DW(AID: longint): TRealType;
function CAD_Circle2DSetCenterPoint2DW(AID: longint; var AValue: TRealType): boolean;

function CAD_Circle2DGetRadius(AID: longint): TRealType;
function CAD_Circle2DSetRadius(AID: longint; var AValue: TRealType): boolean;

function CAD_Circle2DGetDirection(AID: longint): TArcDirection;
function CAD_Circle2DSetDirection(AID: longint; var AValue: TArcDirection): boolean;

function CAD_Circle2DGetCurvePrecision(AID: longint): word;
function CAD_Circle2DSetCurvePrecision(AID: longint; var AValue: word): boolean;

//Polyline2D////////////////////////////////////////////////////////////////////
function CAD_Polyline2DGetPointsCount(AID: longint): integer;

//Polygon2D/////////////////////////////////////////////////////////////////////
function CAD_Polygon2DGetPointsCount(AID: longint): integer;

//Bspline2D/////////////////////////////////////////////////////////////////////
function CAD_BSpline2DGetPointsCount(AID: longint): integer;

//Frame2D/////////////////////////////////////////////////////////////////////
function CAD_Frame2DGetDirection(AID: longint): TArcDirection;
function CAD_Frame2DSetDirection(AID: longint; var AValue: TArcDirection): boolean;

function CAD_Frame2DGetHeight(AID: longint): TRealType;
function CAD_Frame2DSetHeight(AID: longint; var AValue: TRealType): boolean;
function CAD_Frame2DGetWidth(AID: longint): TRealType;
function CAD_Frame2DSetWidth(AID: longint; var AValue: TRealType): boolean;

function CAD_Frame2DGetStartCorner(AID: longint): TFrameStartCorner;
function CAD_Frame2DSetStartCorner(AID: longint; var AValue: TFrameStartCorner): boolean;


//Ellipse2D/////////////////////////////////////////////////////////////////////
function CAD_Ellipse2DGetDirection(AID: longint): TArcDirection;
function CAD_Ellipse2DSetDirection(AID: longint; var AValue: TArcDirection): boolean;

function CAD_Ellipse2DGetHeight(AID: longint): TRealType;
function CAD_Ellipse2DSetHeight(AID: longint; var AValue: TRealType): boolean;
function CAD_Ellipse2DGetWidth(AID: longint): TRealType;
function CAD_Ellipse2DSetWidth(AID: longint; var AValue: TRealType): boolean;

//TSector2D/////////////////////////////////////////////////////////////////////
function CAD_Sector2DGetStartAngle(AID: longint): TRealType;
function CAD_Sector2DSetStartAngle(AID: longint; var AValue: TRealType): boolean;
function CAD_Sector2DGetEndAngle(AID: longint): TRealType;
function CAD_Sector2DSetEndAngle(AID: longint; var AValue: TRealType): boolean;
function CAD_Sector2DGetArcAngle(AID: longint): TRealType;
function CAD_Sector2DSetArcAngle(AID: longint; var AValue: TRealType): boolean;

function CAD_Sector2DGetCenterPoint2D(AID: longint): TPoint2D;
function CAD_Sector2DSetCenterPoint2D(AID: longint;  var APoint2D: TPoint2D): boolean;
function CAD_Sector2DGetCenterPoint2DX(AID: longint): TRealType;
function CAD_Sector2DSetCenterPoint2DX(AID: longint; var AValue: TRealType): boolean;
function CAD_Sector2DGetCenterPoint2DY(AID: longint): TRealType;
function CAD_Sector2DSetCenterPoint2DY(AID: longint; var AValue: TRealType): boolean;
function CAD_Sector2DGetCenterPoint2DW(AID: longint): TRealType;
function CAD_Sector2DSetCenterPoint2DW(AID: longint; var AValue: TRealType): boolean;

function CAD_Sector2DGetRadius(AID: longint): TRealType;
function CAD_Sector2DSetRadius(AID: longint; var AValue: TRealType): boolean;

function CAD_Sector2DGetDirection(AID: longint): TArcDirection;
function CAD_Sector2DSetDirection(AID: longint; var AValue: TArcDirection): boolean;

function CAD_Sector2DGetCurvePrecision(AID: longint): word;
function CAD_Sector2DSetCurvePrecision(AID: longint; var AValue: word): boolean;

//Segment2D////////////////////////////////////////////////////////////////////
function CAD_Segment2DGetStartAngle(AID: longint): TRealType;
function CAD_Segment2DSetStartAngle(AID: longint; var AValue: TRealType): boolean;
function CAD_Segment2DGetEndAngle(AID: longint): TRealType;
function CAD_Segment2DSetEndAngle(AID: longint; var AValue: TRealType): boolean;
function CAD_Segment2DGetArcAngle(AID: longint): TRealType;
function CAD_Segment2DSetArcAngle(AID: longint; var AValue: TRealType): boolean;

function CAD_Segment2DGetCenterPoint2D(AID: longint): TPoint2D;
function CAD_Segment2DSetCenterPoint2D(AID: longint;  var APoint2D: TPoint2D): boolean;
function CAD_Segment2DGetCenterPoint2DX(AID: longint): TRealType;
function CAD_Segment2DSetCenterPoint2DX(AID: longint; var AValue: TRealType): boolean;
function CAD_Segment2DGetCenterPoint2DY(AID: longint): TRealType;
function CAD_Segment2DSetCenterPoint2DY(AID: longint; var AValue: TRealType): boolean;
function CAD_Segment2DGetCenterPoint2DW(AID: longint): TRealType;
function CAD_Segment2DSetCenterPoint2DW(AID: longint; var AValue: TRealType): boolean;

function CAD_Segment2DGetRadius(AID: longint): TRealType;
function CAD_Segment2DSetRadius(AID: longint; var AValue: TRealType): boolean;

function CAD_Segment2DGetDirection(AID: longint): TArcDirection;
function CAD_Segment2DSetDirection(AID: longint; var AValue: TArcDirection): boolean;

function CAD_Segment2DGetCurvePrecision(AID: longint): word;
function CAD_Segment2DSetCurvePrecision(AID: longint; var AValue: word): boolean;

//TSymetricSymbol2D/////////////////////////////////////////////////////////////
function CAD_SymSymbol2DGetStartAngle(AID: longint): TRealType;
function CAD_SymSymbol2DSetStartAngle(AID: longint; var AValue: TRealType): boolean;

function CAD_SymSymbol2DGetCenterPoint2D(AID: longint): TPoint2D;
function CAD_SymSymbol2DSetCenterPoint2D(AID: longint;  var APoint2D: TPoint2D): boolean;
function CAD_SymSymbol2DGetCenterPoint2DX(AID: longint): TRealType;
function CAD_SymSymbol2DSetCenterPoint2DX(AID: longint; var AValue: TRealType): boolean;
function CAD_SymSymbol2DGetCenterPoint2DY(AID: longint): TRealType;
function CAD_SymSymbol2DSetCenterPoint2DY(AID: longint; var AValue: TRealType): boolean;
function CAD_SymSymbol2DGetCenterPoint2DW(AID: longint): TRealType;
function CAD_SymSymbol2DSetCenterPoint2DW(AID: longint; var AValue: TRealType): boolean;

function CAD_SymSymbol2DGetRadius(AID: longint): TRealType;
function CAD_SymSymbol2DSetRadius(AID: longint; var AValue: TRealType): boolean;

function CAD_SymSymbol2DGetDirection(AID: longint): TArcDirection;
function CAD_SymSymbol2DSetDirection(AID: longint; var AValue: TArcDirection): boolean;

function CAD_SymSymbol2DGetCurvePrecision(AID: longint): word;
function CAD_SymSymbol2DSetCurvePrecision(AID: longint; var AValue: word): boolean;

//ASymetricSymbol2D/////////////////////////////////////////////////////////////
function CAD_ASymSymbol2DGetDirection(AID: longint): TArcDirection;
function CAD_ASymSymbol2DSetDirection(AID: longint; var AValue: TArcDirection): boolean;

function CAD_ASymSymbol2DGetHeight(AID: longint): TRealType;
function CAD_ASymSymbol2DSetHeight(AID: longint; var AValue: TRealType): boolean;
function CAD_ASymSymbol2DGetWidth(AID: longint): TRealType;
function CAD_ASymSymbol2DSetWidth(AID: longint; var AValue: TRealType): boolean;

//Text2D////////////////////////////////////////////////////////////////////////
function CAD_Text2DGetText(AID: longint): string;
function CAD_Text2DSetTex(AID: longint; AValue: string): boolean;

function CAD_Text2DGetFaceName(AID: longint): string;
function CAD_Text2DSetFaceName(AID: longint; AValue: string): boolean;

function CAD_Text2DGetAutoSize(AID: longint): boolean;
function CAD_Text2DSetAutoSize(AID: longint; AValue: boolean): boolean;
function CAD_Text2DGetBold(AID: longint): boolean;
function CAD_Text2DSetBold(AID: longint; AValue: boolean): boolean;
function CAD_Text2DGetUnderline(AID: longint): boolean;
function CAD_Text2DSetUnderline(AID: longint; AValue: boolean): boolean;
function CAD_Text2DGetStrikeout(AID: longint): boolean;
function CAD_Text2DSetStrikeout(AID: longint; AValue: boolean): boolean;
function CAD_Text2DGetItalic(AID: longint): boolean;
function CAD_Text2DSetItalic(AID: longint; AValue: boolean): boolean;
function CAD_Text2DGetDrawBox(AID: longint): boolean;
function CAD_Text2DSetDrawBox(AID: longint; AValue: boolean): boolean;
function CAD_Text2DGetHeight(AID: longint): TRealType;
function CAD_Text2DSetHeight(AID: longint; AValue: TRealType): boolean;

function CAD_Text2DGetColorSource(AID: longint): TColorSource;
function CAD_Text2DSetColorSource(AID: longint; AValue: TColorSource): boolean;
function CAD_Text2DGetColor(AID: longint): TColor;
function CAD_Text2DSetColor(AID: longint; AValue: TColor): boolean;

//TBitmap2D/////////////////////////////////////////////////////////////////////
function CAD_Bitmap2DGetScaleFactor(AID: longint): TRealType;
function CAD_Bitmap2DSetScaleFactor(AID: longint; AValue: TRealType): boolean;

function CAD_Bitmap2DGetAspectRatio(AID: longint): TRealType;
function CAD_Bitmap2DSetAspectRatio(AID: longint; AValue: TRealType): boolean;

function CAD_Bitmap2DGetCopyMode(AID: longint): TCopyMode;
function CAD_Bitmap2DSetCopyMode(AID: longint; AValue: TCopyMode): boolean;

//JustifiedVectText2D///////////////////////////////////////////////////////////
function CAD_JVectText2DGetBasePoint(AID: longint): TPoint2D;
function CAD_JVectText2DSetBasePoint(AID: longint; var APoint2D: TPoint2D): boolean;
function CAD_JVectText2DGetCharSpace(AID: longint): TRealType;
function CAD_JVectText2DSetCharSpace(AID: longint; AValue: TRealType): boolean;
function CAD_JVectText2DGetDrawBox(AID: longint): boolean;
function CAD_JVectText2DSetDrawBox(AID: longint; AValue: boolean): boolean;

function CAD_JVectText2DGetHeight(AID: longint): TRealType;
function CAD_JVectText2DSetHeight(AID: longint; AValue: TRealType): boolean;

function CAD_JVectText2DGetHJustification(AID: longint): THJustification;
function CAD_JVectText2DSetHJustification(AID: longint; AValue: THJustification): boolean;
function CAD_JVectText2DGetVJustification(AID: longint): TVJustification;
function CAD_JVectText2DSetVJustification(AID: longint; AValue: TVJustification): boolean;

function CAD_JVectText2DGetInterline(AID: longint): TRealType;
function CAD_JVectText2DSetInterline(AID: longint; AValue: TRealType): boolean;

function CAD_JVectText2DGetText(AID: longint): string;
function CAD_JVectText2DSetText(AID: longint; AValue: string): boolean;

//Block2D///////////////////////////////////////////////////////////////////////
function CAD_Block2DGetOriginPoint(AID: longint): TPoint2D;
function CAD_Block2DSetOriginPoint(AID: longint; var APoint2D: TPoint2D): boolean;
function CAD_Block2DGetSourceName(AID: longint): string;

//TTF2Vect///////////////////////////////////////////////////////////////////////
//----------
///////

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
//function CAD_OpenIterator: TExclusiveGraphicObjIterator;
//function CAD_CloseIterator: word;

function CAD_ImportDXF(AFileName: string): boolean;
function CAD_ExportDXF(AFileName: string): boolean;

function CAD_ImportESSI(AFileName: string): boolean;
//procedure CAD_ExportESSI;

//procedure CAD_ImportGCODE;
//procedure CAD_ExportGCODE;


var ExportFileName: string;

implementation

//TGraphicObject////////////////////////////////////////////////////////////////
function CAD_GrObjGetClassName(AID: longint): string;
var TmpObject2D: TObject2D;
begin
  result := '';
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TGraphicObject)  then
    result := TGraphicObject(TmpObject2D).ClassName;
end;

function CAD_GrObjGetEnabled(AID: longint): boolean;
var TmpObject2D: TObject2D;
begin
  result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TGraphicObject)  then
    result := TGraphicObject(TmpObject2D).Enabled;
end;

function CAD_GrObjSetEnabled(AID: longint; AValue: boolean): boolean;
var TmpObject2D: TObject2D;
begin
  result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TGraphicObject)  then
  begin
    TGraphicObject(TmpObject2D).Enabled := AValue;
    result := true;
  end;
end;

function CAD_GrObjGetVisible(AID: longint): boolean;
var TmpObject2D: TObject2D;
begin
  result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TGraphicObject)  then
    result := TGraphicObject(TmpObject2D).Visible;
end;

function CAD_GrObjSetVisible(AID: longint; AValue: boolean): boolean;
var TmpObject2D: TObject2D;
begin
  result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TGraphicObject)  then
  begin
    TGraphicObject(TmpObject2D).Visible := AValue;
    result := true;
  end;
end;

function CAD_GrObjGetLayerIndex(AID: longint): word;
var TmpObject2D: TObject2D;
begin
  result := -1;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TGraphicObject)  then
    result := TGraphicObject(TmpObject2D).LayerIndex;
end;

function CAD_GrObjSetLayerIndex(AID: longint; AValue: word): boolean;
var TmpObject2D: TObject2D;
begin
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TGraphicObject)  then
  begin
    TGraphicObject(TmpObject2D).LayerIndex := AValue;
    result := true;
  end;
end;

function CAD_GrObjGetLayerName(AID: longint): string;
var TmpObject2D: TObject2D;
begin
  result := '';
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TGraphicObject)  then
    result := TGraphicObject(TmpObject2D).LayerName;
end;

function CAD_GrObjSetLayerName(AID: longint; AValue: string): boolean;
var TmpObject2D: TObject2D;
begin
  result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TGraphicObject)  then
  begin
    TGraphicObject(TmpObject2D).LayerName := AValue;
    result := true;
  end;
end;

function CAD_GrObjGetTag(AID: longint): integer;
var TmpObject2D: TObject2D;
begin
  result := INVALID_INTEGER_VALUE;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TGraphicObject)  then
    result := TGraphicObject(TmpObject2D).Tag;
end;

function CAD_GrObjSetTag(AID: longint; AValue: integer): boolean;
var TmpObject2D: TObject2D;
begin
  result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TGraphicObject)  then
  begin
    TGraphicObject(TmpObject2D).Tag := AValue;
    result := true;
  end;
end;

//TObject2//////////////////////////////////////////////////////////////////////
function CAD_Obj2DGetAngle(AID: longint): TRealType;
var TmpObject2D: TObject2D;
begin
  result := INVALID_INTEGER_VALUE;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TObject2D)  then
    result := TmpObject2D.Angle;
end;

function CAD_Obj2DSetAngle(AID: longint; AValue: TRealType): boolean;
var TmpObject2D: TObject2D;
begin
  result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TObject2D)  then
  begin
    TmpObject2D.Angle := AValue;
    result := true;
  end;
end;

function CAD_Obj2DGetBottom(AID: longint): TRealType;
var TmpObject2D: TObject2D;
begin
  result := INVALID_FLOAT_VALUE;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TObject2D)  then
    result := TmpObject2D.Bottom;
end;

function CAD_Obj2DSetBottom(AID: longint; var AValue: TRealType): boolean;
var TmpObject2D: TObject2D;
begin
  result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TObject2D)  then
  begin
    TmpObject2D.Bottom := AValue;
    result := true;
  end;
end;

function CAD_Obj2DGetLeft(AID: longint): TRealType;
var TmpObject2D: TObject2D;
begin
  result := INVALID_FLOAT_VALUE;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TObject2D)  then
    result := TmpObject2D.Left;
end;

function CAD_Obj2DSetLeft(AID: longint; var AValue: TRealType): boolean;
var TmpObject2D: TObject2D;
begin
  result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TObject2D)  then
  begin
    TmpObject2D.Left := AValue;
    result := true;
  end;
end;

function CAD_Obj2DGetRight(AID: longint): TRealType;
var TmpObject2D: TObject2D;
begin
  result := INVALID_FLOAT_VALUE;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TObject2D)  then
    result := TmpObject2D.Right;
end;

function CAD_Obj2DSetRight(AID: longint; var AValue: TRealType): boolean;
var TmpObject2D: TObject2D;
begin
  result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TObject2D)  then
  begin
    TmpObject2D.Right := AValue;
    result := true;
  end;
end;

function CAD_Obj2DGetTop(AID: longint): TRealType;
var TmpObject2D: TObject2D;
begin
  result := INVALID_FLOAT_VALUE;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TObject2D)  then
    result := TmpObject2D.Top;
end;

function CAD_Obj2DSetTop(AID: longint; var AValue: TRealType): boolean;
var TmpObject2D: TObject2D;
begin
  result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TObject2D)  then
  begin
    TmpObject2D.Top := AValue;
    result := true;
  end;
end;

function CAD_Obj2DGetMiddlePoint2D(AID: longint): TPoint2D;
var TmpObject2D: TObject2D;
begin
  //result := ;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TObject2D)  then
    result := TmpObject2D.MiddLePoint;
end;

function CAD_Obj2DSetMiddlePoint2D(AID: longint;  var APoint2D: TPoint2D): boolean;
var TmpObject2D: TObject2D; TmpPoint2D: TPoint2D;
begin
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TObject2D)  then
  begin
    TmpObject2D.MiddLePoint := APoint2D;
    result := true;
  end;
end;

function CAD_Obj2DGetMiddlePoint2DX(AID: longint): TRealType;
var TmpObject2D: TObject2D;
begin
  result := INVALID_FLOAT_VALUE;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TObject2D)  then
    result := TmpObject2D.MiddLePointX;
end;

function CAD_Obj2DSetMiddlePoint2DX(AID: longint; var AValue: TRealType): boolean;
var TmpObject2D: TObject2D;
begin
  result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TObject2D)  then
  begin
    TmpObject2D.MiddLePointX := AValue;
    result := true;
  end;
end;

function CAD_Obj2DGetMiddlePoint2DY(AID: longint): TRealType;
var TmpObject2D: TObject2D;
begin
  result := INVALID_FLOAT_VALUE;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TObject2D)  then
    result := TmpObject2D.MiddLePointY;
end;

function CAD_Obj2DSetMiddlePoint2DY(AID: longint; var AValue: TRealType): boolean;
var TmpObject2D: TObject2D;
begin
  result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TObject2D)  then
  begin
    TmpObject2D.MiddLePointY := AValue;
    result := true;
  end;
end;

function CAD_Obj2DGetMiddlePoint2DW(AID: longint): TRealType;
var TmpObject2D: TObject2D;
begin
  result := INVALID_FLOAT_VALUE;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TObject2D)  then
    result := TmpObject2D.MiddLePoint.W;
end;

function CAD_Obj2DSetMiddlePoint2DW(AID: longint; var AValue: TRealType): boolean;
var TmpObject2D: TObject2D;
begin
  result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TObject2D)  then
  begin
    //TmpObject2D.MiddLePoint.W := AValue;
    //result := true;
  end;
end;

//TPrimitive2D//////////////////////////////////////////////////////////////////
function CAD_Prim2DGetPenColor(AID: longint): TColor;
var TmpObject2D: TObject2D;
begin
  result := clWhite;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TPrimitive2D)  then
    result := TPrimitive2D(TmpObject2D).PenColor;
end;

function CAD_Prim2DSetPenColor(AID:integer; AColor: TColor): boolean;
var TmpObject2D: TObject2D;
begin
  result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if TmpObject2D <> nil then
  begin
    TPrimitive2D(TmpObject2D).PenColor := AColor;
    result := true;
  end;
end;

function CAD_Prim2DGetPenSource(AID: longint): TPenSource;
var TmpObject2D: TObject2D;
begin
  result := psByLayer;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TPrimitive2D)  then
    result := TPrimitive2D(TmpObject2D).PenSource;
end;

function CAD_Prim2DSetPenSource(AID: longint; AValue: TPenSource): boolean;
var TmpObject2D: TObject2D;
begin
  result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TPrimitive2D)  then
  begin
    if TPrimitive2D(TmpObject2D).PenSource <> AValue then
      TPrimitive2D(TmpObject2D).PenSource := AValue;
    result := true;
  end;
end;

function CAD_Prim2DGetPenWidth(AID: longint): word;
var TmpObject2D: TObject2D;
begin
  result := 0;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TPrimitive2D)  then
    result := TPrimitive2D(TmpObject2D).PenWidth;
end;

function CAD_Prim2DSetPenWidth(AID:integer; AValue: word): boolean;
var TmpObject2D: TObject2D;
begin
  result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TPrimitive2D)  then
  begin
    if TPrimitive2D(TmpObject2D).PenWidth <> AValue then
      TPrimitive2D(TmpObject2D).PenWidth := AValue;
    result := true;
  end;
end;

function CAD_Prim2DGetPenStyle(AID: longint): TPenStyle;
var TmpObject2D: TObject2D;
begin
  result := psSolid;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TPrimitive2D)  then
    result := TPrimitive2D(TmpObject2D).PenStyle;
end;

function CAD_Prim2DSetPenStyle(AID:integer; AValue: TPenStyle): boolean;
var TmpObject2D: TObject2D;
begin
  result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TPrimitive2D)  then
  begin
    if TPrimitive2D(TmpObject2D).PenStyle <> AValue then
      TPrimitive2D(TmpObject2D).PenStyle := AValue;
    result := true;
  end;
end;

//TSimplePrimitive2D////////////////////////////////////////////////////////////
function CAD_SimplePrim2DGetStartPoint2D(AID: longint): TPoint2D;
var TmpObject2D: TObject2D;
begin
  //result := ;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TSimplePrimitive2D)  then
    result := TSimplePrimitive2D(TmpObject2D).StartPoint;
end;

function CAD_SimplePrim2DSetStartPoint2D(AID: longint;  var APoint2D: TPoint2D): boolean;
var TmpObject2D: TObject2D;  TmpPoint2D: TPoint2D;
begin
  result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TSimplePrimitive2D)  then
  begin
    TSimplePrimitive2D(TmpObject2D).SetStartPointX(APoint2D.X);
    TSimplePrimitive2D(TmpObject2D).SetStartPointY(APoint2D.Y);
    result := true;
  end;
end;

function CAD_SimplePrim2DGetStartPoint2DX(AID: longint): TRealType;
var TmpObject2D: TObject2D;
begin
  //result := ;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TSimplePrimitive2D)  then
    result := TSimplePrimitive2D(TmpObject2D).StartPoint.X;
end;

function CAD_SimplePrim2DSetStartPoint2DX(AID: longint; var AValue: TRealType): boolean;
var TmpObject2D: TObject2D;
begin
  result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TSimplePrimitive2D)  then
  begin
    TSimplePrimitive2D(TmpObject2D).SetStartPointX(AValue);
    result := true;
  end;
end;

function CAD_SimplePrim2DGetStartPoint2DY(AID: longint): TRealType;
var TmpObject2D: TObject2D;
begin
  //result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TSimplePrimitive2D)  then
    result := TSimplePrimitive2D(TmpObject2D).StartPoint.Y;
end;

function CAD_SimplePrim2DSetStartPoint2DY(AID: longint; var AValue: TRealType): boolean;
var TmpObject2D: TObject2D;
begin
  result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TSimplePrimitive2D)  then
  begin
    TSimplePrimitive2D(TmpObject2D).SetStartPointY(AValue);
    result := true;
  end;
end;

function CAD_SimplePrim2DGetStartPoint2DW(AID: longint): TRealType;
var TmpObject2D: TObject2D;
begin
  //result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TSimplePrimitive2D)  then
    result := TSimplePrimitive2D(TmpObject2D).StartPoint.W;
end;

function CAD_SimplePrim2DSetStartPoint2DW(AID: longint; var AValue: TRealType): boolean;
var TmpObject2D: TObject2D;  TmpPoint2D: TPoint2D;
begin
{  result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TSimplePrimitive2D)  then
  begin
    TmpPoint2D.X := TSimplePrimitive2D(TmpObject2D).StartPoint.X;
    TmpPoint2D.Y := TSimplePrimitive2D(TmpObject2D).StartPoint.Y;
    TmpPoint2D.W := AValue;
    TSimplePrimitive2D(TmpObject2D).StartPoint := TmpPoint2D;;
    result := true;
  end;  }
end;

function CAD_SimplePrim2DGetEndPoint2D(AID: longint): TPoint2D;
var TmpObject2D: TObject2D;
begin
  //result := ;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TSimplePrimitive2D)  then
    result := TSimplePrimitive2D(TmpObject2D).EndPoint;
end;

function CAD_SimplePrim2DSetEndPoint2D(AID: longint;  var APoint2D: TPoint2D): boolean;
var TmpObject2D: TObject2D;
begin
  result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TSimplePrimitive2D)  then
  begin
    TSimplePrimitive2D(TmpObject2D).SetEndPointX(APoint2D.X);
    TSimplePrimitive2D(TmpObject2D).SetEndPointY(APoint2D.Y);
    result := true;
  end;
end;

function CAD_SimplePrim2DGetEndPoint2DX(AID: longint): TRealType;
var TmpObject2D: TObject2D;
begin
  //result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TSimplePrimitive2D)  then
    result := TSimplePrimitive2D(TmpObject2D).EndPoint.X;
end;

function CAD_SimplePrim2DSetEndPoint2DX(AID: longint; var AValue: TRealType): boolean;
var TmpObject2D: TObject2D;
begin
  result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TSimplePrimitive2D)  then
  begin
    TSimplePrimitive2D(TmpObject2D).SetEndPointX(AValue);
    result := true;
  end;
end;

function CAD_SimplePrim2DGetEndPoint2DY(AID: longint): TRealType;
var TmpObject2D: TObject2D;
begin
  //result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TSimplePrimitive2D)  then
    result := TSimplePrimitive2D(TmpObject2D).EndPoint.Y;
end;

function CAD_SimplePrim2DSetEndPoint2DY(AID: longint; var AValue: TRealType): boolean;
var TmpObject2D: TObject2D;
begin
  result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TSimplePrimitive2D)  then
  begin
    TSimplePrimitive2D(TmpObject2D).SetEndPointY(AValue);
    result := true;
  end;
end;

function CAD_SimplePrim2DGetEndPoint2DW(AID: longint): TRealType;
var TmpObject2D: TObject2D;
begin
  //result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TSimplePrimitive2D)  then
    result := TSimplePrimitive2D(TmpObject2D).EndPoint.W;
end;

function CAD_SimplePrim2DSetEndPoint2DW(AID: longint; var AValue: TRealType): boolean;
var TmpObject2D: TObject2D;  TmpPoint2D: TPoint2D;
begin
  result := false;
  {GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TSimplePrimitive2D)  then
  begin
    TmpPoint2D.X := TSimplePrimitive2D(TmpObject2D).EndPoint.X;
    TmpPoint2D.Y := TSimplePrimitive2D(TmpObject2D).EndPoint.Y;
    TmpPoint2D.W := AValue;
    TSimplePrimitive2D(TmpObject2D).EndPoint := TmpPoint2D;
    result := true;
  end;  }
end;

function CAD_SimplePrim2DGetShowDirection(AID: longint): boolean;
var TmpObject2D: TObject2D;
begin
  //result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TSimplePrimitive2D)  then
    result := TSimplePrimitive2D(TmpObject2D).ShowDirection;
end;

function CAD_SimplePrim2DSetShowDirection(AID: longint; var AValue: boolean): boolean;
var TmpObject2D: TObject2D;
begin
  result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TSimplePrimitive2D)  then
  begin
    TSimplePrimitive2D(TmpObject2D).ShowDirection := AValue;
    result := true;
  end;
end;

function CAD_SimplePrim2DGetObjectLength(AID: longint): TRealType;
var TmpObject2D: TObject2D;
begin
  //result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TSimplePrimitive2D)  then
    result := TSimplePrimitive2D(TmpObject2D).ObjectLength;
end;


//TCircularArc2D////////////////////////////////////////////////////////////////
function CAD_CircularArc2DGetStartAngle(AID: longint): TRealType;
var TmpObject2D: TObject2D;
begin
  //result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TCircularArc2D)  then
    result := RadToDeg(TCircularArc2D(TmpObject2D).StartAngle);
end;

function CAD_CircularArc2DSetStartAngle(AID: longint; var AValue: TRealType): boolean;
var TmpObject2D: TObject2D;
begin
  result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TCircularArc2D)  then
  begin
    TCircularArc2D(TmpObject2D).StartAngle := DegToRad(AValue);
    result := true;
  end;
end;

function CAD_CircularArc2DGetEndAngle(AID: longint): TRealType;
var TmpObject2D: TObject2D;
begin
  //result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TCircularArc2D)  then
    result := RadToDeg(TCircularArc2D(TmpObject2D).EndAngle);
end;

function CAD_CircularArc2DSetEndAngle(AID: longint; var AValue: TRealType): boolean;
var TmpObject2D: TObject2D;
begin
  result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TCircularArc2D)  then
  begin
    TCircularArc2D(TmpObject2D).EndAngle := DegToRad(AValue);
    result := true;
  end;
end;

function CAD_CircularArc2DGetArcAngle(AID: longint): TRealType;
var TmpObject2D: TObject2D;
begin
  //result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TCircularArc2D)  then
    result := RadToDeg(TCircularArc2D(TmpObject2D).ArcAngle);
end;

function CAD_CircularArc2DSetArcAngle(AID: longint; var AValue: TRealType): boolean;
var TmpObject2D: TObject2D;
begin
  result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TCircularArc2D)  then
  begin
    TCircularArc2D(TmpObject2D).ArcAngle := DegToRad(AValue);
    result := true;
  end;
end;

function CAD_CircularArc2DGetCenterPoint2D(AID: longint): TPoint2D;
var TmpObject2D: TObject2D;
begin
  //result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TCircularArc2D)  then
    result := TCircularArc2D(TmpObject2D).CenterPoint2D;
end;

function CAD_CircularArc2DSetCenterPoint2D(AID: longint;  var APoint2D: TPoint2D): boolean;
var TmpObject2D: TObject2D;
begin
  result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TCircularArc2D)  then
  begin
    TCircularArc2D(TmpObject2D).CenterPoint2D := APoint2D;
    result := true;
  end;
end;

function CAD_CircularArc2DGetCenterPoint2DX(AID: longint): TRealType;
var TmpObject2D: TObject2D;
begin
  //result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TCircularArc2D)  then
    result := TCircularArc2D(TmpObject2D).CenterPoint2D.X;
end;

function CAD_CircularArc2DSetCenterPoint2DX(AID: longint; var AValue: TRealType): boolean;
var TmpObject2D: TObject2D;
begin
  result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TCircularArc2D)  then
  begin
    TCircularArc2D(TmpObject2D).CenterPoint2DX := AValue;
    result := true;
  end;
end;

function CAD_CircularArc2DGetCenterPoint2DY(AID: longint): TRealType;
var TmpObject2D: TObject2D;
begin
  //result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TCircularArc2D)  then
    result := TCircularArc2D(TmpObject2D).CenterPoint2D.Y;
end;

function CAD_CircularArc2DSetCenterPoint2DY(AID: longint; var AValue: TRealType): boolean;
var TmpObject2D: TObject2D;
begin
  result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TCircularArc2D)  then
  begin
    TCircularArc2D(TmpObject2D).CenterPoint2DY := AValue;
    result := true;
  end;
end;

function CAD_CircularArc2DGetCenterPoint2DW(AID: longint): TRealType;
begin
  //
end;

function CAD_CircularArc2DSetCenterPoint2DW(AID: longint; var AValue: TRealType): boolean;
begin
  result := false;
end;

function CAD_CircularArc2DGetRadius(AID: longint): TRealType;
var TmpObject2D: TObject2D;
begin
  //result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TCircularArc2D)  then
    result := TCircularArc2D(TmpObject2D).Radius;
end;

function CAD_CircularArc2DSetRadius(AID: longint; var AValue: TRealType): boolean;
var TmpObject2D: TObject2D;
begin
  result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TCircularArc2D)  then
  begin
    TCircularArc2D(TmpObject2D).Radius := AValue;
    result := true;
  end;
end;

function CAD_CircularArc2DGetDirection(AID: longint): TArcDirection;
var TmpObject2D: TObject2D;
begin
  //result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TCircularArc2D)  then
    result := TCircularArc2D(TmpObject2D).Direction;
end;

function CAD_CircularArc2DSetDirection(AID: longint; var AValue: TArcDirection): boolean;
var TmpObject2D: TObject2D;
begin
  result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TCircularArc2D)  then
  begin
    TCircularArc2D(TmpObject2D).Direction := AValue;
    result := true;
  end;
end;

function CAD_CircularArc2DGetCurvePrecision(AID: longint): word;
var TmpObject2D: TObject2D;
begin
  //result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TCircularArc2D)  then
    result := TCircularArc2D(TmpObject2D).CurvePrecision;
end;

function CAD_CircularArc2DSetCurvePrecision(AID: longint; var AValue: word): boolean;
var TmpObject2D: TObject2D;
begin
  result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TCircularArc2D)  then
  begin
    TCircularArc2D(TmpObject2D).CurvePrecision := AValue;
    result := true;
  end;
end;

//EllipticalArc2D///////////////////////////////////////////////////////////////
function CAD_ElliArc2DGetStartAngle(AID: longint): TRealType;
var TmpObject2D: TObject2D;
begin
  //result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TEllipticalArc2D)  then
    result := RadToDeg(TEllipticalArc2D(TmpObject2D).StartAngle);
end;

function CAD_ElliArc2DSetStartAngle(AID: longint; var AValue: TRealType): boolean;
var TmpObject2D: TObject2D;
begin
  result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TEllipticalArc2D)  then
  begin
    TEllipticalArc2D(TmpObject2D).StartAngle := DegToRad(AValue);
    result := true;
  end;
end;

function CAD_ElliArc2DGetEndAngle(AID: longint): TRealType;
var TmpObject2D: TObject2D;
begin
  //result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TEllipticalArc2D)  then
    result := RadToDeg(TEllipticalArc2D(TmpObject2D).EndAngle);
end;

function CAD_ElliArc2DSetEndAngle(AID: longint; var AValue: TRealType): boolean;
var TmpObject2D: TObject2D;
begin
  result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TEllipticalArc2D)  then
  begin
    TEllipticalArc2D(TmpObject2D).EndAngle := DegToRad(AValue);
    result := true;
  end;
end;

function CAD_ElliArc2DGetArcAngle(AID: longint): TRealType;
var TmpObject2D: TObject2D;
begin
  //result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TEllipticalArc2D)  then
    result := RadToDeg(TEllipticalArc2D(TmpObject2D).ArcAngle);
end;

function CAD_ElliArc2DSetArcAngle(AID: longint; var AValue: TRealType): boolean;
var TmpObject2D: TObject2D;
begin
  result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TEllipticalArc2D)  then
  begin
    TEllipticalArc2D(TmpObject2D).ArcAngle := DegToRad(AValue);
    result := true;
  end;
end;

function CAD_ElliArc2DGetCurvePrecision(AID: longint): word;
var TmpObject2D: TObject2D;
begin
  //result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TEllipticalArc2D)  then
    result := TEllipticalArc2D(TmpObject2D).CurvePrecision;
end;

function CAD_ElliArc2DSetCurvePrecision(AID: longint; var AValue: word): boolean;
var TmpObject2D: TObject2D;
begin
  result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TEllipticalArc2D)  then
  begin
    TEllipticalArc2D(TmpObject2D).CurvePrecision := AValue;
    result := true;
  end;
end;

function CAD_ElliArc2DGetDirection(AID: longint): TArcDirection;
var TmpObject2D: TObject2D;
begin
  //result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TEllipticalArc2D)  then
    result := TEllipticalArc2D(TmpObject2D).Direction;
end;

function CAD_ElliArc2DSetDirection(AID: longint; var ADirection: TArcDirection): boolean;
var TmpObject2D: TObject2D;
begin
  result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TEllipticalArc2D)  then
  begin
    TEllipticalArc2D(TmpObject2D).Direction := ADirection;
    result := true;
  end;
end;

//TClosedCurve2D & TClosedPolyline2D////////////////////////////////////////////
//Brush implementiert
function CAD_ClosedCurve2DGetArea(AID: longint): TRealType;
var TmpObject2D: TObject2D;
begin
  //result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TClosedCurve2D)  then
    result := TClosedCurve2D(TmpObject2D).Area;
end;

function CAD_ClosedPolyline2DGetArea(AID: longint): TRealType;
var TmpObject2D: TObject2D;
begin
  //result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TClosedPolyline2D)  then
    result := TClosedPolyline2D(TmpObject2D).Area;
end;


//TCircle2D/////////////////////////////////////////////////////////////////////
function CAD_Circle2DGetStartAngle(AID: longint): TRealType;
var TmpObject2D: TObject2D;
begin
  //result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TCircle2D)  then
    result := RadToDeg(TCircle2D(TmpObject2D).StartAngle);
end;

function CAD_Circle2DSetStartAngle(AID: longint; var AValue: TRealType): boolean;
var TmpObject2D: TObject2D;
begin
  result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TCircle2D)  then
  begin
    TCircle2D(TmpObject2D).StartAngle := DegToRad(AValue);
    result := true;
  end;
end;

function CAD_Circle2DGetCenterPoint2D(AID: longint): TPoint2D;
var TmpObject2D: TObject2D;
begin
  //result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TCircle2D)  then
    result := TCircle2D(TmpObject2D).CenterPoint2D;
end;

function CAD_Circle2DSetCenterPoint2D(AID: longint;  var APoint2D: TPoint2D): boolean;
var TmpObject2D: TObject2D;
begin
  result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TCircle2D)  then
  begin
    TCircle2D(TmpObject2D).CenterPoint2D := APoint2D;
    result := true;
  end;
end;

function CAD_Circle2DGetCenterPoint2DX(AID: longint): TRealType;
var TmpObject2D: TObject2D;
begin
  //result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TCircle2D)  then
    result := TCircle2D(TmpObject2D).CenterPoint2D.X;
end;

function CAD_Circle2DSetCenterPoint2DX(AID: longint; var AValue: TRealType): boolean;
var TmpObject2D: TObject2D;
begin
  result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TCircle2D)  then
  begin
    TCircle2D(TmpObject2D).CenterPoint2DX := AValue;
    result := true;
  end;
end;

function CAD_Circle2DGetCenterPoint2DY(AID: longint): TRealType;
var TmpObject2D: TObject2D;
begin
  //result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TCircle2D)  then
    result := TCircle2D(TmpObject2D).CenterPoint2D.Y;
end;

function CAD_Circle2DSetCenterPoint2DY(AID: longint; var AValue: TRealType): boolean;
var TmpObject2D: TObject2D;
begin
  result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TCircle2D)  then
  begin
    TCircle2D(TmpObject2D).CenterPoint2DY := AValue;
    result := true;
  end;
end;

function CAD_Circle2DGetCenterPoint2DW(AID: longint): TRealType;
begin
  //
end;

function CAD_Circle2DSetCenterPoint2DW(AID: longint; var AValue: TRealType): boolean;
begin
  result := false;
end;

function CAD_Circle2DGetRadius(AID: longint): TRealType;
var TmpObject2D: TObject2D;
begin
  //result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TCircle2D)  then
    result := TCircle2D(TmpObject2D).Radius;
end;

function CAD_Circle2DSetRadius(AID: longint; var AValue: TRealType): boolean;
var TmpObject2D: TObject2D;
begin
  result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TCircle2D)  then
  begin
    TCircle2D(TmpObject2D).Radius := AValue;
    result := true;
  end;
end;

function CAD_Circle2DGetDirection(AID: longint): TArcDirection;
var TmpObject2D: TObject2D;
begin
  //result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TCircle2D)  then
    result := TCircle2D(TmpObject2D).Direction;
end;

function CAD_Circle2DSetDirection(AID: longint; var AValue: TArcDirection): boolean;
var TmpObject2D: TObject2D;
begin
  result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TCircle2D)  then
  begin
    TCircle2D(TmpObject2D).Direction := AValue;
    result := true;
  end;
end;

function CAD_Circle2DGetCurvePrecision(AID: longint): word;
var TmpObject2D: TObject2D;
begin
  //result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TCircle2D)  then
    result := TCircle2D(TmpObject2D).CurvePrecision;
end;

function CAD_Circle2DSetCurvePrecision(AID: longint; var AValue: word): boolean;
var TmpObject2D: TObject2D;
begin
  result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TCircle2D)  then
  begin
    TCircle2D(TmpObject2D).CurvePrecision := AValue;
    result := true;
  end;
end;

//Polyline2D////////////////////////////////////////////////////////////////////
function CAD_Polyline2DGetPointsCount(AID: longint): integer;
var TmpObject2D: TObject2D;
begin
  //result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TPolyline2D)  then
    result := TPolyline2D(TmpObject2D).Points.Count;
end;

//Polygon2D/////////////////////////////////////////////////////////////////////
function CAD_Polygon2DGetPointsCount(AID: longint): integer;
var TmpObject2D: TObject2D;
begin
  //result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TPolygon2D)  then
    result := TPolygon2D(TmpObject2D).Points.Count;
end;

//Bspline2D/////////////////////////////////////////////////////////////////////
function CAD_BSpline2DGetPointsCount(AID: longint): integer;
var TmpObject2D: TObject2D;
begin
  //result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TBSpline2D)  then
    result := TBSpline2D(TmpObject2D).Points.Count;
end;

//Frame2D/////////////////////////////////////////////////////////////////////
function CAD_Frame2DGetDirection(AID: longint): TArcDirection;
var TmpObject2D: TObject2D;
begin
  //result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TFrame2D)  then
    result := TFrame2D(TmpObject2D).Direction;
end;

function CAD_Frame2DSetDirection(AID: longint; var AValue: TArcDirection): boolean;
var TmpObject2D: TObject2D;
begin
  result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TFrame2D)  then
  begin
    TFrame2D(TmpObject2D).Direction := AValue;
    result := true;
  end;
end;

function CAD_Frame2DGetHeight(AID: longint): TRealType;
var TmpObject2D: TObject2D;
begin
  //result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TFrame2D)  then
    result := TFrame2D(TmpObject2D).Height;
end;

function CAD_Frame2DSetHeight(AID: longint; var AValue: TRealType): boolean;
var TmpObject2D: TObject2D;
begin
  result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TFrame2D)  then
  begin
    TFrame2D(TmpObject2D).Height := AValue;
    result := true;
  end;
end;

function CAD_Frame2DGetWidth(AID: longint): TRealType;
var TmpObject2D: TObject2D;
begin
  //result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TFrame2D)  then
    result := TFrame2D(TmpObject2D).Width;
end;

function CAD_Frame2DSetWidth(AID: longint; var AValue: TRealType): boolean;
var TmpObject2D: TObject2D;
begin
  result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TFrame2D)  then
  begin
    TFrame2D(TmpObject2D).Width := AValue;
    result := true;
  end;
end;

function CAD_Frame2DGetStartCorner(AID: longint): TFrameStartCorner;
var TmpObject2D: TObject2D;
begin
  //result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TFrame2D)  then
    result := TFrame2D(TmpObject2D).StartCorner;
end;

function CAD_Frame2DSetStartCorner(AID: longint; var AValue: TFrameStartCorner): boolean;
var TmpObject2D: TObject2D;
begin
  result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TFrame2D)  then
  begin
    TFrame2D(TmpObject2D).StartCorner := AValue;
    result := true;
  end;
end;

//Ellipse2D/////////////////////////////////////////////////////////////////////
function CAD_Ellipse2DGetDirection(AID: longint): TArcDirection;
var TmpObject2D: TObject2D;
begin
  //result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TEllipse2D)  then
    result := TEllipse2D(TmpObject2D).Direction;
end;

function CAD_Ellipse2DSetDirection(AID: longint; var AValue: TArcDirection): boolean;
var TmpObject2D: TObject2D;
begin
  result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TEllipse2D)  then
  begin
    TEllipse2D(TmpObject2D).Direction := AValue;
    result := true;
  end;
end;

function CAD_Ellipse2DGetHeight(AID: longint): TRealType;
var TmpObject2D: TObject2D;
begin
  //result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TEllipse2D)  then
    result := TEllipse2D(TmpObject2D).Height;
end;

function CAD_Ellipse2DSetHeight(AID: longint; var AValue: TRealType): boolean;
var TmpObject2D: TObject2D;
begin
  result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TEllipse2D)  then
  begin
    TEllipse2D(TmpObject2D).Height := AValue;
    result := true;
  end;
end;

function CAD_Ellipse2DGetWidth(AID: longint): TRealType;
var TmpObject2D: TObject2D;
begin
  //result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TEllipse2D)  then
    result := TEllipse2D(TmpObject2D).Width;
end;

function CAD_Ellipse2DSetWidth(AID: longint; var AValue: TRealType): boolean;
var TmpObject2D: TObject2D;
begin
  result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TEllipse2D)  then
  begin
    TEllipse2D(TmpObject2D).Width := AValue;
    result := true;
  end;
end;


//TSector2D/////////////////////////////////////////////////////////////////////
function CAD_Sector2DGetStartAngle(AID: longint): TRealType;
var TmpObject2D: TObject2D;
begin
  //result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TSector2D)  then
    result := RadToDeg(TSector2D(TmpObject2D).StartAngle);
end;

function CAD_Sector2DSetStartAngle(AID: longint; var AValue: TRealType): boolean;
var TmpObject2D: TObject2D;
begin
  result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TSector2D)  then
  begin
    TSector2D(TmpObject2D).StartAngle := DegToRad(AValue);
    result := true;
  end;
end;

function CAD_Sector2DGetEndAngle(AID: longint): TRealType;
var TmpObject2D: TObject2D;
begin
  //result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TSector2D)  then
    result := RadToDeg(TSector2D(TmpObject2D).EndAngle);
end;

function CAD_Sector2DSetEndAngle(AID: longint; var AValue: TRealType): boolean;
var TmpObject2D: TObject2D;
begin
  result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TSector2D)  then
  begin
    TSector2D(TmpObject2D).EndAngle := DegToRad(AValue);
    result := true;
  end;
end;

function CAD_Sector2DGetArcAngle(AID: longint): TRealType;
var TmpObject2D: TObject2D;
begin
  //result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TSector2D)  then
    result := RadToDeg(TSector2D(TmpObject2D).ArcAngle);
end;

function CAD_Sector2DSetArcAngle(AID: longint; var AValue: TRealType): boolean;
var TmpObject2D: TObject2D;
begin
  result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TSector2D)  then
  begin
    TSector2D(TmpObject2D).ArcAngle := DegToRad(AValue);
    result := true;
  end;
end;

function CAD_Sector2DGetCenterPoint2D(AID: longint): TPoint2D;
var TmpObject2D: TObject2D;
begin
  //result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TSector2D)  then
    result := TSector2D(TmpObject2D).CenterPoint2D;
end;

function CAD_Sector2DSetCenterPoint2D(AID: longint;  var APoint2D: TPoint2D): boolean;
var TmpObject2D: TObject2D;
begin
  result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TSector2D)  then
  begin
    TSector2D(TmpObject2D).CenterPoint2D := APoint2D;
    result := true;
  end;
end;

function CAD_Sector2DGetCenterPoint2DX(AID: longint): TRealType;
var TmpObject2D: TObject2D;
begin
  //result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TSector2D)  then
    result := TSector2D(TmpObject2D).CenterPoint2D.X;
end;

function CAD_Sector2DSetCenterPoint2DX(AID: longint; var AValue: TRealType): boolean;
var TmpObject2D: TObject2D;
begin
  result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TSector2D)  then
  begin
    TSector2D(TmpObject2D).CenterPoint2DX := AValue;
    result := true;
  end;
end;

function CAD_Sector2DGetCenterPoint2DY(AID: longint): TRealType;
var TmpObject2D: TObject2D;
begin
  //result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TSector2D)  then
    result := TSector2D(TmpObject2D).CenterPoint2D.Y;
end;

function CAD_Sector2DSetCenterPoint2DY(AID: longint; var AValue: TRealType): boolean;
var TmpObject2D: TObject2D;
begin
  result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TSector2D)  then
  begin
    TSector2D(TmpObject2D).CenterPoint2DY := AValue;
    result := true;
  end;
end;

function CAD_Sector2DGetCenterPoint2DW(AID: longint): TRealType;
begin
  //
end;

function CAD_Sector2DSetCenterPoint2DW(AID: longint; var AValue: TRealType): boolean;
begin
  result := false;
end;

function CAD_Sector2DGetRadius(AID: longint): TRealType;
var TmpObject2D: TObject2D;
begin
  //result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TSector2D)  then
    result := TSector2D(TmpObject2D).Radius;
end;

function CAD_Sector2DSetRadius(AID: longint; var AValue: TRealType): boolean;
var TmpObject2D: TObject2D;
begin
  result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TSector2D)  then
  begin
    TSector2D(TmpObject2D).Radius := AValue;
    result := true;
  end;
end;

function CAD_Sector2DGetDirection(AID: longint): TArcDirection;
var TmpObject2D: TObject2D;
begin
  //result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TSector2D)  then
    result := TSector2D(TmpObject2D).Direction;
end;

function CAD_Sector2DSetDirection(AID: longint; var AValue: TArcDirection): boolean;
var TmpObject2D: TObject2D;
begin
  result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TSector2D)  then
  begin
    TSector2D(TmpObject2D).Direction := AValue;
    result := true;
  end;
end;

function CAD_Sector2DGetCurvePrecision(AID: longint): word;
var TmpObject2D: TObject2D;
begin
  //result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TSector2D)  then
    result := TSector2D(TmpObject2D).CurvePrecision;
end;

function CAD_Sector2DSetCurvePrecision(AID: longint; var AValue: word): boolean;
var TmpObject2D: TObject2D;
begin
  result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TSector2D)  then
  begin
    TSector2D(TmpObject2D).CurvePrecision := AValue;
    result := true;
  end;
end;

//Segment2D////////////////////////////////////////////////////////////////////
function CAD_Segment2DGetStartAngle(AID: longint): TRealType;
var TmpObject2D: TObject2D;
begin
  //result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TSegment2D)  then
    result := RadToDeg(TSegment2D(TmpObject2D).StartAngle);
end;

function CAD_Segment2DSetStartAngle(AID: longint; var AValue: TRealType): boolean;
var TmpObject2D: TObject2D;
begin
  result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TSegment2D)  then
  begin
    TSegment2D(TmpObject2D).StartAngle := DegToRad(AValue);
    result := true;
  end;
end;

function CAD_Segment2DGetEndAngle(AID: longint): TRealType;
var TmpObject2D: TObject2D;
begin
  //result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TSegment2D)  then
    result := RadToDeg(TSegment2D(TmpObject2D).EndAngle);
end;

function CAD_Segment2DSetEndAngle(AID: longint; var AValue: TRealType): boolean;
var TmpObject2D: TObject2D;
begin
  result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TSegment2D)  then
  begin
    TSegment2D(TmpObject2D).EndAngle := DegToRad(AValue);
    result := true;
  end;
end;

function CAD_Segment2DGetArcAngle(AID: longint): TRealType;
var TmpObject2D: TObject2D;
begin
  //result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TSegment2D)  then
    result := RadToDeg(TSegment2D(TmpObject2D).ArcAngle);
end;

function CAD_Segment2DSetArcAngle(AID: longint; var AValue: TRealType): boolean;
var TmpObject2D: TObject2D;
begin
  result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TSegment2D)  then
  begin
    TSegment2D(TmpObject2D).ArcAngle := DegToRad(AValue);
    result := true;
  end;
end;

function CAD_Segment2DGetCenterPoint2D(AID: longint): TPoint2D;
var TmpObject2D: TObject2D;
begin
  //result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TSegment2D)  then
    result := TSegment2D(TmpObject2D).CenterPoint2D;
end;

function CAD_Segment2DSetCenterPoint2D(AID: longint;  var APoint2D: TPoint2D): boolean;
var TmpObject2D: TObject2D;
begin
  result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TSegment2D)  then
  begin
    TSegment2D(TmpObject2D).CenterPoint2D := APoint2D;
    result := true;
  end;
end;

function CAD_Segment2DGetCenterPoint2DX(AID: longint): TRealType;
var TmpObject2D: TObject2D;
begin
  //result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TSegment2D)  then
    result := TSegment2D(TmpObject2D).CenterPoint2D.X;
end;

function CAD_Segment2DSetCenterPoint2DX(AID: longint; var AValue: TRealType): boolean;
var TmpObject2D: TObject2D;
begin
  result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TSegment2D)  then
  begin
    TSegment2D(TmpObject2D).CenterPoint2DX := AValue;
    result := true;
  end;
end;

function CAD_Segment2DGetCenterPoint2DY(AID: longint): TRealType;
var TmpObject2D: TObject2D;
begin
  //result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TSegment2D)  then
    result := TSegment2D(TmpObject2D).CenterPoint2D.Y;
end;

function CAD_Segment2DSetCenterPoint2DY(AID: longint; var AValue: TRealType): boolean;
var TmpObject2D: TObject2D;
begin
  result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TSegment2D)  then
  begin
    TSegment2D(TmpObject2D).CenterPoint2DY := AValue;
    result := true;
  end;
end;

function CAD_Segment2DGetCenterPoint2DW(AID: longint): TRealType;
begin
  //
end;

function CAD_Segment2DSetCenterPoint2DW(AID: longint; var AValue: TRealType): boolean;
begin
  result := false;
end;

function CAD_Segment2DGetRadius(AID: longint): TRealType;
var TmpObject2D: TObject2D;
begin
  //result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TSegment2D)  then
    result := TSegment2D(TmpObject2D).Radius;
end;

function CAD_Segment2DSetRadius(AID: longint; var AValue: TRealType): boolean;
var TmpObject2D: TObject2D;
begin
  result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TSegment2D)  then
  begin
    TSegment2D(TmpObject2D).Radius := AValue;
    result := true;
  end;
end;

function CAD_Segment2DGetDirection(AID: longint): TArcDirection;
var TmpObject2D: TObject2D;
begin
  //result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TSegment2D)  then
    result := TSegment2D(TmpObject2D).Direction;
end;

function CAD_Segment2DSetDirection(AID: longint; var AValue: TArcDirection): boolean;
var TmpObject2D: TObject2D;
begin
  result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TSegment2D)  then
  begin
    TSegment2D(TmpObject2D).Direction := AValue;
    result := true;
  end;
end;

function CAD_Segment2DGetCurvePrecision(AID: longint): word;
var TmpObject2D: TObject2D;
begin
  //result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TSegment2D)  then
    result := TSegment2D(TmpObject2D).CurvePrecision;
end;

function CAD_Segment2DSetCurvePrecision(AID: longint; var AValue: word): boolean;
var TmpObject2D: TObject2D;
begin
  result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TSegment2D)  then
  begin
    TSegment2D(TmpObject2D).CurvePrecision := AValue;
    result := true;
  end;
end;

//TSymetricSymbol2D/////////////////////////////////////////////////////////////
function CAD_SymSymbol2DGetStartAngle(AID: longint): TRealType;
var TmpObject2D: TObject2D;
begin
  //result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TSymetricSymbol2D)  then
    result := RadToDeg(TSymetricSymbol2D(TmpObject2D).StartAngle);
end;

function CAD_SymSymbol2DSetStartAngle(AID: longint; var AValue: TRealType): boolean;
var TmpObject2D: TObject2D;
begin
  result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TSymetricSymbol2D)  then
  begin
    TSymetricSymbol2D(TmpObject2D).StartAngle := DegToRad(AValue);
    result := true;
  end;
end;

function CAD_SymSymbol2DGetCenterPoint2D(AID: longint): TPoint2D;
var TmpObject2D: TObject2D;
begin
  //result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TSymetricSymbol2D)  then
    result := TSymetricSymbol2D(TmpObject2D).CenterPoint2D;
end;

function CAD_SymSymbol2DSetCenterPoint2D(AID: longint;  var APoint2D: TPoint2D): boolean;
var TmpObject2D: TObject2D;
begin
  result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TCircle2D)  then
  begin
    TCircle2D(TmpObject2D).CenterPoint2D := APoint2D;
    result := true;
  end;
end;

function CAD_SymSymbol2DGetCenterPoint2DX(AID: longint): TRealType;
var TmpObject2D: TObject2D;
begin
  //result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TSymetricSymbol2D)  then
    result := TSymetricSymbol2D(TmpObject2D).CenterPoint2D.X;
end;

function CAD_SymSymbol2DSetCenterPoint2DX(AID: longint; var AValue: TRealType): boolean;
var TmpObject2D: TObject2D;
begin
  result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TSymetricSymbol2D)  then
  begin
    TSymetricSymbol2D(TmpObject2D).CenterPoint2DX := AValue;
    result := true;
  end;
end;

function CAD_SymSymbol2DGetCenterPoint2DY(AID: longint): TRealType;
var TmpObject2D: TObject2D;
begin
  //result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TSymetricSymbol2D)  then
    result := TSymetricSymbol2D(TmpObject2D).CenterPoint2D.Y;
end;

function CAD_SymSymbol2DSetCenterPoint2DY(AID: longint; var AValue: TRealType): boolean;
var TmpObject2D: TObject2D;
begin
  result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TSymetricSymbol2D)  then
  begin
    TSymetricSymbol2D(TmpObject2D).CenterPoint2DY := AValue;
    result := true;
  end;
end;

function CAD_SymSymbol2DGetCenterPoint2DW(AID: longint): TRealType;
begin
  //
end;

function CAD_SymSymbol2DSetCenterPoint2DW(AID: longint; var AValue: TRealType): boolean;
begin
  result := false;
end;

function CAD_SymSymbol2DGetRadius(AID: longint): TRealType;
var TmpObject2D: TObject2D;
begin
  //result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TSymetricSymbol2D)  then
    result := TSymetricSymbol2D(TmpObject2D).Radius;
end;

function CAD_SymSymbol2DSetRadius(AID: longint; var AValue: TRealType): boolean;
var TmpObject2D: TObject2D;
begin
  result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TSymetricSymbol2D)  then
  begin
    TSymetricSymbol2D(TmpObject2D).Radius := AValue;
    result := true;
  end;
end;

function CAD_SymSymbol2DGetDirection(AID: longint): TArcDirection;
var TmpObject2D: TObject2D;
begin
  //result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TSymetricSymbol2D)  then
    result := TSymetricSymbol2D(TmpObject2D).Direction;
end;

function CAD_SymSymbol2DSetDirection(AID: longint; var AValue: TArcDirection): boolean;
var TmpObject2D: TObject2D;
begin
  result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TSymetricSymbol2D)  then
  begin
    TSymetricSymbol2D(TmpObject2D).Direction := AValue;
    result := true;
  end;
end;

function CAD_SymSymbol2DGetCurvePrecision(AID: longint): word;
var TmpObject2D: TObject2D;
begin
  //result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TSymetricSymbol2D)  then
    result := TSymetricSymbol2D(TmpObject2D).CurvePrecision;
end;

function CAD_SymSymbol2DSetCurvePrecision(AID: longint; var AValue: word): boolean;
var TmpObject2D: TObject2D;
begin
  result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TSymetricSymbol2D)  then
  begin
    TSymetricSymbol2D(TmpObject2D).CurvePrecision := AValue;
    result := true;
  end;
end;


//ASymetricSymbol2D/////////////////////////////////////////////////////////////
function CAD_ASymSymbol2DGetDirection(AID: longint): TArcDirection;
var TmpObject2D: TObject2D;
begin
  //result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TASymetricSymbol2D)  then
    result := TASymetricSymbol2D(TmpObject2D).Direction;
end;

function CAD_ASymSymbol2DSetDirection(AID: longint; var AValue: TArcDirection): boolean;
var TmpObject2D: TObject2D;
begin
  result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TASymetricSymbol2D)  then
  begin
    TASymetricSymbol2D(TmpObject2D).Direction := AValue;
    result := true;
  end;
end;

function CAD_ASymSymbol2DGetHeight(AID: longint): TRealType;
var TmpObject2D: TObject2D;
begin
  //result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TASymetricSymbol2D)  then
    result := TASymetricSymbol2D(TmpObject2D).Height;
end;

function CAD_ASymSymbol2DSetHeight(AID: longint; var AValue: TRealType): boolean;
var TmpObject2D: TObject2D;
begin
  result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TASymetricSymbol2D)  then
  begin
    TASymetricSymbol2D(TmpObject2D).Height := AValue;
    result := true;
  end;
end;

function CAD_ASymSymbol2DGetWidth(AID: longint): TRealType;
var TmpObject2D: TObject2D;
begin
  //result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TASymetricSymbol2D)  then
    result := TASymetricSymbol2D(TmpObject2D).Width;
end;

function CAD_ASymSymbol2DSetWidth(AID: longint; var AValue: TRealType): boolean;
var TmpObject2D: TObject2D;
begin
  result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TASymetricSymbol2D)  then
  begin
    TASymetricSymbol2D(TmpObject2D).Width := AValue;
    result := true;
  end;
end;

//Text2D////////////////////////////////////////////////////////////////////////
function CAD_Text2DGetText(AID: longint): string;
var TmpObject2D: TObject2D;
begin
  //result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TText2D)  then
    result := TText2D(TmpObject2D).Text;
end;

function CAD_Text2DSetTex(AID: longint; AValue: string): boolean;
var TmpObject2D: TObject2D;
begin
  result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TText2D)  then
  begin
    TText2D(TmpObject2D).Text := AValue;
    result := true;
  end;
end;

function CAD_Text2DGetFaceName(AID: longint): string;
var TmpObject2D: TObject2D;
begin
  //result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TText2D)  then
    result := TText2D(TmpObject2D).FaceName;
end;

function CAD_Text2DSetFaceName(AID: longint; AValue: string): boolean;
var TmpObject2D: TObject2D;
begin
  result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TText2D)  then
  begin
    TText2D(TmpObject2D).FaceName := AValue;
    result := true;
  end;
end;

function CAD_Text2DGetAutoSize(AID: longint): boolean;
var TmpObject2D: TObject2D;
begin
  //result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TText2D)  then
    result := TText2D(TmpObject2D).AutoSize;
end;

function CAD_Text2DSetAutoSize(AID: longint; AValue: boolean): boolean;
var TmpObject2D: TObject2D;
begin
  result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TText2D)  then
  begin
    TText2D(TmpObject2D).AutoSize := AValue;
    result := true;
  end;
end;

function CAD_Text2DGetBold(AID: longint): boolean;
var TmpObject2D: TObject2D;
begin
  //result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TText2D)  then
    result := TText2D(TmpObject2D).Bold;
end;

function CAD_Text2DSetBold(AID: longint; AValue: boolean): boolean;
var TmpObject2D: TObject2D;
begin
  result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TText2D)  then
  begin
    TText2D(TmpObject2D).Bold := AValue;
    result := true;
  end;
end;

function CAD_Text2DGetUnderline(AID: longint): boolean;
var TmpObject2D: TObject2D;
begin
  //result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TText2D)  then
    result := TText2D(TmpObject2D).Underline;
end;

function CAD_Text2DSetUnderline(AID: longint; AValue: boolean): boolean;
var TmpObject2D: TObject2D;
begin
  result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TText2D)  then
  begin
    TText2D(TmpObject2D).Underline := AValue;
    result := true;
  end;
end;

function CAD_Text2DGetStrikeout(AID: longint): boolean;
var TmpObject2D: TObject2D;
begin
  //result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TText2D)  then
    result := TText2D(TmpObject2D).Strikeout;
end;

function CAD_Text2DSetStrikeout(AID: longint; AValue: boolean): boolean;
var TmpObject2D: TObject2D;
begin
  result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TText2D)  then
  begin
    TText2D(TmpObject2D).Strikeout := AValue;
    result := true;
  end;
end;

function CAD_Text2DGetItalic(AID: longint): boolean;
var TmpObject2D: TObject2D;
begin
  //result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TText2D)  then
    result := TText2D(TmpObject2D).Italic;
end;

function CAD_Text2DSetItalic(AID: longint; AValue: boolean): boolean;
var TmpObject2D: TObject2D;
begin
  result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TText2D)  then
  begin
    TText2D(TmpObject2D).Italic := AValue;
    result := true;
  end;
end;

function CAD_Text2DGetDrawBox(AID: longint): boolean;
var TmpObject2D: TObject2D;
begin
  //result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TText2D)  then
    result := TText2D(TmpObject2D).DrawBox;
end;

function CAD_Text2DSetDrawBox(AID: longint; AValue: boolean): boolean;
var TmpObject2D: TObject2D;
begin
  result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TText2D)  then
  begin
    TText2D(TmpObject2D).DrawBox := AValue;
    result := true;
  end;
end;

function CAD_Text2DGetHeight(AID: longint): TRealType;
var TmpObject2D: TObject2D;
begin
  //result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TText2D)  then
    result := TText2D(TmpObject2D).Height;
end;

function CAD_Text2DSetHeight(AID: longint; AValue: TRealType): boolean;
var TmpObject2D: TObject2D;
begin
  result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TText2D)  then
  begin
    TText2D(TmpObject2D).Height := AValue;
    result := true;
  end;
end;

function CAD_Text2DGetColorSource(AID: longint): TColorSource;
var TmpObject2D: TObject2D;
begin
  //result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TText2D)  then
    result := TText2D(TmpObject2D).ColorSource;
end;

function CAD_Text2DSetColorSource(AID: longint; AValue: TColorSource): boolean;
var TmpObject2D: TObject2D;
begin
  result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TText2D)  then
  begin
    TText2D(TmpObject2D).ColorSource := AValue;
    result := true;
  end;
end;

function CAD_Text2DGetColor(AID: longint): TColor;
var TmpObject2D: TObject2D;
begin
  //result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TText2D)  then
    result := TText2D(TmpObject2D).Color;
end;

function CAD_Text2DSetColor(AID: longint; AValue: TColor): boolean;
var TmpObject2D: TObject2D;
begin
  result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TText2D)  then
  begin
    TText2D(TmpObject2D).Color := AValue;
    result := true;
  end;
end;


//TBitmap2D/////////////////////////////////////////////////////////////////////
function CAD_Bitmap2DGetScaleFactor(AID: longint): TRealType;
var TmpObject2D: TObject2D;
begin
  //result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TBitmap2D)  then
    result := TBitmap2D(TmpObject2D).ScaleFactor;
end;

function CAD_Bitmap2DSetScaleFactor(AID: longint; AValue: TRealType): boolean;
var TmpObject2D: TObject2D;
begin
  result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TBitmap2D)  then
  begin
    TBitmap2D(TmpObject2D).ScaleFactor := AValue;
    result := true;
  end;
end;

function CAD_Bitmap2DGetAspectRatio(AID: longint): TRealType;
var TmpObject2D: TObject2D;
begin
  //result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TBitmap2D)  then
    result := TBitmap2D(TmpObject2D).AspectRatio;
end;

function CAD_Bitmap2DSetAspectRatio(AID: longint; AValue: TRealType): boolean;
var TmpObject2D: TObject2D;
begin
  result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TBitmap2D)  then
  begin
    TBitmap2D(TmpObject2D).AspectRatio := AValue;
    result := true;
  end;
end;

function CAD_Bitmap2DGetCopyMode(AID: longint): TCopyMode;
var TmpObject2D: TObject2D;
begin
  //result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TBitmap2D)  then
    result := TBitmap2D(TmpObject2D).CopyMode;
end;

function CAD_Bitmap2DSetCopyMode(AID: longint; AValue: TCopyMode): boolean;
var TmpObject2D: TObject2D;
begin
  result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TBitmap2D)  then
  begin
    TBitmap2D(TmpObject2D).CopyMode := AValue;
    result := true;
  end;
end;

//JustifiedVectText2D///////////////////////////////////////////////////////////
function CAD_JVectText2DGetBasePoint(AID: longint): TPoint2D;
var TmpObject2D: TObject2D;
begin
  //result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TJustifiedVectText2D)  then
    result := TJustifiedVectText2D(TmpObject2D).BasePoint;
end;

function CAD_JVectText2DSetBasePoint(AID: longint; var APoint2D: TPoint2D): boolean;
var TmpObject2D: TObject2D;
begin
  result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TJustifiedVectText2D)  then
  begin
    TJustifiedVectText2D(TmpObject2D).BasePoint := APoint2D;
    result := true;
  end;
end;

function CAD_JVectText2DGetCharSpace(AID: longint): TRealType;
var TmpObject2D: TObject2D;
begin
  //result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TJustifiedVectText2D)  then
    result := TJustifiedVectText2D(TmpObject2D).CharSpace;
end;

function CAD_JVectText2DSetCharSpace(AID: longint; AValue: TRealType): boolean;
var TmpObject2D: TObject2D;
begin
  result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TJustifiedVectText2D)  then
  begin
    TJustifiedVectText2D(TmpObject2D).CharSpace := AValue;
    result := true;
  end;
end;

function CAD_JVectText2DGetDrawBox(AID: longint): boolean;
var TmpObject2D: TObject2D;
begin
  //result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TJustifiedVectText2D)  then
    result := TJustifiedVectText2D(TmpObject2D).DrawBox;
end;

function CAD_JVectText2DSetDrawBox(AID: longint; AValue: boolean): boolean;
var TmpObject2D: TObject2D;
begin
  result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TJustifiedVectText2D)  then
  begin
    TJustifiedVectText2D(TmpObject2D).DrawBox := AValue;
    result := true;
  end;
end;

function CAD_JVectText2DGetHeight(AID: longint): TRealType;
var TmpObject2D: TObject2D;
begin
  //result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TJustifiedVectText2D)  then
    result := TJustifiedVectText2D(TmpObject2D).Height;
end;

function CAD_JVectText2DSetHeight(AID: longint; AValue: TRealType): boolean;
var TmpObject2D: TObject2D;
begin
  result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TJustifiedVectText2D)  then
  begin
    TJustifiedVectText2D(TmpObject2D).Height := AValue;
    result := true;
  end;
end;

function CAD_JVectText2DGetHJustification(AID: longint): THJustification;
var TmpObject2D: TObject2D;
begin
  //result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TJustifiedVectText2D)  then
    result := TJustifiedVectText2D(TmpObject2D).HorizontalJust;
end;

function CAD_JVectText2DSetHJustification(AID: longint; AValue: THJustification): boolean;
var TmpObject2D: TObject2D;
begin
  result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TJustifiedVectText2D)  then
  begin
    TJustifiedVectText2D(TmpObject2D).HorizontalJust := AValue;
    result := true;
  end;
end;

function CAD_JVectText2DGetVJustification(AID: longint): TVJustification;
var TmpObject2D: TObject2D;
begin
  //result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TJustifiedVectText2D)  then
    result := TJustifiedVectText2D(TmpObject2D).VerticalJust;
end;

function CAD_JVectText2DSetVJustification(AID: longint; AValue: TVJustification): boolean;
var TmpObject2D: TObject2D;
begin
  result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TJustifiedVectText2D)  then
  begin
    TJustifiedVectText2D(TmpObject2D).VerticalJust := AValue;
    result := true;
  end;
end;

function CAD_JVectText2DGetInterline(AID: longint): TRealType;
var TmpObject2D: TObject2D;
begin
  //result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TJustifiedVectText2D)  then
    result := TJustifiedVectText2D(TmpObject2D).InterLine;
end;

function CAD_JVectText2DSetInterline(AID: longint; AValue: TRealType): boolean;
var TmpObject2D: TObject2D;
begin
  result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TJustifiedVectText2D)  then
  begin
    TJustifiedVectText2D(TmpObject2D).InterLine := AValue;
    result := true;
  end;
end;

function CAD_JVectText2DGetText(AID: longint): string;
var TmpObject2D: TObject2D;
begin
  //result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TJustifiedVectText2D)  then
    result := TJustifiedVectText2D(TmpObject2D).Text;
end;

function CAD_JVectText2DSetText(AID: longint; AValue: string): boolean;
var TmpObject2D: TObject2D;
begin
  result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TJustifiedVectText2D)  then
  begin
    TJustifiedVectText2D(TmpObject2D).Text := AValue;
    result := true;
  end;
end;

//Block2D///////////////////////////////////////////////////////////////////////
function CAD_Block2DGetOriginPoint(AID: longint): TPoint2D;
var TmpObject2D: TObject2D;
begin
  //result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TBlock2D)  then
    result := TBlock2D(TmpObject2D).OriginPoint;
end;

function CAD_Block2DSetOriginPoint(AID: longint; var APoint2D: TPoint2D): boolean;
var TmpObject2D: TObject2D;
begin
  result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TBlock2D)  then
  begin
    TBlock2D(TmpObject2D).OriginPoint := APoint2D;
    result := true;
  end;
end;

function CAD_Block2DGetSourceName(AID: longint): string;
var TmpObject2D: TObject2D;
begin
  //result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TBlock2D)  then
    result := TBlock2D(TmpObject2D).SourceName;
end;


////////////////////////////////////////////////////////////////////////////////
function CAD_GetBrushSource(AID: longint): TBrushSource;
var TmpObject2D: TObject2D;
begin
  result := bsByLayer;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TClosedCurve2D)  then
    result := TClosedCurve2D(TmpObject2D).BrushSource
  else if (TmpObject2D is TClosedPolyline2D)  then
    result := TClosedPolyline2D(TmpObject2D).BrushSource;
end;

function CAD_SetBrushSource(AID: longint; AValue: TBrushSource): boolean;
var TmpObject2D: TObject2D;
begin
  result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TClosedCurve2D) or (TmpObject2D is TClosedPolyline2D)then
  begin
    if (TmpObject2D is TClosedCurve2D) then
    begin
      if TClosedCurve2D(TmpObject2D).BrushSource <> AValue then
        TClosedCurve2D(TmpObject2D).BrushSource := AValue;
      exit;
    end;
    if (TmpObject2D is TClosedPolyline2D) then
    begin
      if TClosedPolyline2D(TmpObject2D).BrushSource <> AValue then
        TClosedPolyline2D(TmpObject2D).BrushSource := AValue;
      exit;
    end;
    result := true;
  end;
end;

function CAD_GetBrushColor(AID: longint): TColor;
var TmpObject2D: TObject2D;
begin
  result := clWhite;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TClosedCurve2D)  then
    result := TClosedCurve2D(TmpObject2D).BrushColor
  else if (TmpObject2D is TClosedPolyline2D)  then
    result := TClosedPolyline2D(TmpObject2D).BrushColor;
end;

function CAD_SetBrushColor(AID: longint; AValue: TColor): boolean;
var TmpObject2D: TObject2D;
begin
  result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TClosedCurve2D) or (TmpObject2D is TClosedPolyline2D)then
  begin
    if (TmpObject2D is TClosedCurve2D) then
    begin
      if TClosedCurve2D(TmpObject2D).BrushColor <> AValue then
        TClosedCurve2D(TmpObject2D).BrushColor := AValue;
      exit;
    end;
    if (TmpObject2D is TClosedPolyline2D) then
    begin
      if TClosedPolyline2D(TmpObject2D).BrushColor <> AValue then
        TClosedPolyline2D(TmpObject2D).BrushColor := AValue;
      exit;
    end;
    result := true;
  end;
end;

function CAD_GetBrushStyle(AID: longint): TBrushStyle;
var TmpObject2D: TObject2D;
begin
  result := bsClear;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TClosedCurve2D)  then
    result := TClosedCurve2D(TmpObject2D).BrushStyle
  else if (TmpObject2D is TClosedPolyline2D)  then
    result := TClosedPolyline2D(TmpObject2D).BrushStyle;
end;

function CAD_SetBrushStyle(AID: longint; AValue: TBrushStyle): boolean;
var TmpObject2D: TObject2D;
begin
  result := false;
  GetActiveDocument;
  TmpObject2D := CADCmp2D.GetObject(AID);
  if (TmpObject2D is TClosedCurve2D) or (TmpObject2D is TClosedPolyline2D)then
  begin
    if (TmpObject2D is TClosedCurve2D) then
    begin
      if TClosedCurve2D(TmpObject2D).BrushStyle <> AValue then
        TClosedCurve2D(TmpObject2D).BrushStyle := AValue;
      exit;
    end;
    if (TmpObject2D is TClosedPolyline2D) then
    begin
      if TClosedPolyline2D(TmpObject2D).BrushStyle <> AValue then
        TClosedPolyline2D(TmpObject2D).BrushStyle := AValue;
      exit;
    end;
    result := true;
  end;
end;

procedure GetActiveDocument;
begin
  CADDocument.GetDocument(CADPrg2D, CADCmp2D, CADViewport2D, ProgressBar, PropertyGrid);
end;

function CAD_Rect2D(var Left, Bottom, Right, Top: TRealType): TRect2D;
begin
  Result := Rect2D(Left, Bottom, Right, Top);
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

function CAD_ImportDXF(AFileName: string): boolean;
var DXF2DImport: TDXF2DImport;
begin
  result := false;
  GetActiveDocument;
  if CADCmp2D = nil then exit;
  try
    DXF2DImport := TDXF2DImport.Create(AFileName, CADCmp2D);
    DXF2DImport.SetTextFont(CADSysFindFontByIndex(0));
    DXF2DImport.ReadDXF;
    result := true;
  finally
    DXF2DImport.Free;
  end;
  if (CADCmp2D.Viewports[0] <> nil) then
    CADCmp2D.Viewports[0].ZoomToExtension;
end;

function CAD_ExportDXF(AFileName: string): boolean;
var DXF2DExport: TDXF2DExport;
begin
  result := false;
  GetActiveDocument;
  if CADCmp2D = nil then exit;
  DXF2DExport := DXF2DExport.Create(AFileName, CADCmp2D);
  try
    DXF2DExport.WriteDXF;
    result := true;
  finally
    DXF2DExport.Free;
  end;
end;

function CAD_ImportESSI(AFileName: string): boolean;
var ImportEssi: TImportEssi;
begin
  result := false;
  GetActiveDocument;
  if CADCmp2D = nil then exit;
  try
    ImportEssi := TImportEssi.create(CADCmp2D, CADViewport2D, ProgressBar);
    ImportEssi.LoadFromFile(AFileName);
    ImportEssi.ImportMoves := false;
    ImportEssi.Import;
    result := true;
  finally
    ImportEssi.Free;
  end;
  if (CADCmp2D.Viewports[0] <> nil) then
    CADCmp2D.Viewports[0].ZoomToExtension;
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
  result := ktNone;
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

function  CAD_CombinedFigures(AIndex1, AIndex2: integer): boolean;
var x1, y1, x2, y2: TRealType; hRes: boolean;  Point1, Point2: TPoint2D;
begin
  result := false;
  GetActiveDocument;
  if CADCmp2D = nil then exit;

  Point1 := Point2D(CAD_GetFigureSP_X(AIndex1), CAD_GetFigureSP_Y(AIndex1));
  Point2 := Point2D(CAD_GetFigureEP_X(AIndex2), CAD_GetFigureEP_Y(AIndex2));
  hRes := ( (abs(Point1.x - Point2.x) <= MIN_FIGURE_DISTANCE) and
              (abs(Point1.y - Point2.y) <= MIN_FIGURE_DISTANCE)
            );

  result := hRes;
end;


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

