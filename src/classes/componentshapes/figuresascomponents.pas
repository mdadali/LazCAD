unit FiguresAsComponents;

{$IFNDEF VER150}
{$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils,
  CADSys4,
  CS4BaseTypes,
  CS4Shapes;

  {fComponentLine,
  fComponentArc,
  fcomponentcirculararc,
  fcomponentcircle,
  fComponentRect,
  fComponentFrame,
  fComponentBSpline,
  fComponentPolygon,
  fComponentContainer,

  fcomponentSimplePrim2D,
  fcompdirectionalcurve2d,
  fcompclosedcurve2d,
  fcompclosedpolyline2d,
  fcomponentsegment2d,
  fcomponentsector2d;}

  var
    {CADSysLine2D          : TCADSysLine2D;
    CADSysEllipticalArc2D : TCADSysEllipticalArc2D;
    CADSysCircularArc2D   : TCADSysCircularArc2D;
    CADSysCircle2D        : TCADSysCircle2D;
    CADSysRectangle2D     : TCADSysRectangle2D;
    CADSysFrame2D         : TCADSysFrame2D;
    CADSysBSpline2D       : TCADSysBSpline2D;
    CADSysPolygon2D       : TCADSysPolygon2D;
    CADSysContainer2D     : TCADSysContainer2D;

    CADSysSegment2D       : TCADSysSegment2D;
    CADSysSectort2D       : TCADSysSector2D;

    CADSysSimplePrimitive2D:  TCADSysSimplePrimitive2D;
    CADSysDirectionalCurve2D: TCADSysDirectionalCurve2D;
    CADSysClosedCurve2D:      TCADSysClosedCurve2D;
    CADSysClosedPolyline2D:   TCADSysClosedPolyline2D;
    }

    GlobalObject2D: TObject2D;

implementation

initialization
  {CADSysLine2D          := TCADSysLine2D.Create;
  CADSysEllipticalArc2D := TCADSysEllipticalArc2D.Create;
  CADSysCircularArc2D   := TCADSysCircularArc2D.Create;
  CADSysCircle2D        := TCADSysCircle2D.Create;
  CADSysRectangle2D     := TCADSysRectangle2D.Create;
  CADSysFrame2D         := TCADSysFrame2D.Create;
  CADSysBSpline2D       := TCADSysBSpline2D.Create;
  CADSysPolygon2D       := TCADSysPolygon2D.Create;
  CADSysContainer2D     := TCADSysContainer2D.Create;

  CADSysSimplePrimitive2D  := TCADSysSimplePrimitive2D.Create;
  CADSysDirectionalCurve2D := TCADSysDirectionalCurve2D.Create;
  CADSysClosedCurve2D      := TCADSysClosedCurve2D.Create;
  CADSysClosedPolyline2D   := TCADSysClosedPolyline2D.Create;

  CADSysSegment2D          := TCADSysSegment2D.Create;
  CADSysSectort2D          := TCADSysSector2D.Create;
  }

  GlobalObject2D := nil;

finalization
  {CADSysLine2D.Free;
  CADSysEllipticalArc2D.Free;
  CADSysCircularArc2D.Free;
  CADSysCircle2D.Free;
  CADSysRectangle2D.Free;
  CADSysFrame2D.Free;
  CADSysBSpline2D.Free;
  CADSysPolygon2D.Free;
  CADSysContainer2D.Free;

  CADSysSimplePrimitive2D.Free;
  CADSysDirectionalCurve2D.Free;
  CADSysClosedCurve2D.Free;
  CADSysClosedPolyline2D.Free;

  CADSysSegment2D.Free;
  CADSysSectort2D.Free;
  }

  GlobalObject2D.Free;
end.



