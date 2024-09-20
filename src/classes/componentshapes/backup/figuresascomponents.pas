unit FiguresAsComponents;

{$IFNDEF VER150}
{$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils,
  CADSys4,
  CS4BaseTypes,
  CS4Shapes,

  fComponentLine,
  fComponentArc,
  fcomponentcirculararc,
  fcomponentcircle,
  fComponentRect,
  fcomponentPLine,
  fcomponentEllipse,
  fComponentFrame,
  fComponentBSpline,
  fComponentPolygon,
  fComponentContainer;

  var
    CADSysLine2D          : TCADSysLine2D;
    CADSysEllipticalArc2D : TCADSysEllipticalArc2D;
    CADSysCircularArc2D   : TCADSysCircularArc2D;
    CADSysCircle2D        : TCADSysCircle2D;
    CADSysRectangle2D     : TCADSysRectangle2D;
    CADSysPLine2D         : TCADSysPLine2D;
    CADSysEllipse2D       : TCADSysEllipse2D;
    CADSysFrame2D         : TCADSysFrame2D;
    CADSysBSpline2D       : TCADSysBSpline2D;
    CADSysPolygon2D       : TCADSysPolygon2D;
    CADSysContainer2D     : TCADSysContainer2D;

    GlobalObject2D: TObject2D;

implementation

initialization
  CADSysLine2D          := TCADSysLine2D.Create;
  CADSysEllipticalArc2D := TCADSysEllipticalArc2D.Create;
  CADSysCircularArc2D   := TCADSysCircularArc2D.Create;
  CADSysCircle2D        := TCADSysCircle2D.Create;
  CADSysRectangle2D     := TCADSysRectangle2D.Create;
  CADSysPLine2D         := TCADSysPLine2D.Create;
  CADSysEllipse2D       := TCADSysEllipse2D.Create;
  CADSysFrame2D         := TCADSysFrame2D.Create;
  CADSysBSpline2D       := TCADSysBSpline2D.Create;
  CADSysPolygon2D       := TCADSysPolygon2D.Create;
  CADSysContainer2D     := TCADSysContainer2D.Create;

  GlobalObject2D := nil;

finalization
  CADSysLine2D.Free;
  CADSysEllipticalArc2D.Free;
  CADSysCircularArc2D.Free;
  CADSysCircle2D.Free;
  CADSysRectangle2D.Free;
  CADSysPLine2D.Free;
  CADSysEllipse2D.Free;
  CADSysFrame2D.Free;
  CADSysBSpline2D.Free;
  CADSysPolygon2D.Free;
  GlobalObject2D.Free;
end.



