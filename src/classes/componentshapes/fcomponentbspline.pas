unit fComponentBSpline;

{$IFNDEF VER150}
{$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils,
  CADSys4,
  CS4BaseTypes,
  CS4Shapes,
  fcomponentcurve2d;

type

TCADSysBSpline2D = class(TCADSysCurve2D) //class(tpersistent)
  private
    function  GetGraphicObject: TGraphicObject; override;
    procedure SetGraphicObject(AGraphicObject: TGraphicObject); override;
  public
    fBSpline2D: TBSpline2D;
    constructor Create;
    property    GraphicObject;
  published
end;

implementation

constructor TCADSysBSpline2D.create;
begin
  inherited create;
end;

function TCADSysBSpline2D.GetGraphicObject: TGraphicObject;
begin
  result := fBSpline2D;
end;

procedure TCADSysBSpline2D.SetGraphicObject(AGraphicObject: TGraphicObject);
begin
  fBSpline2D := TBSpline2D(AGraphicObject);
  fSimplePrimitive2D := TSimplePrimitive2D(AGraphicObject);
  fPrimitive2D := TPrimitive2D(AGraphicObject);
  fObject2D := TObject2D(AGraphicObject);
  fGraphicObject := AGraphicObject;
end;

end.

