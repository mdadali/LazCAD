unit fcomponentsector2d;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  CADSys4,
  CS4BaseTypes,
  CS4Shapes,
  //fcomponentsegment2d;
  fcompclosedcurve2d;

type

TCADSysSector2D = class(TCADSysClosedCurve2D)
  private
    function  GetGraphicObject: TGraphicObject; override;
    procedure SetGraphicObject(AGraphicObject: TGraphicObject); override;
  public
    fSector2D:  TSector2D;
    constructor Create;
    property    GraphicObject;
  published
end;

implementation

constructor TCADSysSector2D.create;
begin
  inherited create;
end;

function TCADSysSector2D.GetGraphicObject: TGraphicObject;
begin
  result := fSector2D;
end;

procedure TCADSysSector2D.SetGraphicObject(AGraphicObject: TGraphicObject);
begin
  fSector2D  := TSector2D(AGraphicObject);
  fSimplePrimitive2D := TSimplePrimitive2D(AGraphicObject);
  fPrimitive2D := TPrimitive2D(AGraphicObject);
  fObject2D := TObject2D(AGraphicObject);
  fGraphicObject := AGraphicObject;
end;


end.

