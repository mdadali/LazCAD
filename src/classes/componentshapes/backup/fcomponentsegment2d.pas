unit fcomponentsegment2d;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  CADSys4,
  CS4BaseTypes,
  CS4Shapes,
  fcompclosedcurve2d;

type

TCADSysSegment2D = class(TCADSysClosedCurve2D)
  private
    function  GetGraphicObject: TGraphicObject; override;
    procedure SetGraphicObject(AGraphicObject: TGraphicObject); override;
  public
    fSegment2D: TSegment2D;
    constructor Create;
    property    GraphicObject;
  published
end;

implementation

constructor TCADSysSegment2D.create;
begin
  inherited create;
end;

function TCADSysSegment2D.GetGraphicObject: TGraphicObject;
begin
  result := fSegment2D;
end;

procedure TCADSysSegment2D.SetGraphicObject(AGraphicObject: TGraphicObject);
begin
  fSegment2D := TSegment2D(AGraphicObject);
  fSimplePrimitive2D := TSimplePrimitive2D(AGraphicObject);
  fPrimitive2D := TPrimitive2D(AGraphicObject);
  fObject2D := TObject2D(AGraphicObject);
  fGraphicObject := AGraphicObject;
end;

end.

