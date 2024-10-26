unit fComponentPolygon;

{$IFNDEF VER150}
{$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, dialogs,  StdCtrls,  Graphics,
  CADSys4,
  CS4BaseTypes,
  CS4Shapes,
  fcompclosedpolyline2d;

type

TCADSysPolygon2D = class(TCADSysClosedPolyline2D) //class(tpersistent)
  private
    function    GetArcDirection: TArcDirection;
    procedure   SetArcDirection(ADirection: TArcDirection);

    function  GetGraphicObject: TGraphicObject; override;
    procedure SetGraphicObject(AGraphicObject: TGraphicObject); override;
  public
    fPolygon2D: TPolygon2D;
    constructor Create;
    property    GraphicObject;
  published
    property Direction: TArcDirection  read GetArcDirection write SetArcDirection;
  end;

implementation


function TCADSysPolygon2D.GetGraphicObject: TGraphicObject;
begin
  result := fPolygon2D;
end;

procedure TCADSysPolygon2D.SetGraphicObject(AGraphicObject: TGraphicObject);
begin
  fPolygon2D := TPolygon2D(AGraphicObject);
  fSimplePrimitive2D := TSimplePrimitive2D(AGraphicObject);
  fPrimitive2D := TPrimitive2D(AGraphicObject);
  fObject2D := TObject2D(AGraphicObject);
  fGraphicObject := AGraphicObject;
end;

constructor TCADSysPolygon2D.create;
begin
  inherited create;
end;

function   TCADSysPolygon2D.GetArcDirection: TArcDirection;
begin
  result := fPolygon2D.Direction;
end;

procedure  TCADSysPolygon2D.SetArcDirection(ADirection: TArcDirection);
begin
  fPolygon2D.Direction := ADirection;
end;

end.

