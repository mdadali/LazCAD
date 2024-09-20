unit fComponentPolygon;

{$IFNDEF VER150}
{$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, dialogs,  StdCtrls,
  CADSys4,
  CS4BaseTypes,
  CS4Shapes,
  fBaseComponent;

type

TCADSysPolygon2D = class(TCADSysBaseComponent2D) //class(tpersistent)
  private
    fabc: integer;
    fPolygon2D: TPolygon2D;
    function    GetPolygon2D: TPolygon2D;
    procedure   SetPolygon2D(APolygon2D: TPolygon2D);
    function    GetDirection: TArcDirection;
    procedure   SetDirection(AValue: TArcDirection);
  public
    constructor Create;
    property Polygon2D: TPolygon2D read GetPolygon2D write SetPolygon2D;
  published
    property Direction; // read  GetDirection write  SetDirection;

    //property BrushColor;
    //property BrushStyle;
    //property Filled;
end;

implementation

constructor TCADSysPolygon2D.create;
begin
  inherited create;
  self.fPrimitive2D := fPolygon2D;
end;

function TCADSysPolygon2D.GetPolygon2D: TPolygon2D;
begin
  result := fPolygon2D;
end;

procedure TCADSysPolygon2D.SetPolygon2D(APolygon2D: TPolygon2D);
begin
  fPolygon2D := APolygon2D;
  self.fPrimitive2D := fPolygon2D;
end;

function  TCADSysPolygon2D.GetDirection: TArcDirection;
begin
  result :=  self.fPrimitive2D.Direction;
end;

procedure TCADSysPolygon2D.SetDirection(AValue: TArcDirection);
begin
  fPrimitive2D.Direction := AValue;
end;

end.

