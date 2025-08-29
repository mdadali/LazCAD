unit fcompclosedcurve2d;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics,
  CADSys4,
  CS4BaseTypes,
  CS4Shapes,
  fcompdirectionalcurve2d;

type

TCADSysClosedCurve2D = class(TCADSysDirectionalCurve2D) //class(tpersistent)
  private
    function  GetGraphicObject: TGraphicObject; override;
    procedure SetGraphicObject(AGraphicObject: TGraphicObject); override;

    function  GetBrush: TBrush;
    procedure SetBrush(ABrush: TBrush);
    function  GetDirection: TArcDirection;
    procedure SetArcDirection(ADirection: TArcDirection);
  protected
  public
    fBrush: TBrush;
    fClosedCurve2D: TClosedCurve2D;
    constructor Create;
    property    GraphicObject;
  published
    property Brush:     TBrush        read  GetBrush     write  SetBrush;
    property Direction: TArcDirection read  GetDirection write  SetArcDirection;
end;


implementation

constructor TCADSysClosedCurve2D.create;
begin
  inherited create;
end;

function TCADSysClosedCurve2D.GetGraphicObject: TGraphicObject;
begin
  result := fClosedCurve2D;
end;

procedure TCADSysClosedCurve2D.SetGraphicObject(AGraphicObject: TGraphicObject);
begin
  fClosedCurve2D  := TClosedCurve2D(AGraphicObject);
  fSimplePrimitive2D := TSimplePrimitive2D(AGraphicObject);
  fPrimitive2D := TPrimitive2D(AGraphicObject);
  fObject2D := TObject2D(AGraphicObject);
  fGraphicObject := AGraphicObject;
  self.Curve2D :=  TCurve2D(AGraphicObject);
  self.fBrush := TClosedCurve2D(AGraphicObject).Brush;
end;

function  TCADSysClosedCurve2D.GetBrush: TBrush;
begin
  result := fBrush;
end;

procedure TCADSysClosedCurve2D.SetBrush(ABrush: TBrush);
begin
  fBrush := ABrush;
end;

function  TCADSysClosedCurve2D.GetDirection: TArcDirection;
begin
  result := fClosedCurve2D.Direction;
end;

procedure TCADSysClosedCurve2D.SetArcDirection(ADirection: TArcDirection);
begin
  fClosedCurve2D.Direction := ADirection;
end;

end.

