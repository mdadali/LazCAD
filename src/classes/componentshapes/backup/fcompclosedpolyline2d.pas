unit fcompclosedpolyline2d;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics,
  CADSys4,
  CS4BaseTypes,
  CS4Shapes,
  fComponentPLine;

type

TCADSysClosedPolyline2D = class(TCADSysPLine2D) //class(tpersistent)
  private
    fClosedPolyline2D: TClosedPolyline2D;
    function   GetClosedPolyline2D: TClosedPolyline2D;
    procedure  SetClosedPolyline2D(AClosedPolyline2D: TClosedPolyline2D);

    function   GetArcDirection: TArcDirection;
    procedure  SetArcDirection(ADirection: TArcDirection);

    function  GetBrush: TBrush;
    procedure SetBrush(ABrush: TBrush);
    //function   GetObjectLength: TRealType;
    //procedure  SetObjectLength(AValue: TRealType);
  public
    constructor Create;
    property ClosedPolyline2D: TClosedPolyline2D read GetClosedPolyline2D write SetClosedPolyline2D;
  published

    //property ObjectLength read GetObjectLength write SetObjectLength;
    //property ObjectPosition2D;
end;


implementation

constructor TCADSysClosedPolyline2D.create;
begin
  inherited create;
  self.fClosedPolyline2D := fClosedPolyline2D;
end;


function TCADSysClosedPolyline2D.GetClosedPolyline2D: TClosedPolyline2D;
begin
  result := TClosedPolyline2D(fClosedPolyline2D);
end;

procedure TCADSysClosedPolyline2D.SetClosedPolyline2D(AClosedPolyline2D: TClosedPolyline2D);
begin
  fClosedPolyline2D := AClosedPolyline2D;
end;

function   TCADSysClosedPolyline2D.GetArcDirection: TArcDirection;
begin
  result := fClosedPolyline2D.Direction;
end;

procedure  TCADSysClosedPolyline2D.SetArcDirection(ADirection: TArcDirection);
begin
  fClosedPolyline2D.Direction := ADirection;
end;

function  TCADSysClosedPolyline2D.GetBrush: TBrush;
begin
  result := fClosedPolyline2D.Brush;
end;

procedure TCADSysClosedPolyline2D.SetBrush(ABrush: TBrush);
begin
  fClosedPolyline2D.Brush := ABrush;
end;

end.

