unit fcompcurve2d;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics,
  CADSys4,
  CS4BaseTypes,
  CS4Shapes,
  fcomponentoutline2d;

type

TCADSysCurve2D = class(TCADSysOutline2D) //class(tpersistent)
  private
   fCurve2D: TCurve2D;
    function   GetCurve2D: TCurve2D;
    procedure  SetCurve2D(ACurve2DD: TCurve2D);
  public
    constructor Create;
    property Curve2D: TCurve2D read fCurve2D write fCurve2D;
  published
end;


implementation

constructor TCADSysCurve2D.create;
begin
  inherited create;
  self.Object2D := fCurve2D;
end;


function TCADSysCurve2D.GetCurve2D: TCurve2D;
begin
  result := TCurve2D(fCurve2D);
end;

procedure TCADSysCurve2D.SetDirectionalCurve2D(ACurve2D: TCurve2D);
begin
  fCurve2D := ACurve2D;
  self.fCurve2D := fCurve2D;
end;


end.

