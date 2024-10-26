unit fcomponentcurve2d;

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
  public
    constructor Create;
    property Curve2D: TCurve2D read fCurve2D write fCurve2D;
  published
end;


implementation

constructor TCADSysCurve2D.create;
begin
  inherited create;
end;


end.

