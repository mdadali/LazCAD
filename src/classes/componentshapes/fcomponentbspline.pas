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
  fBaseComponent;

type

TCADSysBSpline2D = class(TCADSysBaseComponent2D) //class(tpersistent)

  private
    fBSpline2D: TBSpline2D;
    function   GetBSpline2D: TBSpline2D;
    procedure  SetBSpline2D(ABSpline2D: TBSpline2D);
  public
    constructor Create;
    property BSpline2D: TBSpline2D read GetBSpline2D write SetBSpline2D;
  published
    //property BrushColor;
    //property BrushStyle;
    //property Filled;
end;


implementation

constructor TCADSysBSpline2D.create;
begin
  inherited create;
  self.fPrimitive2D := fBSpline2D;
end;


function TCADSysBSpline2D.GetBSpline2D: TBSpline2D;
begin
  result := TBSpline2D(fPrimitive2D);
end;

procedure TCADSysBSpline2D.SetBSpline2D(ABSpline2D: TBSpline2D);
begin
  fBSpline2D := ABSpline2D;
  self.fPrimitive2D := fBSpline2D;
end;


end.

