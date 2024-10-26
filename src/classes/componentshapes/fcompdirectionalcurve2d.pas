unit fcompdirectionalcurve2d;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics,
  CADSys4,
  CS4BaseTypes,
  CS4Shapes,
  fcomponentCurve2D;

type

TCADSysDirectionalCurve2D = class(TCADSysCurve2D) //class(tpersistent)
  private
    fDirectionalCurve2D: TDirectionalCurve2D;
    function   GetDirectionalCurve2D: TDirectionalCurve2D;
    procedure  SetDirectionalCurve2D(ADirectionalCurve2D: TDirectionalCurve2D);
    //function   GetObjectLength: TRealType;
    //procedure  SetObjectLength(AValue: TRealType);
  public
    constructor Create;
    property DirectionalCurve2D: TDirectionalCurve2D read GetDirectionalCurve2D write SetDirectionalCurve2D;
  published
    //property ObjectLength read GetObjectLength write SetObjectLength;
    //property ObjectPosition2D;
end;


implementation

constructor TCADSysDirectionalCurve2D.create;
begin
  inherited create;
  self.fDirectionalCurve2D := fDirectionalCurve2D;
end;


function TCADSysDirectionalCurve2D.GetDirectionalCurve2D: TDirectionalCurve2D;
begin
  //result := fLine2D;
  result := TDirectionalCurve2D(fDirectionalCurve2D);
end;

procedure TCADSysDirectionalCurve2D.SetDirectionalCurve2D(ADirectionalCurve2D: TDirectionalCurve2D);
begin
  fDirectionalCurve2D := ADirectionalCurve2D;
  self.fDirectionalCurve2D := fDirectionalCurve2D;
end;


end.

