unit fComponentEllipse;

{$IFNDEF VER150}
{$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, dialogs,
  CADSys4,
  CS4BaseTypes,
  CS4Shapes,
  fBaseComponent;

type

TCADSysEllipse2D = class(TCADSysBaseComponent2D) //class(tpersistent)
  private
    fEllipse2D: TEllipse2D;
    function   GetEllipse2D: TEllipse2D;
    procedure  SetEllipse2D(AEllipse2D: TEllipse2D);
    function   GetCurvePrecision: Word;
    procedure  SetCurvePrecision(APrecision: Word);
  public
    constructor Create;
    property Ellipse2D: TEllipse2D read GetEllipse2D write SetEllipse2D;
  published
    property Direction;
    property CurvePrecision: Word  read GetCurvePrecision write SetCurvePrecision;
    //property BrushColor;
    //property BrushStyle;
    //property Filled;
end;

implementation

constructor TCADSysEllipse2D.create;
begin
  inherited create;
  self.fPrimitive2D := fEllipse2D;
end;

function TCADSysEllipse2D.GetEllipse2D: TEllipse2D;
begin
  result := fEllipse2D;
  //result := TCircle2D_CPR(fPrimitive2D);
end;

procedure TCADSysEllipse2D.SetEllipse2D(AEllipse2D: TEllipse2D);
begin
  fEllipse2D := AEllipse2D;
  self.fPrimitive2D := fEllipse2D;
  //SetLayerIDX(self.LayerIndex);
end;

function   TCADSysEllipse2D.GetCurvePrecision: Word;
begin
  result := fEllipse2D.CurvePrecision;
end;

procedure  TCADSysEllipse2D.SetCurvePrecision(APrecision: Word);
begin
  fEllipse2D.CurvePrecision := APrecision;
end;


end.

