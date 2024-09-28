unit fcomponentcircle;

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

TCADSysCircle2D = class(TCADSysBaseComponent2D) //class(tpersistent)

  private
    fCircle2D: TCircle2D;
    function   GetCircle2D: TCircle2D;
    procedure  SetCircle2D(ACircle2D: TCircle2D);

    function   GetRadius: TRealType;
    procedure  SetRadius(ARadius: TRealType);
    function   GetDirection: TArcDirection;
    procedure  SetDirection(D: TArcDirection);
    function   GetCurvePrecision: word;
    procedure  SetCurvePrecision(APrecision: word);
    function   GetStartAngle: TRealType;
    procedure  SetStartAngle(AValue: TRealType);

  public
    constructor Create;
    property Circle2D: TCircle2D read GetCircle2D write SetCircle2D;

  published
    property Radius:         TRealType       read GetRadius           write SetRadius;
    property Direction:      TArcDirection   read GetDirection        write SetDirection;
    property StartAngle:     TRealType       read GetStartAngle       write SetStartAngle;
    property EdgeCount:      Word            read GetCurvePrecision   write SetCurvePrecision;


    //property Visible;
end;


implementation

function   TCADSysCircle2D.GetRadius: TRealType;
begin
  result := fCircle2D.Radius;
end;

procedure  TCADSysCircle2D.SetRadius(ARadius: TRealType);
begin
  fCircle2D.Radius := ARadius;
end;

function   TCADSysCircle2D.GetDirection: TArcDirection;
begin
  result := fCircle2D.Direction;
end;

procedure  TCADSysCircle2D.SetDirection(D: TArcDirection);
begin
  if (D <> fCircle2D.Direction) then
  begin
    fCircle2D.Direction := D;
    //fCircle2D.OwnerCAD.RefreshViewports;
    fCircle2D.UpdateExtension(nil);
  end;
end;

function   TCADSysCircle2D.GetCurvePrecision: word;
begin
  result := fCircle2D.CurvePrecision;
end;

procedure  TCADSysCircle2D.SetCurvePrecision(APrecision: word);
begin
  fCircle2D.CurvePrecision := APrecision;
  fCircle2D.UpdateExtension(nil);
end;

function TCADSysCircle2D.GetStartAngle:  TRealType;
begin
  result := RadToDeg(fCircle2D.StartAngle);
end;

procedure TCADSysCircle2D.SetStartAngle(AValue: TRealType);
begin
  fCircle2D.StartAngle := DegToRad(AValue);
end;

constructor TCADSysCircle2D.create;
begin
  inherited create;
  self.fPrimitive2D := fCircle2D;
end;


function TCADSysCircle2D.GetCircle2D: TCircle2D;
begin
  result := fCircle2D;
  //result := TCircle2D_CPR(fPrimitive2D);
end;

procedure TCADSysCircle2D.SetCircle2D(ACircle2D: TCircle2D);
begin
  fCircle2D := ACircle2D;
  self.fPrimitive2D := fCircle2D;
  //SetLayerIDX(self.LayerIndex);
end;

initialization
  //CADSysInitClassRegister;

  CADSysRegisterClass(260, TCircle2D);
finalization

end.

