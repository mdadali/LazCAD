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
  fcompclosedcurve2d;

type

TCADSysCircle2D = class(TCADSysClosedCurve2D)
  private
    function   GetRadius: TRealType;
    procedure  SetRadius(ARadius: TRealType);
    function   GetDirection: TArcDirection;
    procedure  SetDirection(D: TArcDirection);
    function   GetCurvePrecision: word;
    procedure  SetCurvePrecision(APrecision: word);
    function   GetStartAngle: TRealType;
    procedure  SetStartAngle(AValue: TRealType);

    function  GetGraphicObject: TGraphicObject; override;
    procedure SetGraphicObject(AGraphicObject: TGraphicObject); override;
  public
    fCircle2D: TCircle2D;
    constructor Create;
    property GraphicObject;
  published
    property Radius:         TRealType       read GetRadius           write SetRadius;
    property Direction:      TArcDirection   read GetDirection        write SetDirection;
    property StartAngle:     TRealType       read GetStartAngle       write SetStartAngle;
    property EdgeCount:      Word            read GetCurvePrecision   write SetCurvePrecision;
end;


implementation

function  TCADSysCircle2D.GetGraphicObject: TGraphicObject;
begin
  result := fCircle2D;
end;

procedure TCADSysCircle2D.SetGraphicObject(AGraphicObject: TGraphicObject);
begin
  fCircle2D :=  TCircle2D(AGraphicObject);
  fSimplePrimitive2D := TSimplePrimitive2D(AGraphicObject);
  fPrimitive2D := TPrimitive2D(AGraphicObject);
  fObject2D := TObject2D(AGraphicObject);
  fGraphicObject := AGraphicObject;
end;

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
    fCircle2D.UpdateExtension(nil);
  end;
end;

function   TCADSysCircle2D.GetCurvePrecision: word;
begin
  result := fCircle2D.CurvePrecision;
end;

procedure  TCADSysCircle2D.SetCurvePrecision(APrecision: word);
begin
  if APrecision = fCircle2D.CurvePrecision then exit;
  if (APrecision < 3) then
  begin
    MessageDlg('Warning', 'The minimum allowed number of edges is 3.', mtWarning, [mbOK], 0);
    APrecision := 3;
  end;
  fCircle2D.CurvePrecision := APrecision;
  fCircle2D.UpdateExtension(self);
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
end;


end.

