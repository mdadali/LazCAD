unit fcomponentcirculararc;

{$IFNDEF VER150}
{$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, dialogs,
  CADSys4,
  CS4BaseTypes,
  CS4Shapes,
  fcompdirectionalcurve2d;

type

  TCADSysCircularArc2D = class(TCADSysDirectionalCurve2D) //class(tpersistent)
  private
    fCircularArc2D: TCircularArc2D;
    {function   GetStartPointX: TRealType;
    function   GetStartPointY: TRealType;
    function   GetEndPointX: TRealType;
    function   GetEndPointY: TRealType;

    procedure  SetStartPointX(AValue: TRealType);
    procedure  SetStartPointY(AValue: TRealType);
    procedure  SetEndPointX(AValue: TRealType);
    procedure  SetEndPointY(AValue: TRealType);}

    function   GetRadius: TRealType;
    procedure  SetRadius(ARadius: TRealType);
    function   GetDirection: TArcDirection;
    procedure  SetDirection(D: TArcDirection);
    function   GetStartAngle: TRealType;
    procedure  SetStartAngle(AValue:TRealType);

    function   GetEndAngle: TRealType;
    procedure  SetEndAngle(AValue:TRealType);

    function   GetCurvePrecision: word;
    procedure  SetCurvePrecision(APrecision: word);

    function  GetGraphicObject: TGraphicObject; override;
    procedure SetGraphicObject(AGraphicObject: TGraphicObject); override;
  public
    constructor Create;
    property GraphicObject;

  published
    property Radius:      TRealType      read GetRadius         write SetRadius;
    property Direction:   TArcDirection  read GetDirection      write SetDirection;
    property StartAngle:  TRealType      read GetStartAngle     write SetStartAngle;
    property EndAngle:    TRealType      read GetEndAngle       write SetEndAngle;

    {property StartPointX: TRealType   read GetStartPointX    write SetStartPointX;
    property StartPointY: TRealType   read GetStartPointY    write SetStartPointY;
    property EndPointX:   TRealType   read GetEndPointX      write SetEndPointX;
    property EndPointY:   TRealType   read GetEndPointY      write SetEndPointY; }
    property CurvePrecision: word  read  GetCurvePrecision write SetCurvePrecision;

end;


implementation

function  TCADSysCircularArc2D.GetGraphicObject: TGraphicObject;
begin
  result :=  fCircularArc2D;
end;

procedure TCADSysCircularArc2D.SetGraphicObject(AGraphicObject: TGraphicObject);
begin
  fCircularArc2D := TCircularArc2D(AGraphicObject);
  fSimplePrimitive2D := TSimplePrimitive2D(AGraphicObject);
  fPrimitive2D := TPrimitive2D(AGraphicObject);
  fObject2D := TObject2D(AGraphicObject);
  fGraphicObject := AGraphicObject;
end;


function   TCADSysCircularArc2D.GetRadius: TRealType;
begin
  result := fCircularArc2D.Radius;
end;

procedure  TCADSysCircularArc2D.SetRadius(ARadius: TRealType);
begin
  fCircularArc2D.Radius := ARadius;
  fCircularArc2D.UpdateExtension(nil);
end;

function   TCADSysCircularArc2D.GetDirection: TArcDirection;
begin
  result := fCircularArc2D.Direction;
end;

procedure  TCADSysCircularArc2D.SetDirection(D: TArcDirection);
begin
  if (D <> fCircularArc2D.Direction) then
  begin
    fCircularArc2D.Direction := D;
    fCircularArc2D.UpdateExtension(nil);
    //fCIArc2D_CSE.OwnerCAD.RefreshViewports;
  end;
end;

function   TCADSysCircularArc2D.GetCurvePrecision: word;
begin
  result := fCircularArc2D.CurvePrecision;
end;

procedure  TCADSysCircularArc2D.SetCurvePrecision(APrecision: word);
begin
  fCircularArc2D.CurvePrecision := APrecision;
  fCircularArc2D.UpdateExtension(self);
end;

constructor TCADSysCircularArc2D.create;
begin
  inherited create;
  self.fPrimitive2D := fCircularArc2D;
end;

function   TCADSysCircularArc2D.GetStartAngle: TrealType;
begin
  result := RadToDeg(fCircularArc2D.StartAngle);
end;

procedure  TCADSysCircularArc2D.SetStartAngle(AValue: TRealType);
var hAngle: TRealType;
begin
  hAngle :=  DegToRad(AValue);
  //if  hAngle = 2*pi then hAngle := 0;
  fCircularArc2D.StartAngle := hAngle;
end;

function   TCADSysCircularArc2D.GetEndAngle: TRealType;
begin
  result := RadToDeg(fCircularArc2D.EndAngle);
end;

procedure  TCADSysCircularArc2D.SetEndAngle(AValue:TRealType);
begin
  fCircularArc2D.EndAngle :=  DegToRad(AValue);
end;


{function TCADSysCiArc2D_CSE.GetStartPointX: TRealType;
begin
  result := TPrimitive2D(fCIArc2D_CSE).Points[1].X;
end;

function TCADSysCiArc2D_CSE.GetStartPointY: TRealType;
begin
  result := TPrimitive2D(fCIArc2D_CSE).Points[1].Y;
end;

function TCADSysCiArc2D_CSE.GetEndPointX: TRealType;
begin
  result := TPrimitive2D(fCIArc2D_CSE).Points[2].X;
end;

function TCADSysCiArc2D_CSE.GetEndPointY: TRealType;
begin
  result := TPrimitive2D(fCIArc2D_CSE).Points[2].Y;
end;

procedure  TCADSysCiArc2D_CSE.SetStartPointX(AValue: TRealType);
var hPoint2D: TPoint2D;
begin
  hPoint2D.X := AValue;
  hPoint2D.Y := fCIArc2D_CSE.Points.Points[1].Y;
  hPoint2D.W := fCIArc2D_CSE.Points.Points[1].W;
  fCIArc2D_CSE.Points.Delete(1);
  fCIArc2D_CSE.Points.Insert(1, hPoint2D);
end;

procedure  TCADSysCiArc2D_CSE.SetStartPointY(AValue: TRealType);
var hPoint2D: TPoint2D;
begin
  hPoint2D.Y := AValue;
  hPoint2D.W := fCIArc2D_CSE.Points.Points[1].W;
  hPoint2D.X := fCIArc2D_CSE.Points.Points[1].X;
  fCIArc2D_CSE.Points.Delete(1);
  fCIArc2D_CSE.Points.Insert(1, hPoint2D);

end;

procedure  TCADSysCiArc2D_CSE.SetEndPointX(AValue: TRealType);
var hPoint2D: TPoint2D;
begin
  hPoint2D.X := AValue;
  hPoint2D.Y := fCIArc2D_CSE.Points.Points[2].Y;
  hPoint2D.W := fCIArc2D_CSE.Points.Points[2].W;
  fCIArc2D_CSE.Points.Delete(2);
  fCIArc2D_CSE.Points.Insert(2, hPoint2D);
end;

procedure  TCADSysCiArc2D_CSE.SetEndPointY(AValue: TRealType);
var hPoint2D: TPoint2D;
begin
  hPoint2D.Y := AValue;
  hPoint2D.W := fCIArc2D_CSE.Points.Points[2].W;
  hPoint2D.X := fCIArc2D_CSE.Points.Points[2].X;
  fCIArc2D_CSE.Points.Delete(2);
  fCIArc2D_CSE.Points.Insert(2, hPoint2D);
end; }

initialization
  //CADSysInitClassRegister;

  CADSysRegisterClass(250, TCircularArc2D);
finalization

end.
