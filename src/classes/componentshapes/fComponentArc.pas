unit fComponentArc;

{$IFNDEF VER150}
{$MODE Delphi}
{$ENDIF}

interface


uses
  Classes, SysUtils,
  CADSys4,
  CS4BaseTypes,
  CS4Shapes,
  fcompdirectionalcurve2d;

  type

    TCADSysEllipticalArc2D = class(TCADSysDirectionalCurve2D)
    private
      fEllipticalArc2D: TEllipticalArc2D;

      function   GetStartAngle: single;
      function   GetEndAngle: single;

      procedure  SetStartAngle(AValue: single);
      procedure  SetEndAngle(AValue: single);

      function  GetGraphicObject: TGraphicObject; override;
      procedure SetGraphicObject(AGraphicObject: TGraphicObject); override;
    public
      constructor Create;
      property GraphicObject;
     published
       property StartAngle : single   read GetStartAngle     write SetStartAngle;
       property EndAngle   : single   read GetEndAngle       write SetEndAngle;
  end;


implementation

function  TCADSysEllipticalArc2D.GetGraphicObject: TGraphicObject;
begin
  result :=  fEllipticalArc2D;
end;

procedure TCADSysEllipticalArc2D.SetGraphicObject(AGraphicObject: TGraphicObject);
begin
  fEllipticalArc2D := TEllipticalArc2D(AGraphicObject);
  fSimplePrimitive2D := TSimplePrimitive2D(AGraphicObject);
  fPrimitive2D := TPrimitive2D(AGraphicObject);
  fObject2D := TObject2D(AGraphicObject);
  fGraphicObject := AGraphicObject;
end;

constructor TCADSysEllipticalArc2D.create;
begin
  inherited create;
end;

function TCADSysEllipticalArc2D.GetStartAngle: single;
begin
  result := RadToDeg(fEllipticalArc2D.StartAngle);
end;

function TCADSysEllipticalArc2D.GetEndAngle: single;
begin
  result := RadToDeg(fEllipticalArc2D.EndAngle);
end;

procedure  TCADSysEllipticalArc2D.SetStartAngle(AValue: single);
begin
  fEllipticalArc2D.StartAngle := DegToRad(AValue);
end;

procedure  TCADSysEllipticalArc2D.SetEndAngle(AValue: single);
begin
  fEllipticalArc2D.EndAngle := DegToRad(AValue);
end;


end.

