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
  fBaseComponent;

  type

    TCADSysEllipticalArc2D = class(TCADSysBaseComponent2D)
    private
      fEllipticalArc2D: TEllipticalArc2D;
      function   GetEllipticalArc2D: TEllipticalArc2D;
      procedure  SetEllipticalArc2D(AEllipticalArc2D: TEllipticalArc2D);

      function   GetStartAngle: single;
      function   GetEndAngle: single;
      function   GetDirection: TArcDirection;

      function   GetCurvePrecision: Word;
      procedure  SetCurvePrecision(APrecision: Word);

      procedure  SetStartAngle(AValue: single);
      procedure  SetEndAngle(AValue: single);
      procedure  SetDirection(AValue: TArcDirection);

    public
      constructor Create;
      property EllipticalArc2D: TEllipticalArc2D read GetEllipticalArc2D write SetEllipticalArc2D;
     published
       property Direction;
      //property BrushColor;
      //property BrushStyle;
      //property Filled;
      property StartAngle : single   read GetStartAngle     write SetStartAngle;
      property EndAngle   : single   read GetEndAngle       write SetEndAngle;
      property CurvePrecision: Word  read GetCurvePrecision write SetCurvePrecision;

  end;



implementation


constructor TCADSysEllipticalArc2D.create;
begin
  inherited create;
end;

function TCADSysEllipticalArc2D.GetEllipticalArc2D: TEllipticalArc2D;
begin
  result := fEllipticalArc2D;
end;

procedure TCADSysEllipticalArc2D.SetEllipticalArc2D(AEllipticalArc2D: TEllipticalArc2D);
begin
  fEllipticalArc2D  := AEllipticalArc2D;
  self.fPrimitive2D := fEllipticalArc2D ;
end;

function TCADSysEllipticalArc2D.GetStartAngle: single;
begin
  result := RadToDeg(fEllipticalArc2D.StartAngle);
end;

function TCADSysEllipticalArc2D.GetEndAngle: single;
begin
  result := RadToDeg(fEllipticalArc2D.EndAngle);
end;

function TCADSysEllipticalArc2D.GetDirection: TArcDirection;
begin
  result := fEllipticalArc2D.Direction;
end;

procedure  TCADSysEllipticalArc2D.SetDirection(AValue: TArcDirection);
begin
  fEllipticalArc2D.Direction := AValue;
end;

function   TCADSysEllipticalArc2D.GetCurvePrecision: Word;
begin
  result := fEllipticalArc2D.CurvePrecision;
end;

procedure  TCADSysEllipticalArc2D.SetCurvePrecision(APrecision: Word);
begin
  fEllipticalArc2D.CurvePrecision := APrecision;
  fEllipticalArc2D.UpdateExtension(self);
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

