unit fComponentCiArc_BBOX;

{$IFNDEF VER150}
{$MODE Delphi}
{$ENDIF}

interface


uses
  Classes, SysUtils,
  CADSys4,
  CS4BaseTypes,
  CS4Shapes,
  fBaseComponent,
  CiArc2D_BBOX;

  type

    TCADSysCiArc2D_BBOX = class(TCADSysBaseComponent2D)

    private
      fCiArc2D_BBOX: TCiArc2D_BBOX;
      function   GetCiArc2D_BBOX: TCiArc2D_BBOX;
      procedure  SetCiArc2D_BBOX(ACiArc2D_BBOX: TCiArc2D_BBOX);

      function   GetStartAngle: TRealType;
      function   GetEndAngle: TRealType;
      function   GetDirection: byte;

      procedure  SetStartAngle(AValue: TRealType);
      procedure  SetEndAngle(AValue: TRealType);
      procedure  SetDirection(AValue: byte);

    public
      constructor Create;
      property CiArc2D_BBOX: TCiArc2D_BBOX read GetCiArc2D_BBOX write SetCiArc2D_BBOX;

    published
      property StartAngle : TRealType   read GetStartAngle     write SetStartAngle;
      property EndAngle   : TRealType   read GetEndAngle       write SetEndAngle;
      property Direction  : byte        read GetDirection      write SetDirection;
      property Tag; //        : LongInt     read GetTag            write SetTag;

  end;


implementation


constructor TCADSysCiArc2D_BBOX.create;
begin
  inherited create;
end;

function TCADSysCiArc2D_BBOX.GetCiArc2D_BBOX: TCiArc2D_BBOX;
begin
  result := fCiArc2D_BBOX;
end;

procedure TCADSysCiArc2D_BBOX.SetCiArc2D_BBOX(ACiArc2D_BBOX: TCiArc2D_BBOX);
begin
  fCiArc2D_BBOX := ACiArc2D_BBOX;
  self.fPrimitive2D :=fCiArc2D_BBOX ;
end;

function TCADSysCiArc2D_BBOX.GetStartAngle: TRealType;
begin
  result := RadToDeg(fCiArc2D_BBOX.StartAngle);
end;

function TCADSysCiArc2D_BBOX.GetEndAngle: TRealType;
begin
  result := RadToDeg(fCiArc2D_BBOX.EndAngle);
end;

function TCADSysCiArc2D_BBOX.GetDirection: byte;
begin
  if fCiArc2D_BBOX.Direction = adCounterClockwise then
    result := 0
  else
    result := 1;
end;

procedure  TCADSysCiArc2D_BBOX.SetDirection(AValue: byte);
begin
  if  AValue = 0 then
    fCiArc2D_BBOX.Direction := adCounterClockwise
  else
    fCiArc2D_BBOX.Direction := adClockwise;
end;

procedure  TCADSysCiArc2D_BBOX.SetStartAngle(AValue: TRealType);
begin
  fCiArc2D_BBOX.StartAngle := DegToRad(AValue);
end;

procedure  TCADSysCiArc2D_BBOX.SetEndAngle(AValue: TRealType);
begin
  fCiArc2D_BBOX.EndAngle := DegToRad(AValue);
end;

end.

