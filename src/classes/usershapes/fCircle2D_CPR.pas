unit fCircle2D_CPR;

{$IFNDEF VER150}
{$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, Math,
  CS4BaseTypes,
  CS4Tasks,
  CS4Shapes,
  CADSys4;

type

  TCircle2D_CPR = class(TCurve2D)
  private
    fName: string;
    fDirection: TArcDirection;
    fRadius: TRealType;
    //fCurvePrecision: integer;

    procedure GetCircleParams(var CX, CY, R: TRealType);
  protected
    function PopulateCurvePoints(N: Word): TRect2D; override;

  public
    {: This constructor creates a new circle.
       Parameters:
       P1 = Centerpoint
       P2 = Radius end point
    }
    constructor Create(ID: LongInt; const P0: TPoint2D; R : TRealType; D: TArcDirection; ACurvePrecision: word);
    procedure Assign(const Obj: TGraphicObject); override;
    procedure SaveToStream(const Stream: TStream); override;
    constructor CreateFromStream(const Stream: TStream; const Version: TCADVersion); override;
    property  Radius: TRealType  read  fRadius  write fRadius;
    property  Direction: TArcDirection read fDirection       write fDirection;
    //property  Precision: integer  read CurvePrecision; //   write CurvePrecision;
    //property
  end;


implementation

// =====================================================================
// TCircle2D_CPR
// =====================================================================

procedure TCircle2D_CPR.GetCircleParams(var CX, CY, R: TRealType);
var
  P0, P1 : TPoint2D;
begin
  P0 := CartesianPoint2D(Points[0]);
  P1 := CartesianPoint2D(Points[1]);
  R  := SQRT(SQR(P1.X - P0.X)+SQR(P1.Y - P0.Y)) ;
  CX := P0.X;
  CY := P0.Y;
  Radius := R;
end;

function TCircle2D_CPR.PopulateCurvePoints(N: Word): TRect2D;
var
  Cont: Integer;  tmpPt: TPoint2D;
  Delta, CurrAngle, CX, CY, R: TRealType;
begin
  if CurvePrecision = 0 then
   begin
     Result := Rect2D(0, 0, 0, 0);
     Exit;
   end;

  inherited PopulateCurvePoints(CurvePrecision + 1);
  GetCircleParams(CX, CY, R);
  Delta := TWOPI / CurvePrecision;
  CurrAngle := AngleFromPoints2D(Points[0], Points[1]);
  if (fDirection = adClockwise) then
  begin
    for Cont := 0 to CurvePrecision  do
    begin
      ProfilePoints.Add(Point2D(CX + R * Cos(CurrAngle), CY + R * Sin(CurrAngle)));
      CurrAngle := CurrAngle - Delta
    end;
  end else
  begin
    for Cont := 0 to CurvePrecision  do
    begin
      ProfilePoints.Add(Point2D(CX + R * Cos(CurrAngle), CY + R * Sin(CurrAngle)));
      CurrAngle := CurrAngle + Delta
    end;
  end;
  Result := TransformBoundingBox2D(ProfilePoints.Extension, ModelTransform);
end;

constructor TCircle2D_CPR.Create(ID: LongInt; const P0: TPoint2D; R : TRealType;
              D: TArcDirection; ACurvePrecision: word);
var P1 : TPoint2d;
begin
  inherited Create(ID, 2, ACurvePrecision);
  Points.DisableEvents := True;
  try
    Points.Add(P0);
    P1 := P0;
    Points.Add(P1);
    Radius := R;
    fDirection := D;
    CurvePrecision:= ACurvePrecision;
    Points.GrowingEnabled := False;
  finally
    Points.DisableEvents := False;
    UpdateExtension(Self);
  end;
end;

procedure TCircle2D_CPR.Assign(const Obj: TGraphicObject);
begin
  if (Obj = Self) then
   Exit;
  inherited Assign(Obj);
  if (Obj is TCircle2D_CPR) or (Obj is TFrame2D) then
   begin
     Points.Copy(TPrimitive2D(Obj).Points, 0, 1);
     Points.GrowingEnabled := False;
   end;
end;

constructor TCircle2D_CPR.CreateFromStream(const Stream: TStream; const Version: TCADVersion);
begin
  { Load the standard properties }
  inherited;
  Points.DisableEvents := True;
  with Stream do
   try
     Read(FDirection, SizeOf(FDirection));
   finally
     Points.DisableEvents := False;
   end;
end;

procedure TCircle2D_CPR.SaveToStream(const Stream: TStream);
begin
  { Save the standard properties }
  inherited SaveToStream(Stream);
  with Stream do
   Write(fDirection, SizeOf(FDirection));
end;

initialization
  //CADSysInitClassRegister;

CADSysRegisterClass(202, TCircle2D_CPR);

end.


