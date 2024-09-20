unit fCiArc_CSE;

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

  TCIArc2D_CSE = class(TCurve2D)
  private
    FStartAngle, FEndAngle: TRealType;
    FDirection: TArcDirection;

    procedure SetStartAngle(A: TRealType);
    procedure SetEndAngle(A: TRealType);
    procedure SetArcDirection(D: TArcDirection);
    procedure GetArcParams(var CX, CY, R, SA, EA: TRealType);
  protected
    function PopulateCurvePoints(N: Word): TRect2D; override;
  public
    {: This constructor  creates a new arc of a 2D circle.

       Parameters:

       <LI=<I=ID> is the object identifier.>
       <LI=<I=P1> and <I=P2> are the corner points of the frame
        that defines the arc's ellipse.>
       <LI=<I=SA> is the starting angle (in radiants) of the
        arc. The angle that correspond to zero radiants is
        along the positive x-axis (if no transformation is applied
        to the object).>
       <LI=<I=SA> is the ending angle (in radiants) of the
        arc. The angle that correspond to zero radiants is
        along the positive x-axis (if no transformation is applied
        to the object).>

       Note: Once created, the arc has four control points. The
       first two are <I=P1> and <I=P2>; the third is the point
       that lies on the segment from the center of the arc's ellipse
       and the starting point of the arc; the fourth is the point
       that lies on the segment from the center of the arc's ellipse and
       the ending point of the arc.
    }
    constructor Create(ID: LongInt; const P1, P2, P3: TPoint2D; SA, EA: TRealType; APrecision: word; D: TArcDirection);
    procedure Assign(const Obj: TGraphicObject); override;
    constructor CreateFromStream(const Stream: TStream; const Version: TCADVersion); override;
    procedure SaveToStream(const Stream: TStream); override;
    {: This property contains the starting angle of the arc in radiants.

       The angle that correspond to zero radiants is along the
       positive x-axis (if no transformation is applied to the
       object).
    }
    property StartAngle: TRealType read FStartAngle write SetStartAngle;
    {: This property contains the ending angle of the arc in radiants.

       The angle that correspond to zero radiants is along the
       positive x-axis (if no transformation is applied to the
       object).
    }
    property EndAngle: TRealType read FEndAngle write SetEndAngle;
    {: This property contains the direction used to draw the arc.

       See <See Type=TArcDirection> for details.
    }
    property Direction: TArcDirection read FDirection write SetArcDirection;
  end;


implementation

// =====================================================================
// TCIArc2D_CSE
// =====================================================================

procedure TCIArc2D_CSE.GetArcParams(var CX, CY, R, SA, EA: TRealType);
var
  P1, P0, P2: TPoint2D;
begin
  P0:=  CartesianPoint2D(Points[0]);
  P1 := CartesianPoint2D(Points[1]);
  P2 := CartesianPoint2D(Points[2]);
  CX := P0.X;
  CY := P0.Y;
  R := SQRT(SQR(P1.X - P0.X)+SQR(P1.Y - P0.Y)) ;
  if Points.Count < 2 then
    Exit;
  P0 := CartesianPoint2D(Points[1]);
  P1 := CartesianPoint2D(Points[2]);
  case FDirection of
   adClockwise: begin
     SA := AngleFromPoints2D(Points[0], Points[1]);
     EA := AngleFromPoints2D(Points[0], Points[2]);
     //SA := ArcTan2(CY - P0.Y, P0.X - CX);
     //EA := ArcTan2(CY - P1.Y, P1.X - CX);
   end;
   adCounterClockwise: begin
      SA := AngleFromPoints2D(Points[0], Points[2]);
      EA := AngleFromPoints2D(Points[0], Points[1]);
     //SA := ArcTan2(P0.Y - CY, P0.X - CX);
     //EA := ArcTan2(P1.Y - CY, P1.X - CX);
   end;
  end;

end;

procedure TCIArc2D_CSE.SetStartAngle(A: TRealType);
var
  R, CX, RX, CY, RY, SA, EA: TRealType;
begin
  if fStartAngle <> A then
   begin
     fStartAngle := A;
     GetArcParams(CX, CY, R, fStartAngle, fEndAngle);
     Points[1] := Point2D(CX + RX * Cos(A), CY + RY * Sin(A));
   end;
end;

procedure TCIArc2D_CSE.SetEndAngle(A: TRealType);
var
  R, CX, RX, CY, RY, SA, EA: TRealType;
begin
  if fEndAngle <> A then
   begin
     fEndAngle := A;
     GetArcParams(CX, CY, R, fStartAngle, fEndAngle);
     Points[2] := Point2D(CX + RX * Cos(A), CY + RY * Sin(A));
   end;
end;

procedure TCIArc2D_CSE.SetArcDirection(D: TArcDirection);
begin
  if D <> FDirection then
   begin
     FDirection := D;
     UpdateExtension(Self);
   end;
end;

function TCIArc2D_CSE.PopulateCurvePoints(N: Word): TRect2D;
var
  Cont, NPts: Integer;
  CX, CY, R: TRealType;
  Delta, CurrAngle: TRealType;
begin
  if CurvePrecision = 0 then
   begin
     Result := Rect2D(0, 0, 0, 0);
     Exit;
   end;
  GetArcParams(CX, CY, R, fStartAngle, fEndAngle);
  // Calcola il numero di punti effetivi nella curva
  NPts := CurvePrecision;
  // Calcola il delta angolare tra due punti
  if fStartAngle < fEndAngle then
   Delta := (fEndAngle - fStartAngle) / (NPts + 1)
  else
   Delta := (TWOPI - fStartAngle + fEndAngle) / (NPts + 1);
  // Crea il vettore curvilineo.
  inherited PopulateCurvePoints(NPts + 1);
  // Popola il vettore curvilineo.
  if fDirection = adClockwise then
   begin
     CurrAngle := fStartAngle;
     for Cont := 0 to NPts + 1 do
      begin
        ProfilePoints.Add(Point2D(CX + R * Cos(CurrAngle), CY + R * Sin(CurrAngle)));
        CurrAngle := CurrAngle - Delta
      end;
      //ProfilePoints.Add(Point2D(CX + R * Cos(fEndAngle), CY - R * Sin(fEndAngle)));
   end
  else
   begin
     CurrAngle := fStartAngle;
     for Cont := 0 to NPts + 1 do
      begin
        ProfilePoints.Add(Point2D(CX + R * Cos(CurrAngle), CY + R * Sin(CurrAngle)));
        CurrAngle := CurrAngle + Delta
      end;
      //ProfilePoints.Add(Point2D(CX + R * Cos(fEndAngle), CY + R * Sin(fEndAngle)));
   end;
  Result := TransformBoundingBox2D(ProfilePoints.Extension, ModelTransform);
end;

{ Angles are in radiants. }
constructor TCIArc2D_CSE.Create(ID: LongInt; const P1, P2, P3: TPoint2D; SA, EA: TRealType; APrecision: word; D: TArcDirection);
begin
  inherited Create(ID, 3, 50);
  Points.DisableEvents := True;
  try
    Points.Add(P1);
    Points.Add(P2);
    Points.Add(P3);
    fDirection := D;
    StartAngle := SA;
    EndAngle := EA;
    Points.GrowingEnabled := False;
  finally
    Points.DisableEvents := False;
    UpdateExtension(Self);
  end;
end;

procedure TCIArc2D_CSE.Assign(const Obj: TGraphicObject);
begin
  if (Obj = Self) then
   Exit;
  inherited Assign(Obj);
  if (Obj is TEllipse2D) or (Obj is TFrame2D) then
   begin
     fStartAngle := 0;
     fEndAngle := TWOPI;
     Points.DisableEvents := True;
     try
       Points.Copy(TPrimitive2D(Obj).Points, 0, 1);
       Points.Add(Point2D(0, 0));
       Points.Add(Point2D(0, 0));
       Points.GrowingEnabled := False;
     finally
       Points.DisableEvents := False;
       UpdateExtension(Self);
     end;
   end
  else if Obj is TCIArc2D_CSE then
   begin
     fStartAngle := (Obj as TCIArc2D_CSE).StartAngle;
     fEndAngle := (Obj as TCIArc2D_CSE).EndAngle;
     fDirection := (Obj as TCIArc2D_CSE).Direction;
     Points.Copy(TPrimitive2D(Obj).Points, 0, 3);
     Points.GrowingEnabled := False;
   end;
end;

constructor TCIArc2D_CSE.CreateFromStream(const Stream: TStream; const Version: TCADVersion);
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

procedure TCIArc2D_CSE.SaveToStream(const Stream: TStream);
begin
  { Save the standard properties }
  inherited SaveToStream(Stream);
  with Stream do
   Write(fDirection, SizeOf(FDirection));
end;

initialization
  //CADSysInitClassRegister;

CADSysRegisterClass(203, TCIArc2D_CSE);

end.

