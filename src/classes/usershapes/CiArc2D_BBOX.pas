unit CiArc2D_BBOX;

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

    {: This class defines an arc segment of a 2D circle.

     The arc is defined by the two corner of the box that
     contains the arc's ellipse, and the starting and ending
     angles of the arc.
  }
  TCiArc2D_BBOX = class(TCurve2D)
  private
    FStartAngle, FEndAngle: TRealType;
    FDirection: TArcDirection;

    fCX, fCY: TRealType;

    procedure SetStartAngle(A: TRealType);
    procedure SetEndAngle(A: TRealType);
    procedure SetArcDirection(D: TArcDirection);
    procedure GetArcParams(var CX, CY, RX, RY, SA, EA: TRealType);
  protected
    function PopulateCurvePoints(N: Word): TRect2D; override;
  public
    {: This constructor  creates a new arc of a 2D ellipse.

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
    constructor Create(ID: LongInt; const P1, P2: TPoint2D; SA, EA: TRealType);
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
// TCiArc2D_BBOX
// =====================================================================

procedure TCiArc2D_BBOX.GetArcParams(var CX, CY, RX, RY, SA, EA: TRealType);
var
  P1, P0: TPoint2D;
  xLength, yLength: TRealType;
begin
  P0 := CartesianPoint2D(Points[0]);
  P1 := CartesianPoint2D(Points[1]);

  xLength :=  P1.X - P0.X;
  yLength :=  P1.Y - P0.Y;
  P1.Y:= P1.Y + (xLength - yLength);

  CX := (P1.X + P0.X) / 2.0;
  CY := (P1.Y + P0.Y) / 2.0;
  CY :=  CY  - (xLength - yLength);
  fCX := CX;
  fCY := CY;

  //CY :=  CY  + (xLength - yLength);


  RX := Abs(P1.X - P0.X) / 2.0;
  RY := Abs(P1.Y - P0.Y) / 2.0;



  if Points.Count < 3 then
   Exit;
  P0 := CartesianPoint2D(Points[2]);
  P1 := CartesianPoint2D(Points[3]);
  case FDirection of
   adClockwise: begin
     SA := ArcTan2(CY - P0.Y, P0.X - CX);
     EA := ArcTan2(CY - P1.Y, P1.X - CX);
   end;
   adCounterClockwise: begin
     SA := ArcTan2(P0.Y - CY, P0.X - CX);
     EA := ArcTan2(P1.Y - CY, P1.X - CX);
   end;
  end;
end;

procedure TCiArc2D_BBOX.SetStartAngle(A: TRealType);
var
  CX, RX, CY, RY, SA, EA: TRealType;
begin
  if fStartAngle <> A then
   begin
     fStartAngle := A;
     GetArcParams(CX, CY, RX, RY, SA, EA);
     Points[2] := Point2D(CX + RX * Cos(A), CY + RY * Sin(A));
   end;
end;

procedure TCiArc2D_BBOX.SetEndAngle(A: TRealType);
var
  CX, RX, CY, RY, SA, EA: TRealType;
begin
  if fEndAngle <> A then
   begin
     fEndAngle := A;
     GetArcParams(CX, CY, RX, RY, SA, EA);
     Points[3] := Point2D(CX + RX * Cos(A), CY + RY * Sin(A));
   end;
end;

procedure TCiArc2D_BBOX.SetArcDirection(D: TArcDirection);
begin
  if D <> FDirection then
   begin
     FDirection := D;
     UpdateExtension(Self);
   end;
end;

function TCiArc2D_BBOX.PopulateCurvePoints(N: Word): TRect2D;
var
  Cont, NPts: Integer;
  CX, RX, CY, RY: TRealType;
  Delta, CurrAngle: TRealType;
begin
  if CurvePrecision = 0 then
   begin
     Result := Rect2D(0, 0, 0, 0);
     Exit;
   end;
  GetArcParams(CX, CY, RX, RY, fStartAngle, fEndAngle);
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
     for Cont := 0 to NPts - 1 do
      begin
        ProfilePoints.Add(Point2D(CX + RX * Cos(CurrAngle), CY - RY * Sin(CurrAngle)));
        CurrAngle := CurrAngle + Delta
      end;
     ProfilePoints.Add(Point2D(CX + RX * Cos(fEndAngle), CY - RY * Sin(fEndAngle)));
   end
  else
   begin
     CurrAngle := fStartAngle;
     for Cont := 0 to NPts - 1 do
      begin
        ProfilePoints.Add(Point2D(CX + RX * Cos(CurrAngle), CY + RY * Sin(CurrAngle)));
        CurrAngle := CurrAngle + Delta
      end;
     ProfilePoints.Add(Point2D(CX + RX * Cos(fEndAngle), CY + RY * Sin(fEndAngle)));
   end;
  Result := TransformBoundingBox2D(ProfilePoints.Extension, ModelTransform);
end;

{ Angles are in radiants. }
constructor TCiArc2D_BBOX.Create(ID: LongInt; const P1, P2: TPoint2D; SA, EA: TRealType);
begin
  inherited Create(ID, 4, 100);
  Points.DisableEvents := True;
  try
    Points.Add(P1);
    Points.Add(P2);
    Points.Add(Point2D(0, 0));
    Points.Add(Point2D(0, 0));

    //fDirection := adClockwise;
    fDirection := adCounterClockwise;
    StartAngle := SA;
    EndAngle := EA;
    Points.GrowingEnabled := False;
  finally
    Points.DisableEvents := False;
    UpdateExtension(Self);
  end;
end;

procedure TCiArc2D_BBOX.Assign(const Obj: TGraphicObject);
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
  else if Obj is TCiArc2D_BBOX then
   begin
     fStartAngle := (Obj as TCiArc2D_BBOX).StartAngle;
     fEndAngle := (Obj as TCiArc2D_BBOX).EndAngle;
     fDirection := (Obj as TCiArc2D_BBOX).Direction;
     Points.Copy(TPrimitive2D(Obj).Points, 0, 3);
     Points.GrowingEnabled := False;
   end;
end;

constructor TCiArc2D_BBOX.CreateFromStream(const Stream: TStream; const Version: TCADVersion);
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

procedure TCiArc2D_BBOX.SaveToStream(const Stream: TStream);
begin
  { Save the standard properties }
  inherited SaveToStream(Stream);
  with Stream do
   Write(fDirection, SizeOf(FDirection));
end;

initialization
  //CADSysInitClassRegister;

  CADSysRegisterClass(210, TCiArc2D_BBOX);
finalization
end.

