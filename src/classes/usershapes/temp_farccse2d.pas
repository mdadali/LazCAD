unit temp_fArcCSE2D;
{$mode delphi}

interface

uses
  Classes, SysUtils, Math,
  CS4BaseTypes,
  CS4Tasks,
  CS4Shapes,
  CADSys4;

type

  TArcCSE2D = class(TCurve2D)
  private
    FStartAngle, FEndAngle: TRealType;
    FDirection: TArcDirection;

    procedure SetStartAngle(A: TRealType);
    procedure SetEndAngle(A: TRealType);
    procedure SetArcDirection(D: TArcDirection);
    procedure GetArcParams(var CX, CY, RX, RY, SA, EA: TRealType);
  protected
    function PopulateCurvePoints(N: Word): TRect2D; override;
  public
    constructor Create(ID: LongInt; const P1, P2, P3, P4: TPoint2D);
    procedure Assign(const Obj: TGraphicObject); override;
    constructor CreateFromStream(const Stream: TStream; const Version: TCADVersion); override;
    procedure SaveToStream(const Stream: TStream); override;
    property StartAngle: TRealType read FStartAngle write SetStartAngle;
     property EndAngle: TRealType read FEndAngle write SetEndAngle;
    property Direction: TArcDirection read FDirection write SetArcDirection;
  end;


implementation

// =====================================================================
// TArcCSE2D
// =====================================================================
procedure TArcCSE2D.GetArcParams(var CX, CY, RX, RY, SA, EA: TRealType);
var CP, RP, SP, EP, tmpRP : TPoint2D;
  xLength, yLength: TRealType;
begin
  CP := CartesianPoint2D(Points[0]);
  RP := CartesianPoint2D(Points[1]);
  SP := CartesianPoint2D(Points[2]);
  EP := CartesianPoint2D(Points[3]);

  CX := CP.X;
  CY := CP.Y;

  RX := RP.X - CP.X;
  //RY := RP.Y - CP.Y;  //Ellipse
  RY :=  RX;//Circle

  if Points.Count < 3 then
   Exit;
  case FDirection of
   adClockwise: begin
     SA := ArcTan2(CY - SP.Y, SP.X - CX);
     EA := ArcTan2(CY - EP.Y, EP.X - CX);
   end;
   adCounterClockwise: begin
     SA := ArcTan2(SP.Y - CY, SP.X - CX);
     EA := ArcTan2(EP.Y - CY, EP.X - CX);
   end;
  end;
end;

procedure TArcCSE2D.SetStartAngle(A: TRealType);
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

procedure TArcCSE2D.SetEndAngle(A: TRealType);
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

procedure TArcCSE2D.SetArcDirection(D: TArcDirection);
begin
  if D <> FDirection then
   begin
     FDirection := D;
     UpdateExtension(Self);
   end;
end;

function TArcCSE2D.PopulateCurvePoints(N: Word): TRect2D;
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
constructor TArcCSE2D.Create(ID: LongInt; const P1, P2, P3, P4: TPoint2D);
begin
  inherited Create(ID,4 , 50);
  Points.DisableEvents := True;
  try
    Points.Add(P1);  //CP
    Points.Add(P2);  //RP
    Points.Add(P3);  //SP
    Points.Add(P4);  //EP

    //Points.Add(Point2D(0, 0));
    //Points.Add(Point2D(0, 0));

    fDirection := adClockwise;
    StartAngle := 0;
    EndAngle := 0;
    Points.GrowingEnabled := False;
  finally
    Points.DisableEvents := False;
    UpdateExtension(Self);
  end;
end;

procedure TArcCSE2D.Assign(const Obj: TGraphicObject);
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
  else if Obj is TArcCSE2D then
   begin
     fStartAngle := (Obj as TArcCSE2D).StartAngle;
     fEndAngle := (Obj as TArcCSE2D).EndAngle;
     fDirection := (Obj as TArcCSE2D).Direction;
     Points.Copy(TPrimitive2D(Obj).Points, 0, 3);
     Points.GrowingEnabled := False;
   end;
end;

constructor TArcCSE2D.CreateFromStream(const Stream: TStream; const Version: TCADVersion);
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

procedure TArcCSE2D.SaveToStream(const Stream: TStream);
begin
  { Save the standard properties }
  inherited SaveToStream(Stream);
  with Stream do
   Write(fDirection, SizeOf(FDirection));
end;

initialization
  //CADSysInitClassRegister;

CADSysRegisterClass(202, TArcCSE2D);

end.

