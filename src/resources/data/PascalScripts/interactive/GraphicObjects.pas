Program GraphicObjects;


var
    Line2D,
    CircularArc2D,
    EllipticalArc2D,
    Circle2D,
    Polyline2D,
    Polygon2D,
    Frame2D,
    BSpline2D,
    Ellipse2D,
    Sector2D,
    Segment2D,
    SymetricSymbol2D,
    ASymetricSymbol2D,
    Text2D,
    JustifiedVectText2D,
    Bitmap2D: longint;

    TmpP0, TmpP1, TmpCenterPoint: TPoint2D;
    TmpLeft, TmpBottom, TmpRight, TmpTop, TmpHeight: TRealType;
    TmpRadius, TmpStartAngle, TmpEndAngle:  TRealType; TmpArcDirection: TArcDirection;
    TmpCurvePrecision, TmpFontIndex: word;


    TmpRect2D: TRect2D;
    TmpString, fileName: string;

    Form1: TForm;
    btnDrawLine: TButton;

function DrawLine(AID: longint; P0, P1: TPoint2D): longInt;
begin
  result := CAD_DrawLine2D(AID, P0, P1);
end;

function DrawCircularArc2D(AID: longint; CP: TPoint2D; R, SA, EA: TRealType; ADirection: TArcDirection): longint;
begin
  result := CAD_DrawCircularArc2D(AID, CP, R, SA, EA, ADirection);
end;

function DrawEllipticalArc2D(AID: longint; P0, P1: TPoint2D; SA, EA: TRealType; ADirection: TArcDirection): longint;
begin
  result := CAD_DrawEllipticalArc2D(AID, P0, P1, SA, EA, ADirection);
end;

function DrawCircle2D(AID: longint; CP: TPoint2D; R, SA: TRealType; ADirection: TArcDirection): longint;
begin
  result := CAD_DrawCircle2D(AID, CP, R, SA, ADirection);
end;

function DrawPolyline2D(AID: longint): longint;
begin
  result := CAD_DrawPolyline2D(AID);
end;

function DrawPolygon2D(AID: longint; ADirection: TArcDirection): longint;
begin
  result := CAD_DrawPolygon2D(AID, ADirection);
end;

function DrawBSpline2D(AID: longint): longint;
begin
  result := CAD_DrawBSpline2D(AID);
end;

function DrawFrame2D(AID: longint; P0, P1: TPoint2D; ADirection: TArcDirection): longint;
begin
  result := CAD_DrawFrame2D(AID, P0, P1, ADirection);
end;

function DrawEllipse2D(AID: longint; P0, P1: TPoint2D; ACurvePrecision: word; ADirection: TArcDirection): longint;
begin
  result := CAD_DrawEllipse2D(AID, P0, P1, ACurvePrecision, ADirection);
end;

function DrawSector2D(AID:  longint; CP: TPoint2D; R, SA, EA: TRealType; ACurvePrecision: word; ADirection: TArcDirection): longint;
begin
  result := CAD_DrawSector2D(AID, CP, R, SA, EA, ACurvePrecision, ADirection);
end;

function DrawSegment2D(AID: longint; CP: TPoint2D; R, SA, EA: TRealType; ACurvePrecision: word; ADirection: TArcDirection): longint;
begin
  result := CAD_DrawSegment2D(AID, CP, R, SA, EA, ACurvePrecision, ADirection);
end;

function DrawSymetricSymbol2D(AID:  longint; CP: TPoint2D; R, SA: TRealType; ACurvePrecision: word; ADirection: TArcDirection): longint;
begin
  result := CAD_DrawSymetricSymbol2D(AID, CP, R, SA, ACurvePrecision, ADirection);
end;

function DrawASymetricSymbol2D(AID: longint;  P0, P1: TPoint2D; R, SA: TRealType; ACurvePrecision: word; ADirection: TArcDirection): longint;
begin
  result := CAD_DrawASymetricSymbol2D(AID, P0, P1, R, SA, ACurvePrecision, ADirection);
end;

function DrawTex2D(AID: longint; ARect2D: TRect2D; AHeight: TRealType; AStr: string): longint;
begin
  result := CAD_DrawText2D(AID, ARect2D, AHeight, AStr);
end;

function DrawJustifiedVectText2D(AID: longint; var AFontIndex: word; ARect2D: TRect2D; AHeight: TrealType; AStr: string): longint;
begin
  result := CAD_DrawJustifiedVectText2D(AID, AFontIndex, ARect2D, AHeight, AStr);
end;

function DrawBitmap2D(AID: longint; P0, P1: TPoint2D; AFileName: string): longInt;
begin
  result := CAD_DrawBitmap2D(AID, P0, P1, AFileName);
end;

procedure Button1Click(Sender: TObject);
begin
  CAD_Clear;

  TmpP0.X := 0.0;
  TmpP0.Y := 0.0;
  TmpP0.W := 1.0;
  TmpP1.X := 100.0;
  TmpP1.Y := 100.0;
  TmpP1.W := 1.0;
  Line2D := DrawLine(-1, TmpP0, TmpP1);
  CAD_Prim2DSetPenSource(Line2D, psCustom);
  CAD_Prim2DSetPenColor(Line2D, clRed);
  CAD_Prim2DSetPenStyle(Line2D, psDot);
  CAD_Prim2DSetPenWidth(Line2D, 5);
  CAD_ZoomToExtentions;
end;


begin
  CAD_Clear;

  TmpP0.X := 0.0;
  TmpP0.Y := 0.0;
  TmpP0.W := 1.0;
  TmpP1.X := 100.0;
  TmpP1.Y := 100.0;
  TmpP1.W := 1.0;
  Line2D := DrawLine(-1, TmpP0, TmpP1);
  CAD_Prim2DSetPenSource(Line2D, psCustom);
  CAD_Prim2DSetPenColor(Line2D, clRed);
  CAD_Prim2DSetPenStyle(Line2D, psDot);
  CAD_Prim2DSetPenWidth(Line2D, 5);
  CAD_ZoomToExtentions;

  TmpP0.X := 100.0;
  TmpP0.Y := 0.0;
  TmpP0.W := 1.0;
  TmpP1.X := 200.0;
  TmpP1.Y := 100.0;
  TmpP1.W := 1.0;
  TmpCenterPoint.X := 120.0;
  TmpCenterPoint.Y := 100.0;
  TmpCenterPoint.W := 1.0;
  TmpRadius       := 100.0;
  TmpStartAngle   := 0.0;
  TmpEndAngle     := 90.0;
  TmpArcDirection := adCounterClockwise;
  CircularArc2D := DrawCircularArc2D(-1, TmpCenterPoint, TmpRadius, TmpStartAngle, TmpEndAngle, TmpArcDirection);
  CAD_Prim2DSetPenColor(CircularArc2D, clRed);

  TmpP0.X := 0.0;
  TmpP0.Y := 0.0;

  CAD_CircularArc2DSetCenterPoint2D(CircularArc2D, TmpP0);
  CAD_ZoomToExtentions;

  TmpP0.X := 300.0;
  TmpP0.Y := 0.0;
  TmpP0.W := 1.0;
  TmpP1.X := 500.0;
  TmpP1.Y := 100.0;
  TmpP1.W := 1.0;
  TmpArcDirection := adClockwise;
  EllipticalArc2D := DrawEllipticalArc2D(-1, TmpP0, TmpP1, TmpStartAngle, TmpEndAngle, TmpArcDirection);
  CAD_Prim2DSetPenColor(EllipticalArc2D, clYellow);
  CAD_ZoomToExtentions;

  TmpRadius := 75.0;
  TmpCenterPoint.X := 600.0;
  TmpCenterPoint.Y := 75.0;
  TmpStartAngle := 90.0;
  TmpArcDirection := adCounterClockwise;
  Circle2D := DrawCircle2D(-1, TmpCenterPoint,  TmpRadius, TmpStartAngle, TmpArcDirection);
  CAD_Prim2DSetPenSource(Circle2D, psCustom);
  CAD_Prim2DSetPenColor(Circle2D, clGreen);
  CAD_SetBrushSource(Circle2D, bsCustom);
  CAD_SetBrushColor(Circle2D, clRed);
  CAD_SetBrushStyle(Circle2D, bsDiagCross);
  CAD_ZoomToExtentions;

  Polyline2D := DrawPolyline2D(-1);
  TmpP0.X := 700.0;
  TmpP0.Y := 0.0;
  TmpP0.W := 1.0;
  CAD_Polyline2DAddPoint2D(Polyline2D, TmpP0);
  TmpP0.X := 750.0;
  TmpP0.Y := 20.0;
  TmpP0.W := 1.0;
  CAD_Polyline2DAddPoint2D(Polyline2D, TmpP0);
  TmpP0.X := 750.0;
  TmpP0.Y := 40.0;
  TmpP0.W := 1.0;
  CAD_Polyline2DAddPoint2D(Polyline2D, TmpP0);
  TmpP0.X := 700.0;
  TmpP0.Y := 40.0;
  TmpP0.W := 1.0;
  CAD_Polyline2DAddPoint2D(Polyline2D, TmpP0);
  CAD_ZoomToExtentions;

  Polygon2D := DrawPolygon2D(-1, adClockwise);
  TmpP0.X := 800.0;
  TmpP0.Y := 0.0;
  TmpP0.W := 1.0;
  CAD_Polygon2DAddPoint2D(Polygon2D, TmpP0);
  TmpP0.X := 850.0;
  TmpP0.Y := 20.0;
  TmpP0.W := 1.0;
  CAD_Polygon2DAddPoint2D(Polygon2D, TmpP0);
  TmpP0.X := 850.0;
  TmpP0.Y := 40.0;
  TmpP0.W := 1.0;
  CAD_Polygon2DAddPoint2D(Polygon2D, TmpP0);
  CAD_ZoomToExtentions;

  BSpline2D := DrawBSpline2D(-1);
  TmpP0.X := 350.0;
  TmpP0.Y := 200.0;
  TmpP0.W := 1.0;
  CAD_BSpline2DAddPoint2D(BSpline2D, TmpP0);
  TmpP0.X := 850.0;
  TmpP0.Y := 300.0;
  TmpP0.W := 1.0;
  CAD_BSpline2DAddPoint2D(BSpline2D, TmpP0);
  TmpP0.X := 850.0;
  TmpP0.Y := 360.0;
  TmpP0.W := 1.0;
  CAD_BSpline2DAddPoint2D(BSpline2D, TmpP0);
  TmpP0.X := 800.0;
  TmpP0.Y := 350.0;
  TmpP0.W := 1.0;
  CAD_BSpline2DAddPoint2D(BSpline2D, TmpP0);
  CAD_ZoomToExtentions;

  TmpP0.X := 0.0;
  TmpP0.Y := 300.0;
  TmpP0.W := 1.0;
  TmpP1.X := 300.0;
  TmpP1.Y := 200.0;
  TmpP1.W := 1.0;
  Frame2D := DrawFrame2D(-1, TmpP0, TmpP1, adClockwise);

  TmpCurvePrecision := 50;
  Ellipse2D :=  DrawEllipse2D(-1, TmpP0, TmpP1, TmpCurvePrecision, adClockwise);
  CAD_ZoomToExtentions;

  TmpCenterPoint.X := 735.0;
  TmpCenterPoint.Y := 180.0;
  TmpCenterPoint.W := 1.0;
  TmpStartAngle := 0.0;
  TmpEndAngle   := 90.0;
  Sector2D := DrawSector2D(-1, TmpCenterPoint, TmpRadius, TmpStartAngle, TmpEndAngle, TmpCurvePrecision, TmpArcDirection);
  CAD_ZoomToExtentions;


  TmpCenterPoint.X := 800.0;
  TmpCenterPoint.Y := 180.0;
  TmpCenterPoint.W := 1.0;
  TmpStartAngle := 0.0;
  TmpEndAngle   := 90.0;
  Segment2D := DrawSegment2D(-1, TmpCenterPoint, TmpRadius, TmpStartAngle, TmpEndAngle, TmpCurvePrecision, TmpArcDirection);
  CAD_ZoomToExtentions;

  TmpCurvePrecision := 4;
  TmpCenterPoint.X := 970.0;
  TmpCenterPoint.Y := 180.0;
  TmpCenterPoint.W := 1.0;
  SymetricSymbol2D := DrawSymetricSymbol2D(-1, TmpCenterPoint, TmpRadius, TmpStartAngle, TmpCurvePrecision, TmpArcDirection);
  CAD_ZoomToExtentions;

  TmpCurvePrecision := 5;
  TmpP0.X := 350.0;
  TmpP0.Y := 340.0;
  TmpP0.W := 1.0;
  TmpP1.X := 500.0;
  TmpP1.Y := 250.0;
  TmpP1.W := 1.0;
  ASymetricSymbol2D :=  DrawASymetricSymbol2D(-1, TmpP0, TmpP1, TmpRadius, TmpStartAngle, TmpCurvePrecision, TmpArcDirection);
  CAD_ZoomToExtentions;

  TmpLeft := 0.0;
  TmpBottom := 500.0;
  TmpRight := 200.0;
  TmpTop := 100.0;
  TmpRect2D := CAD_Rect2D(TmpLeft, TmpBottom, TmpRight, TmpTop);
  TmpString := 'Text2D' + #13#10 + 'This is no longer' + #13#10 + 'a SingleLineText object.';
  TmpHeight := 50;
  Text2D := DrawTex2D(-1, TmpRect2D, TmpHeight, TmpString);
  CAD_ZoomToExtentions;

  TmpFontIndex := 3;
  TmpString := 'TJustifiedVectText2D';
  TmpHeight := 100.0;
  TmpBottom := 600.0;
  TmpRect2D := CAD_Rect2D(TmpLeft, TmpBottom, TmpRight, TmpTop);
  JustifiedVectText2D := DrawJustifiedVectText2D(-1, TmpFontIndex, TmpRect2D, TmpHeight, TmpString);
  CAD_ZoomToExtentions;
  //

  TmpP0.X := 1040.0;
  TmpP0.Y := 550.0;
  TmpP0.W := 1.0;
  TmpP1.X := 1500.0;
  TmpP1.Y := 300.0;
  TmpP1.W := 1.0;

  //fileName := OpenFileDialog('Please choose a file', 'C:\', 'Bitmap-Files (*.bmp)|*.bmp');
  //if fileName <> '' then
    //CAD_DrawBitmap2D(-1, TmpP0, TmpP1, fileName)
  //else
    //Writeln('No file selected.');


  CAD_Text2DSetColorSource(Text2D, csCustom);
  CAD_Text2DSetColor(Text2D, clRed);

  CAD_ZoomToExtentions;
end. 
