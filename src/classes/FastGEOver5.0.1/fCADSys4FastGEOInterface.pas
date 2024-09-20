unit fCADSys4FastGEOInterface;

interface

uses CADSys4,
     CS4BaseTypes,
     CS4Shapes,
     FastGeo;


  function Point2DToFGPoint2D(const P2D: TPoint2D): TFGPoint2D;
  function FGPoint2DToPoint2D(P: TFGPoint2D): TPoint2D;

  function Line2DToFGSegment2D(ALine2D: TLine2D): TFGSegment2D;


  {

  function Segment2DToFGSegment2D(ASegment2D: TSegment2D; ATransf2D: TTransf2D): TFGSegment2D;
  function FGSegment2DToSegment2D(AFGSegment: TFGSegment2D; ATransf2D: TTransf2D): TSegment2D;

  function SegmentsSet2DToFGSegmentsSet2D(ASegmentsSet2D: TSegmentsSet2D; ATransf2D: TTransf2D): TFGSegmentsSet2D;
  function FGSegmentsSet2DToSegmentsSet2D(AFGSegmentsSet2D: TFGSegmentsSet2D; ATransf2D: TTransf2D): TSegmentsSet2D;

  function FGLine2DToFGSegment2D(AFGLine2D: TFGLine2D; ATransf2D: TTransf2D): TFGSegment2D;
  function FGSegment2DToFGLine2D(AFGSegment2D: TFGSegment2D; ATransf2D: TTransf2D): TFGLine2D;

  function Line2DToFGLine2D(ALine2D: TLine2D; ATransf2D: TTransf2D): TFGLine2D;
  function FGLine2DToLine2D(AFGLine2D: TFGLine2D; ATransf2D: TTransf2D): TLine2D;


  function Circle2D_CPRToFGCircle(P: TPoint2D; R: TRealType; ATransf2D: TTransf2D): TFGCircle;
  function FGCircularArc2DToCiArc2D(AFGCircularArc2D: TFGCircularArc2D; ATransf2D: TTransf2D): TPointsSet2D;
  function CiArc2DToFGCircularArc2D(P0, P1, P2, P3: TPoint2D; SA, EA: TRealType; ADirection: word; ATransf2D: TTransf2D): TFGCircularArc2D;
  function FGTriangle2DToTriangle2D(AFGTriangle2D: TFGTriangle2D; ATransf2D: TTransf2D): TPointsSet2D;
  function Triangle2DToFGTriangle2D(const P0, P1, P2: TPoint2D; ATransf2D: TTransf2D): TFGTriangle2D;
  function FGRectAngleToRectangle2D(AFGRectangle: TFGRectangle; ATransf2D: TTransf2D): TPointsSet2D;
  function Rectangle2DToFGRectAngle(const P0, P1: TPoint2D): TFGRectangle;

  }


implementation

//FastGEO

function Point2DToFGPoint2D(const P2D: TPoint2D): TFGPoint2D;
begin
  {if (P2D.W <> 1.0) and (P2D.W <> 0.0) then
   begin
     Result.X := P2D.X / P2D.W;
     Result.Y := P2D.Y / P2D.W;
   end else
   begin }
     Result.X := P2D.X;
     Result.Y := P2D.Y;
  // end
end;

function FGPoint2DToPoint2D(P: TFGPoint2D): TPoint2D;
begin
  Result.X := P.X;
  Result.Y := P.Y;
  Result.W := 1.0;
end;

function Line2DToFGSegment2D(ALine2D: TLine2D): TFGSegment2D;
begin
  result[1] := Point2DToFGPoint2D(TransformPoint2D(ALine2D.Points[0], ALine2D.ModelTransform));
  result[2] := Point2DToFGPoint2D(TransformPoint2D(ALine2D.Points[1], ALine2D.ModelTransform));
end;

{function Segment2DToFGSegment2D(ASegment2D: TSegment2D; ATransf2D: TTransf2D): TFGSegment2D;
begin
  Result[1] := Point2DToFGPoint2D(ASegment2D[1], ATransf2D);
  Result[2] := Point2DToFGPoint2D(ASegment2D[2], ATransf2D);
end;

function FGSegment2DToSegment2D(AFGSegment: TFGSegment2D; ATransf2D: TTransf2D): TSegment2D;
begin
  result[1] := FGPoint2DToPoint2D(AFGSegment[1], ATransf2D);
  result[2] := FGPoint2DToPoint2D(AFGSegment[2], ATransf2D);
end;

function SegmentsSet2DToFGSegmentsSet2D(ASegmentsSet2D: TSegmentsSet2D; ATransf2D: TTransf2D): TFGSegmentsSet2D;
begin

end;

function FGSegmentsSet2DToSegmentsSet2D(AFGSegmentsSet2D: TFGSegmentsSet2D; ATransf2D: TTransf2D): TSegmentsSet2D;
begin

end;

function Line2DToFGLine2D(const P0, P1: TPoint2D): TFGLine2D;
begin
  Result[1] := Point2DToFGPoint2D(P0);
  Result[2] := Point2DToFGPoint2D(P1);
end;

function FGLine2DToLine2D(AFGLine2D: TFGLine2D): TPointsSet2D;
begin
  result := TPointsSet2D.Create(2);
  result.Add(FGPoint2DToPoint2D(AFGLine2D[1]));
  result.Add(FGPoint2DToPoint2D(AFGLine2D[2]));
end;

function Rectangle2DToFGRectAngle(const P0, P1: TPoint2D): TFGRectangle;
begin
  Result[1] := Point2DToFGPoint2D(P0);
  Result[2] := Point2DToFGPoint2D(P1);
end;

function FGRectAngleToRectangle2D(AFGRectangle: TFGRectangle): TPointsSet2D;
begin
  result := TPointsSet2D.Create(2);
  result.Add(FGPoint2DToPoint2D(AFGRectangle[1]));
  result.Add(FGPoint2DToPoint2D(AFGRectangle[2]));
end;

function Triangle2DToFGTriangle2D(const P0, P1, P2: TPoint2D): TFGTriangle2D;
begin
  Result[1] := Point2DToFGPoint2D(P0);
  Result[2] := Point2DToFGPoint2D(P1);
  Result[3] := Point2DToFGPoint2D(P2);
end;

function FGTriangle2DToTriangle2D(AFGTriangle2D: TFGTriangle2D): TPointsSet2D;
begin
  result := TPointsSet2D.Create(2);
  result.Add(FGPoint2DToPoint2D(AFGTriangle2D[1]));
  result.Add(FGPoint2DToPoint2D(AFGTriangle2D[2]));
  result.Add(FGPoint2DToPoint2D(AFGTriangle2D[3]));
end;

function CiArc2DToFGCircularArc2D(P0, P1, P2, P3: TPoint2D; SA, EA: TRealType; ADirection: word): TFGCircularArc2D;
begin
  result.x1 := P0.X;
  result.y1 := P0.Y;

  result.x2 := P1.X;
  result.y2 := P1.Y;

  result.cx := P2.X;
  result.cy := P2.Y;

  result.px := P3.X;
  result.py := P3.Y;

  result.angle1 := SA;
  result.angle2 := EA;

  result.Orientation := ADirection;
end;

function FGCircularArc2DToCiArc2D(AFGCircularArc2D: TFGCircularArc2D): TPointsSet2D;
begin
  result := TPointsSet2D.Create(2);
  result.Add(Point2D(AFGCircularArc2D.x1, AFGCircularArc2D.y1));
  result.Add(Point2D(AFGCircularArc2D.x2, AFGCircularArc2D.y2));
  result.Add(Point2D(AFGCircularArc2D.cx, AFGCircularArc2D.cy));
  result.Add(Point2D(AFGCircularArc2D.px, AFGCircularArc2D.py));
  //SA, EA, Orientation.
end;

function Circle2D_CPRToFGCircle(P: TPoint2D; R: TRealType): TFGCircle;
begin
  result.x := P.X;
  result.y := P.Y;
  result.Radius := R;
end;

//End FastGEO
}
end.
