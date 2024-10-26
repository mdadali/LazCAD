unit CADSys4ClipperInterface;

interface


uses
  CS4BaseTypes,
  CS4Shapes,
  CADSys4,
  Clipper;
  

const
  INTEGER_PRECISION = 10000000000;

type
      TClipperInterface = class
        private
          fClipper: TClipper;
          fClipType: TClipType;         //(ctIntersection, ctUnion, ctDifference, ctXor)
          fJoinType: TJoinType;         //(jtSquare, jtRound, jtMiter);
          fPolyFillType: TPolyFillType; //(pftEvenOdd, pftNonZero, pftPositive, pftNegative);


          fSubject,
          fClip,
          fSolution: TPolygons;

          fSrcGraphicObjList,
          fDestGraphicObjList: TGraphicObjList;
          //fPolygons: TPolygons;

          procedure  SetSrcGraphicObjList(AGraphicObjList:TGraphicObjList);
          procedure  UpdateSubject;

        public
          constructor Create;
          destructor  Destroy; override;
          procedure   Clear;
          procedure   AddGraphicObject(AGraphicObject: TGraphicObject);

          function   Point2DToClipperPoint(APoint2D: TPoint2D): TIntPoint;
          function   ClipperPointToPoint2D(AIntPoint: TIntPoint): TPoint2D;

          function   MakeClipperPolygon(AGraphicObject: TGraphicObject): TPolygon;
          function   MakeClipperPolygons(AGraphicObjList: TGraphicObjList): TPolygons;

          function   MakeGraphicObject(APolygon: TPolygon): TPolyline2D;
          function   MakeGraphicObjects(APolygons: TPolygons): TGraphicObjList;

          procedure  ClipperUnion;
          procedure  ClipperIntersection;
          procedure  ClipperDifference;
          procedure  ClipperXor;
          procedure  ClipperOffset(ADelta: TRealType);

          procedure  ExportToCAD(ACADCmp2D: TCADCmp2D);

          property SrcGraphicObjList: TGraphicObjList read fSrcGraphicObjList write SetSrcGraphicObjList;
          //property Polygons: TPolygons  read  fPolygons  write fPolygons;

      end;

implementation

constructor TClipperInterface.create;
begin
  inherited;
  SetLength(fSubject, 0);
  SetLength(fSolution, 0);
  fClipper            := TClipper.Create;
  fSrcGraphicObjList  := TGraphicObjList.Create;
  fDestGraphicObjList := TGraphicObjList.Create;
end;

procedure TClipperInterface.clear;
begin
  SetLength(fSubject, 0);
  SetLength(fSolution, 0);
  fClipper.Clear;
  fSrcGraphicObjList.Clear;
  fDestGraphicObjList.Clear;
end;

destructor TClipperInterface.destroy;
begin
  Clear;
  fClipper.Free;
  fSrcGraphicObjList.Free;
  fDestGraphicObjList.Free;
  inherited;
end;

procedure  TClipperInterface.ClipperUnion;
begin
  fDestGraphicObjList.Clear;
  fClipType := ctUnion;          //(ctIntersection, ctUnion, ctDifference, ctXor)
  fClipper.Clear;
  fClipper.AddPolygon(fSubject[0], ptSubject);
  fClipper.AddPolygons(fSubject, ptClip);
  fClipper.Execute(fClipType, fSolution, pftEvenOdd, pftEvenOdd);
  fDestGraphicObjList := MakeGraphicObjects(fSolution);
end;

procedure  TClipperInterface.ClipperIntersection;
begin
  fDestGraphicObjList.Clear;
  fClipType := ctIntersection;          //(ctIntersection, ctUnion, ctDifference, ctXor)
  fClipper.Clear;
  fClipper.AddPolygon(fSubject[0], ptSubject);
  fClipper.AddPolygons(fSubject, ptClip);
  fClipper.Execute(fClipType, fSolution, pftEvenOdd, pftEvenOdd);
  fDestGraphicObjList := MakeGraphicObjects(fSolution);
end;

procedure  TClipperInterface.ClipperDifference;
begin
  fDestGraphicObjList.Clear;
  fClipType := ctDifference;          //(ctIntersection, ctUnion, ctDifference, ctXor)
  fClipper.Clear;
  fClipper.AddPolygon(fSubject[0], ptSubject);
  fClipper.AddPolygons(fSubject, ptClip);
  fClipper.Execute(fClipType, fSolution, pftEvenOdd, pftEvenOdd);
  fDestGraphicObjList := MakeGraphicObjects(fSolution);
end;

procedure  TClipperInterface.ClipperXor;
begin
  fDestGraphicObjList.Clear;
  fClipType := ctXor;          //(ctIntersection, ctUnion, ctDifference, ctXor)
  fClipper.Clear;
  fClipper.AddPolygon(fSubject[0], ptSubject);
  fClipper.AddPolygons(fSubject, ptClip);
  fClipper.Execute(fClipType, fSolution, pftEvenOdd, pftEvenOdd);
  fDestGraphicObjList := MakeGraphicObjects(fSolution);
end;

procedure  TClipperInterface.ClipperOffset(ADelta: TRealType);
begin
  fDestGraphicObjList.Clear;
  //fClipper.Clear;
  //TJoinType = (jtSquare, jtRound, jtMiter);
  //fSolution := OffsetPolygons(fSubject, (INTEGER_PRECISION * ADelta), jtMiter, 5);
  fSolution := OffsetPolygons(fSubject, (INTEGER_PRECISION * ADelta), jtRound, 2);
  fDestGraphicObjList := MakeGraphicObjects(fSolution);
end;

procedure  TClipperInterface.SetSrcGraphicObjList(AGraphicObjList: TGraphicObjList);
var TmpIter: TGraphicObjIterator;
TmpClass: TGraphicObjectClass; TmpObj: TGraphicObject;
begin
  TmpIter := AGraphicObjList.GetIterator;
  if TmpIter.Count > 0 then
  begin
    fSrcGraphicObjList.Clear;
    try
      TmpIter.First;
      while TmpIter.Current <> nil do
      begin
        TmpClass := TGraphicObjectClass(TmpIter.Current.ClassType);
        TmpObj := TmpClass.Create(TmpIter.Current.ID);
        TmpObj.Assign(TmpIter.Current);
        fSrcGraphicObjList.Add(TmpObj);
        TmpIter.Next;
      end;
    finally
      TmpIter.Free;
    end;
    UpdateSubject;
  end;
end;

procedure TClipperInterface.UpdateSubject;
begin
  fSubject := MakeClipperPolygons(fSrcGraphicObjList);
end;

procedure TClipperInterface.AddGraphicObject(AGraphicObject: TGraphicObject);
var TmpClass: TGraphicObjectClass; TmpObj: TGraphicObject;
begin
  TmpClass := TGraphicObjectClass(AGraphicObject.ClassType);
  TmpObj   := TmpClass.Create(AGraphicObject.ID);
  TmpObj.Assign(AGraphicObject);
  fSrcGraphicObjList.Add(TmpObj);
  UpdateSubject;
end;

procedure  TClipperInterface.ExportToCAD(ACADCmp2D: TCADCmp2D);
var TmpIter: TGraphicObjIterator;
TmpClass: TGraphicObjectClass; TmpObj: TGraphicObject;
begin
  TmpIter := fDestGraphicObjList.GetIterator;
  TmpIter.First;
  try
    repeat
      TmpClass := TGraphicObjectClass(TmpIter.Current.ClassType);
      TmpObj := TmpClass.Create(TmpIter.Current.ID);
      TmpObj.Assign(TmpIter.Current);
      ACADCmp2D.AddObject(-1, TObject2D(TmpObj));
    until TmpIter.Next = nil;
  finally
    TmpIter.Free;
  end;
end;

function  TClipperInterface.MakeClipperPolygon(AGraphicObject: TGraphicObject): TPolygon;
var i, PointsCount: integer; TmpPolygon:  TPolygon;
begin
  SetLength(TmpPolygon, 0);
  if (AGraphicObject <> nil) then
  begin
    if (AGraphicObject is TLine2D) then
    begin
      SetLength(TmpPolygon, 2);
      TmpPolygon[0] := Point2DToClipperPoint(TransformPoint2D(TLine2D(AGraphicObject).Points[0], TLine2D(AGraphicObject).ModelTransform));
      //TmpPolygon[1] := Point2DToClipperPoint(TLine2D(AGraphicObject).Points[1]);
      TmpPolygon[1] := Point2DToClipperPoint(TransformPoint2D(TLine2D(AGraphicObject).Points[1], TLine2D(AGraphicObject).ModelTransform));
    end else
    if (AGraphicObject is TOutline2D) then
    begin
      PointsCount := TOutline2D(AGraphicObject).ProfilePoints.Count;
      SetLength(TmpPolygon, PointsCount);
      for i := 0 to PointsCount - 1 do
        //TmpPolygon[i] := Point2DToClipperPoint(TOutLine2D(AGraphicObject).ProfilePoints[i]);   
        TmpPolygon[i] := Point2DToClipperPoint(TransformPoint2D(TOutline2D(AGraphicObject).ProfilePoints[i], TOutline2D(AGraphicObject).ModelTransform));
    end else
    begin
      SetLength(TmpPolygon, 0);
    end;
  end;
  result := TmpPolygon;
end;

function  TClipperInterface.MakeClipperPolygons(AGraphicObjList: TGraphicObjList): TPolygons;
var TmpIter: TGraphicObjIterator;
i: integer; TmpPolygons: TPolygons;
begin
  SetLength(TmpPolygons, 0);
  if (AGraphicObjList <> nil) then
  begin
    TmpIter := AGraphicObjList.GetIterator;
    if (TmpIter.Count > 0) then
    begin
      SetLength(TmpPolygons, TmpIter.Count);
      i := 0;
      TmpIter.First;
      try
        while TmpIter.Current <> nil do
        begin
          TmpPolygons[i] := MakeClipperPolygon(TmpIter.Current);
          TmpIter.Next;
          inc(i);
        end;
      finally
        TmpIter.Free;
      end;
    end;
  end;
  result := TmpPolygons;
end;

function  TClipperInterface.MakeGraphicObject(APolygon: TPolygon): TPolyline2D;
var i, PointCount: integer; TmpPolyline2D: TPolyline2D;
begin
  TmpPolyline2D := nil;
  PointCount := Length(APolygon);
  if (PointCount > 0) then
  begin
    TmpPolyline2D := TPolyline2D.Create(-1, []);
    for i := 0 to  PointCount - 1 do
      TmpPolyline2D.ProfilePoints.Add(ClipperPointToPoint2D(APolygon[i]));
    TmpPolyline2D.ProfilePoints.Add(TmpPolyline2D.ProfilePoints[0]);  //Polygon abschliessen.
  end;
  result := TmpPolyline2D;
end;

function  TClipperInterface.MakeGraphicObjects(APolygons: TPolygons): TGraphicObjList;
var i, PolygonsCount, PointCount: integer; TmpPolyline2D: TPolyline2D;  TmpPolygon: TPolygon;
TmpGraphicObjList:  TGraphicObjList;
begin
  TmpGraphicObjList := nil;
  PolygonsCount := Length(APolygons);
  if (PolygonsCount > 0) then
  begin
    TmpGraphicObjList :=  TGraphicObjList.Create;
    for i := 0 to PolygonsCount - 1 do
      TmpGraphicObjList.Add(MakeGraphicObject(APolygons[i]));
  end;
  result := TmpGraphicObjList;
end;

function TClipperInterface.Point2DToClipperPoint(APoint2D: TPoint2D): TIntPoint;
begin
  result.X := round(INTEGER_PRECISION * APoint2D.X);
  result.Y := round(INTEGER_PRECISION * APoint2D.Y);
end;

function TClipperInterface.ClipperPointToPoint2D(AIntPoint: TIntPoint): TPoint2D;
begin
  result.X :=  AIntPoint.X/INTEGER_PRECISION;
  result.Y :=  AIntPoint.Y/INTEGER_PRECISION;
  result.W :=  1;
end;

end.
