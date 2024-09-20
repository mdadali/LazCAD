unit contours;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  CADSys4,
  CS4BaseTypes,
  CS4Shapes;

type

     TJumps = array of TLine2D;

     TContour = class(TContainer2D)
       fStartPoint,
       fEndPoint:   TPoint2D;
       fDirection: TArcDirection;
       fOuterContour: boolean;
       fObjectLength: TRealType;
     public
       property ObjectLength: TRealType read fObjectLength;
     end;

     TContours = array of TContour;

     TCAMDrawing = class
       fCADCmp2D: TCADCmp2D;
       fContours: TContours;
       fJumps: TJumps;
       fCuttingSpeed, //mm/min
       fJumpSpeed,    //mm/min
       fHeatingTime,  //sec
       fCuttingTime,  //sec
       fJumpsTime:  TRealType; //sec

       fLinesCount,
       fArcCount: integer;

       function GetJumpsLength:  TrealType;
       function GetJumpsTime: TrealType;

       function GetTotalPrimitivesLength: TRealType;
       function GetPrimitivesCuttingTime: TRealType;

       function GetTotalHeatingTime:  TrealType;

       function GetTotalProcessTime: TRealType;

     public
       constructor create(ACADCmp2D: TCADCmp2D; AHeatingTime, ACuttingSpeed, AJumpSpeed: TRealType);
       destructor  destroy;
       procedure   init;

       procedure  ProcessDrawing;

       property LinesCount:  integer read fLinesCount;
       property ArcCount  :  integer read fArcCount;

       property TotalProcessTime: TRealType read GetTotalProcessTime;
     end;


implementation


constructor TCAMDrawing.create(ACADCmp2D: TCADCmp2D; AHeatingTime, ACuttingSpeed, AJumpSpeed: TRealType);
begin
  fCADCmp2D     := ACADCmp2D;
  fHeatingTime  := AHeatingTime;
  fCuttingSpeed := ACuttingSpeed;
  fJumpSpeed    := AJumpSpeed;
  init;
end;

destructor TCAMDrawing.destroy;
begin
  SetLength(fContours, 0);
  SetLength(fJumps, 0);
end;

procedure TCAMDrawing.init;
begin
  fCuttingTime := 0;
  fJumpsTime   := 0;
  SetLength(fContours, 0);
  SetLength(fJumps, 0);
  fLinesCount    := 0;
  fArcCount      := 0;
end;

//in mm.
function TCAMDrawing.GetJumpsLength:  TrealType;
var i: integer;
begin
  result := 0;
  for i := 0 to Length(fJumps) - 1  do
    result := result + TLine2D(fJumps[i]).ObjectLength;
end;

//in sec.
function TCAMDrawing.GetJumpsTime: TRealType;
begin
  result := GetJumpsLength / fJumpSpeed;
end;

//in mm.
function TCAMDrawing.GetTotalPrimitivesLength: TRealType;
var i: integer;
begin
  result := 0;
  for i := 0 to Length(fContours) - 1 do
    result := result +  TPrimitive2D(fContours[i]).ObjectLength;
end;

//in sec.
function TCAMDrawing.GetPrimitivesCuttingTime: TRealType;
begin
  result := GetTotalPrimitivesLength / fCuttingSpeed;
end;

//in sec.
function TCAMDrawing.GetTotalHeatingTime:  TrealType;
begin
  result := Length(fJumps) * fHeatingTime;
end;

//in sec.
function TCAMDrawing.GetTotalProcessTime: TRealType;
begin
  result := GetJumpsTime + GetTotalHeatingTime +  GetPrimitivesCuttingTime;
end;

procedure TCAMDrawing.ProcessDrawing;
begin

end;


end.

