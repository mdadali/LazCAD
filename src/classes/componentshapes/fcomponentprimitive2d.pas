unit fcomponentprimitive2d;
{$M+}
{$IFNDEF VER150}
{$MODE Delphi}
{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, Graphics,  Forms, Dialogs,
  CADSys4,
  CS4BaseTypes,
  CS4Shapes,
  fobjectposition,
  camh,
  fcomponentobject2d;

type

  TCADSysPrimitive2D = class(TCADSysObject2D)
  private
    function  GetLeft: TRealType;
    function  GetTop: TRealType;
    function  GetRight: TRealType;
    function  GetBottom: TRealType;

    procedure  SetLeft(AValue: TRealType);
    procedure  SetTop(AValue: TRealType);
    procedure  SetRight(AValue: TRealType);
    procedure  SetBottom(AValue: TRealType);

    function  GetBottomLeft:  TPoint2D;
    function  GetBottomRight: TPoint2D;
    function  GetTopRight:    TPoint2D;
    function  GetTopLeft:     TPoint2D;

    function  GetMiddlePoint: TPoint2D;

    function  GetStartPointX: TRealType;
    function  GetStartPointY: TRealType;
    function  GetEndPointX: TRealType;
    function  GetEndPointY: TRealType;
    function  GetMiddlePointX: TRealType;
    function  GetMiddlePointY: TRealType;

    procedure SetStartPointX(AValue: TRealType);
    procedure SetStartPointY(AValue: TRealType);
    procedure SetEndPointX(AValue: TRealType);
    procedure SetEndPointY(AValue: TRealType);
    procedure SetMiddlePointX(AValue: TRealType);
    procedure SetMiddlePointY(AValue: TRealType);

    function  GetOLength: TRealType;
    function  GetAngle: TRealType;
    procedure SetAngle(AAngle: TRealType);
    function  GetKerfType: TKerfType;
    procedure SetKerfType(AKerfType: TKerfType);
  protected
     fPrimitive2D: TPrimitive2D;
  public
    constructor Create;
    destructor  Destroy;
    function  GetGraphicObject: TGraphicObject; override;
    procedure SetGraphicObject(AGraphicObject: TGraphicObject); override;
  published
    //property  PointsCount: word          read GetPointsCount;

    property  Left:     TRealType        read GetLeft        write SetLeft;
    property  Top:      TRealType        read GetTop         write SetTop;
    property  Right:    TRealType        read GetRight       write SetRight;
    property  Bottom:   TRealType        read GetBottom      write SetBottom;

    property  MiddlePointX:  TRealType        read GetMiddlePointX  write SetMiddlePointX;
    property  MiddlePointY:  TRealType        read GetMiddlePointY  write SetMiddlePointY;
    property  StartPointX:   TRealType        read GetStartPointX   write SetStartPointX;
    property  StartPointtY:  TRealType        read GetStartPointY   write SetStartPointY;
    property  EndPointX:     TRealType        read GetEndPointX     write SetEndPointX;
    property  EndPointtY:    TRealType        read GetEndPointY     write SetEndPointY;

    //property  KerfType: TKerfType  read GetKerfType write SetKerfType;

    property ObjectLength: TRealType read GetOLength;
    //property Angle: TRealType  read GetAngle write SetAngle;
    //property  ObjectPosition2D: TObjectPosition2D read GetObjectPosition2D; // write SetObjectPosition2D;


end;


implementation

function  TCADSysPrimitive2D.GetGraphicObject: TGraphicObject;
begin
  result := fPrimitive2D;
end;

procedure TCADSysPrimitive2D.SetGraphicObject(AGraphicObject: TGraphicObject);
begin
  fPrimitive2D := TPrimitive2D(AGraphicObject);
  //fObject2D := TObject2D(AGraphicObject);
  //fGraphicObject := AGraphicObject;
end;

constructor TCADSysPrimitive2D.Create;
begin
  inherited Create;
end;

destructor  TCADSysPrimitive2D.Destroy;
begin
  inherited;
end;

{function TTCADSysPrimitive2D.GetPointsCount: word;
begin
  if fPrimitive2D <> nil then
    result := fPrimitive2D.Points.Count;
end;
}
function  TCADSysPrimitive2D.GetLeft: TRealType;
begin
  if fPrimitive2D <> nil then
    result := fPrimitive2D.Left;
end;

function  TCADSysPrimitive2D.GetTop: TRealType;
begin
  if fPrimitive2D <> nil then
    result := fPrimitive2D.Top;
end;

function  TCADSysPrimitive2D.GetRight: TRealType;
begin
  if fPrimitive2D <> nil then
    result := fPrimitive2D.Right;
end;

function  TCADSysPrimitive2D.GetBottom: TRealType;
begin
  if fPrimitive2D <> nil then
    result := fPrimitive2D.Bottom;
end;

procedure  TCADSysPrimitive2D.SetLeft(AValue: TRealType);
begin
  if fPrimitive2D <> nil then
    fPrimitive2D.Left := AValue;
end;

procedure  TCADSysPrimitive2D.SetTop(AValue: TRealType);
begin
  if fPrimitive2D <> nil then
    fPrimitive2D.Top := AValue;
end;

procedure  TCADSysPrimitive2D.SetRight(AValue: TRealType);
begin
  if fPrimitive2D <> nil then
    fPrimitive2D.Right := AValue;
end;

procedure  TCADSysPrimitive2D.SetBottom(AValue: TRealType);
begin
  if fPrimitive2D <> nil then
    fPrimitive2D.Bottom := AValue;
end;

function  TCADSysPrimitive2D.GetBottomLeft:  TPoint2D;
begin
  result := fPrimitive2D.LeftBottom;
end;

function  TCADSysPrimitive2D.GetBottomRight: TPoint2D;
begin
    result := fPrimitive2D.RightBottom;
end;

function  TCADSysPrimitive2D.GetTopRight:    TPoint2D;
begin
  result := fPrimitive2D.RightTop;
end;

function  TCADSysPrimitive2D.GetTopLeft:     TPoint2D;
begin
  result := fPrimitive2D.LeftTop;
end;

function  TCADSysPrimitive2D.GetMiddlePointX: TRealType;
begin
  result := fPrimitive2D.MiddlePoint.X;
end;

function  TCADSysPrimitive2D.GetMiddlePointY: TRealType;
begin
  result := fPrimitive2D.MiddlePoint.Y;
end;

function  TCADSysPrimitive2D.GetMiddlePoint: TPoint2D;
begin
   result.X := fPrimitive2D.MiddlePoint.X;
   result.Y := fPrimitive2D.MiddlePoint.Y;
   result.W := 1;
end;

function  TCADSysPrimitive2D.GetStartPointX: TRealType;
begin
  //result := fPrimitive2D.StartPoint.X;
end;

function  TCADSysPrimitive2D.GetStartPointY: TRealType;
begin
  //result := fPrimitive2D.StartPoint.Y;
end;

function  TCADSysPrimitive2D.GetEndPointX: TRealType;
begin
  //result := fPrimitive2D.EndPoint.X;
end;

function  TCADSysPrimitive2D.GetEndPointY: TRealType;
begin
  //result := fPrimitive2D.EndPoint.Y;
end;

procedure TCADSysPrimitive2D.SetMiddlePointX(AValue: TRealType);
begin
  //fPrimitive2D.CenterPointX := AValue;
end;

procedure TCADSysPrimitive2D.SetMiddlePointY(AValue: TRealType);
begin
  //fPrimitive2D.CenterPointY := AValue;
end;

procedure TCADSysPrimitive2D.SetStartPointX(AValue: TRealType);
begin
  //fPrimitive2D.StartPointX := AValue;
end;

procedure TCADSysPrimitive2D.SetStartPointY(AValue: TRealType);
begin
  //fPrimitive2D.StartPointY := AValue;
end;

procedure TCADSysPrimitive2D.SetEndPointX(AValue: TRealType);
begin
  //fPrimitive2D.EndPointX := AValue;
end;

procedure TCADSysPrimitive2D.SetEndPointY(AValue: TRealType);
begin
  //fPrimitive2D.EndPointY := AValue;
end;

function TCADSysPrimitive2D.GetOLength;
begin
  //fPrimitive2D.ObjectLength;
end;

function  TCADSysPrimitive2D.GetAngle: TRealType;
begin
  //fPrimitive2D.Angle;
end;

procedure TCADSysPrimitive2D.SetAngle(AAngle: TRealType);
begin
  //fPrimitive2D.Angle := AAngle;
end;

function  TCADSysPrimitive2D.GetKerfType: TKerfType;
begin
  result := TKerfType(fPrimitive2D.fReserveInt1);
end;

procedure TCADSysPrimitive2D.SetKerfType(AKerfType: TKerfType);
begin
  fPrimitive2D.fReserveInt1 := ord(AKerfType);
end;

end.

