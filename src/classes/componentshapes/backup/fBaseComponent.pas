unit fBaseComponent;
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
  //applicationh,
  camh;

type


tOptions = (Opt1, Opt2, Opt3);
tOptionsFlags = set of tOptions;

TCADSysBaseComponent2D = class(TPersistent)
//TCADSysBaseComponent2D = class(TComponent)
  private
    fObjectPosition2D: TObjectPosition2D;

    Coordinates: tOptionsFlags;
    fKerfType: TKerfType;

    //function  GetPointsCount: word;

    function  GetClassName: string;
    function  GetID: longint;

    function  GetVisible: boolean;
    procedure SetVisible(AVisible: boolean);
    function  GetEnabled: boolean;
    procedure SetEnabled(AEnabled: boolean);
    function  GetShowDirection: boolean;
    procedure SetShowDirection(AValue: boolean);
    function  GetDirection: TArcDirection;
    procedure SetDirection(ADirection: TArcDirection);

    function  GetTag: longint;
    procedure SetTag(ATag: longint);

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

    function  GetCenterPoint: TPoint2D;

    function  GetStartPointX: TRealType;
    function  GetStartPointY: TRealType;
    function  GetEndPointX: TRealType;
    function  GetEndPointY: TRealType;
    function  GetCenterPointX: TRealType;
    function  GetCenterPointY: TRealType;

    procedure SetStartPointX(AValue: TRealType);
    procedure SetStartPointY(AValue: TRealType);
    procedure SetEndPointX(AValue: TRealType);
    procedure SetEndPointY(AValue: TRealType);
    procedure SetCenterPointX(AValue: TRealType);
    procedure SetCenterPointY(AValue: TRealType);

    {procedure SetPenWidth(APenWidth: word);
    function  GetPenWidth: word;
    procedure SetPenStyle(APenStyle: TPenStyle);
    function  GetPenStyle: TPenStyle;
    procedure SetPenColor(AColor: TColor);
    function  GetPenColor: TColor;

    procedure SetBrushStyle(ABrushStyle: TBrushStyle);
    function  GetBrushStyle: TBrushStyle;

    procedure SetBrushColor(AColor: TColor);
    function  GetBrushColor: TColor;

    procedure SetFilled(AValue: boolean);
    function  GetFilled: boolean;

    procedure SetLayerID(ALayerID: TLayerID);
    function  GetLayerID: TLayerID;
     }
    procedure SetLayerName(ALayerName: TLayerName);
    function  GetLayerName: TLayerName;

    function  GetOLength: TRealType;
    function  GetAngle: TRealType;
    procedure SetAngle(AAngle: TRealType);
    function  GetArea: TRealType;
    function  GetKerfType: TKerfType;
    procedure SetKerfType(AKerfType: TKerfType);

    function  GetObjectPosition2D: TObjectPosition2D;
    procedure SetObjectPosition2D(AObjectPosition2D: TObjectPosition2D);
  protected
     fPrimitive2D: TClosedPrimitive2D;  //TPrimitive2D;

     property   Direction: TArcDirection read GetDirection write SetDirection;

     //property BrushColor: TColor         read  GetBrushColor     write SetBrushColor;
     //property BrushStyle: TBrushStyle    read  GetBrushStyle     write SetBrushStyle;
     //property Filled:     boolean        read  GetFilled         write SetFilled;
  public
    constructor Create;
    destructor  Destroy;
    procedure   ObjectPositionChanged(Sender: TObject);
    property    Primitive2D: TPrimitive2D read  fPrimitive2D;

  published
    property  ID: longint                read GetID;
    property  ClassName: string          read GetClassName;
    property  Visible:  boolean          read GetVisible    write SetVisible;
    property  Enabled:  boolean          read GetEnabled    write SetEnabled;
    property  Tag:      longint          read GetTag        write SetTag;

    //property  PointsCount: word          read GetPointsCount;

    property  Left:     TRealType        read GetLeft        write SetLeft;
    property  Top:      TRealType        read GetTop         write SetTop;
    property  Right:    TRealType        read GetRight       write SetRight;
    property  Bottom:   TRealType        read GetBottom      write SetBottom;

    property  CenterPointX:  TRealType        read GetCenterPointX  write SetCenterPointX;
    property  CenterPointY:  TRealType        read GetCenterPointY  write SetCenterPointY;
    property  StartPointX:   TRealType        read GetStartPointX   write SetStartPointX;
    property  StartPointtY:  TRealType        read GetStartPointY   write SetStartPointY;
    property  EndPointX:     TRealType        read GetEndPointX     write SetEndPointX;
    property  EndPointtY:    TRealType        read GetEndPointY     write SetEndPointY;

    property  ShowDirection: boolean read GetShowDirection  write SetShowDirection;

    //property  PenWidth: word                   read  GetPenWidth    write SetPenWidth;
    //property  PenStyle: TPenStyle              read  GetPenStyle    write SetPenStyle;
    //property  PenColor: TColor                 read  GetPenColor    write SetPenColor;
    //property  LayerID: TLayerID                read  GetLayerID;    //write SetLayerID;
    property  LayerName: TLayerName            read  GetLayerName  write SetLayerName;

    property  Test: tOptionsFlags  read Coordinates write Coordinates;
    property  KerfType: TKerfType  read GetKerfType write SetKerfType;

    property ObjectLength: TRealType read GetOLength;
    property ObjectArea: TRealType read GetArea;
    //property Angle: TRealType  read GetAngle write SetAngle;
    property  ObjectPosition2D: TObjectPosition2D read GetObjectPosition2D write SetObjectPosition2D;
end;


implementation

constructor TCADSysBaseComponent2D.Create;
begin
  inherited Create;
  fObjectPosition2D := TObjectPosition2D.create;
  fObjectPosition2D.OnChange := ObjectPositionChanged; // Methode verknüpfen
  //fKerfType := OuterContourCW;  //OuterContour-Clockwise
end;

procedure TCADSysBaseComponent2D.ObjectPositionChanged(Sender: TObject);
begin
  //SetObjectPosition2D(fObjectPosition2D);
  // Aktualisiere die Primitive basierend auf den Änderungen der Position
  fPrimitive2D.Top := fObjectPosition2D.Top;
  fPrimitive2D.Left := fObjectPosition2D.Left;
  fPrimitive2D.Bottom := fObjectPosition2D.Bottom;
  fPrimitive2D.Right := fObjectPosition2D.Right;
end;

destructor  TCADSysBaseComponent2D.Destroy;
begin
  fObjectPosition2D.Free;
  inherited;
end;

function  TCADSysBaseComponent2D.GetObjectPosition2D: TObjectPosition2D;
begin
  fObjectPosition2D.Top    := self.fPrimitive2D.Top;
  fObjectPosition2D.Left   := self.fPrimitive2D.Left;
  fObjectPosition2D.Bottom := self.fPrimitive2D.Bottom;
  fObjectPosition2D.Right  := self.fPrimitive2D.Right;
  result := fObjectPosition2D;
end;

procedure TCADSysBaseComponent2D.SetObjectPosition2D(AObjectPosition2D: TObjectPosition2D);
begin
  self.fPrimitive2D.Top     := fObjectPosition2D.Top;
  self.fPrimitive2D.Left    := fObjectPosition2D.Left;
  self.fPrimitive2D.Bottom  := fObjectPosition2D.Bottom;
  self.fPrimitive2D.Right   := fObjectPosition2D.Right;
end;

function TCADSysBaseComponent2D.GetID: longint;
begin
  if fPrimitive2D <> nil then
    result := fPrimitive2D.ID;
end;

function   TCADSysBaseComponent2D.GetClassName: string;
begin
  if fPrimitive2D <> nil then
    result := fPrimitive2D.ClassName;
end;

function  TCADSysBaseComponent2D.GetVisible: boolean;
begin
  if fPrimitive2D <> nil then
    result := fPrimitive2D.Visible;
end;

procedure TCADSysBaseComponent2D.SetVisible(AVisible: boolean);
begin
  if fPrimitive2D <> nil then
    fPrimitive2D.Visible := AVisible;
end;

function  TCADSysBaseComponent2D.GetEnabled: boolean;
begin
    if fPrimitive2D <> nil then
      result := fPrimitive2D.Enabled;
end;

procedure TCADSysBaseComponent2D.SetEnabled(AEnabled: boolean);
begin
  if fPrimitive2D <> nil then
    fPrimitive2D.Enabled := AEnabled;
end;

function  TCADSysBaseComponent2D.GetShowDirection: boolean;
begin
  result := fPrimitive2D.ShowDirection;
end;

procedure TCADSysBaseComponent2D.SetShowDirection(AValue: boolean);
begin
  if fPrimitive2D <> nil then
    fPrimitive2D.ShowDirection := AValue;
end;

function  TCADSysBaseComponent2D.GetDirection: TArcDirection;
begin
  result := fPrimitive2D.Direction;
end;

procedure TCADSysBaseComponent2D.SetDirection(ADirection: TArcDirection);
begin
  fPrimitive2D.Direction := ADirection;
end;

function  TCADSysBaseComponent2D.GetTag: longint;
begin
  if fPrimitive2D <> nil then
    result := fPrimitive2D.Tag;
end;

procedure TCADSysBaseComponent2D.SetTag(ATag: longint);
begin
  if fPrimitive2D <> nil then
    fPrimitive2D.Tag := ATag;
end;

{function TCADSysBaseComponent2D.GetPointsCount: word;
begin
  if fPrimitive2D <> nil then
    result := fPrimitive2D.Points.Count;
end;
}
function  TCADSysBaseComponent2D.GetLeft: TRealType;
begin
  if fPrimitive2D <> nil then
    result := fPrimitive2D.Left;
end;

function  TCADSysBaseComponent2D.GetTop: TRealType;
begin
  if fPrimitive2D <> nil then
    result := fPrimitive2D.Top;
end;

function  TCADSysBaseComponent2D.GetRight: TRealType;
begin
  if fPrimitive2D <> nil then
    result := fPrimitive2D.Right;
end;

function  TCADSysBaseComponent2D.GetBottom: TRealType;
begin
  if fPrimitive2D <> nil then
    result := fPrimitive2D.Bottom;
end;

procedure  TCADSysBaseComponent2D.SetLeft(AValue: TRealType);
begin
  if fPrimitive2D <> nil then
  begin
    fPrimitive2D.Left := AValue;
  end;
end;

procedure  TCADSysBaseComponent2D.SetTop(AValue: TRealType);
begin
  if fPrimitive2D <> nil then
    fPrimitive2D.Top := AValue;
end;

procedure  TCADSysBaseComponent2D.SetRight(AValue: TRealType);
begin
  if fPrimitive2D <> nil then
    fPrimitive2D.Right := AValue;
end;

procedure  TCADSysBaseComponent2D.SetBottom(AValue: TRealType);
begin
  if fPrimitive2D <> nil then
    fPrimitive2D.Bottom := AValue;
end;

function  TCADSysBaseComponent2D.GetBottomLeft:  TPoint2D;
begin
  result := fPrimitive2D.LeftBottom;
end;

function  TCADSysBaseComponent2D.GetBottomRight: TPoint2D;
begin
    result := fPrimitive2D.RightBottom;
end;

function  TCADSysBaseComponent2D.GetTopRight:    TPoint2D;
begin
  result := fPrimitive2D.RightTop;
end;

function  TCADSysBaseComponent2D.GetTopLeft:     TPoint2D;
begin
  result := fPrimitive2D.LeftTop;
end;

function  TCADSysBaseComponent2D.GetCenterPointX: TRealType;
begin
  result := fPrimitive2D.CenterPoint.X;
end;

function  TCADSysBaseComponent2D.GetCenterPointY: TRealType;
begin
  result := fPrimitive2D.CenterPoint.Y;
end;

function  TCADSysBaseComponent2D.GetCenterPoint: TPoint2D;
begin
   result.X := fPrimitive2D.CenterPoint.X;
   result.Y := fPrimitive2D.CenterPoint.Y;
   result.W := 1;
end;

function  TCADSysBaseComponent2D.GetStartPointX: TRealType;
begin
  result := fPrimitive2D.StartPoint.X;
end;

function  TCADSysBaseComponent2D.GetStartPointY: TRealType;
begin
  result := fPrimitive2D.StartPoint.Y;
end;

function  TCADSysBaseComponent2D.GetEndPointX: TRealType;
begin
  result := fPrimitive2D.EndPoint.X;
end;

function  TCADSysBaseComponent2D.GetEndPointY: TRealType;
begin
  result := fPrimitive2D.EndPoint.Y;
end;

procedure TCADSysBaseComponent2D.SetCenterPointX(AValue: TRealType);
begin
  fPrimitive2D.CenterPointX := AValue;
end;

procedure TCADSysBaseComponent2D.SetCenterPointY(AValue: TRealType);
begin
  fPrimitive2D.CenterPointY := AValue;
end;

procedure TCADSysBaseComponent2D.SetStartPointX(AValue: TRealType);
begin
  fPrimitive2D.StartPointX := AValue;
end;

procedure TCADSysBaseComponent2D.SetStartPointY(AValue: TRealType);
begin
  fPrimitive2D.StartPointY := AValue;
end;

procedure TCADSysBaseComponent2D.SetEndPointX(AValue: TRealType);
begin
  fPrimitive2D.EndPointX := AValue;
end;

procedure TCADSysBaseComponent2D.SetEndPointY(AValue: TRealType);
begin
  fPrimitive2D.EndPointY := AValue;
end;

function TCADSysBaseComponent2D.GetOLength;
begin
  result := fPrimitive2D.ObjectLength;
end;

function  TCADSysBaseComponent2D.GetArea: TRealType;
begin
  result := fPrimitive2D.Area;
end;

function  TCADSysBaseComponent2D.GetAngle: TRealType;
begin
  result := fPrimitive2D.Angle;
end;

procedure TCADSysBaseComponent2D.SetAngle(AAngle: TRealType);
begin
  fPrimitive2D.Angle := AAngle;
end;

function  TCADSysBaseComponent2D.GetKerfType: TKerfType;
begin
  result := TKerfType(fPrimitive2D.ReserveInt1);
end;

procedure TCADSysBaseComponent2D.SetKerfType(AKerfType: TKerfType);
begin
  fPrimitive2D.ReserveInt1 := ord(AKerfType);
end;

{
function  TCADSysBaseComponent2D.GetPenColor: TColor;
begin
  if fPrimitive2D <> nil then
    result := fPrimitive2D.PenColor;
end;
procedure TCADSysBaseComponent2D.SetPenColor(AColor: TColor);
begin
  if fPrimitive2D <> nil then
    fPrimitive2D.PenColor := AColor;
end;

function  TCADSysBaseComponent2D.GetPenStyle: TPenStyle;
begin
  if fPrimitive2D <> nil then
    //result := ord(fPrimitive2D.PenStyle);
    result := fPrimitive2D.PenStyle;
end;

procedure TCADSysBaseComponent2D.SetPenStyle(APenStyle: TPenStyle);
begin
  if fPrimitive2D <> nil then
    fPrimitive2D.PenStyle := APenStyle;
end;

function  TCADSysBaseComponent2D.GetPenWidth: word;
begin
  if fPrimitive2D <> nil then
    result := fPrimitive2D.PenWidth;
end;

procedure TCADSysBaseComponent2D.SetPenWidth(APenWidth: word);
begin
  if fPrimitive2D <> nil then
    fPrimitive2D.PenWidth := APenWidth;
end;

procedure TCADSysBaseComponent2D.SetBrushStyle(ABrushStyle: TBrushStyle);
begin
  if fPrimitive2D <> nil then
    fPrimitive2D.BrushStyle := ABrushStyle;
end;

function  TCADSysBaseComponent2D.GetBrushStyle: TBrushStyle;
begin
  if fPrimitive2D <> nil then
    result := fPrimitive2D.BrushStyle;
end;

procedure TCADSysBaseComponent2D.SetBrushColor(AColor: TColor);
begin
  if fPrimitive2D <> nil then
    fPrimitive2D.BrushColor := AColor;
end;

function  TCADSysBaseComponent2D.GetBrushColor: TColor;
begin
  if fPrimitive2D <> nil then
    result := fPrimitive2D.BrushColor;
end;

procedure TCADSysBaseComponent2D.SetFilled(AValue: boolean);
begin
  if fPrimitive2D <> nil then
    fPrimitive2D.Filled := AValue;
end;

function  TCADSysBaseComponent2D.GetFilled: boolean;
begin
  if fPrimitive2D <> nil then
  result := fPrimitive2D.Filled;
end;


procedure TCADSysBaseComponent2D.SetLayerID(ALayerID: TLayerID);
begin
  if fPrimitive2D <> nil then
    fPrimitive2D.LayerID := ALayerID;
end;

function TCADSysBaseComponent2D.GetLayerID: TLayerID;
begin
  result := -1;
  if fPrimitive2D <> nil then result := fPrimitive2D.LayerID;
end;
}

procedure TCADSysBaseComponent2D.SetLayerName(ALayerName: TLayerName);
var TmpLayer: TLayer;
begin
  if fPrimitive2D <> nil then
  begin
    TmpLayer :=  fPrimitive2D.OwnerCAD.Layers.LayerByName[ALayerName];
    if (TmpLayer <> nil) then
    begin
      fPrimitive2D.LayerIndex := TmpLayer.LayerIndex;
    end else
      Raise ECADSysException.Create('BaseComponent.SetLayerName: Layer do not exists!');
  end;
end;


function  TCADSysBaseComponent2D.GetLayerName: TLayerName;
begin
  if fPrimitive2D <> nil then
    result := fPrimitive2D.OwnerCAD.Layers.Layers[fPrimitive2D.LayerIndex].Name;
end;

{
function  TCADSysBaseComponent2D.GetDrawAsPolygon: boolean;
begin
  if fPrimitive2D <> nil then
    result := fPrimitive2D.DrawAsPolygon;
end;


procedure TCADSysBaseComponent2D.SetDrawAsPolygon(AValue: boolean);
begin
  if fPrimitive2D <> nil then
    fPrimitive2D.DrawAsPolygon := AValue;
end;

function  TCADSysBaseComponent2D.GetKerfInfo: TKerfInfo;
begin
  if fPrimitive2D <> nil then
    result := TKerfInfo(fPrimitive2D.ReserveInt1);
end;

procedure TCADSysBaseComponent2D.SetKerfInfo(AKerfInfo: TKerfInfo);
begin
  if fPrimitive2D <> nil then
    fPrimitive2D.ReserveInt1 := ord(AKerfInfo);
end;

function  TCADSysBaseComponent2D.GetCNCCommandBefore: TCNCCommand;
begin
  if fPrimitive2D <> nil then
    result := TCNCCommand(fPrimitive2D.ReserveInt2);
end;

procedure TCADSysBaseComponent2D.SetCNCCommandBefore(ACNCCommand: TCNCCommand);
begin
  if fPrimitive2D <> nil then
    fPrimitive2D.ReserveInt2 := ord(ACNCCommand);
end;

function  TCADSysBaseComponent2D.GetCNCCommandAfter: TCNCCommand;
begin
  if fPrimitive2D <> nil then
    result := TCNCCommand(fPrimitive2D.ReserveInt3);
end;

procedure TCADSysBaseComponent2D.SetCNCCommandAfter(ACNCCommand: TCNCCommand);
begin
  if fPrimitive2D <> nil then
    fPrimitive2D.ReserveInt3 := ord(ACNCCommand);
end;

function  TCADSysBaseComponent2D.GetCAMSide: TCAMSide;
begin
  if fPrimitive2D <> nil then
    result := TCAMSide(fPrimitive2D.ReserveInt4);
end;

procedure TCADSysBaseComponent2D.SetCAMSide(ACAMSide: TCAMSide);
begin
  if fPrimitive2D <> nil then
    fPrimitive2D.ReserveInt4 := ord(ACAMSide);
end;

function  TCADSysBaseComponent2D.GetBrushColor: TColor;
begin
  if fPrimitive2D <> nil then
    result := fPrimitive2D.BrushColor;
end;

procedure TCADSysBaseComponent2D.SetBrushColor(AColor: TColor);
begin
  if fPrimitive2D <> nil then
    fPrimitive2D.BrushColor := AColor;
end;

function  TCADSysBaseComponent2D.GetBrushStyle: TBrushStyle;
begin
  if fPrimitive2D <> nil then
    result := fPrimitive2D.BrushStyle;
end;

procedure TCADSysBaseComponent2D.SetBrushStyle(ABrushStyle: TBrushStyle);
begin
  if fPrimitive2D <> nil then
    fPrimitive2D.BrushStyle := ABrushStyle;
end;

function  TCADSysBaseComponent2D.GetArrowStyle: word;
begin
  if fPrimitive2D <> nil then
    result := fPrimitive2D.ArrowStyle;
end;

procedure TCADSysBaseComponent2D.SetArrowStyle(AArrowStyle: word);
begin
  if fPrimitive2D <> nil then
    fPrimitive2D.ArrowStyle := AArrowStyle;
end;


function TCADSysBaseComponent2D.GetPreparedLayer(AColor:TColor; APenstyle: integer; APenWidth: integer): word;
begin
  if AColor = clBlack then
    case APenstyle of
      0: case APenWidth of
           1:  result := 0;
           2:  result := 1;
           3:  result := 2;
         end;
      1: case APenWidth of
           1:  result := 3;
           2:  result := 4;
           3:  result := 5;
         end;
      2: case APenWidth of
           1:  result := 6;
           2:  result := 7;
           3:  result := 8;
         end;
      3: case APenWidth of
           1:  result := 9;
           2:  result := 10;
           3:  result := 11;
         end;
    end
  else if AColor = clMaroon then
  case APenstyle of
    0: case APenWidth of
         1:  result := 12;
         2:  result := 13;
         3:  result := 14;
       end;
    1: case APenWidth of
         1:  result := 15;
         2:  result := 16;
         3:  result := 17;
       end;
    2: case APenWidth of
         1:  result := 18;
         2:  result := 19;
         3:  result := 20;
       end;
    3: case APenWidth of
         1:  result := 21;
         2:  result := 22;
         3:  result := 23;
       end;
  end
  else if AColor = clGreen then
  case APenstyle of
    0: case APenWidth of
         1:  result := 24;
         2:  result := 25;
         3:  result := 26;
       end;
    1: case APenWidth of
         1:  result := 27;
         2:  result := 28;
         3:  result := 29;
       end;
    2: case APenWidth of
         1:  result := 30;
         2:  result := 31;
         3:  result := 32;
       end;
    3: case APenWidth of
         1:  result := 33;
         2:  result := 34;
         3:  result := 35;
       end;
  end
  else if AColor = clOlive then
  case APenstyle of
    0: case APenWidth of
         1:  result := 36;
         2:  result := 37;
         3:  result := 38;
       end;
    1: case APenWidth of
         1:  result := 39;
         2:  result := 40;
         3:  result := 41;
       end;
    2: case APenWidth of
         1:  result := 42;
         2:  result := 43;
         3:  result := 44;
       end;
    3: case APenWidth of
         1:  result := 45;
         2:  result := 46;
         3:  result := 47;
       end;
  end;

end;}

end.

