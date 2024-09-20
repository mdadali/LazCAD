unit fComponentLine;

{$mode delphi}

interface

uses
  Classes, SysUtils,
  CADSys4,
  CS4BaseTypes,
  CS4Shapes;

type

TCADSysLine2D = class(TPersistent)

  private
    fLine2D: TLine2D;
    function   GetLine2D: TLine2D;
    procedure  SetLine2D(ALine2D: TLine2D);

    function   GetClassName: string;

    function   GetStartPointX: single;
    function   GetStartPointY: single;
    function   GetEndPointX: single;
    function   GetEndPointY: single;

    function   GetID     : LongInt;
    function   GetLayer  : Byte;
    function   GetEnabled: Boolean;
    function   GetVisible: boolean;
    function   GetTag    : LongInt;

    procedure  SetLayer(ALayer: byte);
    procedure  SetEnabled(AEnabled: boolean);
    procedure  SetVisible(AVisible: boolean);
    procedure  SetTag(ATag: LongInt);

    procedure  SetStartPointX(AValue: single);
    procedure  SetStartPointY(AValue: single);
    procedure  SetEndPointX(AValue: single);
    procedure  SetEndPointY(AValue: single);

  public
    constructor Create;
    property Line2D: TLine2D read GetLine2D write SetLine2D;

  published

    property ID         : LongInt  read GetID;
    property ClassName  : string   read GetClassName;
    property Layer      : byte     read GetLayer          write SetLayer;
    property Enabled    : boolean  read GetEnabled        write SetEnabled;
    property Visible    : boolean  read GetVisible        write SetVisible;
    property StartPointX: single   read GetStartPointX    write SetStartPointX;
    property StartPointY: single   read GetStartPointY    write SetStartPointY;
    property EndPointX:   single   read GetEndPointX      write SetEndPointX;
    property EndPointY:   single   read GetEndPointY      write SetEndPointY;
    property Tag:         LongInt  read GetTag            write SetTag;

end;


implementation

constructor TCADSysLine2D.create;
begin
  inherited create;
end;

function   TCADSysLine2D.GetClassName: string;
begin
  if  (fLine2D <> nil) then
    result := fLine2D.ClassName
  else
    result := '';
end;

function TCADSysLine2D.GetLine2D: TLine2D;
begin
  result := fLine2D;
end;

procedure TCADSysLine2D.SetLine2D(ALine2D: TLine2D);
begin
  fLine2D := ALine2D;
end;

function TCADSysLine2D.GetStartPointX: single;
begin
  result := TPrimitive2D(fLine2D).Points[0].X;
end;

function TCADSysLine2D.GetStartPointY: single;
begin
  result := TPrimitive2D(fLine2D).Points[0].Y;
end;

function TCADSysLine2D.GetEndPointX: single;
begin
  result := TPrimitive2D(fLine2D).Points[1].X;
end;

function TCADSysLine2D.GetEndPointY: single;
begin
  result := TPrimitive2D(fLine2D).Points[1].Y;
end;

procedure  TCADSysLine2D.SetStartPointX(AValue: single);
var hPoint2D: TPoint2D;
begin
  hPoint2D.X := AValue;
  hPoint2D.Y := fLine2D.Points.Points[0].Y;
  hPoint2D.W := fLine2D.Points.Points[0].W;
  fLine2D.Points.Delete(0);
  fLine2D.Points.Insert(0, hPoint2D);
end;

procedure  TCADSysLine2D.SetStartPointY(AValue: single);
var hPoint2D: TPoint2D;
begin
  hPoint2D.Y := AValue;
  hPoint2D.W := fLine2D.Points.Points[0].W;
  hPoint2D.X := fLine2D.Points.Points[0].X;
  fLine2D.Points.Delete(0);
  fLine2D.Points.Insert(0, hPoint2D);
end;

procedure  TCADSysLine2D.SetEndPointX(AValue: single);
var hPoint2D: TPoint2D;
begin
  hPoint2D.X := AValue;
  hPoint2D.Y := fLine2D.Points.Points[1].Y;
  hPoint2D.W := fLine2D.Points.Points[1].W;
  fLine2D.Points.Delete(1);
  fLine2D.Points.Add(hPoint2D);
end;

procedure  TCADSysLine2D.SetEndPointY(AValue: single);
var hPoint2D: TPoint2D;
begin
  hPoint2D.Y := AValue;
  hPoint2D.W := fLine2D.Points.Points[1].W;
  hPoint2D.X := fLine2D.Points.Points[1].X;
  fLine2D.Points.Delete(1);
  fLine2D.Points.Add(hPoint2D);
end;

function   TCADSysLine2D.GetID     : LongInt;
begin
  result :=  fLine2D.ID;
end;

function   TCADSysLine2D.GetLayer  : Byte;
begin
  result :=  fLine2D.Layer;
end;

function   TCADSysLine2D.GetEnabled: Boolean;
begin
  result :=  fLine2D.Enabled;
end;

function   TCADSysLine2D.GetVisible: boolean;
begin
  result :=  fLine2D.Visible;
end;

function   TCADSysLine2D.GetTag    : LongInt;
begin
  result :=  fLine2D.Tag;
end;

procedure  TCADSysLine2D.SetLayer(ALayer: byte);
begin
  fLine2D.Layer := ALayer;
end;

procedure  TCADSysLine2D.SetEnabled(AEnabled: boolean);
begin
  fLine2D.Enabled := AEnabled;
end;

procedure  TCADSysLine2D.SetVisible(AVisible: boolean);
begin
  fLine2D.Visible := AVisible;
end;

procedure  TCADSysLine2D.SetTag(ATag: LongInt);
begin
  fLine2D.Tag := ATag;
end;


end.

