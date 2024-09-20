unit fComponentLayer;

{$mode Delphi}

interface

uses
  Classes, SysUtils, Graphics,
  CADSys4;

type
  TComponentLayer = class(TComponent)
  private
    fLayer: TLayer;
    function  GetLayer: TLayer;
    procedure SetLayer(ALayer: TLayer);
    function  GetID: TLayerID;
    function  GetName: string;
    procedure SetName(AName: string);
    function  GetPenColor: TColor;
    procedure SetPenColor(AColor: TColor);
    function  GetPenWidth: integer;
    procedure SetPenWidth(APenWidth: integer);
    function  GetPenStyle: TPenStyle;
    procedure SetPenStyle(APenStyle: TPenStyle);
    function  GetBrushColor: TColor;
    procedure SetBrushColor(AColor: TColor);
    function  GetBrushStyle: TBrushStyle;
    procedure SetBrushStyle(ABrushStyle: TBrushStyle);
    function  GetVisible: boolean;
    procedure SetVisible(Avisible: boolean);
    function  GetOpaque: boolean;
    procedure SetOpaque(AOpaque: boolean);
    function  GetVisibleInLayerManager: boolean;
    procedure SetVisibleInLayerManager(AVisibleInLayerManager: boolean);
    function  GetLocked: boolean;
    procedure SetLocked(ALocked: boolean);
    function  GetPrintable: boolean;
    procedure SetPrintable(APrintable: boolean);
    function  GetStreamable: boolean;
    procedure SetStreamable(AStreamable: boolean);
  public
    constructor Create;
    property Layer: TLayer read fLayer write fLayer;
  published
    property  ID: TLayerID read GetID;
    property  Name: string read GetName write SetName;
    property  PenColor: TColor read GetPenColor write SetPenColor;
    property  PenWidth: integer read GetPenWidth write SetPenWidth;
    property  PenStyle: TPenStyle read GetPenStyle write SetPenStyle;
    property  BrushColor: TColor read GetBrushColor write SetBrushColor;
    property  BrushStyle: TBrushStyle read GetBrushStyle     write SetBrushStyle;
    property  Visible: boolean read GetVisible write SetVisible;
    property  Opaque: boolean read GetOpaque  write SetOpaque;
    property  VisibleInLayerManager: boolean read GetVisibleInLayerManager write SetVisibleInLayerManager;
    property  Locked: boolean read GetLocked write SetLocked;
    property  Streamable: boolean read GetStreamable write SetStreamable;
    property  Printable: boolean read GetPrintable write SetPrintable;
  end;

implementation

constructor TComponentLayer.Create;
begin
  inherited create;
end;

function TComponentLayer.GetLayer: TLayer;
begin
  result := fLayer;
end;

procedure TComponentLayer.SetLayer(ALayer: TLayer);
begin
  fLayer := ALayer;
end;

function TComponentLayer.GetID: TLayerID;
begin
  result := fLayer.ID;
end;


function  TComponentLayer.GetName: string;
begin
  result := fLayer.Name;
end;

procedure TComponentLayer.SetName(AName: string);
begin
  fLayer.Name := AName;
end;

function  TComponentLayer.GetPenColor: TColor;
begin
  result := fLayer.Pen.Color;
end;

procedure TComponentLayer.SetPenColor(AColor: TColor);
begin
  fLayer.Pen.Color := AColor;
end;

function  TComponentLayer.GetPenWidth: integer;
begin
  result := fLayer.Pen.Width;
end;

procedure TComponentLayer.SetPenWidth(APenWidth: integer);
begin
  fLayer.Pen.Width := APenWidth;
end;

function  TComponentLayer.GetPenStyle: TPenStyle;
begin
  result := fLayer.Pen.Style;
end;

procedure TComponentLayer.SetPenStyle(APenStyle: TPenStyle);
begin
  fLayer.Pen.Style := APenStyle;
end;

function  TComponentLayer.GetBrushColor: TColor;
begin
  result := fLayer.Brush.Color;
end;

procedure TComponentLayer.SetBrushColor(AColor: TColor);
begin
  fLayer.Brush.Color := AColor;
end;

function  TComponentLayer.GetBrushStyle: TBrushStyle;
begin
  result := fLayer.Brush.Style;
end;

procedure  TComponentLayer.SetBrushStyle(ABrushStyle: TBrushStyle);
begin
  fLayer.Brush.Style := ABrushStyle;
end;

function  TComponentLayer.GetVisible: boolean;
begin
  result := fLayer.Visible;
end;

procedure TComponentLayer.SetVisible(Avisible: boolean);
begin
  fLayer.Visible := AVisible
end;

function  TComponentLayer.GetOpaque: boolean;
begin
  result := fLayer.Opaque;
end;

procedure TComponentLayer.SetOpaque(AOpaque: boolean);
begin
  fLayer.Opaque := AOpaque;
end;

function  TComponentLayer.GetVisibleInLayerManager: boolean;
begin
  result := fLayer.VisibleInLayerManager;
end;

procedure TComponentLayer.SetVisibleInLayerManager(AVisibleInLayerManager: boolean);
begin
  fLayer.VisibleInLayerManager := AVisibleInLayerManager;
end;

function  TComponentLayer.GetLocked: boolean;
begin
  result := not fLayer.Active;
end;

procedure TComponentLayer.SetLocked(ALocked: boolean);
begin
  fLayer.Active := (not ALocked);
end;

function  TComponentLayer.GetPrintable: boolean;
begin
  result := fLayer.Printable;
end;

procedure TComponentLayer.SetPrintable(APrintable: boolean);
begin
  fLayer.Printable := APrintable;
end;

function  TComponentLayer.GetStreamable: boolean;
begin
  result := fLayer.Streamable;
end;

procedure TComponentLayer.SetStreamable(AStreamable: boolean);
begin
  fLayer.Streamable := AStreamable;
end;

end.

