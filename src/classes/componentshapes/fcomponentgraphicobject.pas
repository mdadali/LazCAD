unit fcomponentgraphicobject;

{$mode ObjFPC}{$H+}

interface

uses
Classes, SysUtils,
CADSys4,
CS4BaseTypes,
CS4Shapes;

type

  TCADSysGraphicObject = class(TPersistent)
  private
    function   GetOwnerCADCmp: TCADCmp;

    function   GetClassName: string;
    function   GetID: LongInt;
    function   GetLayerIDX: LongInt;
    procedure  SetLayerIDX(AIDX: LongInt);
    function   GetLayerName: string;
    procedure  SetLayerName(ALayerName: string);
    function   GetEnabled:  boolean;
    procedure  SetEnabled(AValue: boolean);
    function   GetVisible: boolean;
    procedure  SetVisible(AValue: boolean);
    function   GetTag: LongInt;
    procedure  SetTag(AValue: LongInt);
  protected
    fGraphicObject: TGraphicObject;
    function   GetGraphicObject: TGraphicObject; virtual abstract;
    procedure  SetGraphicObject(AGraphicObject: TGraphicObject); virtual abstract;
  public
    constructor Create;
    property GraphicObject: TGraphicObject read GetGraphicObject write SetGraphicObject;
    property OwnerCAD: TCADCmp      read GetOwnerCADCmp;
    property ClassName: string      read GetClassName;
    property ID:        Longint     read GetID;
    property LayerIDX:  LongInt     read GetLayerIDX  write SetLayerIDX;
    property LayerName: string      read GetLayerName write SetLayerName;
    property Enabled:   boolean     read GetEnabled   write SetEnabled;
    property Visible:   boolean     read GetVisible   write SetVisible;
    property Tag:       LongInt     read GetTag       write SetTag;
  published
end;

implementation

constructor  TCADSysGraphicObject.create;
begin
  inherited create;
end;

function   TCADSysGraphicObject.GetOwnerCADCmp: TCADCmp;
begin
  if Assigned(fGraphicObject) then
    result := fGraphicObject.OwnerCAD;
end;

function TCADSysGraphicObject.GetClassName: string;
begin
  result := fGraphicObject.ClassName;
end;

function   TCADSysGraphicObject.GetID: LongInt;
begin
  if Assigned(fGraphicObject) then
    result := fGraphicObject.ID
  else
    result := -2;
end;

function   TCADSysGraphicObject.GetLayerIDX: LongInt;
begin
  if Assigned(fGraphicObject) then
    result := fGraphicObject.LayerIndex
  else
    result := -2;
end;

procedure  TCADSysGraphicObject.SetLayerIDX(AIDX: LongInt);
begin
  if Assigned(fGraphicObject) then
    fGraphicObject.LayerIndex := AIDX;
end;

function   TCADSysGraphicObject.GetLayerName: string;
begin
  if Assigned(fGraphicObject) then
    result := fGraphicObject.LayerName;
end;

procedure  TCADSysGraphicObject.SetLayerName(ALayerName: string);
begin
  if Assigned(fGraphicObject) then
    fGraphicObject.LayerName := ALayerName;
end;

function   TCADSysGraphicObject.GetEnabled:  boolean;
begin
  if Assigned(fGraphicObject) then
    result := fGraphicObject.Enabled;
end;

procedure  TCADSysGraphicObject.SetEnabled(AValue: boolean);
begin
  if Assigned(fGraphicObject) then
    fGraphicObject.Enabled := AValue;
end;

function   TCADSysGraphicObject.GetVisible: boolean;
begin
  if Assigned(fGraphicObject) then
    result := fGraphicObject.Visible;
end;

procedure  TCADSysGraphicObject.SetVisible(AValue: boolean);
begin
  if Assigned(fGraphicObject) then
    fGraphicObject.Visible := AValue;
end;

function   TCADSysGraphicObject.GetTag: LongInt;
begin
  if Assigned(fGraphicObject) then
    result := fGraphicObject.Tag;
end;

procedure  TCADSysGraphicObject.SetTag(AValue: LongInt);
begin
  if Assigned(fGraphicObject) then
    fGraphicObject.Tag := AValue;
end;

end.

