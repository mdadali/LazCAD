unit fComponentFrame;

{$IFNDEF VER150}
{$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils,
  CADSys4,
  CS4BaseTypes,
  CS4Shapes,
  fcompclosedcurve2d;

type

TCADSysFrame2D = class(TCADSysClosedCurve2D)
  private
    function   GetStartCorner: TFrameStartCorner;
    procedure  SetStartCorner(AValue: TFrameStartCorner);
    function   GetChamfered: boolean;
    procedure  SetChamfered(AValue: boolean);
    function   GetChamfer: TrealType;
    procedure  SetChamfer(AValue: TrealType);

    function  GetWidth: TrealType;
    procedure SetWidth(AValue: TrealType);

    function  GetHeight: TrealType;
    procedure SetHeight(AValue: TrealType);

    function  GetGraphicObject: TGraphicObject; override;
    procedure SetGraphicObject(AGraphicObject: TGraphicObject); override;
  public
    fFrame2D:   TFrame2D;
    constructor Create;
    property    GraphicObject;
  published
    property StartCorner: TFrameStartCorner read  GetStartCorner write SetStartCorner;
    property Chamfered: boolean read GetChamfered write SetChamfered;
    property Chamfer: TRealType read GetChamfer write SetChamfer;
    property Width: TRealType read GetWidth write SetWidth;
    property Height: TRealType read GetHeight write SetHeight;
end;


implementation

function TCADSysFrame2D.GetGraphicObject: TGraphicObject;
begin
  result := fFrame2D;
end;

procedure TCADSysFrame2D.SetGraphicObject(AGraphicObject: TGraphicObject);
begin
  fFrame2D := TFrame2D(AGraphicObject);
  fSimplePrimitive2D := TSimplePrimitive2D(AGraphicObject);
  fPrimitive2D := TPrimitive2D(AGraphicObject);
  fObject2D := TObject2D(AGraphicObject);
  self.fClosedCurve2D := AGraphicObject;
  fGraphicObject := AGraphicObject;
end;


constructor TCADSysFrame2D.create;
begin
  inherited create;
end;

function   TCADSysFrame2D.GetStartCorner: TFrameStartCorner;
begin
  result :=  TFrame2D(self.fPrimitive2D).StartCorner;
end;

procedure  TCADSysFrame2D.SetStartCorner(AValue: TFrameStartCorner);
begin
  TFrame2D(self.fPrimitive2D).StartCorner :=  AValue;
  self.fPrimitive2D.UpdateExtension(nil);
end;

function   TCADSysFrame2D.GetChamfered: boolean;
begin
  result :=  TFrame2D(self.fPrimitive2D).Chamfered;
end;

procedure  TCADSysFrame2D.SetChamfered(AValue: boolean);
begin
  TFrame2D(self.fPrimitive2D).Chamfered := AValue;
  if TFrame2D(self.fPrimitive2D).Chamfered then
    if TFrame2D(self.fPrimitive2D).Chamfer <= 0 then
      TFrame2D(self.fPrimitive2D).Chamfer := 5;
  self.fPrimitive2D.UpdateExtension(nil);
end;

function   TCADSysFrame2D.GetChamfer: TrealType;
begin
  result :=  TFrame2D(self.fPrimitive2D).Chamfer;
end;

procedure  TCADSysFrame2D.SetChamfer(AValue: TrealType);
begin
  TFrame2D(self.fPrimitive2D).Chamfer := AValue;
  self.fPrimitive2D.UpdateExtension(nil);
end;

function  TCADSysFrame2D.GetWidth: TrealType;
begin
  result := TFrame2D(self.fPrimitive2D).Width;
end;

procedure TCADSysFrame2D.SetWidth(AValue: TrealType);
begin
  TFrame2D(self.fPrimitive2D).Width := AValue;
  self.fPrimitive2D.UpdateExtension(nil);
end;

function  TCADSysFrame2D.GetHeight: TrealType;
begin
  result := TFrame2D(self.fPrimitive2D).Height;
end;

procedure TCADSysFrame2D.SetHeight(AValue: TrealType);
begin
  TFrame2D(self.fPrimitive2D).Height := AValue;
  self.fPrimitive2D.UpdateExtension(nil);
end;

end.

