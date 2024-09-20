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
  fBaseComponent;

type

TCADSysFrame2D = class(TCADSysBaseComponent2D) //class(tpersistent)

  private
    fFrame2D: TFrame2D;
    function   GetFrame2D: TFrame2D;
    procedure  SetFrame2D(AFrame2D: TFrame2D);
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

  public
    constructor Create;
    property Frame2D: TFrame2D read GetFrame2D write SetFrame2D;
  published
    property Direction;
    property StartCorner: TFrameStartCorner read  GetStartCorner write SetStartCorner;
    property Chamfered: boolean read GetChamfered write SetChamfered;
    property Chamfer: TrealType read GetChamfer write SetChamfer default 10.0;
    property Width: TrealType read GetWidth write SetWidth;
    property Height: TrealType read GetHeight write SetHeight;

    //property BrushColor;
    //property BrushStyle;
    //property Filled;
end;


implementation

constructor TCADSysFrame2D.create;
begin
  inherited create;
  self.fPrimitive2D := fFrame2D;
end;

{function   TCADSysFrame2D.GetClassName: string;
begin
  result := fPrimitive2D.ClassName;
end;
}

function TCADSysFrame2D.GetFrame2D: TFrame2D;
begin
  //result := fFrame2D;
  result := TFrame2D(fPrimitive2D);
end;

procedure TCADSysFrame2D.SetFrame2D(AFrame2D: TFrame2D);
begin
  fFrame2D := AFrame2D;
  self.fPrimitive2D := fFrame2D;
  //SetLayerIDX(self.LayerIndex);
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

