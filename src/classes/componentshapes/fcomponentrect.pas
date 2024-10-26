unit fComponentRect;

{$IFNDEF VER150}
{$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils,
  CADSys4,
  CS4BaseTypes,
  CS4Shapes,
  fComponentFrame;

type

TCADSysRectangle2D = class(TCADSysFrame2D) //class(tpersistent)
  private
    function  GetGraphicObject: TGraphicObject; override;
    procedure SetGraphicObject(AGraphicObject: TGraphicObject); override;
  public
    fRectangle2D: TRectangle2D;
    constructor Create;
    property    GraphicObject;
  published
end;


implementation

constructor TCADSysRectangle2D.create;
begin
  inherited create;
  self.fPrimitive2D := fRectangle2D;
end;

function TCADSysRectangle2D.GetGraphicObject: TGraphicObject;
begin
  result := fRectangle2D;
end;

procedure TCADSysRectangle2D.SetGraphicObject(AGraphicObject: TGraphicObject);
begin
  fRectangle2D := TRectangle2D(AGraphicObject);
  fSimplePrimitive2D := TSimplePrimitive2D(AGraphicObject);
  fPrimitive2D := TPrimitive2D(AGraphicObject);
  fObject2D := TObject2D(AGraphicObject);
  fGraphicObject := AGraphicObject;
end;


end.

