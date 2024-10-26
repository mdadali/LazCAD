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

    function   GetRectangle2D: TRectangle2D;
    procedure  SetRectangle2D(ARectangle2D: TRectangle2D);

  public
    fRectangle2D: TRectangle2D;
    constructor Create;
    property Rectangle2D: TRectangle2D read GetRectangle2D write SetRectangle2D;
  published
end;


implementation

constructor TCADSysRectangle2D.create;
begin
  inherited create;
  self.fPrimitive2D := fRectangle2D;
end;


function TCADSysRectangle2D.GetRectangle2D: TRectangle2D;
begin
  //result := fRectangle2D;
  result := TRectangle2D(fPrimitive2D);
end;

procedure TCADSysRectangle2D.SetRectangle2D(ARectangle2D: TRectangle2D);
begin
  fRectangle2D := ARectangle2D;
  self.fPrimitive2D := fRectangle2D;
  //SetLayerIDX(self.LayerIndex);
end;


end.

