unit fComponentLine;

{$IFNDEF VER150}
{$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, Graphics,
  CADSys4,
  CS4BaseTypes,
  CS4Shapes,
  fcomponentSimplePrim2D;

type

TCADSysLine2D = class(TCADSysSimplePrimitive2D)
private
  fLine: TLine2D;
  function  GetGraphicObject: TGraphicObject; override;
  procedure SetGraphicObject(AGraphicObject: TGraphicObject); override;
public
  constructor Create;
published
end;


implementation

constructor TCADSysLine2D.create;
begin
  inherited create;
end;

function  TCADSysLine2D.GetGraphicObject: TGraphicObject;
begin
  result := fLine;
end;

procedure TCADSysLine2D.SetGraphicObject(AGraphicObject: TGraphicObject);
begin
  fLine := TLine2D(AGraphicObject);
  fSimplePrimitive2D := TSimplePrimitive2D(AGraphicObject);
  fPrimitive2D := TPrimitive2D(AGraphicObject);
  fObject2D := TObject2D(AGraphicObject);
  fGraphicObject := AGraphicObject;
end;

end.

