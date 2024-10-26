unit  fComponentPLine;

{$IFNDEF VER150}
{$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, dialogs,
  CADSys4,
  CS4BaseTypes,
  CS4Shapes,
  fcomponentSimplePrim2D;

type

TCADSysPLine2D = class(TCADSysSimplePrimitive2D) //class(tpersistent)
  private
    function  GetGraphicObject: TGraphicObject; override;
    procedure SetGraphicObject(AGraphicObject: TGraphicObject); override;
  public
    fPolyLine2D: TPolyLine2D;
    constructor Create;
    property GraphicObject;
  published
end;

implementation

constructor TCADSysPLine2D.create;
begin
  inherited create;
end;

function TCADSysPLine2D.GetGraphicObject: TGraphicObject;
begin
  result := fPolyLine2D;
end;

procedure TCADSysPLine2D.SetGraphicObject(AGraphicObject: TGraphicObject);
begin
  fPolyLine2D := TPolyLine2D(AGraphicObject);
  fSimplePrimitive2D := TSimplePrimitive2D(AGraphicObject);
  fPrimitive2D := TPrimitive2D(AGraphicObject);
  fObject2D := TObject2D(AGraphicObject);
  fGraphicObject := AGraphicObject;
end;

end.

