unit fcomponentSimplePrim2D;

{$IFNDEF VER150}
{$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, Graphics,
  CADSys4,
  CS4BaseTypes,
  CS4Shapes,
  fcomponentprimitive2d;

type

TCADSysSimplePrimitive2D = class(TCADSysPrimitive2D)
  private

    //function  GetPen: TPen;
    //procedure SetPen(APen: TPen);
  protected
    fSimplePrimitive2D: TSimplePrimitive2D;
  public
    constructor Create;
    function    GetGraphicObject: TGraphicObject; override;
    procedure   SetGraphicObject(AGraphicObject: TGraphicObject); override;
  published
    //property Pen: TPen read GetPen write SetPen;
 end;

implementation

function  TCADSysSimplePrimitive2D.GetGraphicObject: TGraphicObject;
begin
  result := fSimplePrimitive2D;
end;

procedure TCADSysSimplePrimitive2D.SetGraphicObject(AGraphicObject: TGraphicObject);
begin
  fSimplePrimitive2D := TSimplePrimitive2D(AGraphicObject);
  //fPrimitive2D := TPrimitive2D(AGraphicObject);
  //fObject2D := TObject2D(AGraphicObject);
  //fGraphicObject := AGraphicObject;
end;

constructor TCADSysSimplePrimitive2D.create;
begin
  inherited create;
end;

end.

