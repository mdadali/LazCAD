unit fcomponentoutline2d;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics,
  CADSys4,
  CS4BaseTypes,
  CS4Shapes,
  fcomponentSimplePrim2D;

type

TCADSysOutline2D = class(TCADSysSimplePrimitive2D) //class(tpersistent)
  private
    fOutline2D: TOutline2D;
    function  GetOutline2D: TOutline2D;
    procedure SetOutline2D(AOutline2D: TOutline2D);
  public
    constructor Create;
    property    Outline2D: TOutline2D read GetOutline2D write SetOutline2D;
  published
end;

implementation

constructor TCADSysOutline2D.create;
begin
  inherited create;
end;

function  TCADSysOutline2D.GetOutline2D: TOutline2D;
begin

end;

procedure TCADSysOutline2D.SetOutline2D(AOutline2D: TOutline2D);
begin

end;

end.

