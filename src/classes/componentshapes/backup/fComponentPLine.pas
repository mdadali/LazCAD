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
  fBaseComponent;

type

TCADSysPLine2D = class(TCADSysBaseComponent2D) //class(tpersistent)

  private
    fPolyLine2D: TPolyLine2D;
    function   GetPolyLine2D: TPolyLine2D;
    procedure  SetPolyLine2D(APolyLine2D: TPolyLine2D);
  public
    constructor Create;
    property PolyLine2D: TPolyLine2D read GetPolyLine2D write SetPolyLine2D;
     property Direction;
  published
end;

implementation

constructor TCADSysPLine2D.create;
begin
  inherited create;
  self.fPrimitive2D := fPolyLine2D;
end;

function TCADSysPLine2D.GetPolyLine2D: TPolyLine2D;
begin
  result := fPolyLine2D;
  //result := TCircle2D_CPR(fPrimitive2D);
end;

procedure TCADSysPLine2D.SetPolyLine2D(APolyLine2D: TPolyLine2D);
begin
  fPolyLine2D := APolyLine2D;
  self.fPrimitive2D := fPolyLine2D;
  //SetLayerIDX(self.LayerIndex);
end;


end.

