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
  fBaseComponent;

type

TCADSysLine2D = class(TCADSysBaseComponent2D) //class(tpersistent)

  private
    fLine2D: TLine2D;
    fCAD2D: TCADCmp2D;
    function   GetLine2D: TLine2D;
    procedure  SetLine2D(ALine2D: TLine2D);
    function   GetObjectLength: TRealType;
    procedure  SetObjectLength(AValue: TRealType);
  public
    constructor Create;
    property Line2D: TLine2D read GetLine2D write SetLine2D;
  published
    property ObjectLength read GetObjectLength write SetObjectLength;
    property ObjectPosition2D;
end;


implementation

constructor TCADSysLine2D.create;
begin
  inherited create;
  self.fPrimitive2D := fLine2D;
end;


function TCADSysLine2D.GetLine2D: TLine2D;
begin
  //result := fLine2D;
  result := TLine2D(fPrimitive2D);
end;

procedure TCADSysLine2D.SetLine2D(ALine2D: TLine2D);
begin
  fLine2D := ALine2D;
  self.fPrimitive2D := fLine2D;
end;

function   TCADSysLine2D.GetObjectLength: TRealType;
begin
  result := fLine2D.ObjectLength;
end;

procedure  TCADSysLine2D.SetObjectLength(AValue: TRealType);
begin
  fLine2D.SetLength(AValue);
end;

end.

