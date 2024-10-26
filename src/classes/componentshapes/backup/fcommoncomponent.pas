unit fcommoncomponent;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  CADSys4,
  CS4BaseTypes,
  CS4Shapes,
  fcomponentline;

type
     TCommonComponent = class
     private
       fGraphicObject: TGraphicObject;
       procedure SetObject(GraphicObject: TGraphicObject);
     public
       constructor create(AGraphicObject: TGraphicObject);
       function GetObject: TGraphicObject;
     end;

implementation

procedure TCommonComponent.SetObject(GraphicObject: TGraphicObject);
begin

end;

constructor TCommonComponent.create(AGraphicObject: TGraphicObject);
var TmpClass: TGraphicObjectClass;
begin
  inherited create;
  SetObject(AGraphicObject);
  if fGraphicObject  is TLine2D then
  begin
  end else
  if fGraphicObject  is TLine2D then
  begin
  end else
  if fGraphicObject  is TLine2D then
  begin
  end else
  if fGraphicObject  is TLine2D then
  begin
  end else
  if fGraphicObject  is TLine2D then
  begin
  end else
  if fGraphicObject  is TLine2D then
  begin
  end else
  if fGraphicObject  is TLine2D then
  begin
  end else
  if fGraphicObject  is TLine2D then
  begin
  end else
  begin

  end;

end;

end.

