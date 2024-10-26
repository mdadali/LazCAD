unit fcomponen_tobject2d;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  CADSys4,
  CS4BaseTypes,
  CS4Shapes,
  fcomponentgraphicobject;

  type
    TCADSysObject2D = class(TCADSysGraphicObject)
    private
      fObject2D: TObject2D;
      function  GetObject2D: TObject2D;
      procedure SetObject2D(AObject2D: TObject2D);
    public
      constructor Create;
      //property Object2D: TObject2D read fObject2D write fObject2D;
    published
  end;

implementation

constructor TCADSysObject2D.create;
begin
  inherited create;
end;

function TCADSysObject2D.GetObject2D: TObject2D;
begin
  result := fObject2D;
end;

procedure TCADSysObject2D.SetObject2D(AObject2D: TObject2D);
begin
  fObject2D := AObject2D;
  self.GraphicObject := fObject2D;
end;

end.

