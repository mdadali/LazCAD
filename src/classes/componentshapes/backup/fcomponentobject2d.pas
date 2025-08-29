unit fcomponentobject2d;

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

    protected
       fObject2D: TObject2D;
       function  GetGraphicObject: TGraphicObject; override;
       procedure SetGraphicObject(AGraphicObject: TGraphicObject); override;
    public
      constructor Create;
      property  GraphicObject;
    published
      property ClassName;
      property ID;
      property LayerIDX;
      property LayerName;
      property Enabled;
      property Visible;
      property Tag;
   end;

implementation

constructor TCADSysObject2D.create;
begin
  inherited create;
end;

function  TCADSysObject2D.GetGraphicObject: TGraphicObject;
begin
  result :=   TObject2D(fObject2D);
end;

procedure TCADSysObject2D.SetGraphicObject(AGraphicObject: TGraphicObject);
begin
  fObject2D := TObject2D(AGraphicObject);
  //self.fGraphicObject := AGraphicObject;
end;

end.

