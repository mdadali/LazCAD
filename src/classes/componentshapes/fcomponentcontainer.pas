unit fcomponentcontainer;

interface


uses
  Classes, SysUtils,
  CADSys4,
  CS4BaseTypes,
  CS4Shapes,
  fcomponentobject2d;

  type

    TCADSysContainer2D = class(TCADSysObject2D)
    private
      fContainer2D: TContainer2D;
      function   GetContainer2D: TContainer2D;
      procedure  SetContainer2D(AContainer2D: TContainer2D);

    public
       constructor Create;
       property Container2D: TContainer2D read GetContainer2D write SetContainer2D;
     published
  end;


implementation

constructor TCADSysContainer2D.create;
begin
  inherited create;
end;

function TCADSysContainer2D.GetContainer2D: TContainer2D;
begin
  result := fContainer2D;
end;

procedure TCADSysContainer2D.SetContainer2D(AContainer2D: TContainer2D);
begin
  fContainer2D  := AContainer2D;
  //self.fPrimitive2D := fContainer2D ;
end;




end.

