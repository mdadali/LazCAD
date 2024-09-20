unit fComponentGraphicObj;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  CADSys4,
  CS4BaseTypes,
  CS4Shapes;

type
  TCADSysGraphicObj = class(TPersistent)
  private
    fGraphicObj: TGraphicObject;
    function  GetID: integer;
    function  GetClassName: string;
    function  GetVisible: boolean;
    procedure SetVisible(AValue: boolean);
    function  GetEnabled: boolean;
    procedure SetEnabled(AValue: boolean);
    function  GetTag: longint;
    procedure SetTag(AValue: longint);
  protected
  public
  published
    property  ID: longint                 read GetID;
    property  ClassName: string           read GetClassName;
    property  Visible:   boolean          read GetVisible    write SetVisible;
    property  Enabled:   boolean          read GetEnabled    write SetEnabled;
    property  Tag:       longint          read GetTag        write SetTag;
  end;

implementation

function  TCADSysGraphicObj.GetID: integer;
begin

end;

function  TCADSysGraphicObj.GetClassName: string;
begin

end;

function  TCADSysGraphicObj.GetVisible: boolean;
begin

end;

procedure TCADSysGraphicObj.SetVisible(AValue: boolean);
begin

end;

function  TCADSysGraphicObj.GetEnabled: boolean;
begin

end;

procedure TCADSysGraphicObj.SetEnabled(AValue: boolean);
begin

end;

function  TCADSysGraphicObj.GetTag: integer;
begin

end;

procedure TCADSysGraphicObj.SetTag(AValue: integer);
begin

end;


end.

