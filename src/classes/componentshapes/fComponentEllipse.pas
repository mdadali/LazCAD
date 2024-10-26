unit fComponentEllipse;

{$IFNDEF VER150}
{$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, dialogs,
  CADSys4,
  CS4BaseTypes,
  CS4Shapes,
  fcompclosedcurve2d;

type

TCADSysEllipse2D = class(TCADSysClosedCurve2D) //class(tpersistent)
  private
    function   GetCurvePrecision: Word;
    procedure  SetCurvePrecision(APrecision: Word);

    function  GetWidth: TrealType;
    procedure SetWidth(AValue: TrealType);

    function  GetHeight: TrealType;
    procedure SetHeight(AValue: TrealType);

    function  GetGraphicObject: TGraphicObject; override;
    procedure SetGraphicObject(AGraphicObject: TGraphicObject); override;
  public
    fEllipse2D: TEllipse2D;
    constructor Create;
    property    GraphicObject;
  published
    property EdgeCount: Word  read GetCurvePrecision write SetCurvePrecision;

    property Width: TRealType read GetWidth write SetWidth;
    property Height: TRealType read GetHeight write SetHeight;
end;

implementation

constructor TCADSysEllipse2D.create;
begin
  inherited create;
end;

function TCADSysEllipse2D.GetGraphicObject: TGraphicObject;
begin
  result := fEllipse2D;
end;

procedure TCADSysEllipse2D.SetGraphicObject(AGraphicObject: TGraphicObject);
begin
  fEllipse2D := TEllipse2D(AGraphicObject);
  fSimplePrimitive2D := TSimplePrimitive2D(AGraphicObject);
  fPrimitive2D := TPrimitive2D(AGraphicObject);
  fObject2D := TObject2D(AGraphicObject);
  fGraphicObject := AGraphicObject;
end;

function   TCADSysEllipse2D.GetCurvePrecision: Word;
begin
  result := fEllipse2D.CurvePrecision;
end;

procedure  TCADSysEllipse2D.SetCurvePrecision(APrecision: Word);
begin
  if APrecision = fEllipse2D.CurvePrecision then exit;
  if (APrecision < 3) then
  begin
    MessageDlg('Warning', 'The minimum allowed number of edges is 3.', mtWarning, [mbOK], 0);
    APrecision := 3;
  end;
  fEllipse2D.CurvePrecision := APrecision;
  fEllipse2D.UpdateExtension(self);
end;

function  TCADSysEllipse2D.GetWidth: TrealType;
begin
  result := TEllipse2D(self.fPrimitive2D).Width;
end;

procedure TCADSysEllipse2D.SetWidth(AValue: TrealType);
begin
  TEllipse2D(self.fPrimitive2D).Width := AValue;
  self.fPrimitive2D.UpdateExtension(nil);
end;

function  TCADSysEllipse2D.GetHeight: TrealType;
begin
  result := TEllipse2D(self.fPrimitive2D).Height;
end;

procedure TCADSysEllipse2D.SetHeight(AValue: TrealType);
begin
  TEllipse2D(self.fPrimitive2D).Height := AValue;
  self.fPrimitive2D.UpdateExtension(nil);
end;


end.

