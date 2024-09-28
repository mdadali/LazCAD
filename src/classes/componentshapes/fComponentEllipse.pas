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
  fBaseComponent;

type

TCADSysEllipse2D = class(TCADSysBaseComponent2D) //class(tpersistent)
  private
    fEllipse2D: TEllipse2D;
    function   GetEllipse2D: TEllipse2D;
    procedure  SetEllipse2D(AEllipse2D: TEllipse2D);
    function   GetCurvePrecision: Word;
    procedure  SetCurvePrecision(APrecision: Word);

    function  GetWidth: TrealType;
    procedure SetWidth(AValue: TrealType);

    function  GetHeight: TrealType;
    procedure SetHeight(AValue: TrealType);

  public
    constructor Create;
    property Ellipse2D: TEllipse2D read GetEllipse2D write SetEllipse2D;
  published
    property Direction;
    property EdgeCount: Word  read GetCurvePrecision write SetCurvePrecision;

    property Width: TRealType read GetWidth write SetWidth;
    property Height: TRealType read GetHeight write SetHeight;

    //property BrushColor;
    //property BrushStyle;
    //property Filled;
end;

implementation

constructor TCADSysEllipse2D.create;
begin
  inherited create;
  self.fPrimitive2D := fEllipse2D;
end;

function TCADSysEllipse2D.GetEllipse2D: TEllipse2D;
begin
  result := fEllipse2D;
  //result := TCircle2D_CPR(fPrimitive2D);
end;

procedure TCADSysEllipse2D.SetEllipse2D(AEllipse2D: TEllipse2D);
begin
  fEllipse2D := AEllipse2D;
  self.fPrimitive2D := fEllipse2D;
  //SetLayerIDX(self.LayerIndex);
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

