unit fSimulation;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  Buttons, StdCtrls, ActnList, LZRoundedPanel, LZRoundedImage, LZRoundedButton,
  MKnob, indLCDDisplay, AdvLed, BCMDButton, BCMDButtonFocus, BCButtonFocus,
  BCButton, BGRAKnob, SynEdit, CADSys4, CS4BaseTypes;

type

  { TfrmSimulation }

  TfrmSimulation = class(TForm)
    acSimulStart: TAction;
    acSimulStop: TAction;
    acSimulBackwards: TAction;
    ActionList1: TActionList;
    CADCmp2D1: TCADCmp2D;
    CADViewport2D1: TCADViewport2D;
    Image1: TImage;
    Image2: TImage;
    ImageList1: TImageList;
    Label1: TLabel;
    LCDDisplay1: TLCDDisplay;
    LZRoundedImage1: TLZRoundedImage;
    mKnob1: TmKnob;
    pnlBottom: TPanel;
    Panel2: TPanel;
    pnlTop: TPanel;
    pnlCommands: TPanel;
    pnlSpeed: TPanel;
    pnlLCD: TPanel;
    plnRight: TPanel;
    sBtnStop: TSpeedButton;
    sBtnStart: TSpeedButton;
    sBtnBackwards: TSpeedButton;
    StatusBar1: TStatusBar;
    SynEdit1: TSynEdit;
    procedure acSimulBackwardsExecute(Sender: TObject);
    procedure acSimulStartExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure pnlTopClick(Sender: TObject);
    procedure plnRightResize(Sender: TObject);
    procedure pnlLCDResize(Sender: TObject);
    procedure sBtnBackwardsClick(Sender: TObject);
    procedure sBtnStartClick(Sender: TObject);
    procedure sBtnStopClick(Sender: TObject);
    procedure SetLCDDisplay;
    procedure mKnob1Change(Sender: TObject; AValue: Longint);
  private
    X, Y: TRealType;
  public
    RapidMoveSpeed,
    CuttingSpeed,
    CurrentMaxSpped: integer;

  end;

//var
  //frmSimulation: TfrmSimulation;

implementation

{$R *.lfm}

{ TfrmSimulation }

procedure TfrmSimulation.pnlLCDResize(Sender: TObject);
begin

end;

procedure TfrmSimulation.sBtnBackwardsClick(Sender: TObject);
begin
  {if sBtnBackwards.Down then
    sBtnBackwards.Down := false
  else
    sBtnBackwards.Down := true;
 }
  if sBtnBackwards.Down then
    acSimulBackwardsExecute(nil);
end;

procedure TfrmSimulation.SetLCDDisplay;
var hPercent, sSpeed: string;  iSpeed: integer;
begin
  iSpeed := round(CurrentMaxSpped/100 * mKnob1.Position);
  sSpeed := IntToStr(iSpeed) + 'mm/min  ';
  hPercent := '   ' + IntTostr( mKnob1.Position) + '%';
  LCDDisplay1.Lines.Clear;
  LCDDisplay1.Lines.Add(sSpeed);
  LCDDisplay1.Lines.Add(hPercent);

  LCDDisplay1.Lines.Add(Format('X: %6.3f', [X]));
  LCDDisplay1.Lines.Add(Format('Y: %6.3f', [Y]));
end;

procedure TfrmSimulation.FormCreate(Sender: TObject);
begin
  X := 0.00;
  Y := 0.00;
  RapidMoveSpeed  := 24000;
  CuttingSpeed    := 4000;
  CurrentMaxSpped := RapidMoveSpeed;
  SetLCDDisplay;
end;

procedure TfrmSimulation.acSimulStartExecute(Sender: TObject);
begin
  //
end;

procedure TfrmSimulation.acSimulBackwardsExecute(Sender: TObject);
begin
  //
end;

procedure TfrmSimulation.FormResize(Sender: TObject);
begin
  CADViewport2D1.Top  := LZRoundedImage1.Top + 59;
  CADViewport2D1.Left := 68;
end;

procedure TfrmSimulation.pnlTopClick(Sender: TObject);
begin
end;

procedure TfrmSimulation.plnRightResize(Sender: TObject);
begin
  LCDDisplay1.Top   := 0;
  LCDDisplay1.Left  := 0;
  LCDDisplay1.Height:= pnlLCD.Height;
  LCDDisplay1.Width := pnlLCD.Width;
end;

procedure TfrmSimulation.sBtnStartClick(Sender: TObject);
begin
  sBtnStart.Down        := true;
  sBtnStart.Enabled     := false;
  sBtnStop.Down         := false;
  sBtnStop.Enabled      := true;
  sBtnBackwards.Enabled := false;
  acSimulStartExecute(nil);
end;

procedure TfrmSimulation.sBtnStopClick(Sender: TObject);
begin
  sBtnStop.Down         := true;
  sBtnStop.Enabled      := false;
  sBtnStart.Down        := false;
  sBtnStart.Enabled     := true;
  sBtnBackwards.Enabled := true;
  acSimulStartExecute(nil);
end;

procedure TfrmSimulation.mKnob1Change(Sender: TObject; AValue: Longint);
begin
  SetLCDDisplay;
end;


end.

