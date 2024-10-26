unit fLayers;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Spin, StdCtrls, Buttons,
  CADSys4, fPenStyleComboBox, fBrushStyleComboBox;

type

  { TfrmLayers }

  TfrmLayers = class(TForm)
    BitBtn1: TBitBtn;
    BrushStyleComboBox1: TBrushStyleComboBox;
    Label7: TLabel;
    Label8: TLabel;
    PenColorButton: TColorButton;
    BrushColorButton: TColorButton;
    listBoxLayersList: TListBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    PenStyleComboBox1: TPenStyleComboBox;
    TransparentChk: TCheckBox;
    OKBtn: TBitBtn;
    NameEdt: TEdit;
    PenSizeEdt: TSpinEdit;
    ActiveLayerCBox: TComboBox;
    Label5: TLabel;
    Label6: TLabel;
    ActiveChk: TCheckBox;
    VisibleChk: TCheckBox;
    procedure listBoxLayersListClick(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
  private
    { Private declarations }
    fCurrLayer: word;
    fCADCmp: TCADCmp;

    procedure SetCurrLayer(L: word);
    procedure SaveCurrLayer;
  public
    { Public declarations }
    procedure Execute(CADCmp: TCADCmp);
  end;

var
  frmLayers: TfrmLayers;

implementation

{$R *.lfm}

procedure TfrmLayers.Execute(CADCmp: TCADCmp);
var
  Cont: Integer;
  TmpStr: String;
begin
  fCADCmp := CADCmp;
  listBoxLayersList.Items.Clear;
  with fCADCmp do
   begin
     for Cont := 0 to MAX_LAYER_NUMBER do
      begin
        TmpStr := Format('%d - %s', [Cont, Layers[Cont].Name]);
        listBoxLayersList.Items.Add(TmpStr);
        ActiveLayerCBox.Items.Add(TmpStr);
      end;
     SetCurrLayer(fCADCmp.CurrentLayer);
     ActiveLayerCBox.ItemIndex := fCADCmp.CurrentLayer;
     listBoxLayersList.ItemIndex := ActiveLayerCBox.ItemIndex;
   end;
  ShowModal;
end;

procedure TfrmLayers.SaveCurrLayer;
begin
  with fCADCmp do
   begin
     Layers[fCurrLayer].Name := NameEdt.Text;
     Layers[fCurrLayer].Pen.Color := PenColorButton.ButtonColor;
     Layers[fCurrLayer].Pen.Style := PenStyleComboBox1.SelectedPenStyle;
     Layers[fCurrLayer].Pen.Width := PenSizeEdt.Value;

     Layers[fCurrLayer].Brush.Color := BrushColorButton.ButtonColor;
     Layers[fCurrLayer].Brush.Style := BrushStyleComboBox1.SelectedBrushStyle;

     Layers[fCurrLayer].Opaque  := not TransparentChk.Checked;
     Layers[fCurrLayer].Visible := VisibleChk.Checked;
     Layers[fCurrLayer].Active  := ActiveChk.Checked;
   end;
end;

procedure TfrmLayers.SetCurrLayer(L: word);
begin
  with fCADCmp do
   begin
     fCurrLayer := L;
     NameEdt.Text := Layers[fCurrLayer].Name;

     PenColorButton.ButtonColor              := Layers[fCurrLayer].Pen.Color;
     PenStyleComboBox1.SelectedPenStyle      := Layers[fCurrLayer].Pen.Style;
     PenSizeEdt.Value                        := Layers[fCurrLayer].Pen.Width;

     BrushColorButton.ButtonColor            := Layers[fCurrLayer].Brush.Color;
     BrushStyleComboBox1.SelectedBrushStyle  := Layers[fCurrLayer].Brush.Style;

     TransparentChk.Checked := not Layers[fCurrLayer].Opaque;
     ActiveChk.Checked  := Layers[fCurrLayer].Active;
     VisibleChk.Checked := Layers[fCurrLayer].Visible;
   end;
end;

procedure TfrmLayers.listBoxLayersListClick(Sender: TObject);
begin
  SaveCurrLayer;
  SetCurrLayer(listBoxLayersList.ItemIndex);
end;

procedure TfrmLayers.OKBtnClick(Sender: TObject);
begin
  SaveCurrLayer;
  if ActiveLayerCBox.ItemIndex >= 0 then
   fCADCmp.CurrentLayer := ActiveLayerCBox.ItemIndex;
end;

end.
