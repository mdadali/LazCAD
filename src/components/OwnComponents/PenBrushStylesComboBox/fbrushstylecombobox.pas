unit fBrushStyleComboBox;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls, LCLType;

type
  TBrushStyleComboBox = class(TComboBox)
  private
    fBrushStyle: TBrushStyle;
    fStyleName: string;
    function  GetSelectedBrushStyle: TBrushStyle;
    procedure SetSelectedBrushStyle(ABrushStyle: TBrushStyle);
    function  GetSelectedStyleName: string;
    procedure DrawItem(Index: Integer; ARect: TRect; State: TOwnerDrawState); override;
    procedure PopulateItems;
  protected

  public
    constructor Create(AOwner: TComponent); override;
    property    SelectedBrushStyle: TBrushStyle read GetSelectedBrushStyle write SetSelectedBrushStyle;
    property    SelectedBrushStyleName: string read GetSelectedStyleName;
  published

  end;

procedure Register;

implementation

function  TBrushStyleComboBox.GetSelectedBrushStyle: TBrushStyle;
begin
  case self.ItemIndex of
    0: result := bsClear;
    1: result := bsSolid;
    2: result := bsHorizontal;
    3: result := bsVertical;
    4: result := bsFDiagonal;
    5: result := bsBDiagonal;
    6: result := bsCross;
    7: result := bsDiagCross;
    else  result := bsClear;
  end;
end;

function  TBrushStyleComboBox.GetSelectedStyleName: string;
begin
  case self.ItemIndex of
    0: result := 'bsClear';
    1: result := 'bsSolid';
    2: result := 'bsHorizontal';
    3: result := 'bsVertical';
    4: result := 'bsFDiagonal';
    5: result := 'bsBDiagonal';
    6: result := 'bsCross';
    7: result := 'bsDiagCross';
    else  result := 'unknow';
  end;
end;

procedure TBrushStyleComboBox.SetSelectedBrushStyle(ABrushStyle: TBrushStyle);
begin
  case ABrushStyle of
    bsClear:
      ItemIndex := 0;
    bsSolid:
      ItemIndex := 1;
    bsHorizontal:
      ItemIndex := 2;
    bsVertical:
      ItemIndex := 3;
    bsFDiagonal:
      ItemIndex := 4;
    bsBDiagonal:
      ItemIndex := 5;
    bsCross:
      ItemIndex := 6;
    bsDiagCross:
      ItemIndex := 7;
    else
      ItemIndex := 0;
  end;
end;

constructor TBrushStyleComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  // Set the ComboBox style to csOwnerDrawFixed to allow custom drawing
  Self.Style := csOwnerDrawFixed;

  // Add items (all available brush styles)
  PopulateItems;

  // Set the default selected item (index 0)
  Self.Width     := 200;
  Self.ItemIndex := 0;
end;

procedure TBrushStyleComboBox.PopulateItems;
begin
  // Add all brush style names as items to the ComboBox
  Items.Add('Clear');
  Items.Add('Solid');
  Items.Add('FDiagonal');
  Items.Add('Cross');
  Items.Add('DiagCross');
  Items.Add('Horizontal');
  Items.Add('Vertical');
  Items.Add('BDiagonal');
  //Items.Add('test');
end;

procedure TBrushStyleComboBox.DrawItem(Index: Integer; ARect: TRect; State: TOwnerDrawState);
var BrushStyle: TBrushStyle; TmpStyleName: String;
begin
  with Canvas do
  begin
    // Fill the background of the item rectangle
    FillRect(ARect);

    // Set the Brush.Style and corresponding name based on the item index
    case Index of
      0: begin
           BrushStyle := bsClear;
           TmpStyleName := 'Clear';
         end;
      1: begin
           BrushStyle := bsSolid;
           TmpStyleName := 'Solid';
         end;
      2: begin
           BrushStyle := bsHorizontal;
           TmpStyleName := 'Horizontal';
         end;
      3: begin
           BrushStyle := bsVertical;
           TmpStyleName := 'Vertical';
         end;
      4: begin
           BrushStyle := bsFDiagonal;
           TmpStyleName := 'FDiagonal';
         end;
      5: begin
           BrushStyle := bsBDiagonal;
           TmpStyleName := 'BDiagonal';
         end;
      6: begin
           BrushStyle := bsCross;
           TmpStyleName := 'Cross';
         end;
      7: begin
           BrushStyle := bsDiagCross;
           TmpStyleName := 'Diag Cross';
         end;
    else
      BrushStyle := bsClear;  // Default value for invalid index
      TmpStyleName := 'Clear';
    end;

    // Draw the style name in the ComboBox item
    TextOut(ARect.Left + 5, ARect.Top + (ARect.Height div 2) - (TextHeight(TmpStyleName) div 2), TmpStyleName);

    // Draw a filled rectangle demonstrating the Brush.Style
    Brush.Style := BrushStyle;
    if  Brush.Style <> bsClear then
      Brush.Color := clBlack;
    Rectangle(ARect.Left + 100, ARect.Top + 4, ARect.Right - 5, ARect.Bottom - 4);
  end;
end;

procedure Register;
begin
  RegisterComponents('Additional', [TBrushStyleComboBox]);
end;

end.
