unit fPenStyleComboBox;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls;

type
  TPenStyleComboBox = class(TComboBox)
  private
    fPenStyle: TPenStyle;
    fPenStyleName: string;
    function  GetSelectedPenStyle: TPenStyle;
    procedure SetSelectedPenStyle(APenStyle: TPenStyle);
    function  GetSelectedStyleName: string;
    procedure DrawItem(Index: Integer; ARect: TRect; State: TOwnerDrawState); override;
    procedure PopulateItems;
  protected

  public
    constructor Create(AOwner: TComponent); override;
    property    SelectedPenStyle: TPenStyle  read GetSelectedPenStyle write SetSelectedPenStyle;
    property    SelectedPenStyleName: string read GetSelectedStyleName;
  published

  end;

procedure Register;

implementation

function  TPenStyleComboBox.GetSelectedStyleName: string;
begin
  case self.ItemIndex of
    0: result := 'psSolid';
    1: result := 'psDash';
    2: result := 'psDot';
    3: result := 'psDashDot';
    4: result := 'psDashDotDot';
    else result := 'unknow';
  end;
end;

procedure TPenStyleComboBox.SetSelectedPenStyle(APenStyle: TPenStyle);
begin
  case APenStyle of
    psSolid:      ItemIndex := 0;
    psDash:       ItemIndex := 1;
    psDot:        ItemIndex := 2;
    psDashDot:    ItemIndex := 3;
    psDashDotDot: ItemIndex := 4;
    else          ItemIndex := 0;
  end;
end;

function  TPenStyleComboBox.GetSelectedPenStyle: TPenStyle;
begin
  case self.ItemIndex of
    0: result := psSolid;
    1: result := psDash;
    2: result := psDot;
    3: result := psDashDot;
    4: result := psDashDotDot;
    else result := psSolid;
  end;
end;

constructor TPenStyleComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  // Set the ComboBox style to csOwnerDrawFixed to allow custom drawing
  Self.Style := csOwnerDrawFixed;

  // Add items (all available pen styles) to the ComboBox
  PopulateItems;

  // Set the default selected item (index 0)
  Self.ItemIndex := 0;
  Self.Width     := 200;
end;

procedure TPenStyleComboBox.PopulateItems;
begin
  // Add all pen style names as items to the ComboBox
  Items.Add('Solid');
  Items.Add('Dash');
  Items.Add('Dot');
  Items.Add('DashDot');
  Items.Add('DashDotDot');
end;

procedure TPenStyleComboBox.DrawItem(Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  TmpPenStyle: TPenStyle;
  TmpStyleName: String;
begin
  with Canvas do
  begin
    // Fill the background of the item rectangle
    FillRect(ARect);

    // Set the Pen.Style and corresponding name based on the item index
    case Index of
      0: begin
           TmpPenStyle := psSolid;
           TmpStyleName := 'Solid';
         end;
      1: begin
           TmpPenStyle := psDash;
           TmpStyleName := 'Dash';
         end;
      2: begin
           TmpPenStyle := psDot;
           TmpStyleName := 'Dot';
         end;
      3: begin
           TmpPenStyle := psDashDot;
           TmpStyleName := 'Dash Dot';
         end;
      4: begin
           TmpPenStyle := psDashDotDot;
           TmpStyleName := 'Dash Dot Dot';
         end;
    else
      TmpPenStyle := psSolid;  // Default value for invalid index
      TmpStyleName := 'Solid';
    end;

    // Draw the style name in the ComboBox item
    //TextOut(ARect.Left + 5, ARect.Top + (ARect.Height div 4), StyleName);

    TextOut(ARect.Left + 5, ARect.Top + (ARect.Height div 2) - (TextHeight(TmpStyleName) div 2), TmpStyleName);

    Pen.Style := psSolid;
    Rectangle(ARect.Left + 100, ARect.Top + 4, ARect.Right - 5, ARect.Bottom - 4);
    // Draw a line demonstrating the Pen.Style
    Pen.Style := TmpPenStyle;

    MoveTo(ARect.Left + 100, (ARect.Top + ARect.Bottom) div 2);  // Start point of the line
    LineTo(ARect.Right - 5, (ARect.Top + ARect.Bottom) div 2);   // End point of the line
  end;
end;

procedure Register;
begin
  RegisterComponents('Additional',[TPenStyleComboBox]);
end;

end.
