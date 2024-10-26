unit fSetKerfType;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TfrmSetKerfType }

  TfrmSetKerfType = class(TForm)
    btnCancel: TButton;
    btnOK: TButton;
    cboxKerfTypes: TComboBox;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  frmSetKerfType: TfrmSetKerfType;

implementation

{$R *.lfm}

{ TfrmSetKerfType }

procedure TfrmSetKerfType.FormCreate(Sender: TObject);
begin
  cboxKerfTypes.Items.Clear;
  cboxKerfTypes.Items.Add('None');
  cboxKerfTypes.Items.Add('InnerContourCW');
  cboxKerfTypes.Items.Add('InnerContourCCW');
  cboxKerfTypes.Items.Add('OuterContourCW');
  cboxKerfTypes.Items.Add('OuterContourCCW');
  cboxKerfTypes.ItemIndex := 3;
end;

end.

