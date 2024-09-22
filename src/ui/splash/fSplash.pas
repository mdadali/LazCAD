unit fSplash;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, ExtCtrls, StdCtrls,
  ComCtrls, GifAnim,
  applicationh;
  //UVerInfoClass,
  //UVerInfoRoutines,
  //UVerInfoTypes;


type

  { TfrmSplash }

  TfrmSplash = class(TForm)
    GifAnim1: TGifAnim;
    Label2: TLabel;
    ProgressBar1: TProgressBar;
    Panel2: TPanel;
    Image1: TImage;
    Panel3: TPanel;
    Label3: TLabel;
    Label1: TLabel;
    Edit1: TEdit;
    procedure FormCreate(Sender: TObject);
  private
    { Private-Deklarationen }
    //function VerInfo: string;
  public
    { Public-Deklarationen }
  end;

var
  frmSplash: TfrmSplash;

implementation

{$R *.lfm}


procedure TfrmSplash.FormCreate(Sender: TObject);
begin
  if FileExists(applicationh.fGifAnimFile) then
  begin
    GifAnim1.FileName := applicationh.fGifAnimFile;
    GifAnim1.Animate := true;
  end else
    MessageDlg('The GIF animation file ' + applicationh.fGifAnimFile + ' does not exist.' + #13#10 +
             'Please change it in the LazCAD.ini file.' + #13#10 +
             'Ensure the path is correct.', mtWarning, [mbOK], 0);
end;

end.
