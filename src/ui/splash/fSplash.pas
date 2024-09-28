unit fSplash;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, ExtCtrls, StdCtrls,
  ComCtrls, GifAnim,
  applicationh,
  CommonUtils;
  //UVerInfoClass,
  //UVerInfoRoutines,
  //UVerInfoTypes;


type

  { TfrmSplash }

  TfrmSplash = class(TForm)
    GifAnim1: TGifAnim;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    lbDeveloper: TLabel;
    lbFPCVersion: TLabel;
    lbLazarus: TLabel;
    lbOSInfo: TLabel;
    lbPgmVersion: TLabel;
    lbVersion: TLabel;
    lbWidgetSet: TLabel;
    ProgressBar1: TProgressBar;
    Panel2: TPanel;
    Image1: TImage;
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
  lbPgmVersion.Caption := GetProgramVersion;
  lbLazarus.Caption    := GetLazarusVersion;
  lbOSInfo.Caption     := GetOSInfo;
  lbWidgetSet.Caption  := GetLCLWidgetSet;
  lbFPCVersion.Caption := GetFPCVersion;
  lbDeveloper.Caption  := 'Mustafa Dadali';

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
