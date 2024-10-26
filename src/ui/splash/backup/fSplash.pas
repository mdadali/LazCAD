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
    GifAnim2: TGifAnim;
    Image1: TImage;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    lbDeveloper: TLabel;
    lbFPCVersion: TLabel;
    lbLazarus: TLabel;
    lbOSInfo: TLabel;
    labelBasedOn: TLabel;
    lbPgmVersion: TLabel;
    lbVersion: TLabel;
    lbWidgetSet: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    ProgressBar1: TProgressBar;
    Timer1: TTimer;
    procedure FormClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Image1Click(Sender: TObject);
    procedure Panel3Click(Sender: TObject);
    procedure Panel4Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
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
    GifAnim2.FileName := applicationh.fGifAnimFile;
    GifAnim2.Animate := true;
  end else
    MessageDlg('The GIF animation file ' + applicationh.fGifAnimFile + ' does not exist.' + #13#10 +
             'Please change it in the LazCAD.ini file.' + #13#10 +
             'Ensure the path is correct.', mtWarning, [mbOK], 0);
end;

procedure TfrmSplash.FormShow(Sender: TObject);
begin
  Timer1.Enabled := true;
end;

procedure TfrmSplash.Image1Click(Sender: TObject);
begin
  close;
end;

procedure TfrmSplash.Panel3Click(Sender: TObject);
begin
  close;
end;

procedure TfrmSplash.Panel4Click(Sender: TObject);
begin
  close;
end;

procedure TfrmSplash.FormClick(Sender: TObject);
begin

end;


procedure TfrmSplash.Timer1Timer(Sender: TObject);
begin
  GifAnim2.Left := GifAnim2.Left + 7;
  if  GifAnim2.Left > self.Width then
    GifAnim2.Left := 0 - GifAnim2.Width;
end;

end.
