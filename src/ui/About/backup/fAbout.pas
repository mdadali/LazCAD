unit fAbout;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,  ExtCtrls, ComCtrls, GifAnim,
  CommonUtils,
  applicationh, Types;

type

  { TfrmAbout }

  TfrmAbout = class(TForm)
    btnClose: TButton;
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
    Memo1: TMemo;
    Memo2: TMemo;
    Memo3: TMemo;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    Timer1: TTimer;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnCloseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
  public
    { Public declarations }
  end;

implementation

{$R *.lfm}

procedure TfrmAbout.btnCloseClick(Sender: TObject);
begin
  close;
end;

procedure TfrmAbout.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Timer1.Enabled := false;
  Action := caFree;
end;

procedure TfrmAbout.FormCreate(Sender: TObject);
begin
  if FileExists(applicationh.fGifAnimFile) then
  begin
    GifAnim1.FileName := applicationh.fGifAnimFile;
    GifAnim1.Animate := true;
  end else
  MessageDlg('The GIF animation file ' + applicationh.fGifAnimFile + ' does not exist.' + #13#10 +
           'Please change it in the LazCAD.ini file.' + #13#10 +
           'Ensure the path is correct.', mtWarning, [mbOK], 0);

  lbPgmVersion.Caption := GetProgramVersion;
  lbLazarus.Caption    := GetLazarusVersion;
  lbOSInfo.Caption     := GetOSInfo;
  lbWidgetSet.Caption  := GetLCLWidgetSet;
  lbFPCVersion.Caption := GetFPCVersion;
  lbDeveloper.Caption  := 'Mustafa Dadali';
end;

procedure TfrmAbout.FormShow(Sender: TObject);
begin
  btnClose.SetFocus;
  Timer1.Enabled := true;
end;

procedure TfrmAbout.Timer1Timer(Sender: TObject);
begin
  GifAnim1.Left := GifAnim1.Left + 7;
  if  GifAnim1.Left > self.Width then
    GifAnim1.Left := 0 - GifAnim1.Width;
  //Application.ProcessMessages;
end;

end.
