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

{function TfrmSplash.VerInfo: string;
var
  FileName: string;
  VI: TVerInfo;
  FFI: TVSFixedFileInfo;
  TransIdx: Integer;
  Trans: string;
  StrIdx: Integer;
  StrName: string;
begin
  // Choose file name
  FileName := Application.ExeName;

  // Create version info object
  VI := TVerInfo.Create(FileName);
  try
    //Display(FileName);
    if VI.HasVerInfo then
    begin
      // Get fixed file info and display subset of fields
      FFI := VI.FixedFileInfo;
      //ShowMessage(Format('FFI: Product Version: %d.%d.%d.%d',
        //result := (Format('%d.%d.%d.%d',
        //[HiWord(FFI.dwProductVersionMS), LoWord(FFI.dwProductVersionMS),
        //HiWord(FFI.dwProductVersionLS), LoWord(FFI.dwFileVersionLS)]));
      result :=(Format('%d.%d.%d.%d',
        [HiWord(FFI.dwFileVersionMS), LoWord(FFI.dwFileVersionMS),
        HiWord(FFI.dwFileVersionLS), LoWord(FFI.dwFileVersionLS)]));
    end
    else
      result := '';
      // File contains no version info
      //Display('*** No version information ***');
  finally
    VI.Free;
  end;

end;}

procedure TfrmSplash.FormCreate(Sender: TObject);

begin
  GifAnim1.FileName := applicationh.fGifAnimFile;
  GifAnim1.Animate := true;
  //lbVersion.Caption := lbVersion.Caption + VerInfo;
end;

end.
