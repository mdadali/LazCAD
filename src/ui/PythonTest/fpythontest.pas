unit fPythonTest;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, simpleipc, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ComCtrls, ActnList, Menus, fileutil, PythonEngine,
  PythonGUIInputOutput, SynEdit, SynHighlighterPython, applicationh,
  CADDocument;

type

  { TfrmPythonIDE }

  TfrmPythonIDE = class(TForm)
    acFileOpen: TAction;
    acFileNew: TAction;
    acFileSave: TAction;
    acScriptRun: TAction;
    acClose: TAction;
    ActionList1: TActionList;
    ImageList1: TImageList;
    Label1: TLabel;
    Memo1: TMemo;
    MenuItem1: TMenuItem;
    OpenDialog1: TOpenDialog;
    DXFOpenDialog: TOpenDialog;
    Panel1: TPanel;
    Panel2: TPanel;
    PDVar_hLcBlock: TPythonDelphiVar;
    PDVar_hLcWnd: TPythonDelphiVar;
    PDVar_hLcDrw: TPythonDelphiVar;
    PDVar_DrwFileName: TPythonDelphiVar;
    PDVar_ProgressBar: TPythonDelphiVar;
    PDVar_OpenFileName: TPythonDelphiVar;
    PDVar_ExportFileName: TPythonDelphiVar;
    PDVar_ApplicationPath: TPythonDelphiVar;
    PopupMenu1: TPopupMenu;
    PythonEngine1: TPythonEngine;
    PythonGUIInputOutput1: TPythonGUIInputOutput;
    PythonModule1: TPythonModule;
    SaveDialog1: TSaveDialog;
    ExportFileDialog: TSaveDialog;
    Splitter1: TSplitter;
    SynEdit1: TSynEdit;
    SynPythonSyn1: TSynPythonSyn;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    procedure acCloseExecute(Sender: TObject);
    procedure acFileNewExecute(Sender: TObject);
    procedure acFileOpenExecute(Sender: TObject);
    procedure acFileSaveExecute(Sender: TObject);
    procedure acScriptRunExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure PDVar_ApplicationPathGetData(Sender: TObject; var Data: Variant);
    procedure PDVar_DrwFileNameGetData(Sender: TObject; var Data: Variant);
    procedure PDVar_hLcBlockChange(Sender: TObject);
    procedure PDVar_hLcBlockGetData(Sender: TObject; var Data: Variant);
    procedure PDVar_hLcDrwGetData(Sender: TObject; var Data: Variant);
    procedure PDVar_hLcWndGetData(Sender: TObject; var Data: Variant);
    procedure PDVar_ProgressBarGetData(Sender: TObject; var Data: Variant);
    procedure SynEdit1Change(Sender: TObject);
  private
    procedure WritePythonInfoToConsole;
  public

  end;

var
  frmPythonIDE: TfrmPythonIDE;
  ScriptChanged: boolean;

implementation

{$R *.lfm}

{ TfrmPythonIDE }

procedure TfrmPythonIDE.WritePythonInfoToConsole;
begin
  PythonEngine1.ExecString('import sys; print(' + QuotedStr('Python') + ', sys.version)');
end;

procedure TfrmPythonIDE.acFileSaveExecute(Sender: TObject);
begin
  If SaveDialog1.Execute then
  begin
    SynEdit1.Lines.SaveToFile(SaveDialog1.FileName);
    ScriptChanged := false;
  end;
end;

procedure TfrmPythonIDE.acScriptRunExecute(Sender: TObject);
begin
  if SynEdit1.Lines.Count > 1 then
  begin
    //Memo1.Lines.Clear;
    //Self.WindowState := wsMinimized;
    PythonEngine1.ExecString(UTF8Encode(SynEdit1.Text));
  end;
end;

procedure TfrmPythonIDE.acFileNewExecute(Sender: TObject);
begin
  //if Saved then begin
    SynEdit1.Lines.Clear;
    Memo1.Lines.Clear;
    WritePythonInfoToConsole;
  //end;
end;

procedure TfrmPythonIDE.acCloseExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmPythonIDE.FormClose(Sender: TObject; var CloseAction: TCloseAction
  );
begin
  CloseAction := caHide;
end;

procedure TfrmPythonIDE.FormCreate(Sender: TObject);
begin
  MaskFPUExceptions(True);
  PythonEngine1.DllPath := applicationh.fIniFile.ReadString('Python', 'DllPath', '');
  PythonEngine1.DllName := applicationh.fIniFile.ReadString('Python', 'DllName', '');
  PythonEngine1.LoadDll;
end;

procedure TfrmPythonIDE.FormDestroy(Sender: TObject);
begin

  //PythonModule1.Engine := nil;

end;

procedure TfrmPythonIDE.MenuItem1Click(Sender: TObject);
begin
  memo1.Text := '';
end;

procedure TfrmPythonIDE.PDVar_ApplicationPathGetData(Sender: TObject;
  var Data: Variant);
begin
  Data := applicationh.GetAppPath;
end;

procedure TfrmPythonIDE.PDVar_DrwFileNameGetData(Sender: TObject;
  var Data: Variant);
begin
  Data := CADDocument.CADFileName;
end;

procedure TfrmPythonIDE.PDVar_hLcBlockChange(Sender: TObject);
begin
  ShowMessage('PDVar_hLcBlock Changed');
end;


procedure TfrmPythonIDE.PDVar_hLcBlockGetData(Sender: TObject; var Data: Variant
  );
begin
  Data := CADDocument.hBlock;
end;

procedure TfrmPythonIDE.PDVar_hLcDrwGetData(Sender: TObject; var Data: Variant);
begin
  Data := CADDocument.hLcDrv;
end;

procedure TfrmPythonIDE.PDVar_hLcWndGetData(Sender: TObject; var Data: Variant);
begin
  Data := CADDocument.hLcWnd;
end;

procedure TfrmPythonIDE.acFileOpenExecute(Sender: TObject);
begin
  if OpenDialog1.Execute then
    SynEdit1.Lines.LoadFromFile(OpenDialog1.FileName);
end;

procedure TfrmPythonIDE.PDVar_ProgressBarGetData(Sender: TObject;
  var Data: Variant);
begin
end;

procedure TfrmPythonIDE.SynEdit1Change(Sender: TObject);
begin
  ScriptChanged := true;
end;



end.

