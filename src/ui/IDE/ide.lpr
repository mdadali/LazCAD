program ide;

{$MODE Delphi}

uses
  Forms, Interfaces,
  ide_editor in 'ide_editor.pas' {editor},
  //ide_debugoutput in 'ide_debugoutput.pas' {debugoutput},
  uFrmGotoLine in 'uFrmGotoLine.pas' {frmGotoLine};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(Teditor, editor);
  //Application.CreateForm(Tdebugoutput, debugoutput);
  Application.CreateForm(TfrmGotoLine, frmGotoLine);
  //Application.CreateForm(TConfirmReplaceDialog, ConfirmReplaceDialog);
  Application.Run;
end.
