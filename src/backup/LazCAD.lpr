program LazCAD;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  runtimetypeinfocontrols, printer4lazarus, pascalscript,
  {$IFDEF UNIX}  pascalscriptfcl, {$ENDIF} crt, Forms, pkg_gifanim,
  abbrevia, fSplash, fAbout, fMain, fDrawing, fLibraryBlocks,
  fLayers, applicationh, CADDocument, cad2dscripinterface, camh,
  FiguresAsComponents, cImportEssi, fSimulation, fTTF2Vector, ide_editor,
  upsi_cad2dscripinterface, uPSI_CommonUtils, fCADSys4ClipperInterface,
  UndoRedo, uPSI_MathScriptinterface, mainscriptinterface;

{$R *.res}

var //FastmmLogFile: PAnsiChar;
    i: integer;

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;

  frmSplash := TfrmSplash.Create(nil);
  frmSplash.Position := poScreenCenter;
  frmSplash.ProgressBar1.Min := 0;
  frmSplash.ProgressBar1.Max := 99999; //399999;
  frmSplash.ProgressBar1.Step := 1;
  frmSplash.ProgressBar1.Position := 0;
  frmSplash.Show;
  frmSplash.Update;

  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TIDE, IDE);

  //frmSplash.ProgressBar1.StepIt;
  //Delay(500);
  //Application.ProcessMessages;

  for i := 0 to  99999 do //3999 do
  begin
    frmSplash.ProgressBar1.StepIt;
    Application.ProcessMessages;
  end;

  frmSplash.Hide;
  Application.Run;
  IDE.Free;
  frmSplash.Free;

end.

