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
  runtimetypeinfocontrols,
  printer4lazarus,
  crt,
  Forms,

  pkg_gifanim,
  abbrevia,

  jvRuntimeDesign,

  //uPSDisassembly,
  pascalscript,
  //pascalscriptfcl,


  applicationh,
  fMain,
  fDrawing,
  CADDocument,
  camh,
  fLibraryBlocks,
  fLayers,
  fSplash,
  fAbout,

  FiguresAsComponents,
  cImportEssi,
  fSimulation,
  fTTF2Vector,
  fCADSys4ClipperInterface,

  u_consoleide,
  u_psstudio,
  uCodeGenerator,

  mainscriptinterface,
  cad2dscripinterface,
  upsi_cad2dscripinterface,
  uPSI_CommonUtils,
  uPSI_DialogsScriptInterface,
  uPSI_MathScriptinterface,

  UndoRedo;

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
  Application.CreateForm(TfrmConsoleIDE, ConsoleIDE);
  //Application.CreateForm(TfrmPSStudio, PSStudio);  //crash!!!!!!!!!! (jvDesigner-Component)
  PSStudio := TfrmPSStudio.Create(nil);
  PSStudio.Width  := 1000;
  PSStudio.Height := 600;

  //frmSplash.ProgressBar1.StepIt;
  //Delay(500);
  //Application.ProcessMessages;

  for i := 0 to  {$IFDEF LINUX}99999{$ELSE} 99 {$ENDIF} do
  begin
    frmSplash.ProgressBar1.StepIt;
    Application.ProcessMessages;
  end;

  frmSplash.Hide;
  Application.Run;
  ConsoleIDE.Free;

  PSStudio.JvDesignPanel1.Active := false;
  PSStudio.JvDesignPanel1.Invalidate;
  PSStudio.Free;

  frmSplash.Free;
end.

