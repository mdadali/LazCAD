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
  runtimetypeinfocontrols, crt, Forms, pkg_gifanim, indylaz, abbrevia, fSplash,
  fAbout, FiguresAsComponents, fBaseComponent, fComponentArc, fComponentBSpline,
  fComponentEllipse, fComponentFrame, fComponentLine, fComponentPLine,
  fComponentPolygon, fComponentRect, fcomponentcirculararc, fMain, fDrawing,
  fcomponentcircle, fLibraryBlocks, fLayers, applicationh, CADDocument, camh,
  cImportGCODE2, cImportEssi, fSimulation, fTTF2Vector,
  fCADSys4ClipperInterface, fComponentGraphicObj, fComponentContainer, contours,
  fSetKerfType, UndoRedo, fobjectposition;

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
  frmSplash.ProgressBar1.StepIt;
  Delay(500);
  Application.ProcessMessages;

  for i := 0 to  99999 do //3999 do
  begin
    frmSplash.ProgressBar1.StepIt;
    Application.ProcessMessages;
    //Delay(10);
  end;

  frmSplash.Hide;
  Application.Run;
  frmSplash.Free;
end.

