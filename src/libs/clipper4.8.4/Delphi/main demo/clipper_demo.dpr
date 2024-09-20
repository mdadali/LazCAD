program clipper_demo;

uses
  Forms,
  main in 'main.pas' {MainForm},
  clipper in '..\clipper.pas',
  GR32_VPR in '..\..\..\VPR\GR32_VPR.pas',
  gpc in '..\..\..\VPR\gpc.pas',
  GR32_CFDG in '..\..\..\VPR\GR32_CFDG.pas',
  GR32_PathsEx in '..\..\..\VPR\GR32_PathsEx.pas',
  GR32_PolygonsEx in '..\..\..\VPR\GR32_PolygonsEx.pas',
  GR32_VectorGraphics in '..\..\..\VPR\GR32_VectorGraphics.pas',
  GR32_VectorUtils in '..\..\..\VPR\GR32_VectorUtils.pas',
  GR32 in '..\..\..\Graphic32\GR32.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
