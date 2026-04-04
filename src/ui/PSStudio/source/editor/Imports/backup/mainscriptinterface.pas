unit mainscriptinterface;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  math
 ,uPSComponent
 ,uPSRuntime
 ,uPSCompiler,

  uPSI_MathScriptinterface,
  upsi_cad2dscripinterface,
  uPSI_DialogsScriptInterface,
  uPSI_Dialogs;

procedure SIRegister_MainScriptInterface(CL: TPSPascalCompiler);
procedure RIRegister_MainScriptInterface_Routines(S: TPSExec; x: TPSRuntimeClassImporter);

implementation

procedure SIRegister_MainScriptInterface(CL: TPSPascalCompiler);
begin
  SIRegister_CAD2DScripInterface(CL);
  SIRegister_MathScriptInterface(CL);
  SIRegister_DialogsScriptInterface(CL);
  SIRegister_Dialogs(CL);
end;

procedure RIRegister_MainScriptInterface_Routines(S: TPSExec; x: TPSRuntimeClassImporter);
begin
  RIRegister_CAD2DScripInterface_Routines(S);
  RIRegister_MathScriptInterface_Routines(S);
  RIRegister_DialogsScriptInterface_Routines(S);
  RIRegister_Dialogs(x);

  RIRegister_Dialogs_Routines(S);
end;

end.
