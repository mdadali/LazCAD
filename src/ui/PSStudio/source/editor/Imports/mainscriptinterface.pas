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
  {$IFDEF CADSys4}
    upsi_cad2dscripinterface,
  {$ENDIF}
  //uPSI_DialogsScriptInterface, //old
  uPSI_Dialogs;

procedure SIRegister_MainScriptInterface(CL: TPSPascalCompiler);
procedure RIRegister_MainScriptInterface_Routines(S: TPSExec; x: TPSRuntimeClassImporter);

implementation

procedure SIRegister_MainScriptInterface(CL: TPSPascalCompiler);
begin
  {$IFDEF CADSys4}
    SIRegister_CAD2DScripInterface(CL);
  {$ENDIF}

  SIRegister_MathScriptInterface(CL);
  //SIRegister_DialogsScriptInterface(CL); //old
  SIRegister_Dialogs(CL);
end;

procedure RIRegister_MainScriptInterface_Routines(S: TPSExec; x: TPSRuntimeClassImporter);
begin
  {$IFDEF CADSys4}
    RIRegister_CAD2DScripInterface_Routines(S);
  {$ENDIF}

  RIRegister_MathScriptInterface_Routines(S);
  //RIRegister_DialogsScriptInterface_Routines(S); //oöd
  RIRegister_Dialogs(x);

  RIRegister_Dialogs_Routines(S);
end;

end.
