unit MinScriptInterface;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  math
 ,uPSComponent
 ,uPSRuntime
 ,uPSCompiler,

  uPSI_MathScriptinterface,
  upsi_cad2dscripinterface;

procedure SIRegister_MainScriptInterface(CL: TPSPascalCompiler);
procedure RIRegister_MainScriptInterface_Routines(S: TPSExec);


implementation

procedure SIRegister_MainScriptInterface(CL: TPSPascalCompiler);
begin

end;

procedure RIRegister_MainScriptInterface_Routines(S: TPSExec);
begin

end;

end.

