unit uPSI_applicationh;
{
This file has been generated by UnitParser v0.7, written by M. Knight
and updated by NP. v/d Spek and George Birbilis. 
Source Code from Carlo Kok has been used to implement various sections of
UnitParser. Components of ROPS are used in the construction of UnitParser,
code implementing the class wrapper is taken from Carlo Kok's conv utility

}
interface
 

 
uses
   SysUtils
  ,Classes
  ,uPSComponent
  ,uPSRuntime
  ,uPSCompiler
  ;
 
type 
(*----------------------------------------------------------------------------*)
  TPSImport_applicationh = class(TPSPlugin)
  public
    procedure CompileImport1(CompExec: TPSScript); override;
    procedure ExecImport1(CompExec: TPSScript; const ri: TPSRuntimeClassImporter); override;
  end;
 
 
{ compile-time registration functions }
procedure SIRegister_applicationh(CL: TPSPascalCompiler);

{ run-time registration functions }
procedure RIRegister_applicationh_Routines(S: TPSExec);

procedure Register;

implementation


uses
   Forms
  ,Dialogs
  ,IniFiles
  ,messages
  ,LResources
  ,AbUnzper
  ,AbZBrows
  ,AbArcTyp
  ,AbZipTyp
  ,windows
  ,CommonUtils
  ,CADSys4
  ,CS4Shapes
  ,CS4BaseTypes
  ,applicationh
  ;
 
 
procedure Register;
begin
  RegisterComponents('Pascal Script', [TPSImport_applicationh]);
end;

(* === compile-time registration functions === *)
(*----------------------------------------------------------------------------*)
procedure SIRegister_applicationh(CL: TPSPascalCompiler);
begin
 CL.AddConstantN('CAD_LAYER_TEMPLATE','String').SetString( 'Template');
 CL.AddConstantN('CAD_ONSTART_FILE_STATE_NONE','LongInt').SetInt( 0);
 CL.AddConstantN('CAD_ONSTART_FILE_STATE_LAST','LongInt').SetInt( 1);
 CL.AddConstantN('CAD_ONSTART_FILE_STATE_NEW','LongInt').SetInt( 2);
 CL.AddDelphiFunction('Procedure ReadIniFile');
 CL.AddDelphiFunction('Procedure WriteIniFile');
 CL.AddDelphiFunction('Function GetAppExeName : string');
 CL.AddDelphiFunction('Function GetAppFullPath : string');
 CL.AddDelphiFunction('Function GetAppPath : string');
 CL.AddDelphiFunction('Function GetOnlyFileName( AFileName : string) : string');
 CL.AddDelphiFunction('Function GetFileExtention( AFileName : string) : string');
 CL.AddDelphiFunction('Function GetAppDrawingsPath : string');
 CL.AddDelphiFunction('Function GetAppTempPath : string');
 CL.AddDelphiFunction('Function GetAppDBPath : string');
 CL.AddDelphiFunction('Function GetAppDBName : string');
 CL.AddDelphiFunction('Function GetAppConfigPath : string');
 CL.AddDelphiFunction('Function GetAppDataPath : string');
 CL.AddDelphiFunction('Function GetAppSystemPath : string');
 CL.AddDelphiFunction('Function GetAppApplicationsPath : string');
 CL.AddDelphiFunction('Function GetAppFontsPath : string');
 CL.AddDelphiFunction('Function GetAppFontsFNTPath : string');
 CL.AddDelphiFunction('Function GetAppFontsTTFPath : string');
 CL.AddDelphiFunction('Function GetAppTemplatesPath : string');
 CL.AddDelphiFunction('Function GetAppLanguagesPath : string');
 CL.AddDelphiFunction('Function GetAppPlugInsPath : string');
 CL.AddDelphiFunction('Function GetAppImagesPath : string');
 CL.AddDelphiFunction('Function GetAppBlockLibrarysPath : string');
 CL.AddDelphiFunction('Function GetAppProjectsPath : string');
 CL.AddDelphiFunction('Function GetAppPascalScriptsPath : string');
end;

(* === run-time registration functions === *)
(*----------------------------------------------------------------------------*)
procedure RIRegister_applicationh_Routines(S: TPSExec);
begin
 S.RegisterDelphiFunction(@ReadIniFile, 'ReadIniFile', cdRegister);
 S.RegisterDelphiFunction(@WriteIniFile, 'WriteIniFile', cdRegister);
 S.RegisterDelphiFunction(@GetAppExeName, 'GetAppExeName', cdRegister);
 S.RegisterDelphiFunction(@GetAppFullPath, 'GetAppFullPath', cdRegister);
 S.RegisterDelphiFunction(@GetAppPath, 'GetAppPath', cdRegister);
 S.RegisterDelphiFunction(@GetOnlyFileName, 'GetOnlyFileName', cdRegister);
 S.RegisterDelphiFunction(@GetFileExtention, 'GetFileExtention', cdRegister);
 S.RegisterDelphiFunction(@GetAppDrawingsPath, 'GetAppDrawingsPath', cdRegister);
 S.RegisterDelphiFunction(@GetAppTempPath, 'GetAppTempPath', cdRegister);
 S.RegisterDelphiFunction(@GetAppDBPath, 'GetAppDBPath', cdRegister);
 S.RegisterDelphiFunction(@GetAppDBName, 'GetAppDBName', cdRegister);
 S.RegisterDelphiFunction(@GetAppConfigPath, 'GetAppConfigPath', cdRegister);
 S.RegisterDelphiFunction(@GetAppDataPath, 'GetAppDataPath', cdRegister);
 S.RegisterDelphiFunction(@GetAppSystemPath, 'GetAppSystemPath', cdRegister);
 S.RegisterDelphiFunction(@GetAppApplicationsPath, 'GetAppApplicationsPath', cdRegister);
 S.RegisterDelphiFunction(@GetAppFontsPath, 'GetAppFontsPath', cdRegister);
 S.RegisterDelphiFunction(@GetAppFontsFNTPath, 'GetAppFontsFNTPath', cdRegister);
 S.RegisterDelphiFunction(@GetAppFontsTTFPath, 'GetAppFontsTTFPath', cdRegister);
 S.RegisterDelphiFunction(@GetAppTemplatesPath, 'GetAppTemplatesPath', cdRegister);
 S.RegisterDelphiFunction(@GetAppLanguagesPath, 'GetAppLanguagesPath', cdRegister);
 S.RegisterDelphiFunction(@GetAppPlugInsPath, 'GetAppPlugInsPath', cdRegister);
 S.RegisterDelphiFunction(@GetAppImagesPath, 'GetAppImagesPath', cdRegister);
 S.RegisterDelphiFunction(@GetAppBlockLibrarysPath, 'GetAppBlockLibrarysPath', cdRegister);
 S.RegisterDelphiFunction(@GetAppProjectsPath, 'GetAppProjectsPath', cdRegister);
 S.RegisterDelphiFunction(@GetAppPascalScriptsPath, 'GetAppPascalScriptsPath', cdRegister);
end;

 
 
{ TPSImport_applicationh }
(*----------------------------------------------------------------------------*)
procedure TPSImport_applicationh.CompileImport1(CompExec: TPSScript);
begin
  SIRegister_applicationh(CompExec.Comp);
end;
(*----------------------------------------------------------------------------*)
procedure TPSImport_applicationh.ExecImport1(CompExec: TPSScript; const ri: TPSRuntimeClassImporter);
begin
  RIRegister_applicationh_Routines(CompExec.Exec); // comment it if no routines
end;
(*----------------------------------------------------------------------------*)
 
 
end.
