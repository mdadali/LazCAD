unit uPSI_Registry;
{
This file has been generated by UnitParser v0.4b, written by M. Knight
and updated by NP. v/d Spek.
Source Code from Carlo Kok has been used to implement various sections of
UnitParser. Components of ifps3 are used in the construction of UnitParser,
code implementing the class wrapper is taken from Carlo Kok''s conv unility
}

{$IFDEF MSWINDOWS}
{$I ..\PascalScript.inc}
{$ELSE}
{$I ../PascalScript.inc}
{$ENDIF}

interface

uses
  SysUtils, Classes, uPSComponent, uPSCompiler, uPSRuntime;

type
(*----------------------------------------------------------------------------*)
  TPSImport_Registry = class(TPSPlugin)
  public
    procedure CompOnUses(CompExec: TPSScript); override;
    procedure ExecOnUses(CompExec: TPSScript); override;
    procedure CompileImport1(CompExec: TPSScript); override;
    procedure CompileImport2(CompExec: TPSScript); override;
    procedure ExecImport1(CompExec: TPSScript; const ri: TPSRuntimeClassImporter); override;
    procedure ExecImport2(CompExec: TPSScript; const ri: TPSRuntimeClassImporter); override;
  end;

implementation


uses
   Windows ,IniFiles ,Registry ;

(* === compile-time registration functions === *)
(*----------------------------------------------------------------------------*)
procedure SIRegister_TRegistryIniFile(CL: TPSPascalCompiler);
begin
  //with RegClassS(CL,'TCustomIniFile', 'TRegistryIniFile') do
  with CL.AddClassN(CL.FindClass('TCustomIniFile'),'TRegistryIniFile') do
  begin
    RegisterMethod('constructor Create(const FileName: string);');
    RegisterMethod('constructor CreateA(const FileName: string; AAccess: LongWord);');
    RegisterProperty('RegIniFile', 'TRegIniFile', iptr);
  end;
end;

(*----------------------------------------------------------------------------*)
procedure SIRegister_TRegIniFile(CL: TPSPascalCompiler);
begin
  //with RegClassS(CL,'TRegistry', 'TRegIniFile') do
  with CL.AddClassN(CL.FindClass('TRegistry'),'TRegIniFile') do
  begin
    RegisterMethod('constructor Create(const FileName: string);');
    RegisterMethod('constructor CreateA(const FileName: string; AAccess: LongWord);');
    RegisterMethod('function ReadString(const Section, Ident, Default: string): string');
    RegisterMethod('function ReadInteger(const Section, Ident: string; Default: LongInt): LongInt');
    RegisterMethod('procedure WriteInteger(const Section, Ident: string; Value: LongInt)');
    RegisterMethod('procedure WriteString(const Section, Ident, Value: string)');
    RegisterMethod('function ReadBool(const Section, Ident: string; Default: Boolean): Boolean');
    RegisterMethod('procedure WriteBool(const Section, Ident: string; Value: Boolean)');
    RegisterMethod('procedure ReadSection(const Section: string; Strings: TStrings)');
    RegisterMethod('procedure ReadSections(Strings: TStrings)');
    RegisterMethod('procedure ReadSectionValues(const Section: string; Strings: TStrings)');
    RegisterMethod('procedure EraseSection(const Section: string)');
    RegisterMethod('procedure DeleteKey(const Section, Ident: string)');
    RegisterProperty('FileName', 'string', iptr);
  end;
end;

(*----------------------------------------------------------------------------*)
procedure SIRegister_TRegistry(CL: TPSPascalCompiler);
begin
  //with RegClassS(CL,'TObject', 'TRegistry') do
  with CL.AddClassN(CL.FindClass('TObject'),'TRegistry') do
  begin
    RegisterMethod('constructor Create;');
    RegisterMethod('constructor CreateA(AAccess: LongWord);');
    RegisterMethod('procedure CloseKey');
    RegisterMethod('function CreateKey(const Key: string): Boolean');
    RegisterMethod('function DeleteKey(const Key: string): Boolean');
    RegisterMethod('function DeleteValue(const Name: string): Boolean');
    RegisterMethod('function GetDataInfo(const ValueName: string; var Value: TRegDataInfo): Boolean');
    RegisterMethod('function GetDataSize(const ValueName: string): Integer');
    RegisterMethod('function GetDataType(const ValueName: string): TRegDataType');
    RegisterMethod('function GetKeyInfo(var Value: TRegKeyInfo): Boolean');
    RegisterMethod('procedure GetKeyNames(Strings: TStrings)');
    RegisterMethod('procedure GetValueNames(Strings: TStrings)');
    RegisterMethod('function HasSubKeys: Boolean');
    RegisterMethod('function KeyExists(const Key: string): Boolean');
    RegisterMethod('function LoadKey(const Key, FileName: string): Boolean');
    RegisterMethod('procedure MoveKey(const OldName, NewName: string; Delete: Boolean)');
    RegisterMethod('function OpenKey(const Key: string; CanCreate: Boolean): Boolean');
    RegisterMethod('function OpenKeyReadOnly(const Key: string): Boolean');
    RegisterMethod('function ReadCurrency(const Name: string): Currency');
    RegisterMethod('function ReadBool(const Name: string): Boolean');
    RegisterMethod('function ReadDate(const Name: string): TDateTime');
    RegisterMethod('function ReadDateTime(const Name: string): TDateTime');
    RegisterMethod('function ReadFloat(const Name: string): Double');
    RegisterMethod('function ReadInteger(const Name: string): Integer');
    RegisterMethod('function ReadString(const Name: string): string');
    RegisterMethod('function ReadTime(const Name: string): TDateTime');
    RegisterMethod('function RegistryConnect(const UNCName: string): Boolean');
    RegisterMethod('procedure RenameValue(const OldName, NewName: string)');
    RegisterMethod('function ReplaceKey(const Key, FileName, BackUpFileName: string): Boolean');
    RegisterMethod('function RestoreKey(const Key, FileName: string): Boolean');
    RegisterMethod('function SaveKey(const Key, FileName: string): Boolean');
    RegisterMethod('function UnLoadKey(const Key: string): Boolean');
    RegisterMethod('function ValueExists(const Name: string): Boolean');
    RegisterMethod('procedure WriteCurrency(const Name: string; Value: Currency)');
    RegisterMethod('procedure WriteBool(const Name: string; Value: Boolean)');
    RegisterMethod('procedure WriteDate(const Name: string; Value: TDateTime)');
    RegisterMethod('procedure WriteDateTime(const Name: string; Value: TDateTime)');
    RegisterMethod('procedure WriteFloat(const Name: string; Value: Double)');
    RegisterMethod('procedure WriteInteger(const Name: string; Value: Integer)');
    RegisterMethod('procedure WriteString(const Name, Value: string)');
    RegisterMethod('procedure WriteExpandString(const Name, Value: string)');
    RegisterMethod('procedure WriteTime(const Name: string; Value: TDateTime)');
    RegisterProperty('CurrentKey', 'HKEY', iptr);
    RegisterProperty('CurrentPath', 'string', iptr);
    RegisterProperty('LazyWrite', 'Boolean', iptrw);
    RegisterProperty('RootKey', 'HKEY', iptrw);
    RegisterProperty('Access', 'LongWord', iptrw);
  end;
end;

(*----------------------------------------------------------------------------*)
procedure SIRegister_Registry(CL: TPSPascalCompiler);
begin
  CL.AddClassN(CL.FindClass('TObject'),'ERegistryException');
  SIRegister_TRegistry(CL);
  SIRegister_TRegIniFile(CL);
  SIRegister_TRegistryIniFile(CL);
end;

(* === run-time registration functions === *)
{$IFDEF DELPHI10UP}{$REGION 'TRegistryIniFile'}{$ENDIF}
{$IFDEF class_helper_present}
type
  TRegistryIniFile_PSHelper = class helper for TRegistryIniFile
  public
    Function Create_P(CreateNewInstance: Boolean;  const FileName : string):TObject;
    Function CreateA_P(CreateNewInstance: Boolean;  const FileName : string; AAccess : LongWord):TObject;
    procedure RegIniFile_R(var T: TRegIniFile);
  end;
(*----------------------------------------------------------------------------*)
procedure TRegistryIniFile_PSHelper.RegIniFile_R(var T: TRegIniFile);
begin T := Self.RegIniFile; end;

(*----------------------------------------------------------------------------*)
Function TRegistryIniFile_PSHelper.CreateA_P(CreateNewInstance: Boolean;  const FileName : string; AAccess : LongWord):TObject;
Begin Result := TRegistryIniFile.Create(FileName, AAccess); END;

(*----------------------------------------------------------------------------*)
Function TRegistryIniFile_PSHelper.Create_P(CreateNewInstance: Boolean;  const FileName : string):TObject;
Begin Result := TRegistryIniFile.Create(FileName); END;

procedure RIRegister_TRegistryIniFile(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(TRegistryIniFile) do
  begin
    RegisterConstructor(@TRegistryIniFile.Create_P, 'Create');
    RegisterConstructor(@TRegistryIniFile.CreateA_P, 'CreateA');
    RegisterPropertyHelper(@TRegistryIniFile.RegIniFile_R,nil,'RegIniFile');
  end;
end;

{$ELSE}
(*----------------------------------------------------------------------------*)
procedure TRegistryIniFileRegIniFile_R(Self: TRegistryIniFile; var T: TRegIniFile);
begin T := Self.RegIniFile; end;

(*----------------------------------------------------------------------------*)
Function TRegistryIniFileCreateA_P(Self: TClass; CreateNewInstance: Boolean;  const FileName : string; AAccess : LongWord):TObject;
Begin Result := TRegistryIniFile.Create(FileName, AAccess); END;

(*----------------------------------------------------------------------------*)
Function TRegistryIniFileCreate_P(Self: TClass; CreateNewInstance: Boolean;  const FileName : string):TObject;
Begin Result := TRegistryIniFile.Create(FileName); END;

procedure RIRegister_TRegistryIniFile(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(TRegistryIniFile) do
  begin
    RegisterConstructor(@TRegistryIniFileCreate_P, 'Create');
    RegisterConstructor(@TRegistryIniFileCreateA_P, 'CreateA');
    RegisterPropertyHelper(@TRegistryIniFileRegIniFile_R,nil,'RegIniFile');
  end;
end;

{$ENDIF class_helper_present}
{$IFDEF DELPHI10UP}{$ENDREGION}{$ENDIF}

{$IFDEF DELPHI10UP}{$REGION 'TRegIniFile'}{$ENDIF}
{$IFDEF class_helper_present}
type
  TRegIniFile_PSHelper = class helper for TRegIniFile
  public
    Function Create_P(CreateNewInstance: Boolean;  const FileName : string):TObject;
    Function CreateA_P(CreateNewInstance: Boolean;  const FileName : string; AAccess : LongWord):TObject;
    procedure FileName_R(var T: string);
  end;

(*----------------------------------------------------------------------------*)
procedure TRegIniFile_PSHelper.FileName_R(var T: string);
begin T := Self.FileName; end;

(*----------------------------------------------------------------------------*)
Function TRegIniFile_PSHelper.CreateA_P(CreateNewInstance: Boolean;  const FileName : string; AAccess : LongWord):TObject;
Begin Result := TRegIniFile.Create(FileName, AAccess); END;

(*----------------------------------------------------------------------------*)
Function TRegIniFile_PSHelper.Create_P(CreateNewInstance: Boolean;  const FileName : string):TObject;
Begin Result := TRegIniFile.Create(FileName); END;

procedure RIRegister_TRegIniFile(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(TRegIniFile) do
  begin
    RegisterConstructor(@TRegIniFile.Create_P, 'Create');
    RegisterConstructor(@TRegIniFile.CreateA_P, 'CreateA');
    RegisterMethod(@TRegIniFile.ReadString, 'ReadString');
    RegisterMethod(@TRegIniFile.ReadInteger, 'ReadInteger');
    RegisterMethod(@TRegIniFile.WriteInteger, 'WriteInteger');
    RegisterMethod(@TRegIniFile.WriteString, 'WriteString');
    RegisterMethod(@TRegIniFile.ReadBool, 'ReadBool');
    RegisterMethod(@TRegIniFile.WriteBool, 'WriteBool');
    RegisterMethod(@TRegIniFile.ReadSection, 'ReadSection');
    RegisterMethod(@TRegIniFile.ReadSections, 'ReadSections');
    RegisterMethod(@TRegIniFile.ReadSectionValues, 'ReadSectionValues');
    RegisterMethod(@TRegIniFile.EraseSection, 'EraseSection');
    RegisterMethod(@TRegIniFile.DeleteKey, 'DeleteKey');
    RegisterPropertyHelper(@TRegIniFile.FileName_R,nil,'FileName');
  end;
end;

{$ELSE}
(*----------------------------------------------------------------------------*)
procedure TRegIniFileFileName_R(Self: TRegIniFile; var T: string);
begin T := Self.FileName; end;

(*----------------------------------------------------------------------------*)
Function TRegIniFileCreateA_P(Self: TClass; CreateNewInstance: Boolean;  const FileName : string; AAccess : LongWord):TObject;
Begin Result := TRegIniFile.Create(FileName, AAccess); END;

(*----------------------------------------------------------------------------*)
Function TRegIniFileCreate_P(Self: TClass; CreateNewInstance: Boolean;  const FileName : string):TObject;
Begin Result := TRegIniFile.Create(FileName); END;

procedure RIRegister_TRegIniFile(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(TRegIniFile) do
  begin
    RegisterConstructor(@TRegIniFileCreate_P, 'Create');
    RegisterConstructor(@TRegIniFileCreateA_P, 'CreateA');
    RegisterMethod(@TRegIniFile.ReadString, 'ReadString');
    RegisterMethod(@TRegIniFile.ReadInteger, 'ReadInteger');
    RegisterMethod(@TRegIniFile.WriteInteger, 'WriteInteger');
    RegisterMethod(@TRegIniFile.WriteString, 'WriteString');
    RegisterMethod(@TRegIniFile.ReadBool, 'ReadBool');
    RegisterMethod(@TRegIniFile.WriteBool, 'WriteBool');
    RegisterMethod(@TRegIniFile.ReadSection, 'ReadSection');
    RegisterMethod(@TRegIniFile.ReadSections, 'ReadSections');
    RegisterMethod(@TRegIniFile.ReadSectionValues, 'ReadSectionValues');
    RegisterMethod(@TRegIniFile.EraseSection, 'EraseSection');
    RegisterMethod(@TRegIniFile.DeleteKey, 'DeleteKey');
    RegisterPropertyHelper(@TRegIniFileFileName_R,nil,'FileName');
  end;
end;

{$ENDIF class_helper_present}
{$IFDEF DELPHI10UP}{$ENDREGION}{$ENDIF}

{$IFDEF DELPHI10UP}{$REGION 'TRegistry'}{$ENDIF}
{$IFDEF class_helper_present}
type
  TRegistry_PSHelper = class helper for TRegistry
  public
    Function Create_P(CreateNewInstance: Boolean):TObject;
    Function CreateA_P(CreateNewInstance: Boolean;  AAccess : LongWord):TObject;
    procedure Access_R(var T: LongWord);
    procedure Access_W(const T: LongWord);
    procedure CurrentKey_R(var T: HKEY);
    procedure CurrentPath_R(var T: string);
    procedure LazyWrite_R(var T: Boolean);
    procedure LazyWrite_W(const T: Boolean);
    procedure RootKey_R(var T: HKEY);
    procedure RootKey_W(const T: HKEY);
  end;

(*----------------------------------------------------------------------------*)
procedure TRegistry_PSHelper.Access_W(const T: LongWord);
begin Self.Access := T; end;

(*----------------------------------------------------------------------------*)
procedure TRegistry_PSHelper.Access_R(var T: LongWord);
begin T := Self.Access; end;

(*----------------------------------------------------------------------------*)
procedure TRegistry_PSHelper.RootKey_W(const T: HKEY);
begin Self.RootKey := T; end;

(*----------------------------------------------------------------------------*)
procedure TRegistry_PSHelper.RootKey_R(var T: HKEY);
begin T := Self.RootKey; end;

(*----------------------------------------------------------------------------*)
procedure TRegistry_PSHelper.LazyWrite_W(const T: Boolean);
begin Self.LazyWrite := T; end;

(*----------------------------------------------------------------------------*)
procedure TRegistry_PSHelper.LazyWrite_R(var T: Boolean);
begin T := Self.LazyWrite; end;

(*----------------------------------------------------------------------------*)
procedure TRegistry_PSHelper.CurrentPath_R(var T: string);
begin T := Self.CurrentPath; end;

(*----------------------------------------------------------------------------*)
procedure TRegistry_PSHelper.CurrentKey_R(var T: HKEY);
begin T := Self.CurrentKey; end;

(*----------------------------------------------------------------------------*)
Function TRegistry_PSHelper.CreateA_P(CreateNewInstance: Boolean;  AAccess : LongWord):TObject;
Begin Result := TRegistry.Create(AAccess); END;

(*----------------------------------------------------------------------------*)
Function TRegistry_PSHelper.Create_P(CreateNewInstance: Boolean):TObject;
Begin Result := TRegistry.Create; END;

(*----------------------------------------------------------------------------*)
procedure RIRegister_TRegistry(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(TRegistry) do
  begin
    RegisterConstructor(@TRegistry.CreateA_P, 'CreateA');
    RegisterConstructor(@TRegistry.Create_P, 'Create');
    RegisterMethod(@TRegistry.CloseKey, 'CloseKey');
    RegisterMethod(@TRegistry.CreateKey, 'CreateKey');
    RegisterMethod(@TRegistry.DeleteKey, 'DeleteKey');
    RegisterMethod(@TRegistry.DeleteValue, 'DeleteValue');
    RegisterMethod(@TRegistry.GetDataInfo, 'GetDataInfo');
    RegisterMethod(@TRegistry.GetDataSize, 'GetDataSize');
    RegisterMethod(@TRegistry.GetDataType, 'GetDataType');
    RegisterMethod(@TRegistry.GetKeyInfo, 'GetKeyInfo');
    RegisterMethod(@TRegistry.GetKeyNames, 'GetKeyNames');
    RegisterMethod(@TRegistry.GetValueNames, 'GetValueNames');
    RegisterMethod(@TRegistry.HasSubKeys, 'HasSubKeys');
    RegisterMethod(@TRegistry.KeyExists, 'KeyExists');
    RegisterMethod(@TRegistry.LoadKey, 'LoadKey');
    RegisterMethod(@TRegistry.MoveKey, 'MoveKey');
    RegisterMethod(@TRegistry.OpenKey, 'OpenKey');
    RegisterMethod(@TRegistry.OpenKeyReadOnly, 'OpenKeyReadOnly');
    RegisterMethod(@TRegistry.ReadCurrency, 'ReadCurrency');
    RegisterMethod(@TRegistry.ReadBool, 'ReadBool');
    RegisterMethod(@TRegistry.ReadDate, 'ReadDate');
    RegisterMethod(@TRegistry.ReadDateTime, 'ReadDateTime');
    RegisterMethod(@TRegistry.ReadFloat, 'ReadFloat');
    RegisterMethod(@TRegistry.ReadInteger, 'ReadInteger');
    RegisterMethod(@TRegistry.ReadString, 'ReadString');
    RegisterMethod(@TRegistry.ReadTime, 'ReadTime');
    RegisterMethod(@TRegistry.RegistryConnect, 'RegistryConnect');
    RegisterMethod(@TRegistry.RenameValue, 'RenameValue');
    RegisterMethod(@TRegistry.ReplaceKey, 'ReplaceKey');
    RegisterMethod(@TRegistry.RestoreKey, 'RestoreKey');
    RegisterMethod(@TRegistry.SaveKey, 'SaveKey');
    RegisterMethod(@TRegistry.UnLoadKey, 'UnLoadKey');
    RegisterMethod(@TRegistry.ValueExists, 'ValueExists');
    RegisterMethod(@TRegistry.WriteCurrency, 'WriteCurrency');
    RegisterMethod(@TRegistry.WriteBool, 'WriteBool');
    RegisterMethod(@TRegistry.WriteDate, 'WriteDate');
    RegisterMethod(@TRegistry.WriteDateTime, 'WriteDateTime');
    RegisterMethod(@TRegistry.WriteFloat, 'WriteFloat');
    RegisterMethod(@TRegistry.WriteInteger, 'WriteInteger');
    RegisterMethod(@TRegistry.WriteString, 'WriteString');
    RegisterMethod(@TRegistry.WriteExpandString, 'WriteExpandString');
    RegisterMethod(@TRegistry.WriteTime, 'WriteTime');
    RegisterPropertyHelper(@TRegistry.CurrentKey_R,nil,'CurrentKey');
    RegisterPropertyHelper(@TRegistry.CurrentPath_R,nil,'CurrentPath');
    RegisterPropertyHelper(@TRegistry.LazyWrite_R,@TRegistry.LazyWrite_W,'LazyWrite');
    RegisterPropertyHelper(@TRegistry.RootKey_R,@TRegistry.RootKey_W,'RootKey');
    RegisterPropertyHelper(@TRegistry.Access_R,@TRegistry.Access_W,'Access');
  end;
end;

{$ELSE}
(*----------------------------------------------------------------------------*)
procedure TRegistryAccess_W(Self: TRegistry; const T: LongWord);
begin Self.Access := T; end;

(*----------------------------------------------------------------------------*)
procedure TRegistryAccess_R(Self: TRegistry; var T: LongWord);
begin T := Self.Access; end;

(*----------------------------------------------------------------------------*)
procedure TRegistryRootKey_W(Self: TRegistry; const T: HKEY);
begin Self.RootKey := T; end;

(*----------------------------------------------------------------------------*)
procedure TRegistryRootKey_R(Self: TRegistry; var T: HKEY);
begin T := Self.RootKey; end;

(*----------------------------------------------------------------------------*)
procedure TRegistryLazyWrite_W(Self: TRegistry; const T: Boolean);
begin Self.LazyWrite := T; end;

(*----------------------------------------------------------------------------*)
procedure TRegistryLazyWrite_R(Self: TRegistry; var T: Boolean);
begin T := Self.LazyWrite; end;

(*----------------------------------------------------------------------------*)
procedure TRegistryCurrentPath_R(Self: TRegistry; var T: string);
begin T := Self.CurrentPath; end;

(*----------------------------------------------------------------------------*)
procedure TRegistryCurrentKey_R(Self: TRegistry; var T: HKEY);
begin T := Self.CurrentKey; end;

(*----------------------------------------------------------------------------*)
Function TRegistryCreateA_P(Self: TClass; CreateNewInstance: Boolean;  AAccess : LongWord):TObject;
Begin Result := TRegistry.Create(AAccess); END;

(*----------------------------------------------------------------------------*)
Function TRegistryCreate_P(Self: TClass; CreateNewInstance: Boolean):TObject;
Begin Result := TRegistry.Create; END;

(*----------------------------------------------------------------------------*)
procedure RIRegister_TRegistry(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(TRegistry) do
  begin
    RegisterConstructor(@TRegistryCreateA_P, 'CreateA');
    RegisterConstructor(@TRegistryCreate_P, 'Create');
    RegisterMethod(@TRegistry.CloseKey, 'CloseKey');
    RegisterMethod(@TRegistry.CreateKey, 'CreateKey');
    RegisterMethod(@TRegistry.DeleteKey, 'DeleteKey');
    RegisterMethod(@TRegistry.DeleteValue, 'DeleteValue');
    RegisterMethod(@TRegistry.GetDataInfo, 'GetDataInfo');
    RegisterMethod(@TRegistry.GetDataSize, 'GetDataSize');
    RegisterMethod(@TRegistry.GetDataType, 'GetDataType');
    RegisterMethod(@TRegistry.GetKeyInfo, 'GetKeyInfo');
    RegisterMethod(@TRegistry.GetKeyNames, 'GetKeyNames');
    RegisterMethod(@TRegistry.GetValueNames, 'GetValueNames');
    RegisterMethod(@TRegistry.HasSubKeys, 'HasSubKeys');
    RegisterMethod(@TRegistry.KeyExists, 'KeyExists');
    RegisterMethod(@TRegistry.LoadKey, 'LoadKey');
    RegisterMethod(@TRegistry.MoveKey, 'MoveKey');
    RegisterMethod(@TRegistry.OpenKey, 'OpenKey');
    RegisterMethod(@TRegistry.OpenKeyReadOnly, 'OpenKeyReadOnly');
    RegisterMethod(@TRegistry.ReadCurrency, 'ReadCurrency');
    RegisterMethod(@TRegistry.ReadBool, 'ReadBool');
    RegisterMethod(@TRegistry.ReadDate, 'ReadDate');
    RegisterMethod(@TRegistry.ReadDateTime, 'ReadDateTime');
    RegisterMethod(@TRegistry.ReadFloat, 'ReadFloat');
    RegisterMethod(@TRegistry.ReadInteger, 'ReadInteger');
    RegisterMethod(@TRegistry.ReadString, 'ReadString');
    RegisterMethod(@TRegistry.ReadTime, 'ReadTime');
    RegisterMethod(@TRegistry.RegistryConnect, 'RegistryConnect');
    RegisterMethod(@TRegistry.RenameValue, 'RenameValue');
    RegisterMethod(@TRegistry.ReplaceKey, 'ReplaceKey');
    RegisterMethod(@TRegistry.RestoreKey, 'RestoreKey');
    RegisterMethod(@TRegistry.SaveKey, 'SaveKey');
    RegisterMethod(@TRegistry.UnLoadKey, 'UnLoadKey');
    RegisterMethod(@TRegistry.ValueExists, 'ValueExists');
    RegisterMethod(@TRegistry.WriteCurrency, 'WriteCurrency');
    RegisterMethod(@TRegistry.WriteBool, 'WriteBool');
    RegisterMethod(@TRegistry.WriteDate, 'WriteDate');
    RegisterMethod(@TRegistry.WriteDateTime, 'WriteDateTime');
    RegisterMethod(@TRegistry.WriteFloat, 'WriteFloat');
    RegisterMethod(@TRegistry.WriteInteger, 'WriteInteger');
    RegisterMethod(@TRegistry.WriteString, 'WriteString');
    RegisterMethod(@TRegistry.WriteExpandString, 'WriteExpandString');
    RegisterMethod(@TRegistry.WriteTime, 'WriteTime');
    RegisterPropertyHelper(@TRegistryCurrentKey_R,nil,'CurrentKey');
    RegisterPropertyHelper(@TRegistryCurrentPath_R,nil,'CurrentPath');
    RegisterPropertyHelper(@TRegistryLazyWrite_R,@TRegistryLazyWrite_W,'LazyWrite');
    RegisterPropertyHelper(@TRegistryRootKey_R,@TRegistryRootKey_W,'RootKey');
    RegisterPropertyHelper(@TRegistryAccess_R,@TRegistryAccess_W,'Access');
  end;
end;

{$ENDIF class_helper_present}
{$IFDEF DELPHI10UP}{$ENDREGION}{$ENDIF}


(*----------------------------------------------------------------------------*)
procedure RIRegister_Registry(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(ERegistryException) do
  RIRegister_TRegistry(CL);
  RIRegister_TRegIniFile(CL);
  RIRegister_TRegistryIniFile(CL);
end;

(* === compile-time registration functions === *)
(*----------------------------------------------------------------------------*)
procedure SIRegister_TMemIniFile(CL: TPSPascalCompiler);
begin
  //with RegClassS(CL,'TCustomIniFile', 'TMemIniFile') do
  with CL.AddClassN(CL.FindClass('TCustomIniFile'),'TMemIniFile') do
  begin
    RegisterMethod('constructor Create(const FileName: string)');
    RegisterMethod('procedure Clear');
    RegisterMethod('procedure GetStrings(List: TStrings)');
    RegisterMethod('procedure Rename(const FileName: string; Reload: Boolean)');
    RegisterMethod('procedure SetStrings(List: TStrings)');
  end;
end;


(*----------------------------------------------------------------------------*)
procedure SIRegister_TIniFile(CL: TPSPascalCompiler);
begin
  //with RegClassS(CL,'TCustomIniFile', 'TIniFile') do
  with CL.AddClassN(CL.FindClass('TCustomIniFile'),'TIniFile') do
  begin
    RegisterMethod('function ReadString(const Section, Ident, Default: string): string');
    RegisterMethod('procedure WriteString(const Section, Ident, Value: string)');
    RegisterMethod('procedure ReadSection(const Section: string; Strings: TStrings)');
    RegisterMethod('procedure ReadSections(Strings: TStrings)');
    RegisterMethod('procedure ReadSectionValues(const Section: string; Strings: TStrings)');
    RegisterMethod('procedure EraseSection(const Section: string)');
    RegisterMethod('procedure DeleteKey(const Section, Ident: string)');
    RegisterMethod('procedure UpdateFile');
  end;
end;

(*----------------------------------------------------------------------------*)
procedure SIRegister_TCustomIniFile(CL: TPSPascalCompiler);
begin
  //with RegClassS(CL,'TObject', 'TCustomIniFile') do
  with CL.AddClassN(CL.FindClass('TObject'),'TCustomIniFile') do
  begin
    RegisterMethod('constructor Create(const FileName: string)');
    RegisterMethod('function SectionExists(const Section: string): Boolean');
//    RegisterMethod('function ReadString(const Section, Ident, Default: string): string');
//    RegisterMethod('procedure WriteString(const Section, Ident, Value: string)');
    RegisterMethod('function ReadInteger(const Section, Ident: string; Default: LongInt): LongInt');
    RegisterMethod('procedure WriteInteger(const Section, Ident: string; Value: LongInt)');
    RegisterMethod('function ReadBool(const Section, Ident: string; Default: Boolean): Boolean');
    RegisterMethod('procedure WriteBool(const Section, Ident: string; Value: Boolean)');
    RegisterMethod('function ReadDate(const Section, Name: string; Default: TDateTime): TDateTime');
    RegisterMethod('function ReadDateTime(const Section, Name: string; Default: TDateTime): TDateTime');
    RegisterMethod('function ReadFloat(const Section, Name: string; Default: Double): Double');
    RegisterMethod('function ReadTime(const Section, Name: string; Default: TDateTime): TDateTime');
    RegisterMethod('procedure WriteDate(const Section, Name: string; Value: TDateTime)');
    RegisterMethod('procedure WriteDateTime(const Section, Name: string; Value: TDateTime)');
    RegisterMethod('procedure WriteFloat(const Section, Name: string; Value: Double)');
    RegisterMethod('procedure WriteTime(const Section, Name: string; Value: TDateTime)');
//    RegisterMethod('procedure ReadSection(const Section: string; Strings: TStrings)');
//    RegisterMethod('procedure ReadSections(Strings: TStrings)');
//    RegisterMethod('procedure ReadSectionValues(const Section: string; Strings: TStrings)');
//    RegisterMethod('procedure EraseSection(const Section: string)');
//    RegisterMethod('procedure DeleteKey(const Section, Ident: string)');
//    RegisterMethod('procedure UpdateFile');
    RegisterMethod('function ValueExists(const Section, Ident: string): Boolean');
    RegisterProperty('FileName', 'string', iptr);
  end;
end;

(*----------------------------------------------------------------------------*)
procedure SIRegister_IniFiles(CL: TPSPascalCompiler);
begin
  SIRegister_TCustomIniFile(CL);
  SIRegister_TIniFile(CL);
  SIRegister_TMemIniFile(CL);
end;

(* === run-time registration functions === *)
(*----------------------------------------------------------------------------*)
procedure TCustomIniFileFileName_R(Self: TCustomIniFile; var T: string);
begin T := Self.FileName; end;

(*----------------------------------------------------------------------------*)
procedure RIRegister_TMemIniFile(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(TMemIniFile) do
  begin
    RegisterConstructor(@TMemIniFile.Create, 'Create');
    RegisterMethod(@TMemIniFile.Clear, 'Clear');
    RegisterMethod(@TMemIniFile.GetStrings, 'GetStrings');
    RegisterMethod(@TMemIniFile.Rename, 'Rename');
    RegisterMethod(@TMemIniFile.SetStrings, 'SetStrings');
  end;
end;

(*----------------------------------------------------------------------------*)
procedure RIRegister_TIniFile(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(TIniFile) do
  begin
    RegisterMethod(@TIniFile.ReadString, 'ReadString');
    RegisterMethod(@TIniFile.WriteString, 'WriteString');
    RegisterMethod(@TIniFile.ReadSection, 'ReadSection');
    RegisterMethod(@TIniFile.ReadSections, 'ReadSections');
    RegisterMethod(@TIniFile.ReadSectionValues, 'ReadSectionValues');
    RegisterMethod(@TIniFile.EraseSection, 'EraseSection');
    RegisterMethod(@TIniFile.DeleteKey, 'DeleteKey');
    RegisterMethod(@TIniFile.UpdateFile, 'UpdateFile');
  end;
end;

(*----------------------------------------------------------------------------*)
procedure RIRegister_TCustomIniFile(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(TCustomIniFile) do
  begin
    RegisterConstructor(@TCustomIniFile.Create, 'Create');
    RegisterMethod(@TCustomIniFile.SectionExists, 'SectionExists');
//    RegisterVirtualAbstractMethod(@TCustomIniFile, @!.ReadString, 'ReadString');
//    RegisterVirtualAbstractMethod(@TCustomIniFile, @!.WriteString, 'WriteString');
    RegisterVirtualMethod(@TCustomIniFile.ReadInteger, 'ReadInteger');
    RegisterVirtualMethod(@TCustomIniFile.WriteInteger, 'WriteInteger');
    RegisterVirtualMethod(@TCustomIniFile.ReadBool, 'ReadBool');
    RegisterVirtualMethod(@TCustomIniFile.WriteBool, 'WriteBool');
    RegisterVirtualMethod(@TCustomIniFile.ReadDate, 'ReadDate');
    RegisterVirtualMethod(@TCustomIniFile.ReadDateTime, 'ReadDateTime');
    RegisterVirtualMethod(@TCustomIniFile.ReadFloat, 'ReadFloat');
    RegisterVirtualMethod(@TCustomIniFile.ReadTime, 'ReadTime');
    RegisterVirtualMethod(@TCustomIniFile.WriteDate, 'WriteDate');
    RegisterVirtualMethod(@TCustomIniFile.WriteDateTime, 'WriteDateTime');
    RegisterVirtualMethod(@TCustomIniFile.WriteFloat, 'WriteFloat');
    RegisterVirtualMethod(@TCustomIniFile.WriteTime, 'WriteTime');
//  RegisterVirtualAbstractMethod(@TCustomIniFile, @!.ReadSection, 'ReadSection');
//  RegisterVirtualAbstractMethod(@TCustomIniFile, @!.ReadSections, 'ReadSections');
//  RegisterVirtualAbstractMethod(@TCustomIniFile, @!.ReadSectionValues, 'ReadSectionValues');
//  RegisterVirtualAbstractMethod(@TCustomIniFile, @!.EraseSection, 'EraseSection');
//  RegisterVirtualAbstractMethod(@TCustomIniFile, @!.DeleteKey, 'DeleteKey');
//  RegisterVirtualAbstractMethod(@TCustomIniFile, @!.UpdateFile, 'UpdateFile');
    RegisterMethod(@TCustomIniFile.ValueExists, 'ValueExists');
    RegisterPropertyHelper(@TCustomIniFileFileName_R,nil,'FileName');
  end;
end;

(*----------------------------------------------------------------------------*)
procedure RIRegister_IniFiles(CL: TPSRuntimeClassImporter);
begin
  RIRegister_TCustomIniFile(CL);
  RIRegister_TIniFile(CL);
  RIRegister_TMemIniFile(CL);
end;

{ TPSImport_Registry }
(*----------------------------------------------------------------------------*)
procedure TPSImport_Registry.CompOnUses(CompExec: TPSScript);
begin
  { nothing }
end;
(*----------------------------------------------------------------------------*)
procedure TPSImport_Registry.ExecOnUses(CompExec: TPSScript);
begin
  { nothing }
end;
(*----------------------------------------------------------------------------*)
procedure TPSImport_Registry.CompileImport1(CompExec: TPSScript);
begin
  SIRegister_Registry(CompExec.Comp);
  SIRegister_IniFiles(CompExec.Comp);
end;
(*----------------------------------------------------------------------------*)
procedure TPSImport_Registry.CompileImport2(CompExec: TPSScript);
begin
  { nothing }
end;
(*----------------------------------------------------------------------------*)
procedure TPSImport_Registry.ExecImport1(CompExec: TPSScript; const ri: TPSRuntimeClassImporter);
begin
  RIRegister_Registry(ri);
  RIRegister_IniFiles(ri);
end;
(*----------------------------------------------------------------------------*)
procedure TPSImport_Registry.ExecImport2(CompExec: TPSScript; const ri: TPSRuntimeClassImporter);
begin
  { nothing }
end;

end.