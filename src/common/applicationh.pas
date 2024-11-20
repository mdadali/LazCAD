unit applicationh;

{$mode delphi}{$H+}


interface

uses Forms, SysUtils, Classes, Dialogs, IniFiles, messages,
     LResources, AbUnzper,   AbZBrows, AbArcTyp, AbZipTyp,
     {$IFDEF WINDOWS} windows, {$ENDIF}
     CommonUtils,
     CADSys4,
     CS4Shapes,
     CS4BaseTypes;

const
      CAD_LAYER_TEMPLATE          = 'Template';

      CAD_ONSTART_FILE_STATE_NONE = 0;
      CAD_ONSTART_FILE_STATE_LAST = 1;
      CAD_ONSTART_FILE_STATE_NEW  = 2;

var
  fGifAnimFile,

  fDefaultBlockLibrary,
  fCurrentFontFile,

  fPythonDLLPath,
  fPythonDLLName: string;
  fHackMode: boolean;
  fAdminPassword: string;
  fConnStrFileName: string;
  fLoginPrompt: boolean;
  fOnStartFileState: integer;
  fLastFileName: string;

  fFakeMDIChildCount: integer = 0;

  fTraceFileName: string;
  fTraceON: string; //boolean;
  fTraceSaveErrorMsg: string;
  fIniFile:  TIniFile;
  fIniFileName: string;

  fLeftPanelVisible : boolean;
  fShowRulerMarker: boolean;
  SimulationIsStarted: boolean = false;
  CancelCloseAction:   boolean = false;

  fUseTemplates: string;
  fStdTemplate: string;

  fLanguage: string;

  ImportFileName: string;
  fAllImportFilterExtentions: string;
  fAllExtentions:  string;

procedure ReadIniFile;
procedure WriteIniFile;

function GetAppExeName: string;
function GetAppFullPath: string;
function GetAppPath: string;
function GetOnlyFileName(AFileName: string): string; //Filename without extention (FileName.txt -> FileName)!
function GetFileExtention(AFileName: string): string;

function GetAppDrawingsPath: string;
function GetAppTempPath: string;
function GetAppDBPath: string;
function GetAppDBName: string;

function GetAppConfigPath: string;
function GetAppDataPath: string;
function GetAppSystemPath: string;
function GetAppApplicationsPath: string;
function GetAppFontsPath: string;
function GetAppFontsFNTPath: string;
function GetAppFontsTTFPath: string;
function GetAppTemplatesPath: string;
function GetAppLanguagesPath: string;
function GetAppPlugInsPath: string;
function GetAppImagesPath: string;
function GetAppBlockLibrarysPath: string;
function GetAppProjectsPath: string;

function GetAppDocPath: string;

function GetAppPascalScriptsPath: string;


implementation

procedure ExtractDataDirectory;
var
  DataStream: TResourceStream;
  MemoryStream: TMemoryStream;
  AbUnZipper: TAbUnZipper;
begin
  AbUnZipper := TAbUnZipper.Create(nil);
  if not DirectoryExists(GetAppDataPath) then
  begin
    DataStream := TResourceStream.Create(HInstance, 'DATA', RT_RCDATA);
    MemoryStream := TMemoryStream.Create;
    try
      // Load the ZIP file from the resource into memory
      MemoryStream.LoadFromStream(DataStream);
      MemoryStream.Position := 0;

      // Extract the contents of the ZIP file to the target directory.
      AbUnZipper.Stream := MemoryStream;
      AbUnZipper.ExtractOptions := [eoCreateDirs, eoRestorePath]; // Create directories
      AbUnZipper.BaseDirectory := GetAppPath;
      AbUnZipper.ExtractFiles('*.*');
    finally
      MemoryStream.Free;
      DataStream.Free;
      AbUnZipper.Free;
    end;
  end;
end;

function FirstRun: boolean;
begin
  result := not DirectoryExists(GetAppDataPath);
end;

procedure ReadIniFile;
begin
  if FirstRun then
    ExtractDataDirectory;

  fIniFileName := ChangeFileExt(Application.ExeName, '.ini');
  fIniFile     := TIniFile.Create(fIniFileName);

  try
    fShowRulerMarker     := fIniFile.ReadString('Application', 'ShowRulerMarker', 'no') = 'yes';
    fUseTemplates        := fIniFile.ReadString('Application', 'UseTemplates', 'yes');
    fStdTemplate         := fIniFile.ReadString('Application', 'DefaultTemplate',     GetAppTemplatesPath + 'cnc.cs4');
    fGifAnimFile         := fIniFile.ReadString('UserInterface', 'GifAnimFile',       GetAppImagesPath + 'fpc_running_logo.gif');
    fDefaultBlockLibrary := fIniFile.ReadString('Application', 'DefaultBlockLibrary', GetAppBlockLibrarysPath + 'library.blk');
    fCurrentFontFile     := fIniFile.ReadString('Application', 'CurrentFontFile',     GetAppFontsFNTPath + 'verdana.fnt');

    fPythonDLLPath       := fIniFile.ReadString('Python', 'DllPath', '');
    fPythonDLLName       := fIniFile.ReadString('Python', 'DllName', '');

    fAdminPassword       := cuDecodeStr(fIniFile.ReadString('Admin', 'Pwd', ''));
    fLoginPrompt         := fIniFile.ReadBool('Admin', 'LoginPromt', true);
    fHackMode            := fIniFile.ReadBool('Admin', 'HackMode', false);
    fConnStrFileName     := fIniFile.ReadString('DB', 'ConnectionStringFile', '');
    fOnStartFileState    := fIniFile.ReadInteger('OnAppStart',  'CADOnStartFileState', 0);
    fLastFileName        := fIniFile.ReadString('OnAppStart',  'LastFileName', '');
    fLeftPanelVisible    := fIniFile.ReadBool('OnAppStart',    'LeftPanelVisible', false);
    fTraceON             := fIniFile.ReadString('Admin',  'TraceON', '1');
    fTraceFileName       := fIniFile.ReadString('Admin',  'TraceFileName', GetAppSystemPath + '');
    fLanguage            := fIniFile.ReadString('UserInterface',  'Language', 'en');
  finally
    fIniFile.Free;
    fIniFile := nil;
  end;
end;


procedure WriteIniFile;
begin
  fIniFileName := ChangeFileExt(Application.ExeName, '.ini');
  fIniFile := TIniFile.Create(fIniFileName);
  try
    if fShowRulerMarker then
      fIniFile.WriteString('Application',   'ShowRulerMarker', 'yes')
    else
      fIniFile.WriteString('Application',   'ShowRulerMarker', 'no');
    fIniFile.WriteString('Application',   'UseTemplates',        fUseTemplates);
    fIniFile.WriteString('Application',   'DefaultTemplate',     fStdTemplate);
    fIniFile.WriteString('Application',   'DefaultBlockLibrary', fDefaultBlockLibrary);
    fIniFile.WriteString('Application',   'CurrentFontFile',     fCurrentFontFile);
    fIniFile.WriteString('UserInterface', 'GifAnimFile',         fGifAnimFile);
    fIniFile.WriteBool('Admin', 'LoginPromt', fLoginPrompt);
    fIniFile.WriteString('DB',  'ConnectionStringFile',  ExtractFileName(fConnStrFileName));
    fIniFile.WriteString('Admin', 'TraceFileName',  ExtractFileName(fTraceFileName));
    fIniFile.WriteString('Admin', 'TraceON', fTraceON);
    fIniFile.WriteString('UserInterface', 'Language', fLanguage);
  finally
    fIniFile.Free;
    fIniFile := nil;
  end;
end;

function GetAppConfigPath: string;
begin
  {$IFDEF WINDOWS}
    result      := GetAppDataPath + 'config\';
  {$ELSE}
    result      := GetAppDataPath + 'config/';
  {$ENDIF}
end;

function GetAppDataPath: string;
begin
  {$IFDEF WINDOWS}
    result      := GetAppPath + 'data\';
  {$ELSE}
    result      := GetAppPath + 'data/';
  {$ENDIF}
end;

function GetAppProjectsPath: string;
begin
  result := GetAppDataPath + 'paxSciptProjects\';
end;

function GetAppSystemPath: string;
begin
  {$IFDEF WINDOWS}
    result      := GetAppDataPath + 'system\';
  {$ELSE}
    result      := GetAppDataPath + 'system/';
  {$ENDIF}
end;

function GetAppPascalScriptsPath: string;
begin
  {$IFDEF WINDOWS}
    result      := GetAppDataPath + 'PascalScripts\';
  {$ELSE}
    result      := GetAppDataPath + 'PascalScripts/';
  {$ENDIF}
end;

function GetAppApplicationsPath: string;
begin
  {$IFDEF WINDOWS}
    result      := GetAppDataPath + 'applications\';
  {$ELSE}
    result      := GetAppDataPath + 'applications/';
  {$ENDIF}
end;


function GetAppDBPath: string;
begin
  {$IFDEF WINDOWS}
    result      := GetAppDataPath + 'db\';
  {$ELSE}
    result := GetAppDataPath + 'db/';
  {$ENDIF}
end;

function GetAppDocPath: string;
begin
  {$IFDEF WINDOWS}
    result      := GetAppDataPath + 'doc\';
  {$ELSE}
    result := GetAppDataPath + 'doc/';
  {$ENDIF}
end;

function GetAppDBName: string;
begin
  //result := GetAppDBPath + DB_NAME;
end;

function GetAppFontsPath: string;
begin
  {$IFDEF WINDOWS}
    result      := GetAppDataPath + 'fonts\';
  {$ELSE}
    result := GetAppDataPath + 'fonts/';
  {$ENDIF}
end;

function GetAppFontsFNTPath: string;
begin
  {$IFDEF WINDOWS}
    result := GetAppFontsPath + 'fnt\';
  {$ELSE}
    result := GetAppFontsPath + 'fnt/';
  {$ENDIF}
end;

function GetAppFontsTTFPath: string;
begin
  {$IFDEF WINDOWS}
    result := GetAppFontsPath + 'ttf\';
  {$ELSE}
    result := GetAppFontsPath + 'ttf/';
  {$ENDIF}
end;

function GetAppTemplatesPath: string;
begin
  {$IFDEF WINDOWS}
    result := GetAppDataPath + 'templates\';
  {$ELSE}
    result := GetAppDataPath + 'templates/';
  {$ENDIF}
end;

function GetAppLanguagesPath: string;
begin
  result := GetAppConfigPath;
end;

function GetAppPlugInsPath: string;
begin
  result := GetAppPath;
end;

function GetAppImagesPath: string;
begin
  {$IFDEF WINDOWS}
    result := GetAppDataPath + 'images\';
  {$ELSE}
    result := GetAppDataPath + 'images/';
  {$ENDIF}
end;

function GetAppBlockLibrarysPath: string;
begin
  {$IFDEF WINDOWS}
    result := GetAppDataPath + 'blocklibs\';
  {$ELSE}
    result := GetAppDataPath + 'blocklibs/';
  {$ENDIF}
end;


function GetAppExeName: string;
begin
  result := ExtractFileName(Application.ExeName);
end;

function GetAppFullPath: string;
begin
  result := Application.ExeName;
end;

function GetAppPath: string;
begin
  result := ExtractFilePath(Application.ExeName);
end;

function GetOnlyFileName(AFileName: string): string;
var Ext: string;
begin
  Ext := ExtractFileExt(AFileName);
  AFileName := ExtractFileName(AFileName);
      //string dateiname, von Punkt ab bis Endung löschen
  Delete(AFileName,Pos('.', AFileName),length(Ext));
  result := AFileName;
end;

function GetFileExtention(AFileName: string): string;
var hExt: string;
begin
  hExt := ExtractFileExt(AFileName);
end;

function GetAppPPsPath: string;
begin
  {$IFDEF WINDOWS}
    result := GetAppPath + 'images\'
  {$ELSE}
    result := GetAppDataPath + 'images/';
  {$ENDIF}
end;

function GetAppDrawingsPath: string;
begin
  {$IFDEF WINDOWS}
    result := GetAppDataPath + 'drawings\';
  {$ELSE}
    result := GetAppDataPath + 'drawings/';
  {$ENDIF}
end;

function GetAppTempPath: string;
begin
  {$IFDEF WINDOWS}
    result := GetAppPath + 'temp\';
  {$ELSE}
    result := GetAppPath + 'temp/';
  {$ENDIF}
end;

initialization
  //fIniFileName := ChangeFileExt(Application.ExeName, '.ini');
  //fIniFile     := TIniFile.Create(fIniFileName);
  ReadIniFile;

finalization
  WriteIniFile;
  //fIniFile.Free;
  //fIniFile := nil;
end.
