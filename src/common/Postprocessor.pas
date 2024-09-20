unit Postprocessor;

interface

uses  Classes, ComCtrls,
      DB,
      applicationh,
      CADTypes,
      CADConstants,
      fdmMain,
      cPostprocessor;

var pp: TPostprocessor; //Global Postprocessor

//Postprocessor-Propertys
function ppID: integer;
function ppDescription: string;
function ppMachineName: string;
function ppAutor: string;
function ppDecimalSep: char;
function ppUnitFactor: TFloatType;
function ppMinEntityDistance: TFloatType;
function ppFloatFormat: string;
function ppIsPgmIncremental: boolean;
function ppArcCenterAbsolute: boolean;
function ppFileFilter: string;
function ppInitialDir: string;
function ppOutputKerfCmd: boolean;
function ppOutputKerfValue: boolean;
function ppKerfValue: TFloatType;
//function ppIcon
function ppPrintLineNumber: boolean;
function ppLineNumPrefix: string;
function ppLineNumStart: integer;
function ppLineNumStep: integer;
function ppSaveWithoutDialog: boolean;
function ppAutoSaveFileNameFormat: string;
function ppAutoSaveFileNameTriger: integer;
function ppScript: string;
function ppGenerateOutput: boolean;
function ppUseCadFileNameForOutput: boolean;
function ppProcessID: integer;
function ppToolID: integer;
function ppToolTableName: string;
function ppOutputUnicode: boolean;

function ppProcessName: string;
function ppToolName: string;

function ppProgramName: string;
function ppOutputFileName: string;

function ppGetCurrPpID : integer;
function ppGetCurrPpName : string;
function ppGetName(APpID: Integer) : string;
function ppGetPpID(APpName: string) : integer;

function ppGetIntegerFieldValue(APpID: Integer; AFieldName: string): integer;
function ppGetStringFieldValue(APpID: Integer; AFieldName: string): string;
function ppGetFloatFieldValue(APpID: Integer; AFieldName: string): double;
function ppGetBoolFieldValue(APpID: Integer; AFieldName: string): boolean;
function ppGetDateTimeFieldValue(APpID: Integer; AFieldName: string): TDateTime;


function ppGetIntegerOption(AParamName: string): integer;
function ppGetStringOption(AParamName: string): string;
function ppGetFloatOption(AParamName: string): double;
function ppGetBoolOption(AParamName: string): boolean;

//Postprocessor-ToolTableValues
function ppGetIntegerToolTableValue(AParamName: string): integer;
function ppGetStringToolTableValue(AParamName: string): string;
function ppGetFloatToolTableValue(AParamName: string): double;
function ppGetBoolToolTableValue(AParamName: string): boolean;


//CMDs
procedure ppReset(ACADDocument: integer; AMachineName: string; AProgressBar: TProgressBar);
procedure ppExecute;
procedure ppLocate(APPName: string);
procedure ppShowSettings;

//utils
procedure PPGetPostprocessorList(AStrings: TStrings);
function  ppGetFileExt(AIndex: integer): string;


implementation

//Postprocessor-Propertys
function ppID: integer;
begin
  result := pp.ID;
end;

function ppDescription: string;
begin
  result := pp.Description ;
end;

function ppMachineName: string;
begin
  result := pp.MachineName ;
end;

function ppAutor: string;
begin
  result := pp.Autor  ;
end;

function ppDecimalSep: char;
begin
  result := pp.DecimalSep   ;
end;

function ppUnitFactor: TFloatType;
begin
  result := pp.UnitFactor ;
end;

function ppMinEntityDistance: TFloatType;
begin
  result := pp.MinEntityDistance ;
end;

function ppFloatFormat: string;
begin
  result := pp.FloatFormat ;
end;

function ppIsPgmIncremental: boolean;
begin
  result := pp.IsPgmIncremental ;
end;

function ppArcCenterAbsolute: boolean;
begin
  result := pp.ArcCenterAbsolute ;
end;

function ppFileFilter: string;
begin
  result := pp.FileFilter;
end;

function ppInitialDir: string;
begin
  result := pp.InitialDir ;
end;

function ppOutputKerfCmd: boolean;
begin
  result := pp.OutputKerfCmd ;
end;

function ppOutputKerfValue: boolean;
begin
  result := pp.OutputKerfValue ;
end;

function ppKerfValue: TFloatType;
begin
  result := pp.KerfValue ;
end;

//function ppIcon
//begin

//end;

function ppPrintLineNumber: boolean;
begin
  result := pp.PrintLineNumber ;
end;

function ppLineNumPrefix: string;
begin
  result := pp.LineNumPrefix ;
end;

function ppLineNumStart: integer;
begin
  result := pp.LineNumStart ;
end;

function ppLineNumStep: integer;
begin
  result := pp.LineNumStep ;
end;

function ppSaveWithoutDialog: boolean;
begin
  result := pp.SaveInBatchMode ;
end;

function ppAutoSaveFileNameFormat: string;
begin
  result := pp.AutoSaveFileNameFormat ;
end;

function ppAutoSaveFileNameTriger: integer;
begin
  result := pp.AutoSaveFileNameCounter ;
end;

function ppScript: string;
begin
  result := pp.OutputScript ;
end;

function ppGenerateOutput: boolean;
begin
  result := pp.GenerateOutput ;
end;

function ppUseCadFileNameForOutput: boolean;
begin
  result := pp.UseCadFileNameForOutput ;
end;

function ppProcessID: integer;
begin
  result := pp.ProcessID ;
end;

function ppToolID: integer;
begin
  result := pp.ToolID ;
end;

function ppToolTableName: string;
begin
  result := pp.ToolTableName ;
end;

function ppOutputUnicode: boolean;
begin
  result := pp.OutputUnicode ;
end;

function ppProcessName: string;
begin
  result := pp.ProcessName ;
end;

function ppToolName: string;
begin
  result := pp.ToolName ;
end;

function ppProgramName: string;
begin
  result := pp.ProgramName ;
end;

function ppOutputFileName: string;
begin
  result := pp.OutputFileName ;
end;

//Postprocessor-Options
function  ppGetPpID(APpName: string) : integer;
var hRes: int64;
begin
  hRes := CAD_INVALID_INTEGER_VALUE;
  if frmdmMain.tblPostprocessors.locate('Name', APpName, [loCaseInsensitive ]) then
    hRes := frmdmMain.tblPostprocessors.Fields[0].AsInteger;
  result := hRes;
end;

function   ppGetName(APpID: Integer) : string;
var hRes: string;
begin
  hRes := CAD_INVALID_STRING_VALUE;
  if frmdmMain.tblPostprocessors.locate('id',  APpID, [])  then
    hRes := frmdmMain.tblPostprocessors.FieldByName('Name').AsString;
  result := hRes;
end;

function  ppGetCurrPpName : string;
begin
  result := frmdmMain.tblPostprocessors.FieldByName('Name').AsString;
end;

function  ppGetCurrPpID : integer;
begin
  result := frmdmMain.tblPostprocessors.Fields[0].AsInteger;
end;


function ppGetIntegerFieldValue(APpID: Integer; AFieldName: string): integer;
var hRes: integer;
begin
  hRes := CAD_INVALID_INTEGER_VALUE;
  if (frmdmMain.tblPostprocessors.FindField(AFieldName) <> nil) then
    hRes := frmdmMain.tblPostprocessors.FieldByName(AFieldName).AsInteger
  else
    if frmdmMain.tblPpExtFields.locate('FieldName',  AFieldName, [loCaseInsensitive ]) then
      hRes := frmdmMain.tblPpExtFields.FieldByName('FieldValue').AsInteger;
  result := hRes;
end;

function ppGetStringFieldValue(APpID: Integer; AFieldName: string): string;
var hRes: string;
begin
  hRes := CAD_INVALID_STRING_VALUE;
  if (frmdmMain.tblPostprocessors.FindField(AFieldName) <> nil) then
    hRes := frmdmMain.tblPostprocessors.FieldByName(AFieldName).AsString
  else
    if frmdmMain.tblPpExtFields.locate('FieldName',  AFieldName, [loCaseInsensitive ]) then
      hRes := frmdmMain.tblPpExtFields.FieldByName('FieldValue').AsString;
  result := hRes;
end;

function  ppGetFloatFieldValue(APpID: Integer; AFieldName: string): TFloatType;
var hRes: TFloatType;
begin
  hRes := CAD_INVALID_FLOAT_VALUE;
  if (frmdmMain.tblPostprocessors.FindField(AFieldName) <> nil) then
    hRes := frmdmMain.tblPostprocessors.FieldByName(AFieldName).AsFloat
  else
    if frmdmMain.tblPpExtFields.locate('FieldName',  AFieldName, [loCaseInsensitive ]) then
      hRes := frmdmMain.tblPpExtFields.FieldByName('FieldValue').AsFloat;
  result := hRes;
end;

function ppGetBoolFieldValue(APpID: Integer; AFieldName: string): boolean;
var hRes: boolean;
begin
  hRes := false;
  if (frmdmMain.tblPostprocessors.FindField(AFieldName) <> nil) then
    hRes := frmdmMain.tblPostprocessors.FieldByName(AFieldName).AsBoolean
  else
    if frmdmMain.tblPpExtFields.locate('FieldName',  AFieldName, [loCaseInsensitive ]) then
      hRes := frmdmMain.tblPpExtFields.FieldByName('FieldValue').AsBoolean;
  result := hRes;
end;

function ppGetDateTimeFieldValue(APpID: Integer; AFieldName: string): TDateTime;
var hRes: TDateTime;
begin
  hRes := 0.001;
  if (frmdmMain.tblPostprocessors.FindField(AFieldName) <> nil) then
    hRes := frmdmMain.tblPostprocessors.FieldByName(AFieldName).AsDateTime
  else
    if frmdmMain.tblPpExtFields.locate('FieldName',  AFieldName, [loCaseInsensitive ]) then
      hRes := frmdmMain.tblPpExtFields.FieldByName('FieldValue').AsDateTime;
  result := hRes;
end;

function PPGetIntegerOption(AParamName: string): integer;
begin
  result := pp.GetIntegerOption(AParamName);
end;

function PPGetStringOption(AParamName: string): string;
begin
 result := pp.GetStringOption(AParamName);
end;

function PPGetFloatOption(AParamName: string): double;
begin
  result := pp.GetFloatOption(AParamName);
end;

function PPGetBoolOption(AParamName: string): boolean;
begin
  result := pp.GetBoolOption(AParamName);
end;

//Postprocessor-ToolTableValues
function PPGetIntegerToolTableValue(AParamName: string): integer;
begin
  result := pp.GetIntegerToolTableValue(AParamName);
end;

function PPGetStringToolTableValue(AParamName: string): string;
begin
  result := pp.GetStringToolTableValue(AParamName);
end;

function PPGetFloatToolTableValue(AParamName: string): double;
begin
  result := pp.GetFloatToolTableValue(AParamName);
end;

function PPGetBoolToolTableValue(AParamName: string): boolean;
begin
  result := pp.GetBoolToolTableValue(AParamName);
end;


//CMDs
procedure PPReset(ACADDocument: integer; AMachineName: string; AProgressBar: TProgressBar);
begin
  pp.Reset(ACADDocument, AMachineName, AProgressBar);
end;

procedure PPExecute;
begin
  pp.Execute;
end;

procedure ppLocate(APPName: string);
begin
  frmdmMain.tblPostProcessors.Locate('Name', APPName, [loCaseInsensitive]);
end;

procedure ppShowSettings;
begin
  //frmMachines.PageControl1.ActivePageIndex := 1;
  //frmMachines.Show;
end;


//utils
procedure ppGetPostprocessorList(AStrings: TStrings);
begin
  pp.GetPostprocessorList(AStrings);
end;

function ppGetFileExt(AIndex: integer): string;
begin
  result := pp.GetFileExtention(AIndex);
end;


end.
