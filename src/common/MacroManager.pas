unit MacroManager;

interface

uses
     DB, Variants, SysUtils,
     LiteCAD,
     fdmMain,
     CADTypes,
     CADConstants,
     fMacroMan;


function  mmGetMacroName(AMacroId: Integer) : string;
function  mmGetMacroID(AMacroName: string) : integer;
function  mmGetCurrMacroName : string;
function  mmGetCurrMacroID : integer;

function mmGetFloatFieldValue(AMacroId: Integer; AFieldName: string): TFloatType;
function mmGetIntegerFieldValue(AMacroId: Integer; AFieldName: string): Integer;
function mmGetStringFieldValue(AMacroId: Integer; AFieldName: string) : string;
function mmGetDateTimeFieldValue(AMacroId: Integer; AFieldName: string) : TDateTime;
function mmGetVariantFieldValue(AMacroId: Integer; AFieldName: string): Variant;

function mmAddLine(x0, y0, x1, x2: TFloatType): integer;
function mmAddCircle(x, y, radius: TFloatType; bFilled: integer): integer;
function mmAddArc(x, y, radius, sa, aa: TFloatType): integer;
function mmAddArc3P(sx, sy, mx, my, ex, ey: TFloatType): integer;

procedure mmZoomRect(Left, Bottom, Right, Top: integer);


implementation

///////////////////////////////////////////////////////////////
//Macro////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////
function   mmGetMacroName(AMacroId: Integer) : string;
var hRes: string;
begin
  hRes := CAD_INVALID_STRING_VALUE;
  if frmdmMain.tblMacros.locate('id',  AMacroId, [])  then
    hRes := frmdmMain.tblMacros.FieldByName('Name').AsString;
  result := hRes;
end;

function  mmGetMacroID(AMacroName: string) : integer;
var hRes: int64;
begin
  hRes := CAD_INVALID_INTEGER_VALUE;
  if frmdmMain.tblMacros.locate('Name', AMacroName, [loCaseInsensitive ]) then
    hRes := frmdmMain.tblMacros.Fields[0].AsInteger;
  result := hRes;
end;

function  mmGetCurrMacroName : string;
begin
  result := frmdmMain.tblMacros.FieldByName('Name').AsString;
end;

function  mmGetCurrMacroID : integer;
begin
  result := frmdmMain.tblMacros.Fields[0].AsInteger;
end;


///////////////////////////////////////////////////////////////
//MacroFields//////////////////////////////////////////////////
///////////////////////////////////////////////////////////////
function  mmGetFloatFieldValue(AMacroId: Integer; AFieldName: string): TFloatType;
var hRes: TFloatType;
begin
  hRes := CAD_INVALID_FLOAT_VALUE;
  if frmdmMain.tblMacros.locate('id',  AMacroId, []) then
    if frmdmMain.tblMacroFields.locate('FieldName',  AFieldName, [loCaseInsensitive ]) then
      hRes := frmdmMain.tblMacroFields.FieldByName('FieldValue').AsFloat;
  result := hRes;
end;

function  mmGetIntegerFieldValue(AMacroId: Integer; AFieldName: string): Integer;
var hRes: int64;
begin
  hRes := CAD_INVALID_INTEGER_VALUE;
  if frmdmMain.tblMacros.locate('id',  AMacroId, []) then
    if frmdmMain.tblMacroFields.locate('FieldName',  AFieldName, [loCaseInsensitive ]) then
      hRes := frmdmMain.tblMacroFields.FieldByName('FieldValue').AsInteger;
  result := hRes;
end;

function  mmGetBoolFieldValue(AMacroId: Integer; AFieldName: string): Boolean;
var hRes: boolean;
begin
  //hRes := CAD_INVALID_FLOAT_VALUE;
  if frmdmMain.tblMacros.locate('id',  AMacroId, []) then
    if frmdmMain.tblMacroFields.locate('FieldName',  AFieldName, [loCaseInsensitive ]) then
      hRes := frmdmMain.tblMacroFields.FieldByName('FieldValue').AsBoolean;
  result := hRes;
end;

function  mmGetStringFieldValue(AMacroId: Integer; AFieldName: string) : string;
var hRes: string;
begin
  hRes := '';
  if frmdmMain.tblMacros.locate('id',  AMacroId, []) then
    if frmdmMain.tblMacroFields.locate('FieldName',  AFieldName, [loCaseInsensitive ]) then
      hRes := frmdmMain.tblMacroFields.FieldByName('FieldValue').AsString;
  result := hRes;
end;

function mmGetVariantFieldValue(AMacroId: Integer; AFieldName: string): Variant;
var hRes: Variant;
begin
  hRes := '';
  if frmdmMain.tblMacros.locate('id',  AMacroId, []) then
    if frmdmMain.tblMacroFields.locate('FieldName',  AFieldName, [loCaseInsensitive ]) then
      hRes := frmdmMain.tblMacroFields.FieldByName('FieldValue').AsVariant;
  result := hRes;
end;

function mmGetDateTimeFieldValue(AMacroId: Integer; AFieldName: string) : TDateTime;
var hRes: TDateTime;
begin
  hRes := now;
  if frmdmMain.tblMacros.locate('id',  AMacroId, []) then
    if frmdmMain.tblMacroFields.locate('FieldName',  AFieldName, [loCaseInsensitive ]) then
      hRes := frmdmMain.tblMacroFields.FieldByName('FieldValue').AsDateTime;
  result := hRes;
end;

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
function mmAddLine(x0, y0, x1, x2: TFloatType): integer;
begin
  result := lcBlockAddLine(frmMacroMan.CADBlock, x0, y0, x1, x2);
end;

function mmAddCircle(x, y, radius: TFloatType; bFilled: integer): integer;
begin
  result := lcBlockAddCircle(frmMacroMan.CADBlock, x, y, radius, bFilled);
end;

function mmAddArc(x, y, radius, sa, aa: TFloatType): integer;
begin
  result := lcBlockAddArc(frmMacroMan.CADBlock, x, y, radius, sa, aa);
end;

function mmAddArc3P(sx, sy, mx, my, ex, ey: TFloatType): integer;
begin
  result := lcBlockAddArc3P(frmMacroMan.CADBlock, sx, sy, mx, my, ex, ey);
end;

procedure mmZoomRect(Left, Bottom, Right, Top: integer);
begin
  lcWndZoomRect (frmMacroMan.CADWindow, Left, Bottom, Right, Top)
end;


end.
