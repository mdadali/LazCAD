unit camh;

{$mode delphi}{$H+}

interface

uses Classes, SysUtils, Dialogs,
     CS4BaseTypes,
     CommonUtils;

const


CAM_DIRECTON_NONE   = -1;
CAM_DIRECTON_CCW    = 0;
CAM_DIRECTON_CW     = 1;

CAM_SIDE_NONE       = -1;
CAM_SIDE_INNER      = 0;
CAM_SIDE_OUTER      = 1;


CAD_LAYER_0                     = '0';
CAM_LAYER_KERF_OFF              = 'KerfOFF';
CAM_LAYER_INNER_CONTOUR_CW      = 'Innercontour-Clockwise';
CAM_LAYER_INNER_CONTOUR_CCW     = 'Innercontour-Counterclockwise';

CAM_LAYER_OUTER_CONTOUR_CW      = 'Outercontour-Clockwise';
CAM_LAYER_OUTER_CONTOUR_CCW     = 'Outercontour-Counterclockwise';

CAM_LAYER_MOVE                  = 'Move';
CAM_LAYER_RAPIDMOVE             = 'RapidMove';

X_VALUE = 1;
Y_VALUE = 2;
I_VALUE = 3;
J_VALUE = 4;


COORDS_SEPARATOR  = ' ';

type
  TString = string[255];

  TPgmType = (ptAbsolute, ptIncremental);


  TCAMSide       = (csNone, csInside, csOutside);

  TKerfType      =  (None, InnerContourCW, InnerContourCCW, OuterContourCW, OuterContourCCW);

  TCNCCommand    = (ccNone, ccPgmStart, ccPgmStop, ccPgmEnd, ccRapidMoveON, ccRapidMoveOFF, ccToolDown, ccToolUp,
                    ccKerfInsideRight, ccKerfInsideLeft, ccKerfOutsideRight, ccKerfOutsideLeft, ccKerfOFF,
                    ccIncrementalPgmON, ccAbsolutePgmON,
                    ccLinearMove, ccCircularMoveCW, ccCircularMoveCCW, ccCommentON, ccCommentOFF,ccDelay, ccLoop);





  TGeometryType =  (gtLine, gtArc, gtRapidMove);
  TCommandType  =  (ctWithParam, ctWithoutParam);
  TRecType      =  (rtCommand, rtGeometry, rtComment, rtUnknow);

  TPTDataRec    = ^TDataRec;
  TDataRec      =  record
                        CAMLine: string[255];
                        CAMLineIDX: integer;
                        Description: string;
                        case RecType: TRecType of       //Datensatztyp
                          rtCommand: (
                                      Command : TString;
                                      case CommandType: TCommandType of
                                         ctWithParam:    (param: TString);
                                      );
                          rtUnknow:   (eMessage      :TString);

                          rtGeometry:(

                                      case GeometryType: TGeometryType of
                                        gtRapidMove: (rsx, rsy, rex, rey   :TRealType);
                                        gtLine: (lsx, lsy, lex, ley   :TRealType; hEntity: integer);
                                        gtArc:  (sx, sy, cx, cy, ex, ey, sAng, eAng, Radius: TRealType;
                                                 direction  :TArcDirection;
                                                );
                                    );

  end;


var ValidCharSet: set of char =
  ['0'..'9', '.', ',', '+', '-', 'x', 'X', 'y', 'Y', 'z', 'T', 'i', 'I', 'j', 'J', 'k', 'K', 'f', 'F', 'm', 'M', 'n', 'N', 'g', 'G', ' '];

  //DIN
  fPrintLineNumber: boolean;
  fLineNumStart,
  fLineNumStep : integer;
  fLineNumPrefix: string;
  fCurrLineCount: integer;

  CNCDataList: TList;



function  CAM_GetFigureDirection(AhEntity: integer): TArcDirection;
procedure CAM_AddCamLayers(ADocument: integer);
procedure CAM_PrintLine(Val1, Val2: string);
function  CAM_FormatOutput(AStr: string): string;
function  CAM_GetNewLineNum: string;
procedure CAM_SaveToStrings(AStrings: TStrings);
procedure CAM_SaveToFile(AFileName: string);
function  CAM_RecordCount: integer;
procedure CAM_ResetList;

implementation

function  CAM_GetFigureDirection(AhEntity: integer): TArcDirection;
var hRes: TArcDirection;  hLayerName: string;
begin
  result := CounterClockwise;
end;

procedure CAM_AddCamLayers(ADocument: integer);
var hLayer: integer; LayerName: PWideChar;
begin
{$IFDEF CAM}
  LayerName := cuStrToWChar(CAM_LAYER_KERF_OFF);
  hLayer := lcDrwGetObjectByName (ADocument, LC_OBJ_LAYER, LayerName);
  if   (hLayer = 0) then
    hLayer :=  lcDrwAddLayer (ADocument,  LayerName, nil, 0, 0);
  if (hLayer <> 0) then
    lcPropPutBool(hLayer, LC_PROP_LAYER_NODLG, 1)
  else
    ShowMessage('Cant add Layer ' + LayerName);
  dispose(LayerName);

  LayerName := cuStrToWChar(CAM_LAYER_INNER_CONTOUR_CW);
  hLayer := lcDrwGetObjectByName (ADocument, LC_OBJ_LAYER, LayerName);
  if   (hLayer = 0) then
    hLayer :=  lcDrwAddLayer (ADocument,  LayerName, nil, 0, 0);
  if (hLayer <> 0) then
    lcPropPutBool(hLayer, LC_PROP_LAYER_NODLG, 1)
  else
    ShowMessage('Cant add Layer ' + LayerName);
  dispose(LayerName);


  LayerName := cuStrToWChar(CAM_LAYER_INNER_CONTOUR_CCW);
  hLayer := lcDrwGetObjectByName (ADocument, LC_OBJ_LAYER, LayerName);
  if   (hLayer = 0) then
    hLayer :=  lcDrwAddLayer (ADocument,  LayerName, nil, 0, 0);
  if (hLayer <> 0) then
    lcPropPutBool(hLayer, LC_PROP_LAYER_NODLG, 1)
  else
    ShowMessage('Cant add Layer ' + LayerName);
  dispose(LayerName);

  LayerName := cuStrToWChar(CAM_LAYER_OUTER_CONTOUR_CW);
  hLayer := lcDrwGetObjectByName (ADocument, LC_OBJ_LAYER, LayerName);
  if   (hLayer = 0) then
    hLayer :=  lcDrwAddLayer (ADocument,  LayerName, nil, 0, 0);
  if (hLayer <> 0) then
    lcPropPutBool(hLayer, LC_PROP_LAYER_NODLG, 1)
  else
    ShowMessage('Cant add Layer ' + LayerName);
  dispose(LayerName);

  LayerName := cuStrToWChar(CAM_LAYER_OUTER_CONTOUR_CCW);
  hLayer := lcDrwGetObjectByName (ADocument, LC_OBJ_LAYER, LayerName);
  if   (hLayer = 0) then
    hLayer :=  lcDrwAddLayer (ADocument,  LayerName, nil, 0, 0);
  if (hLayer <> 0) then
    lcPropPutBool(hLayer, LC_PROP_LAYER_NODLG, 1)
  else
    ShowMessage('Cant add Layer ' + LayerName);
  dispose(LayerName);
{$ENDIF}
end;

procedure CAM_PrintLine(Val1, Val2: string);
var PTDataRec: TPTDataRec;
begin
  new(PTDataRec);
  TDataRec(PTDataRec^).CAMLine :=  CAM_FormatOutput(Val1);
  //PTDataRec.CAMLine     :=  CAM_FormatOutput(Val1);
  //PTDataRec.Description :=  Val2;
  TDataRec(PTDataRec^).Description :=  Val2;
  //PTDataRec.RecType     :=  rtCommand;
  //PTDataRec.Command     :=  KerfStr;
  //PTDataRec.CommandType :=  ctWithParam;
  CNCDataList.Add(PTDataRec);
end;

function CAM_FormatOutput(AStr: string): string;
var hStr: string;
begin
  hStr := AStr;
  if fPrintLineNumber then
  begin
    hStr := CAM_GetNewLineNum  + ' ' + hStr;
    fCurrLineCount := fCurrLineCount + 1;
  end;
  result := hStr;
end;

function CAM_GetNewLineNum: string;
var hStr: string;
begin
  if (fCurrLineCount = 0) then
    hStr := fLineNumPrefix + IntToStr(fLineNumStart)
  else
    hStr := fLineNumPrefix +  IntToStr(((fCurrLineCount) *  fLineNumStep) + fLineNumStart);
  result := hStr;
end;

function CAM_RecordCount: integer;
begin
  result := CNCDataList.Count;
end;

procedure CAM_SaveToStrings(AStrings: TStrings);
var i: integer;
begin
  AStrings.Clear;
  for i := 0 to CNCDataList.Count -1 do
  begin
    AStrings.Add(TDataRec(TPTDataRec(CNCDataList.Items[i])^).CAMLine);
    //AStrings.Add(TPTDataRec(CAM.CNCDataList.Items[i]).CAMLine);
  end;
end;

procedure CAM_SaveToFile(AFileName: string);
var i: integer;  StringList: TStringList;
begin
  if (CNCDataList.Count > 0) then
  begin
    StringList := TStringList.Create;
    for i := 0 to CNCDataList.Count -1 do
    begin
      StringList.Add(TDataRec(TPTDataRec(CNCDataList.Items[i])^).CAMLine);
    end;
    StringList.SaveToFile(AFileName);
    StringList.Free;
  end;
end;

procedure CAM_ResetList;
var i: integer;
begin
{  for i := 0 to  CAM.CNCDataList.Count -1 do
  begin
    dispose(TPTDataRec(CAM.CNCDataList.Items[i]));
  end;
  CNCDataList.Clear;  }
end;

initialization
  //CAM.CNCDataList := TList.Create;

finalization
  //CAM_ResetList;

  //FreeAndNil(CNCDataList);
end.
