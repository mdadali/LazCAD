unit cPostProcessor;

interface

uses Forms, Classes, System.SysUtils, ComCtrls, Math,
     DB,
     //ZAbstractTable, ZDataset, ZAbstractConnection, ZConnection, ZAbstractDataset,
     Dialogs, atScript, atScripter,

     applicationh,
     fdmMain,
     LiteCAD,
     CAM,
     CADDocument,
     CADFunctions,
     CADConstants,
     CADTypes;
     //Postprocessor;

type

     TPostProcessor = class
     private
       fFileExtList: TStringList;
       fProgressBar: TProgressBar;
       fCADDocument, //original Drw.
       fTempDocument, //temp Drv.
       fCADBlock,
       fCADEntitysCount,
       fCAMEntitysCount: integer;

       //Flags
       fLastKerfInfo: TKerfType;
       fKerfIsfON,
       fCommentIsON: boolean;

       //Options
       fID: integer;
       fDescription,
       fMachineName,
       fAutor      : string;
       fDecimalSep: char;
       fUnitFactor: single;
       fMinEntityDistance: TFloatType;
       fFloatFormat: string; //FloatFormat
       fIsPgmIncremental: boolean;
       fArcCenterAbsolute: boolean;
       fFileFilter,
       fInitialDir: string;
       fOutputKerfCmd,
       fOutputKerfValue: boolean;
       fKerfValue: TFloatType;
       fIcon: TBlobField;
       fPrintLineNumber: boolean;
       fLineNumPrefix: string;
       fLineNumStart,
       fLineNumStep: integer;
       fSaveInBatchMode: boolean;
       fAutoSaveFileNameFormat: string;
       fAutoSaveFileNameCounter: integer;
       fOutputScript: string;
       fInputScript: string;
       fGenerateOutput: boolean;
       fUseCadFileNameForOutput: boolean;
       fProcessID,
       fToolID : integer;
       fToolTableName: string;
       fOutputUnicode: boolean;

       fProcessName,
       fToolName: string;

       function  GetCADOnlyFileName: string;

       procedure GenFileExtList;
       function  GenProgramName: string;
       function  GenOutputFileName: string;


       procedure SetMachine(AMachineName: string);
       function  GetMachine: string;

       function  GethEntyFromList(ACAMLineIDX: integer): integer;
       function  GetCAMLineIDXFromList(AhEntity: integer): integer;
       function  GetEntitysCount: integer;
       procedure CopyCAMEntitys;

       function  GetFiguresCount: integer;
       
       function  GetFigureKerfInfo(AhEntity: integer): TKerfType;
       function  GetFigureProfilePointCount(AhEntity: integer): integer;

       procedure ProcessFigure(AhEntity: integer);

     public
       fProgramName: string;
       fOutputFileName: string;
       atScripter1: TatScripter;
       constructor create(ACADDocument: integer; AMachineName: string; AProgressBar: TProgressBar);
       destructor  destroy; override;
       procedure   Execute;
       procedure   Reset(ACADDocument: integer; AMachineName: string; AProgressBar: TProgressBar);
       procedure   SaveCAMToFile(AFileName: string);


       function GetFileExtention(AIndex: integer): string;
       function GetProgramName: string;
       function GetProcessName: string;
       function GetToolTableName: string;
       function GetToolName: string;
       procedure SetProcessName;
       procedure SetToolTableName;
       procedure SetToolName;


       function GetIntegerOption(AParamName: string): integer;
       function GetStringOption(AParamName: string): string;
       function GetFloatOption(AParamName: string): double;
       function GetBoolOption(AParamName: string): boolean;

       function GetIntegerToolTableValue(AParamName: string): integer;
       function GetStringToolTableValue(AParamName: string): string;
       function GetFloatToolTableValue(AParamName: string): double;
       function GetBoolToolTableValue(AParamName: string): boolean;

       function GetEntIntegerProp(AEntity, APropID: integer): integer;
       function GetEntStringProp(AEntity, APropID: integer): string;
       function GetEntFloatProp(AEntity, APropID: integer): double;
       function GetEntBoolProp(AEntity, APropID: integer): boolean;

       procedure GetPostprocessorList(AStrings: TStrings);

       procedure CreateRapidMove(AhEntity: integer);
       procedure ProcessCounturStart(AKerfInfo, ALastKerfInfo: TKerfType);
       procedure ProcessCounturEnd;
       procedure ProcessKerf(AKerfInfo, ALastKerfInfo: TKerfType);

       procedure ProcessProgramStart;
       procedure ProcessProgramEnd;

       procedure PreProcessRapidMove(spx, spy, epx, epy: TFloatType);
       procedure ProcessRapidMove(spx, spy, epx, epy: TFloatType); virtual; abstract;

       procedure PreProcessLine(spx, spy, epx, epy: TFloatType; AEntity: integer);
       procedure ProcessLine(spx, spy, epx, epy: TFloatType); virtual; abstract;

       procedure PreProcessArc(radius, cpx, cpy, spx, spy, epx, epy: TFloatType; ADirection: TEntDirection; AEntity: integer);
       procedure ProcessArc(cpx, cpy, spx, spy, epx, epy: TFloatType; ADirection: TEntDirection); virtual; abstract;

       procedure PreProcessPLine(AhEntity: integer);

       procedure CopyPP(ASourcePostpName, ADestPostpName: string);
       procedure DeletePP(APostpName: string);

       property ID: integer read fID;
       property Description: string read fDescription;
       property MachineName : string read GetMachine write SetMachine;
       property Autor : string read fAutor;
       property DecimalSep:     char     read  fDecimalSep write fDecimalSep;
       property UnitFactor:     single  read  fUnitFactor write fUnitFactor;
       property MinEntityDistance: TFloatType read fMinEntityDistance write fMinEntityDistance ;
       property FloatFormat: string  read  fFloatFormat write fFloatFormat;
       property IsPgmIncremental: boolean read fIsPgmIncremental write fIsPgmIncremental;
       property ArcCenterAbsolute: boolean   read fArcCenterAbsolute write fArcCenterAbsolute;
       property FileFilter: string   read fFileFilter write fFileFilter;
       //property FileFilter2[Index: Integer] : string read GetIndexFileFilter;
       property InitialDir:     string   read fInitialDir write fInitialDir;
       property OutputKerfCmd: boolean read    fOutputKerfCmd  write fOutputKerfCmd;
       property OutputKerfValue: boolean read  fOutputKerfValue  write fOutputKerfValue;
       property KerfValue: TFloatType    read  fKerfValue write fKerfValue;
       property Icon: TBlobField read fIcon;
       property PrintLineNumber: boolean read fPrintLineNumber write fPrintLineNumber;
       property LineNumPrefix: string read  fLineNumPrefix write fLineNumPrefix;
       property LineNumStart: integer read  fLineNumStart write  fLineNumStart;
       property LineNumStep: integer read  fLineNumStep write fLineNumStep;
       property SaveInBatchMode: boolean read  fSaveInBatchMode write fSaveInBatchMode;
       property AutoSaveFileNameFormat: string read  fAutoSaveFileNameFormat write fAutoSaveFileNameFormat;
       property AutoSaveFileNameCounter: integer read  fAutoSaveFileNameCounter write fAutoSaveFileNameCounter;
       property OutputScript: string read  fOutputScript write fOutputScript;
       property InputScript: string read  fInputScript write fInputScript;
       property GenerateOutput: boolean read  fGenerateOutput write fGenerateOutput;
       property UseCadFileNameForOutput: boolean read fUseCadFileNameForOutput write fUseCadFileNameForOutput ;
       property ProcessID  : integer read  fProcessID write fProcessID;
       property ToolID : integer read  fToolID write fToolID;
       property ToolTableName: string read fToolTableName write fToolTableName;
       property OutputUnicode: boolean read fOutputUnicode write fOutputUnicode;

       property ProcessName: string read fProcessName write fProcessName;
       property ToolName: string   read fToolName    write fToolName;

       property   ProgramName : string read fProgramName write fProgramName;
       property   OutputFileName: string read fOutputFileName write fOutputFileName;

     protected
       procedure  Init;
       procedure  ReadSettings;

       procedure  ProgressBarReset(AMin, AMax, AStep: integer);
       procedure  ProgressBarStep;

       procedure  SaveDataListToList(AList: TList);
       procedure  SaveToStringList(AStringList: TStringList);

       function   IsCAMEntity(AEntity: integer): boolean;

       property   ProgressBar: TProgressBar read fProgressBar write fProgressBar;
       property   ActiveCADDocument: integer read fCADDocument;
       property   CADBlock: integer read fCADBlock;
       property   CADEntitysCount: integer read fCADEntitysCount;
       property   CAMEntitysCount: integer read fCAMEntitysCount;

       property   LastKerfInfo: TKerfType       read fLastKerfInfo write fLastKerfInfo;
       property   KerfIsON: boolean             read fKerfIsfON write fKerfIsfON;
       property   CommentIsON: boolean          read fCommentIsON write fCommentIsON;

     end;


implementation

procedure TPostProcessor.Reset(ACADDocument: integer; AMachineName: string; AProgressBar: TProgressBar);
begin
  fCADDocument := ACADDocument;
  fMachineName := AMachineName;
  fProgressBar := AProgressBar;
  Init;
end;

constructor TPostProcessor.create(ACADDocument: integer; AMachineName: string; AProgressBar: TProgressBar);
begin
  inherited create;
  fCADDocument  := ACADDocument;
  fMachineName  := AMachineName;
  fProgressBar  := AProgressBar;
  fFileExtList  := TStringList.Create;
  atScripter1   := TatScripter.Create(nil);
  fTempDocument := lcCreateDrawing ();
  Init;
end;

procedure TPostProcessor.Init;
begin
  //Data
  CAM.fCurrLineCount  := 0;
  CAM.CAM_ResetList;
  fCADEntitysCount := 0;
  fCAMEntitysCount := 0;
  lcDrwNew (fTempDocument, 'ppTempCAD.lcd', 0);
  CopyCAMEntitys;
  //MakeContours(fTempDocument)///
  //Modify fTempDocument
  fCADBlock := lcDrwGetFirstObject( fTempDocument, LC_OBJ_BLOCK );

  //Flags
  CommentIsON         := false;
  LastKerfInfo        := ktNone;
  KerfIsON            := true;
  ReadSettings;

  //Options
  System.SysUtils.FormatSettings.DecimalSeparator := fDecimalSep;

  GenFileExtList;

  fProgramName    := GenProgramName;
  fOutputFileName := GenOutputFileName;
end;

destructor TPostProcessor.destroy;
begin
  try
    atScripter1.Free;
    FreeAndNil(fFileExtList);
    lcDeleteDrawing (fTempDocument);
  finally
    inherited;
  end;
end;

procedure TPostProcessor.CopyPP(ASourcePostpName, ADestPostpName: string);
var SourcePPSourceID, TempFieldCount, i: integer;
begin
  if frmdmMain.dsPostProcessors.DataSet.Locate('Name', ASourcePostpName, [loCaseInsensitive]) then
  begin
    SourcePPSourceID := frmdmMain.dsPostProcessors.DataSet.FieldByName('ID').AsInteger;
    frmdmMain.dsTemp.DataSet := frmdmMain.queryTemp;
    frmdmMain.queryTemp.Open('Select * from PostProcessors where id = ' + IntToStr(SourcePPSourceID));
    TempFieldCount := frmdmMain.queryTemp.FieldCount;

    frmdmMain.dsPostProcessors.DataSet.Insert;
    frmdmMain.dsPostProcessors.DataSet.FieldByName('Name').AsString := ADestPostpName;
    for i := 3 to  TempFieldCount - 1 do
       frmdmMain.dsPostProcessors.DataSet.Fields[i].Value := frmdmMain.queryTemp.Fields[i].Value;

    frmdmMain.dsPostProcessors.DataSet.Post;
    frmdmMain.dsTemp.DataSet.Refresh;
  end;
end;

procedure TPostProcessor.DeletePP(APostpName: string);
var SourcePPSourceID, TempFieldCount, i: integer;
begin
  if frmdmMain.dsPostProcessors.DataSet.Locate('Name', APostpName, [loCaseInsensitive]) then
  begin
    SourcePPSourceID := frmdmMain.dsPostProcessors.DataSet.FieldByName('ID').AsInteger;
    frmdmMain.dsPostProcessors.DataSet.Delete;
  end;
end;

procedure TPostProcessor.GenFileExtList;
begin
  fFileExtList.Clear;
  fFileExtList.Delimiter := '|';
  fFileExtList.DelimitedText := fFileFilter;
end;

procedure TPostProcessor.SetMachine(AMachineName: string);
begin
  fMachineName := AMachineName;
  Init;
end;

function TPostProcessor.GetMachine: string;
begin
  result := fMachineName;
end;

procedure TPostProcessor.ReadSettings;
begin
  if frmdmMain.dsPostProcessors.DataSet.Locate('Name', fMachineName, []) then
  begin
    //Table-Postprocessor
    fID                      := frmdmMain.dsPostProcessors.DataSet.Fields[0].AsInteger;
    //fID                      := ppGetCurrPpID;
    fDescription             := frmdmMain.dsPostProcessors.DataSet.FieldByName('Description').AsString;
    fMachineName             := frmdmMain.dsPostProcessors.DataSet.FieldByName('Name').AsString;
    fAutor                   := frmdmMain.dsPostProcessors.DataSet.FieldByName('Autor').AsString;
    fDecimalSep              := frmdmMain.dsPostProcessors.DataSet.FieldByName('DecimalSep').AsString[1];
    fUnitFactor              := frmdmMain.dsPostProcessors.DataSet.FieldByName('UnitFactor').AsInteger;
    fMinEntityDistance       := frmdmMain.dsPostProcessors.DataSet.FieldByName('MinEntityDistance').AsFloat;
    fFloatFormat             := frmdmMain.dsPostProcessors.DataSet.FieldByName('FloatFormat').AsString;
    fIsPgmIncremental        := frmdmMain.dsPostProcessors.DataSet.FieldByName('IsPgmIncremental').AsBoolean;
    fArcCenterAbsolute       := frmdmMain.dsPostProcessors.DataSet.FieldByName('ArcCenterIsAbolute').AsBoolean;
    fFileFilter              := frmdmMain.dsPostProcessors.DataSet.FieldByName('FileFilter').AsString;
    fInitialDir              := frmdmMain.dsPostProcessors.DataSet.FieldByName('InitialDir').AsString;
    fOutputKerfCmd           := frmdmMain.dsPostProcessors.DataSet.FieldByName('OutputKerfCmd').AsBoolean;
    fOutputKerfValue         := frmdmMain.dsPostProcessors.DataSet.FieldByName('OutputKerfValue').AsBoolean;
    fKerfValue               := frmdmMain.dsPostProcessors.DataSet.FieldByName('KerfValue').AsFloat;
    //fIcon                  := frmdmMain.dsPostProcessors.DataSet.FieldByName('Icon ').Blob;
    fPrintLineNumber         := frmdmMain.dsPostProcessors.DataSet.FieldByName('PrintLineNumber').AsBoolean;
    fLineNumPrefix           := frmdmMain.dsPostProcessors.DataSet.FieldByName('LineNumPrefix').AsString;
    fLineNumStart            := frmdmMain.dsPostProcessors.DataSet.FieldByName('LineNumStart').AsInteger;
    fLineNumStep             := frmdmMain.dsPostProcessors.DataSet.FieldByName('LineNumStep').AsInteger;
    fSaveInBatchMode         := frmdmMain.dsPostProcessors.DataSet.FieldByName('SaveInBatchMode').AsBoolean;
    fAutoSaveFileNameFormat  := frmdmMain.dsPostProcessors.DataSet.FieldByName('AutoSaveFileNameFormat').AsString;
    fAutoSaveFileNameCounter := frmdmMain.dsPostProcessors.DataSet.FieldByName('AutoSaveFileNameCounter').AsInteger;
    fOutputScript            := frmdmMain.dsPostProcessors.DataSet.FieldByName('OutputScript').AsString;
    fInputScript             := frmdmMain.dsPostProcessors.DataSet.FieldByName('InputScript').AsString;

    fGenerateOutput          := frmdmMain.dsPostProcessors.DataSet.FieldByName('GenerateOutput').AsBoolean;
    fUseCadFileNameForOutput := frmdmMain.dsPostProcessors.DataSet.FieldByName('UseCadFileNameForOutput').AsBoolean;

    fProcessID               := frmdmMain.dsPostProcessors.DataSet.FieldByName('ProcessID').AsInteger;
    fToolID                  := frmdmMain.dsPostProcessors.DataSet.FieldByName('ToolID').AsInteger;
    fToolTableName           := frmdmMain.dsPostProcessors.DataSet.FieldByName('ToolTableName').AsString;

    fOutputUnicode           := frmdmMain.dsPostProcessors.DataSet.FieldByName('OutputUnicode').AsBoolean;

    CAM.fPrintLineNumber     := fPrintLineNumber;
    CAM.fLineNumPrefix       := fLineNumPrefix;
    CAM.fLineNumStart        := fLineNumStart;
    CAM.fLineNumStep         := fLineNumStep;
    
    atScripter1.SourceCode.Clear;
    atScripter1.SourceCode.Text :=  fOutputScript;
    SetProcessName;
    SetToolTableName;
    SetToolName;
  end;
end;


function TPostProcessor.GetProgramName: string;
begin
  result := fProgramName;
end;

function TPostProcessor.GetProcessName: string;
begin
  result := fProcessName;
end;

function TPostProcessor.GetToolTableName: string;
begin
  result := fToolTableName;
end;

function TPostProcessor.GetToolName: string;
begin
  result := fToolName;
end;

procedure TPostProcessor.SetProcessName;
var hProcessID: integer;
begin
  hProcessID := frmdmMain.dsPostProcessors.DataSet.Fields[26].AsInteger;
  //frmdmMain.tblProcessTable.Locate('id', hProcessID, [loCaseInsensitive]);
  //fProcessName := frmdmMain.tblProcessTable.Fields[1].AsString;
end;

procedure TPostProcessor.SetToolTableName;
begin
  fToolTableName := frmdmMain.dsPostProcessors.DataSet.Fields[28].AsString;
end;

procedure TPostProcessor.SetToolName;
var hToolID: integer;
begin
  hToolID := frmdmMain.dsPostProcessors.DataSet.Fields[27].AsInteger;
  //frmdmMain.tblToolTable.Locate('id', hToolID, [loCaseInsensitive]);
  //fToolName := frmdmMain.tblToolTable.Fields[1].AsString;
end;

procedure TPostProcessor.SaveCAMToFile(AFileName: string);
var StrList: TStringList; i: integer;
begin
  try
    StrList := TStringList.Create;
    for i := 0 to CNCDataList.Count - 1 do
      StrList.Add(TDataRec(CNCDataList.Items[i]^).CAMLine);
    StrList.SaveToFile(AFileName);
  finally
    FreeAndNil(StrList);
  end;
end;

procedure   TPostProcessor.SaveToStringList(AStringList: TStringList);
begin
end;

function  TPostProcessor.GethEntyFromList(ACAMLineIDX: integer): integer;
var i: integer;
begin
  result := - 1;
  for i := 0 to CNCDataList.Count - 1 do
  begin
    if TDataRec(CNCDataList.Items[i]^).CAMLineIDX = ACAMLineIDX then
    begin
      result :=  TDataRec(CNCDataList.Items[i]^).hEntity;
      exit;
    end;
  end;
end;

function TPostProcessor.GetCAMLineIDXFromList(AhEntity: integer): integer;
var i: integer;
begin
  result := - 1;
  for i := 0 to CNCDataList.Count - 1 do
  begin
    if TDataRec(CNCDataList.Items[i]^).hEntity = AhEntity then
    begin
      result := TDataRec(CNCDataList.Items[i]^).CAMLineIDX;
      exit;
    end;
  end;
end;

procedure TPostProcessor.SaveDataListToList(AList: TList);
var i: integer; PDataRec: TPTDataRec;
begin
  if (AList <> nil) then
  begin
    for i := 0 to CNCDataList.Count - 1 do
    begin
      new(PDataRec);
      PDataRec^.CAMLineIDX     := TDataRec(CNCDataList.Items[i]^).CAMLineIDX;
      PDataRec^.hEntity        := TDataRec(CNCDataList.Items[i]^).hEntity;
      PDataRec^.Description    := TDataRec(CNCDataList.Items[i]^).Description;
      AList.Add(PDataRec);
    end;
  end;
end;

function TPostProcessor.IsCAMEntity(AEntity: integer): boolean;
var hEnt: integer; hLayerName: string;
begin
  hEnt       := lcPropGetInt(AEntity, LC_PROP_ENT_TYPE);
  hLayerName := lcPropGetStr(AEntity, LC_PROP_ENT_LAYER);

  result :=  (
               ((hEnt = LC_ENT_LINE)    or
               (hEnt = LC_ENT_ARC)     or
               (hEnt = LC_ENT_CIRCLE)  or
               (hEnt = LC_ENT_POLYLINE)

               ) and
               ((hLayerName = CAM_LAYER_INNER_CONTOUR_CW)  or
                (hLayerName = CAM_LAYER_INNER_CONTOUR_CCW) or
                (hLayerName = CAM_LAYER_OUTER_CONTOUR_CW)  or
                (hLayerName = CAM_LAYER_OUTER_CONTOUR_CCW) or
                (hLayerName = CAM_LAYER_KERF_OFF)
               )
              );
end;

function  TPostProcessor.GetEntitysCount: integer;
begin
  result := CADEntitysCount;
end;


procedure TPostProcessor.ProgressBarReset(AMin, AMax, AStep: integer);
begin
  if fProgressBar <> nil then
  begin
    fProgressBar.Min := AMin;
    fProgressBar.Max := AMax;
    fProgressBar.Step := AStep;
    fProgressBar.Position := 0;
  end;
end;

procedure TPostProcessor.ProgressBarStep;
begin
  if fProgressBar <> nil then
  begin
    fProgressBar.StepIt;
  end;
end;

procedure TPostProcessor.CopyCAMEntitys;
var EntType: integer; //X, Y, Z, X1, Y1, Z1, X2, Y2, Z2: double;
    hBlock, hTempBlock, hEnt: THANDLE;   //, hLayer:
    hBlockCount, i: integer;  //hEntCount,
begin
  if  fCADDocument <> 0 then
  begin
    hBlockCount := lcDrwCountObjects(fCADDocument, LC_OBJ_BLOCK);
    hBlock     := lcDrwGetFirstObject(fCADDocument, LC_OBJ_BLOCK );
    hTempBlock := lcDrwGetFirstObject(fTempDocument, LC_OBJ_BLOCK );
    for i := 0 to hBlockCount - 1 do
    begin
      hEnt := lcBlockGetFirstEnt( hBlock );
      while( hEnt <> 0) do
      begin
        //inc(fCADEntitysCount);
        EntType := lcPropGetInt( hEnt, LC_PROP_ENT_TYPE );
        if(lcPropGetBool( hEnt, LC_PROP_ENT_DELETED) = 0) then /////////////
        case( EntType ) of
          LC_ENT_POINT: begin
            //ShowMessage('POINT');
            //X := lcPropGetFloat( hEnt, LC_PROP_POINT_X );
            //Y := lcPropGetFloat( hEnt, LC_PROP_POINT_Y );
          end;
          LC_ENT_LINE: begin
            if IsCAMEntity(hEnt) then
              if lcBlockAddClone (hTempBlock, hEnt) > 0 then
                inc(fCAMEntitysCount);
            //ShowMessage('LINE');
            //X1 := lcPropGetFloat( hEnt, LC_PROP_LINE_X0 );
            //Y1 := lcPropGetFloat( hEnt, LC_PROP_LINE_Y0 );
            //X2 := lcPropGetFloat( hEnt, LC_PROP_LINE_X1 );
            //Y2 := lcPropGetFloat( hEnt, LC_PROP_LINE_Y1 );
          end;
          LC_ENT_ARC: begin
            if IsCAMEntity(hEnt) then
              if lcBlockAddClone (hTempBlock, hEnt) > 0 then
                inc(fCAMEntitysCount);
          end;
          LC_ENT_CIRCLE: begin
            if IsCAMEntity(hEnt) then
              if lcBlockAddClone (hTempBlock, hEnt) > 0 then
                inc(fCAMEntitysCount);
          end;
          LC_ENT_SHAPE: begin
            //
          end;
          LC_ENT_RECT: begin
            //ShowMessage('RECTANGLE');
          end;
          LC_ENT_POLYLINE: begin
            if IsCAMEntity(hEnt) then
              if lcBlockAddClone (hTempBlock, hEnt) > 0 then
                inc(fCAMEntitysCount);
          end;

          LC_ENT_ELLIPSE: begin
            //ShowMessage('ELLIPSE');
          end;
          LC_ENT_TEXT: begin
            //ShowMessage('TEXT');
          end;
          LC_ENT_BLOCKREF: begin
            //ShowMessage('BLOCKREF');
          end;
          LC_ENT_IMAGEREF: begin
            //ShowMessage('IMAGEREF');
          end;
          LC_ENT_HATCH: begin
            //ShowMessage('HATCH');
          end;
          LC_ENT_DIMROT: begin
            //ShowMessage('DIMROT');
          end;
          LC_ENT_LEADER: begin
            //ShowMessage('LEADER');
          end;
          LC_ENT_ATTRIB: begin
            //ShowMessage('ATTRIB');
          end;
        end;
        //hLayer := lcPropGetHandle( hEnt, LC_PROP_ENT_LAYER ); // get layer handle, if needed
        hEnt := lcBlockGetNextEnt( hBlock, hEnt ); // get next entity
      end;
      hBlock := lcDrwGetNextObject(fCADDocument, hBlock );
    end;
  end;
end;

procedure TPostProcessor.Execute;
var hKerfInfo: TKerfType; hEntity: integer;
begin
  if CAMEntitysCount = 0 then exit;
  ProgressBarReset(0, CADEntitysCount, 1);
  try
    atScripter1.Compile;
    atScripter1.Execute;
    ProcessProgramStart;

    LastKerfInfo := ktNone;
    //FiguresCount    := GetFiguresCount;
    ProgressBarReset(0, CADEntitysCount, 1);
    hEntity := lcBlockGetFirstEnt(CADBlock);
    while (hEntity <> 0)  do
    begin

      while (not IsCAMEntity(hEntity)) and (hEntity <> 0) do
        hEntity := lcBlockGetNextEnt(CADBlock, hEntity);
      if (hEntity = 0) then exit;

      if CAD_IsCounturStart(hEntity, fMinEntityDistance) then
      begin
        CreateRapidMove(hEntity);
        LastKerfInfo := ktNone;
        hKerfInfo := GetFigureKerfInfo(hEntity);
        ProcessCounturStart(hKerfInfo, LastKerfInfo);
        LastKerfInfo :=  hKerfInfo;
      end;
      ProcessFigure(hEntity);
      if CAD_IsCounturEnd(hEntity, fMinEntityDistance) then
        ProcessCounturEnd;
      ProgressBarStep;
      hEntity := lcBlockGetNextEnt(CADBlock, hEntity);
    end;
    if (CAMEntitysCount > 0) then ProcessProgramEnd;
  finally
    ProgressBarReset(0, CADEntitysCount, 1);
  end;
end;

procedure TPostProcessor.ProcessProgramStart;
begin
  atScripter1.ExecuteSubRoutine('OnPgmStart');
end;

procedure TPostProcessor.ProcessProgramEnd;
begin
  atScripter1.ExecuteSubRoutine('OnPgmEnd');
end;

procedure TPostProcessor.CreateRapidMove(AhEntity: integer);
var spx, spy, epx, epy: TFloatType; tmpEntity: integer;
begin
  epx := CAD_GetFigureSP_X(AhEntity);
  epy := CAD_GetFigureSP_Y(AhEntity);
  tmpEntity := lcBlockGetFirstEnt(CADBlock);
  if (AhEntity = tmpEntity) then
  begin
     spx := 0;
     spy := 0;
  end else
  begin
    tmpEntity := lcBlockGetPrevEnt(CADBlock, AhEntity);
    spx := CAD_GetFigureEP_X(tmpEntity);
    spy := CAD_GetFigureEP_Y(tmpEntity);
  end;
  if   ((abs(spx - epx) > fMinEntityDistance) or (abs(spy - epy) > fMinEntityDistance)) then
    PreProcessRapidMove(spx, spy, epx, epy);
end;

procedure TPostProcessor.ProcessFigure(AhEntity: integer);
var hEntType: integer;  hKerfInfo: TKerfType; hArcDirection: TEntDirection;
radius, spx, spy, epx, epy, cpx, cpy: TFloatType;
begin
  spx  := CAD_GetFigureSP_X(AhEntity);
  spy  := CAD_GetFigureSP_Y(AhEntity);
  epx  := CAD_GetFigureEP_X(AhEntity);
  epy  := CAD_GetFigureEP_Y(AhEntity);

  hEntType := lcPropGetInt(AhEntity, LC_PROP_ENT_TYPE);

  hKerfInfo   :=  GetFigureKerfInfo(AhEntity);
  if (hEntType = LC_ENT_LINE) then
  begin
    ProcessKerf(hKerfInfo, LastKerfInfo);
    PreProcessLine(spx, spy, epx, epy, AhEntity);
    LastKerfInfo :=  hKerfInfo;
  end else if (hEntType = LC_ENT_ARC) then
  begin
    ProcessKerf(hKerfInfo, LastKerfInfo);
    LastKerfInfo :=  hKerfInfo;
    radius :=  CAD_GetFigureRadius(AhEntity);
    cpx  := CAD_GetFigureCP_X(AhEntity);
    cpy  := CAD_GetFigureCP_Y(AhEntity);
    hArcDirection := CAM_GetFigureDirection(AhEntity);
    PreProcessArc(radius, cpx, cpy, spx, spy, epx, epy, hArcDirection, AhEntity);
    LastKerfInfo :=  hKerfInfo;
  end else if (hEntType = LC_ENT_CIRCLE) then
  begin
    ProcessKerf(hKerfInfo, LastKerfInfo);
    LastKerfInfo :=  hKerfInfo;
    radius :=  CAD_GetFigureRadius(AhEntity);
    cpx  := CAD_GetFigureCP_X(AhEntity);
    cpy  := CAD_GetFigureCP_Y(AhEntity);
    hArcDirection := CAM_GetFigureDirection(AhEntity);
    PreProcessArc(radius, cpx, cpy, spx, spy, epx, epy, hArcDirection, AhEntity);
    LastKerfInfo :=  hKerfInfo;
  end else if (hEntType = LC_ENT_POLYLINE) then
  begin
    ProcessKerf(hKerfInfo, LastKerfInfo);
    PreProcessPLine(AhEntity);
    LastKerfInfo := hKerfInfo;
  end
end;

procedure TPostProcessor.PreProcessRapidMove(spx, spy, epx, epy: TFloatType);
begin
  if IsPgmIncremental  then
  begin
    epx := UnitFactor  * (epx - spx);
    epy := UnitFactor  * (epy - spy);
  end else
  begin
    epx := UnitFactor  * epx;
    epy := UnitFactor  * epy;

  end;
  atScripter1.ExecuteSubRoutine('OnRapidMoveON', [spx, spy, epx, epy]);
end;

procedure TPostProcessor.PreProcessLine(spx, spy, epx, epy: TFloatType; AEntity: integer);
begin
  if IsPgmIncremental  then
  begin
    epx := UnitFactor  * (epx - spx);
    epy := UnitFactor  * (epy - spy);
  end else
  begin
    epx := UnitFactor  * epx;
    epy := UnitFactor  * epy;
  end;
  atScripter1.ExecuteSubRoutine('OnLine', [spx, spy, epx, epy, AEntity]);
end;

procedure TPostProcessor.PreProcessArc(radius, cpx, cpy, spx, spy, epx, epy: TFloatType; ADirection: TEntDirection; AEntity: integer);
begin
   if IsPgmIncremental then
   begin  // Program is Relative
     epx := UnitFactor * (epx - spx);
     epy := UnitFactor * (epy - spy);
     if (not ArcCenterAbsolute ) then
     begin   // Program is  Relative CenterPoint is Relative
       cpx := UnitFactor * (cpx - spx);
       cpy := UnitFactor * (cpy - spy);
     end else
     begin   // Program is  Relative CenterPoint is Absolute
       cpx := UnitFactor * cpx;
       cpy := UnitFactor * cpy;
     end;
   end else
   begin  // Program is Absolute
     epx := UnitFactor * epx;
     epy := UnitFactor * epy;
     if (not ArcCenterAbsolute ) then
     begin   // Program is Absolute, CenterPoint is Relative
       cpx := UnitFactor * (cpx - spx);
       cpy := UnitFactor * (cpy - spy);
     end else
     begin   // Program is Absolute, CenterPoint is Absolute
       cpx := UnitFactor * cpx;
       cpy := UnitFactor * cpy;
     end;
   end;

  if (ADirection = edCCW) then
    atScripter1.ExecuteSubRoutine('OnArcCCW', [radius, cpx, cpy, spx, spy, epx, epy, AEntity])
  else
    atScripter1.ExecuteSubRoutine('OnArcCW', [radius, cpx, cpy, spx, spy, epx, epy, AEntity]);
end;

procedure TPostProcessor.PreProcessPLine(AhEntity: integer);
var i, hEntType: integer; CurrVertex, LastVertexFlag: integer;  NVERS: integer; hRadius: TFloatType;
 VER_X, VER_Y, SEG_ARC_ANG: TFloatType;
begin
  {NVERS :=  lcPropGetInt(AhEntity, LC_PROP_PLINE_NVERS);
  //ShowMessage('NVERS = ' + IntToStr(NVERS));
  CurrVertex := lcPlineGetFirstVer(AhEntity);
  while (CurrVertex <> 0)  do
  begin
    if lcPropGetBool(CurrVertex, LC_PROP_VER_LAST) > 0 then
      break;
    hRadius  :=  lcPropGetFloat(CurrVertex, LC_PROP_VER_SEGARCRAD);
    if (hRadius <> 0) then
    begin //Arc-Vertex
      ShowMessage('radius = ' + FloatToStr(hRadius));

      VER_X       := lcPropGetFloat(CurrVertex, LC_PROP_VER_X);         //Arc-StartX
      ShowMessage('Arc-StartX := ' + FloatToStr(VER_X));
      VER_Y       := lcPropGetFloat(CurrVertex, LC_PROP_VER_Y);         //Arc-StartY
      ShowMessage('StartY := ' + FloatToStr(VER_Y));

      SEG_ARC_ANG := lcPropGetFloat(CurrVertex, LC_PROP_VER_SEGARCANG); //Arc included Angle
      ShowMessage('Arc included Angle := ' + FloatToStr(RadToDeg(SEG_ARC_ANG)));
      if (SEG_ARC_ANG > 0) then
        ShowMessage('ArcDir = CCW ')
      else
        ShowMessage('ArcDir = CW ')

    end else
    begin //Line-Vertex
      ShowMessage('Line');
    end;
    CurrVertex := lcPlineGetNextVer(AhEntity, CurrVertex);
  end; }
end;

procedure TPostProcessor.ProcessKerf(AKerfInfo, ALastKerfInfo: TKerfType);
begin
  if not OutputKerfCmd  then exit;
  if  (AKerfInfo <> ALastKerfInfo) then
  begin
    if (AKerfInfo = ktInsRight) then
    begin
      atScripter1.ExecuteSubRoutine('OnKerfInsRight', ALastKerfInfo);
    end else
    if (AKerfInfo = ktInsLeft)   then
    begin
      atScripter1.ExecuteSubRoutine('OnKerfInsLeft', ALastKerfInfo);
    end else
    if (AKerfInfo = ktOutsRight) then
    begin
      atScripter1.ExecuteSubRoutine('OnKerfOutsRight', ALastKerfInfo);
    end else
    if (AKerfInfo = ktOutsLeft)  then
    begin
      atScripter1.ExecuteSubRoutine('OnKerfOutsLeft', ALastKerfInfo);
    end
    else begin
      atScripter1.ExecuteSubRoutine('OnKerfOFF', ALastKerfInfo);
    end;
  end;
end;

procedure TPostProcessor.ProcessCounturStart(AKerfInfo, ALastKerfInfo: TKerfType);
begin
  atScripter1.ExecuteSubRoutine('OnRapidMoveOFF');
  ProcessKerf(AKerfInfo, ALastKerfInfo);
  atScripter1.ExecuteSubRoutine('OnToolDown');
end;

procedure TPostProcessor.ProcessCounturEnd;
var hKerfValue: TFloatType;
begin
  atScripter1.ExecuteSubRoutine('OnToolUp');

  if OutputKerfCmd then
  begin
    if (LastKerfInfo <> ktNone) then
    begin
      hKerfValue := UnitFactor * KerfValue;
      atScripter1.ExecuteSubRoutine('OnKerfOFF', LastKerfInfo);
    end;
  end;
end;

function  TPostProcessor.GetFiguresCount: integer;
begin
  result := 0;
end;

function  TPostProcessor.GetFigureKerfInfo(AhEntity: integer): TKerfType;
var hRes: TKerfType; hLayerName: string;
begin
  hLayerName := lcPropGetStr(AhEntity, LC_PROP_ENT_LAYER);
  if (hLayerName = CAM_LAYER_INNER_CONTOUR_CW) then hRes := ktInsRight
  else if (hLayerName = CAM_LAYER_INNER_CONTOUR_CCW) then hRes := ktInsLeft
  else if (hLayerName = CAM_LAYER_OUTER_CONTOUR_CW) then hRes := ktOutsRight
  else if (hLayerName = CAM_LAYER_OUTER_CONTOUR_CCW) then hRes := ktOutsLeft
  else hRes := ktNone;
  result := hRes;
end;


function  TPostProcessor.GetFigureProfilePointCount(AhEntity: integer): integer;
begin
  result := 0;
end;

//Get Postprocessor Options
function TPostProcessor.GetIntegerOption(AParamName: string): integer;
begin
  result := frmdmMain.dsPostProcessors.DataSet.FieldByName(AParamName).AsInteger;
end;

function TPostProcessor.GetStringOption(AParamName: string): string;
begin
  result := frmdmMain.dsPostProcessors.DataSet.FieldByName(AParamName).AsString;
end;

function TPostProcessor.GetFloatOption(AParamName: string): double;
begin
  result := frmdmMain.dsPostProcessors.DataSet.FieldByName(AParamName).AsFloat;
end;

function TPostProcessor.GetBoolOption(AParamName: string): boolean;
begin
  result := frmdmMain.dsPostProcessors.DataSet.FieldByName(AParamName).AsBoolean;
end;

//Get Tooltable-Values
function TPostProcessor.GetIntegerToolTableValue(AParamName: string): integer;
begin
  //result := frmdmMain.tblToolTable.FieldByName(AParamName).AsInteger;
end;

function TPostProcessor.GetStringToolTableValue(AParamName: string): string;
begin
  //result := frmdmMain.tblToolTable.FieldByName(AParamName).AsString;
end;

function TPostProcessor.GetFloatToolTableValue(AParamName: string): double;
begin
  //result := frmdmMain.tblToolTable.FieldByName(AParamName).AsFloat;
end;

function TPostProcessor.GetBoolToolTableValue(AParamName: string): boolean;
begin
  //result := frmdmMain.tblToolTable.FieldByName(AParamName).AsBoolean;
end;

//Get Entity-Propertys
function TPostProcessor.GetEntIntegerProp(AEntity, APropID: integer): integer;
begin
  result := lcPropGetInt( AEntity, APropID );
end;

function TPostProcessor.GetEntStringProp(AEntity, APropID: integer): string;
begin
  result := lcPropGetStr( AEntity, APropID );
end;

function TPostProcessor.GetEntFloatProp(AEntity, APropID: integer): double;
begin
  result := lcPropGetFloat( AEntity, APropID );
end;

function TPostProcessor.GetEntBoolProp(AEntity, APropID: integer): boolean;
begin
  result := boolean(lcPropGetBool( AEntity, APropID ));
end;


function TPostProcessor.GetCADOnlyFileName: string;
var  strFileName: string;
begin
  //Filename without extention (FileName.txt -> FileName)!
  strFileName  := lcPropGetStr(CADDocument.hLcDrv, LC_PROP_DRW_FILENAME );
  result := applicationh.GetOnlyFileName(strFileName);
  //result := applicationh.GetOnlyFileName(CADDocument.CADFileName);
end;

function TPostProcessor.GetFileExtention(AIndex: Integer): string;
var hExt: string;  i: integer;
begin
  if (AIndex <= 1) then
    i := 1
  else
    i := (AIndex*2) - 1;
  hExt := fFileExtList.Strings[i];
  hExt := Copy(hExt, 2, 4);
  result := hExt;
end;

function TPostProcessor.GenProgramName: string;
var PgmName: string;
begin
    if (frmdmMain.dsPostProcessors.DataSet.Fields[25].AsBoolean) then // UseCADFileName for Output
      PgmName := GetCADOnlyFileName
    else begin
      PgmName := frmdmMain.dsPostProcessors.DataSet.Fields[21].AsString; //Format
      PgmName := PgmName + frmdmMain.dsPostProcessors.DataSet.Fields[22].AsString; //Trigger
      frmdmMain.dsPostProcessors.DataSet.Edit;
      frmdmMain.dsPostProcessors.DataSet.Fields[22].AsInteger := frmdmMain.dsPostProcessors.DataSet.Fields[22].AsInteger +1;
      frmdmMain.dsPostProcessors.DataSet.Post;
    end;
  result := PgmName;
end;

function TPostProcessor.GenOutputFileName: string;
var hExt: string; 
begin
  hExt := GetFileExtention(0);
  if  (self.ProgramName = '') then
    result := WideCharToString(lcStrGet('APP_NONAME'))
  else 
    result := self.InitialDir + '\' + self.ProgramName + hExt;
end;

procedure TPostProcessor.GetPostprocessorList(AStrings: TStrings);
var i, x: integer;
begin
  x := frmdmmain.dsPostProcessors.DataSet.Fields[0].AsInteger;
  frmdmmain.dsPostProcessors.DataSet.First;
  AStrings.Clear;
  for i := 0 to frmdmmain.dsPostProcessors.DataSet.RecordCount - 1 do
  begin
    AStrings.Add(frmdmmain.dsPostProcessors.DataSet.FieldByName('Name').AsString);
    frmdmmain.dsPostProcessors.DataSet.Next;
  end;
  frmdmmain.dsPostProcessors.DataSet.Locate('id', x, []);
end;

end.

