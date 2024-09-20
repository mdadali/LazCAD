unit fdmPython;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls, Forms, Dialogs, Graphics, IdTCPServer,
  Process,
  PythonEngine,
  PythonGUIInputOutput,

  CS4BaseTypes,
  CS4Shapes,
  CS4Tasks,
  CADSys4,
  applicationh, IdCustomTCPServer, IdContext;

type

  { TdmPython }

  TdmPython = class(TDataModule)
    IdTCPServer1: TIdTCPServer;
    PDVarDXFFileName: TPythonDelphiVar;
    PythonEngine1: TPythonEngine;
    PythonGUIInputOutput1: TPythonGUIInputOutput;
    pmCADSys4: TPythonModule;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure IdTCPServer1Execute(AContext: TIdContext);
    procedure pmCADSys4Initialization(Sender: TObject);
    procedure PythonEngine1AfterInit(Sender: TObject);
  private
    procedure Py_SetSysPath(const Dirs: array of string);

    function  TranslatePenBrushStyle(AStyle: string): word;
    function  IntToColor(ABackGroundColor: TColor; AColor: integer): TColor;

    function CAD_NewLayer(pself, args : PPyObject )    : PPyObject; cdecl;

    function CAD_AddLine(pself, args : PPyObject )    : PPyObject; cdecl;
    function CAD_AddArc (pself, args : PPyObject )    : PPyObject; cdecl;
    function CAD_AddCircle (pself, args : PPyObject ) : PPyObject; cdecl;

    function CAD_Repaint(pself, args : PPyObject )    : PPyObject; cdecl;
    function CAD_ZoomExt(pself, args : PPyObject )    : PPyObject; cdecl;
    function CAD_DeleteAllObjects(pself, args : PPyObject ): PPyObject; cdecl;

  public
    fCADCmp2D      : TCADCmp2D;
    fCADPrg2D      : TCADPrg2D;
    fCADViewPort2D : TCADViewport2D;
    fRulerLeft     : TRuler;
    fRulerButtom   : TRuler;

    fProgressBar   : TProgressBar;

    function _AddLine(x0, y0, x1, y1: TrealType;                            APenColor: integer; APenWidth: word; APenstyle: string; ALayerName: TLayerName) : TLine2D;
    function _AddArc(cx, cy, r, sa, ea: TRealType; filled, direction: word; APenColor: integer; APenWidth: word; APenstyle: string; ALayerName: TLayerName) : TEllipticalArc2D;
    function _AddCircle(cx, cy, r, sa: TRealType;  filled, direction: word; APenColor: integer; APenWidth: word; APenstyle: string; ALayerName: TLayerName) : TEllipticalArc2D;

  end;

var
  dmPython: TdmPython;

implementation

uses fMain;

procedure TdmPython.PythonEngine1AfterInit(Sender: TObject);
var dir, path: string;
begin
  {$ifdef windows}
  {dir:= ExtractFilePath(Application.ExeName);

  path := (  dir + 'libs\Python311-amd64\DLLs,' +
                   dir + 'libs\Python311-amd64\lib,' +
                   dir + 'libs\Python311-amd64\Lib\site-packages,' +
                   dir + 'libs\Python311-amd64\Lib\site-packages\ezdxf,' +
                   dir + 'libs\Python311-amd64\Lib\site-packages\thonny'
  );
  Py_SetSysPath([path])
  }
  {$endif}
end;

procedure TdmPython.Py_SetSysPath(const Dirs: array of string);
var Str: string; i: Integer;
begin
  Str:= '';
  for i:= 0 to Length(Dirs)-1 do
    Str:= Str + 'r"' + Dirs[i] + '"' + ',';
  Str:= Format('sys.path = [%s]', [Str]);
  GetPythonEngine.ExecString(UTF8Encode(Str));
end;

procedure TdmPython.pmCADSys4Initialization(Sender: TObject);
begin
  with Sender as TPythonModule do
  begin
    AddDelphiMethod('CAD_AddLine',   CAD_AddLine,    'CAD_AddLine');
    AddDelphiMethod('CAD_AddArc',    CAD_AddArc,     'CAD_AddArc');
    AddDelphiMethod('CAD_AddCircle', CAD_AddCircle,  'CAD_AddCircle');

    AddDelphiMethod('CAD_NewLayer',  CAD_NewLayer,  'CAD_NewLayer');

    AddDelphiMethod('CAD_Repaint',   CAD_Repaint,    'CAD_Repaint');
    AddDelphiMethod('CAD_ZoomExt',   CAD_ZoomExt,    'CAD_ZoomExt');
    AddDelphiMethod('CAD_DeleteAllObjects',   CAD_DeleteAllObjects,  'CAD_DeleteAllObjects');
  end;
end;

procedure TdmPython.DataModuleCreate(Sender: TObject);
var Path: string;  AProcess: TProcess; pgm: string;
begin
  MaskFPUExceptions(True);
  IdTCPServer1.Active := true;

  {$IFDEF linux}
    AProcess := TProcess.Create(self);
    pgm := '/media/mustafa/DATEN/daten/mustafa/entwicklung/lazarus/LazCAD/x86_64-linux/gtk2/debug/LazCAD/libs/python311_Linux64/bin/activate';
    AProcess.Executable:= pgm;
    AProcess.Execute;
  {$ENDIF}
  {$IFDEF windows}
  PythonEngine1.DllPath := applicationh.fPythonDLLPath;
  PythonEngine1.DllName := applicationh.fPythonDLLName;
  PythonEngine1.LoadDll;
  {$ENDIF}
end;

procedure TdmPython.DataModuleDestroy(Sender: TObject);
begin
  IdTCPServer1.Active := false;
end;

procedure TdmPython.IdTCPServer1Execute(AContext: TIdContext);
var cmd: String;
begin
  try
    cmd := AContext.Connection.IOHandler.ReadLn;
    cmd := 'CADSys4.' + cmd;
    PythonEngine1.ExecString(UTF8Encode(cmd));
    //AContext.Connection.IOHandler.WriteLn('Bye!');
  finally
    //AContext.Connection.Disconnect;
  end;
end;

function  TdmPython.IntToColor(ABackGroundColor: TColor; AColor: integer): TColor;
begin
  case AColor of
    //0:   result := BYBLOCK
    //257: result := BYOBJECT
    1:     result := clRed;
    2:     result := clYellow;
    3:     result := clGreen;
    4:     result := clAqua;  //clCyan
    5:     result := clBlue;
    6:     result := clFuchsia; //clMagenta
    8:     result := clGray;
    //9:     result := clLigthGray;
    //256: result := BYLAYER
    7:;
  end;
end;

function  TdmPython.TranslatePenBrushStyle(AStyle: string): word;
begin
  result := 0;
        if LowerCase(AStyle)   = 'pssolid'        then  result  :=  Ord(psSolid)
    else  if LowerCase(AStyle) = 'solid'          then  result  :=  Ord(psSolid)
    else  if LowerCase(AStyle) = 'psdash'         then  result  :=  Ord(psDASH)
    else  if LowerCase(AStyle) = 'dashed'         then  result  :=  Ord(psDASH)
    else  if LowerCase(AStyle) = 'psdot'          then  result  :=  Ord(psDOT)
    else  if LowerCase(AStyle) = 'dotted'         then  result  :=  Ord(psDOT)

    else  if LowerCase(AStyle) = 'psdashdot'      then  result  :=  Ord(psDashDot)
    else  if LowerCase(AStyle) = 'psdshdotdot'    then  result  :=  Ord(psDashDotDot)

    else  if LowerCase(AStyle) = 'bssolid'       then  result  :=  Ord(bsSolid)
    else  if LowerCase(AStyle) = 'bsclear'       then  result  :=  Ord(bsClear)
    else  if LowerCase(AStyle) = 'bsvertical'    then  result  :=  Ord(bsVertical)
    else  if LowerCase(AStyle) = 'bscross'       then  result  :=  Ord(bsCross)
    //else  if LowerCase(AStyle) = 'bsdiagonal'    then  result  :=  Ord(bsDiagonal);
end;

function TdmPython.CAD_NewLayer(pself, args : PPyObject )    : PPyObject; cdecl;
var LayerName: PAnsiChar; PenColor:integer; PenStyle, BrushStyle: PAnsiChar; BrushColor: integer; PenWidth,
    Active, Visible, VisibleInLayerManager, Opaque, Streamable, Printable: word;
begin
  if PythonEngine1.PyArg_ParseTuple(args, 'siiiisis:AddLayer', @LayerName, @Active, @Visible, @PenColor, @PenWidth, @PenStyle, @BrushColor, @BrushStyle) <> 0 then
  begin
    fCADCmp2D.Layers.NewLayer(LayerName, Active, Visible, IntToColor(clRed,   PenColor), PenWidth, TranslatePenBrushStyle(PenStyle), BrushColor, TranslatePenBrushStyle(BrushStyle))
  end;
  Result := PythonEngine1.ReturnNone;
end;

function TdmPython.CAD_AddLine(pself, args : PPyObject ): PPyObject; cdecl;
var x0, y0, x1, y1: TRealType; i, PenColor:integer; PenWidth: word; Penstyle, LayerName: PAnsiChar;
begin
  if PythonEngine1.PyArg_ParseTuple(args, 'ffffiiss:AddLine', @x0, @y0, @x1, @y1, @PenColor, @PenWidth, @Penstyle, @LayerName) <> 0 then
  begin
    _AddLine(x0, y0, x1, y1, PenColor, PenWidth, Penstyle, LayerName);
  end;
  Result := PythonEngine1.ReturnNone;
end;

function TdmPython._AddLine(x0, y0, x1, y1: TrealType; APenColor: integer; APenWidth: word; APenstyle: string; ALayerName: TLayerName): TLine2D;
var TmpLine2D: TLine2D;  TmpLayer: TLayer;
begin
  TmpLine2D := nil;

  TmpLayer := fCADCmp2D.Layers.LayerByName[ALayerName];
  if TmpLayer = nil then
    TmpLayer := fCADCmp2D.Layers.NewLayer(ALayerName, -1, -1, IntToColor(clRed, APenColor), APenWidth, TranslatePenBrushStyle(APenStyle), -1, -1);

  TmpLine2D := TLine2D.Create(-1, Point2D(x0, y0), Point2D(x1, y1));

  if APenColor = 256 then
    APenColor := TmpLayer.Pen.Color
  else
    APenColor := IntToColor(clRed, APenColor);

  TmpLine2D.PenWidth := APenWidth;

  if LowerCase(APenstyle) = 'bylayer' then
    TmpLine2D.PenStyle := TmpLayer.Pen.Style
  else
    TmpLine2D.Penstyle := TPenStyle(TranslatePenBrushStyle(APenstyle));

  TmpLine2D.PenColor := APenColor;
  fCADCmp2D.AddObject(TmpLine2D.ID, TmpLine2D);

  TmpLine2D.LayerID := TmpLayer.ID;

  result := TmpLine2D;
end;

function TdmPython.CAD_AddArc(pself, args : PPyObject ): PPyObject; cdecl;
var cx, cy, r, sa, ea: TRealType; filled, direction: word; PenColor: integer; PenWidth: word; Penstyle, LayerName: PAnsiChar; TmpArc2D: TArc2D;
begin
  if PythonEngine1.PyArg_ParseTuple( args, 'fffffiiiiss:AddArc', @cx, @cy,  @r, @sa, @ea, @filled, @direction, @PenColor, @PenWidth, @Penstyle, @LayerName) <> 0 then
  begin
    _AddArc(cx, cy, r, sa, ea, filled, direction, PenColor, PenWidth, Penstyle, LayerName);
  end;
  Result := PythonEngine1.ReturnNone;
end;

function TdmPython._AddArc(cx, cy, r, sa, ea: TRealType; filled, direction: word; APenColor: integer; APenWidth: word; APenstyle: string; ALayerName: TLayerName): TArc2D;
var TmpArc2D: TArc2D;  p0, p1: TPoint2D;      TmpLayer: TLayer;
begin
  p0.X := cx - r;
  p0.Y := cy - r;
  p0.W := 1;

  p1.X := cx + r;
  p1.Y := cy + r;
  p1.W := 1;

  if sa = 0 then sa := 0.00001; //???
  if ea = 0 then ea := 0.00001; //???
  sa := DegToRad(sa);
  ea := DegToRad(ea);

  TmpLayer := fCADCmp2D.Layers.LayerByName[ALayerName];
  if TmpLayer = nil then
    TmpLayer := fCADCmp2D.Layers.NewLayer(ALayerName, -1, -1, IntToColor(clRed, APenColor), APenWidth, TranslatePenBrushStyle(APenStyle), -1, -1);

  TmpArc2D := TArc2D.Create(-1, p0, p1, sa, ea);
  if direction = 0 then
    TmpArc2D.Direction := adCounterclockwise
  else
    TmpArc2D.Direction := adClockwise;

  if APenColor = 256 then
    APenColor := TmpLayer.Pen.Color
  else
    APenColor := IntToColor(clRed, APenColor);

  TmpArc2D.PenColor := APenColor;
  TmpArc2D.PenWidth := APenWidth;
  TmpArc2D.Penstyle := TPenStyle(TranslatePenBrushStyle(APenstyle));
  TmpArc2D.LayerID  := TmpLayer.ID;

  fCADCmp2D.AddObject(TmpArc2D.ID, TmpArc2D);

  result := TmpArc2D;
end;


function TdmPython.CAD_AddCircle(pself, args : PPyObject ): PPyObject; cdecl;
var cx, cy, r, sa, ea: TRealType; filled, direction: word; PenColor: integer; PenWidth: word; Penstyle, LayerName: PAnsiChar; TmpArc2D: TArc2D;
begin
  if PythonEngine1.PyArg_ParseTuple( args, 'ffffiiiiss:AddCircle', @cx, @cy,  @r, @sa, @filled, @direction,  @PenColor, @PenWidth, @Penstyle, @LayerName) <> 0 then
  begin
    _AddCircle(cx, cy, r, sa, filled, direction, PenColor, PenWidth, Penstyle, LayerName);
  end;
  Result := PythonEngine1.ReturnNone;
end;

function TdmPython._AddCircle(cx, cy, r, sa: TRealType; filled, direction: word; APenColor: integer; APenWidth: word; APenstyle: string; ALayerName: TLayerName): TArc2D;
var TmpArc2D: TArc2D;  p0, p1: TPoint2D;    TmpLayer: TLayer;
begin
  p0.X := cx - r;
  p0.Y := cy - r;
  p0.W := 1;

  p1.X := cx + r;
  p1.Y := cy + r;
  p1.W := 1;

  //if sa = 0 then sa := 0.00001; //???
  sa := DegToRad(sa);

  TmpLayer := fCADCmp2D.Layers.LayerByName[ALayerName];
  if TmpLayer = nil then
    TmpLayer := fCADCmp2D.Layers.NewLayer(ALayerName, -1, -1, IntToColor(clRed, APenColor), APenWidth, TranslatePenBrushStyle(APenStyle), -1, -1);

  TmpArc2D := TArc2D.Create(-1, p0, p1, sa, sa);
  if direction = 0 then
    TmpArc2D.Direction := adCounterclockwise
  else
    TmpArc2D.Direction := adClockwise;

  if APenColor = 256 then
    APenColor := TmpLayer.Pen.Color
  else
    APenColor := IntToColor(clRed, APenColor);

  TmpArc2D.PenColor := APenColor;
  TmpArc2D.PenWidth := APenWidth;
  TmpArc2D.Penstyle := TPenStyle(TranslatePenBrushStyle(APenstyle));
  TmpArc2D.LayerID  := TmpLayer.ID;

  fCADCmp2D.AddObject(TmpArc2D.ID, TmpArc2D);

  result := TmpArc2D;
end;

function TdmPython.CAD_Repaint(pself, args : PPyObject ) : PPyObject; cdecl;
var x: integer;
begin
  if PythonEngine1.PyArg_ParseTuple( args, 'i:CAD_Repaint', @x) <> 0 then
  begin
    fCADViewPort2D.Repaint;
    fRulerLeft.Repaint;
    fRulerButtom.Repaint;
  end;
  Result := PythonEngine1.ReturnNone;
end;

function TdmPython.CAD_ZoomExt(pself, args : PPyObject )    : PPyObject; cdecl;
var x: integer;
begin
  if PythonEngine1.PyArg_ParseTuple( args, 'i:CAD_ZoomExt', @x) <> 0 then
    fCADViewPort2D.ZoomToExtension;
  Result := PythonEngine1.ReturnNone;
end;

function TdmPython.CAD_DeleteAllObjects(pself, args : PPyObject ) : PPyObject; cdecl;
var x: integer;
  begin
  if PythonEngine1.PyArg_ParseTuple( args, 'i:CAD_DeleteAllObjects', @x) <> 0 then
  begin
    fCADCmp2D.DeleteAllObjects;
  end;
  Result := PythonEngine1.ReturnNone;
end;

{$R *.lfm}

end.

