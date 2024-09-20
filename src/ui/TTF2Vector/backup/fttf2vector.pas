unit fTTF2Vector;

{$mode objfpc}{$H+}


interface

uses
  EasyLazFreeType, TTObjs, TTTypes, Classes, SysUtils, FileUtil, Forms,
  Controls, Graphics, Dialogs, Spin, StdCtrls, ExtCtrls, ShellCtrls, ComCtrls,
  ActnList, BCButtonFocus, GifAnim,
  CADSys4,
  CS4Shapes,
  CS4BaseTypes,
  CS4Tasks,
  CS4DXFModule,

  fAbout;

type

  { TfrmTTF2Vector }

  TfrmTTF2Vector = class(TForm)
    acFileSaveAs: TAction;
    acExit: TAction;
    acAbout: TAction;
    acExportToCAD: TAction;
    acZoomPan: TAction;
    acZoomWindow: TAction;
    acZoomExtentions: TAction;
    acZoomOut: TAction;
    acZoomIn: TAction;
    ActionList1: TActionList;
    BCButtonFocus1: TBCButtonFocus;
    BCButtonFocus2: TBCButtonFocus;
    BCButtonFocus5: TBCButtonFocus;
    BCButtonFocus77: TBCButtonFocus;
    BCButtonFocus78: TBCButtonFocus;
    BCButtonFocus79: TBCButtonFocus;
    BCButtonFocus8: TBCButtonFocus;
    BCButtonFocus80: TBCButtonFocus;
    BCButtonFocus83: TBCButtonFocus;
    CADCmp2D1: TCADCmp2D;
    CADPrg2D1: TCADPrg2D;
    CADViewport2D1: TCADViewport2D;
    cboxAutoDraw: TCheckBox;
    cboxHinted: TCheckBox;
    cboxShowControlpoints: TCheckBox;
    cboxBold: TCheckBox;
    cboxItalic: TCheckBox;
    cboxShowDirections: TCheckBox;
    fspeCharHeight: TFloatSpinEdit;
    fspeLineHeight: TFloatSpinEdit;
    GroupBox1: TGroupBox;
    grboxFontPath: TGroupBox;
    GroupBox3: TGroupBox;
    grboxCAD: TGroupBox;
    ImageListClassic: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    Memo1: TMemo;
    Panel1: TPanel;
    Panel4: TPanel;
    pnlBottom: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    pnlFile: TPanel;
    pnlFileGrip: TPanel;
    pnlZoom: TPanel;
    pnlZoomGrip: TPanel;
    rulerBottom: TRuler;
    rulerLeft: TRuler;
    SaveDialog1: TSaveDialog;
    ShellListView1: TShellListView;

    ShellTreeView1: TShellTreeView;
    procedure acAboutExecute(Sender: TObject);
    procedure acExitExecute(Sender: TObject);

    procedure acExportToCADExecute(Sender: TObject);
    procedure acFileSaveAsExecute(Sender: TObject);
    procedure acZoomExtentionsExecute(Sender: TObject);
    procedure acZoomInExecute(Sender: TObject);
    procedure acZoomOutExecute(Sender: TObject);
    procedure acZoomPanExecute(Sender: TObject);
    procedure acZoomWindowExecute(Sender: TObject);
    procedure CADViewport2D1Paint(Sender: TObject);
    procedure cboxBoldChange(Sender: TObject);
    procedure cboxItalicChange(Sender: TObject);
    procedure cboxShowControlpointsChange(Sender: TObject);
    procedure cboxShowDirectionsChange(Sender: TObject);
    procedure ChangeHinted(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure fspeCharHeightChange(Sender: TObject);
    procedure fspeLineHeightChange(Sender: TObject);
    procedure Memo1Change(Sender: TObject);
    procedure ShellListView1Change(Sender: TObject; Item: TListItem;
      Change: TItemChange);
  private
    FTTFFont:TFreeTypeFont;
    TmpPolygon2D: TPolygon2D;
    procedure AddPolygonVertex(x, y, x2, y2: TRealType);
    procedure DrawGlyph(AGlyph: TFreeTypeGlyph; APosX, APosY: TRealType);
    procedure DrawGlyphs;

    function GetFontPath: string;
    procedure SetShowDirection(AValue: boolean);

    procedure SaveDXFFile(AFileName: string);
    procedure SaveCS4File(AFileName: string);

   public
    { public declarations }
  end;

const
     OriginX = 80;
     OriginY = 0;
     SymbolH = 300;

var

  FirstVertex: boolean;

  {$IFNDEF TTF2VECTOR_EMBEDDED}
  frmTTF2Vector: TfrmTTF2Vector;
  {$ENDIF}

implementation

{$IFDEF TTF2VECTOR_EMBEDDED}
uses fMain;
{$ENDIF}

{$R *.lfm}

{ TfrmTTF2Vector }

procedure TfrmTTF2Vector.FormCreate(Sender: TObject);
begin
  {$IFDEF TTF2VECTOR_EMBEDDED}
    acAbout.Visible        := false;
    acExportToCAD.Visible  := true;
  {$ELSE}
    acAbout.Visible       := true;
    acExportToCAD.Visible := false;
  {$ENDIF}
  FTTFFont := TFreeTypeFont.create;
  FTTFFont.Hinted:=false;
  {$IFDEF LINUX}
    ShellTreeView1.Root := '/';
    ShellTreeView1.Path := ExtractFilePath(Application.ExeName) + 'data/fonts/truetype/dejavu/';
    FTTFFont.Name:= ExtractFilePath(Application.ExeName) + 'data/fonts/truetype/dejavu/DejaVuSansMono-Bold.ttf';
  {$ENDIF}
  {$IFDEF WINDOWS}
    ShellTreeView1.Path := ExtractFilePath(Application.ExeName) + 'data\fonts\truetype\dejavu\';
    FTTFFont.Name:= ExtractFilePath(Application.ExeName) + 'data\fonts\truetype\dejavu\DejaVuSansMono-Bold.ttf';
  {$ENDIF}

  cboxHinted.Checked := FTTFFont.Hinted;
  DrawGlyphs;
end;

procedure TfrmTTF2Vector.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FTTFFont.Free;
  CloseAction := caFree;
end;

procedure TfrmTTF2Vector.fspeCharHeightChange(Sender: TObject);
begin
  if cboxAutoDraw.Checked then
    DrawGlyphs;
end;

procedure TfrmTTF2Vector.fspeLineHeightChange(Sender: TObject);
begin
  if cboxAutoDraw.Checked then
    DrawGlyphs;
end;


function TfrmTTF2Vector.GetFontPath: string;
begin
  result := ShellTreeView1.GetPathFromNode(ShellTreeView1.Selected) + ShellListView1.Selected.Caption;
end;

procedure TfrmTTF2Vector.Memo1Change(Sender: TObject);
begin
  if cboxAutoDraw.Checked then
    DrawGlyphs;
end;

procedure TfrmTTF2Vector.ShellListView1Change(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  if ShellListView1.Selected = nil then exit;
  try
    FTTFFont.Name := GetFontPath;
    DrawGlyphs;
  except
    raise;
  end;
end;

procedure TfrmTTF2Vector.cboxBoldChange(Sender: TObject);
begin
  if cboxBold.Checked then
    FTTFFont.Style :=  FTTFFont.Style + [ftsBold]
  else
    FTTFFont.Style :=  FTTFFont.Style - [ftsBold];
  DrawGlyphs;
end;

procedure TfrmTTF2Vector.cboxItalicChange(Sender: TObject);
begin
  if cboxItalic.Checked then
    FTTFFont.Style :=  FTTFFont.Style + [ftsItalic]
  else
    FTTFFont.Style :=  FTTFFont.Style - [ftsItalic];
  DrawGlyphs;
end;

procedure TfrmTTF2Vector.cboxShowControlpointsChange(Sender: TObject);
begin
  CADViewport2D1.ShowControlPoints := cboxShowControlpoints.Checked;
  CADViewport2D1.Repaint;
end;

procedure TfrmTTF2Vector.ChangeHinted(Sender: TObject);
begin
  FTTFFont.Hinted := cboxHinted.Checked;
  DrawGlyphs;
end;

procedure TfrmTTF2Vector.DrawGlyphs;
var
  i, x, hCharIndex: integer;
  Glyph: TFreeTypeGlyph;
  hChar: char;
  xPos, yPos: TRealType;
  LineHeight: TRealType;
begin
  CADCmp2D1.DeleteAllObjects;
  LineHeight := fspeLineHeight.Value;
  for i := 0 to Memo1.Lines.Count - 1 do
  begin
    yPos := i * LineHeight;
    xPos := 0;
    for x := 1 to Length(Memo1.Lines[i]) do
    begin
      hChar := Memo1.Lines[i][x];
      hCharIndex := Ord(hChar);
      Glyph := FTTFFont.Glyph[FTTFFont.CharIndex[hCharIndex]];

      if Assigned(Glyph) then
      begin
        if hChar <> ' ' then
          xPos := xPos + (Glyph.Bounds.Width * 64) + 100
        else
          xPos := xPos + 300;
        DrawGlyph(Glyph, xPos, -yPos);  //-yPos = Reverse - Lines.
      end;
    end;
  end;
  SetShowDirection(cboxShowDirections.Checked);
  CADViewport2D1.ZoomToExtension;
  //CADViewport2D1.Repaint;
end;

procedure TfrmTTF2Vector.DrawGlyph(AGlyph: TFreeTypeGlyph; APosX, APosY: TRealType);
var CharIndex, i, j, cends,lastoncurve: Integer; Glyph:TFreeTypeGlyph; _glyph:PGlyph;
    x,y,x1,y1,scx,scy, k:TrealType; startcountur:boolean;
begin
  FirstVertex := false;
  k := 1;
  _glyph := AGlyph.Data.z;

  cends := 0;
  lastoncurve := 0;
  startcountur:=true;
  for j:= 0 to _glyph^.outline.n_points do
  begin
    x1:=_glyph^.outline.points^[j].x + APosX;
    y1:=_glyph^.outline.points^[j].y + APosY;

    if  startcountur then
    begin
      scx := x1;
      scy := y1;
      startcountur:=false;
      TmpPolygon2D := TPolygon2D.Create(-1, []);
      CADCmp2D1.AddObject(-1, TmpPolygon2D);
      FirstVertex := true;
    end else
    begin
      if (_glyph^.outline.flags^[j] and TT_Flag_On_Curve)<>0 then
      begin
        if j-lastoncurve>3 then
          lastoncurve:=lastoncurve;
        lastoncurve:=j;
      end;
      AddPolygonVertex(x1,y1,x,y);
      if j=_glyph^.outline.conEnds^[cends] then
      begin
        inc(cends);
        startcountur:=true;
        lastoncurve:=j+1;
        FirstVertex := true;
        AddPolygonVertex(x1,y1,scx,scy);
        if cends=_glyph^.outline.n_contours then
          break;
      end;
    end;
    x := x1;
    y := y1;
  end;
end;

procedure TfrmTTF2Vector.AddPolygonVertex(x, y, x2, y2: TRealType);
var P0, P1:  TPoint2D;  CharSize: TRealType;
begin
  CharSize := 1/607 * fspeCharHeight.Value;

  x := x *     CharSize;
  x2 := x2 *   CharSize;

  y := y *    CharSize;
  y2 := y2 *  CharSize;

  y  := - y;
  y2 := - y2;

  if FirstVertex then
  begin
    P0.X := x;  P0.Y := -y;  P0.W := 1;
    TmpPolygon2D.Points.Add(P0);
    P1.X := x2; P1.Y := -y2; P1.W := 1;
    TmpPolygon2D.Points.Add(P1);
    FirstVertex := false;
  end else
  begin
    P1.X := x2; P1.Y := -y2; P1.W := 1;
    TmpPolygon2D.Points.Add(P1);
  end;
end;

procedure TfrmTTF2Vector.acZoomInExecute(Sender: TObject);
begin
  if CADPrg2D1.IsBusy then CADPrg2D1.SendUserEvent(CADPRG_ACCEPT);
  CADViewport2D1.ZoomIn;
end;

procedure TfrmTTF2Vector.acZoomExtentionsExecute(Sender: TObject);
begin
  if CADPrg2D1.IsBusy then CADPrg2D1.SendUserEvent(CADPRG_ACCEPT);
  CADViewport2D1.ZoomToExtension;
end;

procedure TfrmTTF2Vector.acZoomOutExecute(Sender: TObject);
begin
  if CADPrg2D1.IsBusy then CADPrg2D1.SendUserEvent(CADPRG_ACCEPT);
  CADViewport2D1.ZoomOut;
end;

procedure TfrmTTF2Vector.acZoomWindowExecute(Sender: TObject);
begin
  if CADPrg2D1.IsBusy then CADPrg2D1.SendUserEvent(CADPRG_ACCEPT);
  CADPrg2D1.SuspendOperation(TCADPrgZoomArea, nil);
end;

procedure TfrmTTF2Vector.CADViewport2D1Paint(Sender: TObject);
begin
  rulerLeft.Repaint;
  rulerBottom.Repaint;
end;

procedure TfrmTTF2Vector.acZoomPanExecute(Sender: TObject);
begin
  CADPrg2D1.SuspendOperation(TCADPrgRealTimePan, nil);
end;

procedure TfrmTTF2Vector.cboxShowDirectionsChange(Sender: TObject);
begin
  SetShowDirection(cboxShowDirections.Checked);
end;

procedure TfrmTTF2Vector.SetShowDirection(AValue: boolean);
var TmpIter: TExclusiveGraphicObjIterator;
begin
  CADCmp2D1.ShowDirection := AValue;
  TmpIter := CADCmp2D1.ObjectsExclusiveIterator;
  try
    TmpIter.First;
    while TmpIter.Current <> nil do
    begin
      if (TmpIter.Current.LayerName <> CAM_LAYER_STR_JUMPS) and (TmpIter.Current.LayerName <> LAYER_STR_TEMPLATE) then
      begin
        TPrimitive2D(TmpIter.Current).ShowDirection := AValue;
        CADCmp2D1.RedrawObject(TPrimitive2D(TmpIter.Current));
      end;
      TmpIter.Next;
    end;
  finally
    TmpIter.Free;
    CADViewport2D1.Repaint;
  end;
end;

procedure TfrmTTF2Vector.acFileSaveAsExecute(Sender: TObject);
var hExt: string; hFileName: string;
begin
  If SaveDialog1.Execute then
  begin
    hExt := LowerCase(ExtractFileExt(SaveDialog1.FileName));
    if      hExt = '.cs4' then SaveCS4File(SaveDialog1.FileName)
    else if hExt = '.dxf' then SaveDXFFile(SaveDialog1.FileName);
  end;
end;

procedure TfrmTTF2Vector.SaveCS4File(AFileName: string);
begin
  CADCmp2D1.SaveToFile(AFileName);
end;

procedure TfrmTTF2Vector.SaveDXFFile(AFileName: string);
var DXFExport: TDXF2DExport;
begin
  DXFExport := TDXF2DExport.Create(AFileName, CADCmp2D1);
  try
    DXFExport.WriteDXF;
  finally
    DXFExport.Free;
  end;
end;

procedure TfrmTTF2Vector.acExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmTTF2Vector.acExportToCADExecute(Sender: TObject);
begin
  {$IFDEF TTF2VECTOR_EMBEDDED}
  frmMain.ImportFromCADCmp(CADCmp2D1);
  {$ENDIF}
end;

procedure TfrmTTF2Vector.acAboutExecute(Sender: TObject);
var frmAbout: TfrmAbout;
begin
  frmAbout := TfrmAbout.Create(self);
  frmAbout.ShowModal;
end;

end.

