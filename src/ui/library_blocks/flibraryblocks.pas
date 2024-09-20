unit fLibraryBlocks;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  CADSys4,
  CS4Tasks,
  fDrawing;

type

  { TfrmLibraryBlocks }

  TfrmLibraryBlocks = class(TForm)
    btnSetCurrentBlockLibrary: TButton;
    btnClose: TButton;
    btnAddBlockToCAD: TButton;
    btnRenameBlock: TButton;
    btnDeleteBlock: TButton;
    btnNewLibrary: TButton;
    CADCmp2D1: TCADCmp2D;
    CADViewport2D1: TCADViewport2D;
    edtCurrentBlockLibrary: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    lboxBlocks: TListBox;
    OpenBlockLibraryDialog: TOpenDialog;
    procedure btnAddBlockToCADClick(Sender: TObject);
    procedure btnNewLibraryClick(Sender: TObject);
    procedure btnSetCurrentBlockLibraryClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure lboxBlocksSelectionChange(Sender: TObject; User: boolean);
  private
    fDrawing: TDrawing;
    procedure LibraryBlocksFromDrawing;
    procedure ReadBlocks;
    procedure PreviewBlock(AIndex: integer);
  public
    constructor create(AOwner: TComponent; ADrawing: TDrawing) ;
  end;

//var
  //frmLibraryBlocks: TfrmLibraryBlocks;

implementation

uses fMain;

{$R *.lfm}

procedure TfrmLibraryBlocks.btnSetCurrentBlockLibraryClick(Sender: TObject);
begin
  if OpenBlockLibraryDialog.Execute then
  begin
    fDrawing.LoadBlockLibraryFromFile(OpenBlockLibraryDialog.FileName);
    fDrawing.CADCmp2D.CurrentBlockLibrary := OpenBlockLibraryDialog.FileName;
    edtCurrentBlockLibrary.Text := OpenBlockLibraryDialog.FileName;
    frmMain.TIPropertyGrid1.Repaint;
    ReadBlocks;
  end;
end;

procedure TfrmLibraryBlocks.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

procedure TfrmLibraryBlocks.btnAddBlockToCADClick(Sender: TObject);
var TmpStr: String; SrcBlk: TSourceBlock2D;
begin
  if lboxBlocks.ItemIndex = -1 then Exit;
  TmpStr := lboxBlocks.Items[lboxBlocks.ItemIndex];
  SrcBlk := fDrawing.CADCmp2D.FindSourceBlock(StringToBlockName(TmpStr));
  if SrcBlk <> nil then
  begin
    fDrawing.CADPrg2D.StartOperation(TCAD2DPositionObject,
       TCAD2DPositionObjectParam.Create(nil, TBlock2D.Create(-1, SrcBlk)));
  end;
end;

procedure TfrmLibraryBlocks.btnNewLibraryClick(Sender: TObject);
var Strm: TFileStream;  TmpStr: string;
begin
  if not InputQuery('Add block', 'Name', TmpStr) then
  Strm := TFileStream.Create(TmpStr, fmCreate or fmOpenWrite);
  try
    fDrawing.CADCmp2D.SaveLibrary(Strm);
    fDrawing.CADCmp2D.CurrentBlockLibrary := TmpStr;
    fDrawing.LoadBlockLibraryFromFile(TmpStr);
    edtCurrentBlockLibrary.Text := TmpStr;
    ReadBlocks;
    frmMain.TIPropertyGrid1.Repaint;
  finally
    Strm.Free;
  end;
end;

procedure TfrmLibraryBlocks.lboxBlocksSelectionChange(Sender: TObject; User: boolean);
begin
  //if lboxBlocks.ItemIndex >  then
  PreviewBlock(lboxBlocks.ItemIndex);
end;

procedure TfrmLibraryBlocks.LibraryBlocksFromDrawing;
var hSourceBlock: TSourceBlock2D; hStr: string; TmpIter: TGraphicObjIterator;
begin
  TmpIter := fDrawing.CADCmp2D.SourceBlocksIterator;
  hSourceBlock := TmpIter.First as TSourceBlock2D;
  try
    while hSourceBlock <> nil do
    begin
      hStr := BlockNameToString(hSourceBlock.Name);
      CADCmp2D1.AddSourceBlock(hSourceBlock);
      hSourceBlock := TmpIter.Next as TSourceBlock2D;
    end;
  finally
    TmpIter.Free;
  end;
end;

constructor TfrmLibraryBlocks.create(AOwner: TComponent;  ADrawing: TDrawing);
var TmpStm: TFileStream;
begin
  inherited create(AOwner);
  fDrawing := ADrawing;
  edtCurrentBlockLibrary.Text := fDrawing.CADCmp2D.CurrentBlockLibrary;
  CADCmp2D1.DeleteLibrarySourceBlocks;

  //LibraryBlocksFromDrawing;

  try
    TmpStm := TFileStream.Create(fDrawing.CADCmp2D.CurrentBlockLibrary, fmOpenRead);
    CADCmp2D1.LoadLibrary(TmpStm);
  finally
    TmpStm.Free;
  end;

  ReadBlocks;
  {$IFDEF WINDOWS}
    OpenBlockLibraryDialog.InitialDir := ExtractFilePath(Application.ExeName) + '\data\blocklibs';
  {$ELSE}
    OpenBlockLibraryDialog.InitialDir := ExtractFilePath(Application.ExeName) + '/data/blocklibs';
  {$ENDIF}
  OpenBlockLibraryDialog.Filter := '*.blk';
end;

procedure TfrmLibraryBlocks.ReadBlocks;
var TmpIter: TGraphicObjIterator; hSourceBlock: TSourceBlock2D;
    hStr: string;
begin
  lboxBlocks.Items.Clear;
  lboxBlocks.Repaint;
  //TmpIter := CADCmp2D1.SourceBlocksIterator;
  TmpIter := CADCmp2D1.SourceBlocksIterator;
  try
    hSourceBlock := TmpIter.First as TSourceBlock2D;
    while hSourceBlock <> nil do
    begin
      //if SrcName = Result.Name then
      //Exit;
      hStr := BlockNameToString(hSourceBlock.Name);
      lboxBlocks.Items.Add(hStr);
      hSourceBlock := TmpIter.Next as TSourceBlock2D;
    end;
  finally
    TmpIter.Free;
  end;
  if lboxBlocks.Items.Count > 0 then
  begin
    lboxBlocks.Selected[0] := true;
    PreviewBlock(lboxBlocks.ItemIndex);
  end;
  //Raise ECADListObjNotFound.Create(Format('TCADCmp2D.FindSourceBlock: Source block %s not found', [SrcName]));
end;

procedure TfrmLibraryBlocks.PreviewBlock(AIndex: integer);
var hSourceBlock: TSourceBlock2D;
begin
  {hSourceBlock := CADCmp2D1.FindSourceBlock(lboxBlocks.Items[AIndex]);
  CADCmp2D1.DeleteAllObjects;
  CADCmp2D1.RefreshViewports;
  CADCmp2D1.Viewports[0].ZoomToExtension;
  CADCmp2D1.AddBlock(-1, lboxBlocks.Items[AIndex]);
  CADCmp2D1.RefreshViewports;
  CADCmp2D1.Viewports[0].ZoomToExtension; }

  hSourceBlock := CADCmp2D1.FindSourceBlock(lboxBlocks.Items[AIndex]);
  CADCmp2D1.DeleteAllObjects;
  CADCmp2D1.RefreshViewports;
  CADCmp2D1.Viewports[0].ZoomToExtension;
  CADCmp2D1.AddBlock(-1, lboxBlocks.Items[AIndex]);
  CADCmp2D1.RefreshViewports;
  CADCmp2D1.Viewports[0].ZoomToExtension;
end;

end.

