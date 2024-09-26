unit fLibraryBlocks;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  CADSys4,
  CS4Tasks,
  fDrawing,
  applicationh;

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
    procedure btnCloseClick(Sender: TObject);
    procedure btnDeleteBlockClick(Sender: TObject);
    procedure btnNewLibraryClick(Sender: TObject);
    procedure btnRenameBlockClick(Sender: TObject);
    procedure btnSetCurrentBlockLibraryClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure lboxBlocksSelectionChange(Sender: TObject; User: boolean);
  private
    fDrawing: TDrawing;
    procedure RefreshListBox;
    procedure PreviewBlock(AIndex: integer);
  public
    constructor create(AOwner: TComponent; ADrawing: TDrawing) ;
  end;

//var
  //frmLibraryBlocks: TfrmLibraryBlocks;

implementation

uses fMain;

{$R *.lfm}

constructor TfrmLibraryBlocks.create(AOwner: TComponent;  ADrawing: TDrawing);
var TmpStm: TFileStream;
begin
  inherited create(AOwner);
  fDrawing := ADrawing;
  edtCurrentBlockLibrary.Text := ExtractFileName(fDrawing.CADCmp2D.CurrentBlockLibrary);

  RefreshListBox;
  if lboxBlocks.Items.Count > 0 then
    PreviewBlock(lboxBlocks.ItemIndex);

  OpenBlockLibraryDialog.InitialDir := GetAppBlockLibrarysPath;
  OpenBlockLibraryDialog.Filter := '*.blk';
end;

procedure TfrmLibraryBlocks.RefreshListBox;
var TmpIter: TGraphicObjIterator; hSourceBlock: TSourceBlock2D;
    hStr: string;
begin
  lboxBlocks.Items.Clear;
  lboxBlocks.Repaint;
  TmpIter := fDrawing.CADCmp2D.SourceBlocksIterator;
  try
    hSourceBlock := TmpIter.First as TSourceBlock2D;
    while hSourceBlock <> nil do
    begin
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
end;

procedure TfrmLibraryBlocks.btnSetCurrentBlockLibraryClick(Sender: TObject);
begin
  if OpenBlockLibraryDialog.Execute then
  begin
    fDrawing.LoadBlockLibraryFromFile(OpenBlockLibraryDialog.FileName);
    fDrawing.CADCmp2D.CurrentBlockLibrary := OpenBlockLibraryDialog.FileName;
    edtCurrentBlockLibrary.Text := OpenBlockLibraryDialog.FileName;
    frmMain.TIPropertyGrid1.Repaint;
    RefreshListBox;
    if lboxBlocks.Items.Count > 0 then
      PreviewBlock(lboxBlocks.ItemIndex);  //0
  end;
end;

procedure TfrmLibraryBlocks.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  fDrawing.SaveBlockLibraryToFile(fDrawing.CADCmp2D.CurrentBlockLibrary);
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

procedure TfrmLibraryBlocks.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmLibraryBlocks.btnDeleteBlockClick(Sender: TObject);
var TmpStr, TmpStr2: String; SrcBlk: TSourceBlock2D;   idx: integer;
begin
  if lboxBlocks.Items.Count = 0 then exit;
  if MessageDlg('Are you sure you want to delete the LibraryBlock?', mtConfirmation, [mbYes, mbNo], 0) = mrNo then
    exit;
  idx    :=  lboxBlocks.ItemIndex;
  TmpStr := lboxBlocks.Items[idx];
  SrcBlk := fDrawing.CADCmp2D.FindSourceBlock(StringToBlockName(TmpStr));
  try
    if SrcBlk <> nil then
      fDrawing.CADCmp2D.DeleteSourceBlock(SrcBlk.Name);
  finally
    fDrawing.SaveBlockLibraryToFile(fDrawing.CADCmp2D.CurrentBlockLibrary);
    RefreshListBox;
    if lboxBlocks.Items.Count > 0 then
      PreviewBlock(lboxBlocks.ItemIndex)
    else begin
      CADCmp2D1.DeleteAllObjects;
      CADCmp2D1.Viewports[0].ZoomToExtension;
    end;
  end;
end;

procedure TfrmLibraryBlocks.btnNewLibraryClick(Sender: TObject);
var TmpStr: string;
begin
  if not InputQuery('New Library', 'Name', TmpStr) then exit;
  TmpStr := GetAppBlockLibrarysPath + TmpStr;
  if FileExists(TmpStr) then
    if MessageDlg('Library ' + TmpStr + ' already exists! Overwrite?', mtWarning, [mbYes, mbCancel], 0) = mrCancel then
      exit;
  fDrawing.SaveBlockLibraryToFile(TmpStr);
end;

procedure TfrmLibraryBlocks.lboxBlocksSelectionChange(Sender: TObject; User: boolean);
begin
  if lboxBlocks.ItemIndex > -1 then
    if fDrawing.CADCmp2D.FindSourceBlock(lboxBlocks.Items[lboxBlocks.ItemIndex]) <> nil then
      PreviewBlock(lboxBlocks.ItemIndex);
end;

procedure TfrmLibraryBlocks.btnRenameBlockClick(Sender: TObject);
var TmpStr, TmpStr2: String; SrcBlk: TSourceBlock2D;   idx: integer;
begin
  if lboxBlocks.ItemIndex = -1 then Exit;
  TmpStr2 := lboxBlocks.Items[lboxBlocks.ItemIndex];
  if not InputQuery('Rename SourceBlock', 'New name', TmpStr2) then
    exit;
  idx :=  lboxBlocks.ItemIndex;
  TmpStr := lboxBlocks.Items[idx];
  SrcBlk := fDrawing.CADCmp2D.FindSourceBlock(StringToBlockName(TmpStr));
  if SrcBlk <> nil then
  begin
    //Change SourceBlockName in Library
    SrcBlk.Name := TmpStr2;
    lboxBlocks.Items[idx] := TmpStr2;
    fDrawing.SaveBlockLibraryToFile(fDrawing.CADCmp2D.CurrentBlockLibrary);
    //Change SourceBlock in Preview-Window
    PreviewBlock(lboxBlocks.ItemIndex);
  end;
  lboxBlocks.ItemIndex := idx;
end;

procedure TfrmLibraryBlocks.PreviewBlock(AIndex: integer);
var hSourceBlock, TmpSourceBlk: TSourceBlock2D;
begin
  CADCmp2D1.DeleteAllObjects;
  CADCmp2D1.DeleteSavedSourceBlocks;
  CADCmp2D1.DeleteLibrarySourceBlocks;

  hSourceBlock := fDrawing.CADCmp2D.FindSourceBlock(lboxBlocks.Items[AIndex]);
  TmpSourceBlk := TSourceBlock2D.Create(-1, hSourceBlock.Name, [nil]);
  TmpSourceBlk.Assign(hSourceBlock);
  CADCmp2D1.AddObject(-1, TmpSourceBlk);

  CADCmp2D1.Viewports[0].ZoomToExtension;
end;


end.

