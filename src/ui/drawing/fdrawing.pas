unit fDrawing;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, Dialogs, Graphics, ComCtrls,
  Menus, StdCtrls, Types,
  UndoRedo,
  applicationh,
  camh,
  CADSys4,
  CS4Shapes,
  CS4BaseTypes,
  CS4Tasks;

type



  { TDrawing }

   TDrawing    = class;
   TColorsInsp = class;
   TGridInsp   = class;
   TCmdsInsp   = class;
   TLayerInsp  = class;

  TComponentDrawing = class(TPersistent)
  private
    fColorsInsp: TColorsInsp;
    fGridInsp:   TGridInsp;
    fCmdsInsp:   TCmdsInsp;
    fLayerInsp: TLayerInsp;
    fDrawing: TDrawing;
    fEnableDragDrop: boolean;
    function  GetFileName: string;
    procedure SetFileName(AFileName: string);
    function  GetDrawingChanged: boolean;
    procedure SetDrawingChanged(AChanged: boolean);

    //Viewport
    function  GetShowRulerMarker: boolean;
    procedure SetShowRulerMarker(AValue: boolean);
    function  GetBackgroundColor: TColor;
    procedure SetBackgroundColor(AColor: TColor);
    function  GetShowGrid: boolean;
    function  GetShowGridMainAxes: boolean;
    procedure SetShowGridMainAxes(AValue: boolean);
    procedure SetShowGrid(AValue: boolean);
    function  GetGridColor: TColor;
    procedure SetGridColor(AColor: TColor);
    function  GetGridDeltaX: TRealType;
    procedure SetGridDeltaX(AValue: TRealType);
    function  GetGridDeltaY: TRealType;
    procedure SetGridDeltaY(AValue: TRealType);
    function  GetShowControlPoints: boolean;
    procedure SetShowControlPoints(AValue: boolean);
    function  GetPolarTracking: boolean;
    procedure SetPolarTracking(AValue: boolean);
    function  GetPolarTrackingValue: TRealType;
    procedure SetPolarTrackingValue(AValue: TRealType);

    //CADPrg
    function  GetShowCursorCross: boolean;
    procedure SetShowCursorCross(AValue: boolean);
    function  GetCursorCrossColor: TColor;
    procedure SetCursorCrossColor(AColor: TColor);
    function  GetRubberColor: TColor;
    procedure SetRubberColor(AColor: TColor);
    function  GetUseOrto: boolean;
    procedure SetUseOrto(AValue: boolean);
    function  GetUseSnap: boolean;
    procedure SetUseSnap(AValue: boolean);
    function  GetXSnap: TRealType;
    procedure SetXSnap(AValue: TRealType);
    function  GetYSnap: TRealType;
    procedure SetYSnap(AValue: TRealType);
    //CADCmp
    function  GetDefaultLayersColor: TColor;
    procedure SetDefaultLayersColor(AColor: TColor);
    function  GetCurrentBlockLibrary: string;
    procedure SetCurrentBlockLibrary(AValue: string);
    function  GetShowDirection: boolean;
    procedure SetShowDirection(AValue: boolean);

    function   GetActiveLayerName: TLayerName;
    procedure  SetActiveLayerName(ALayerName: TLayerName);
    function   GetActiveLayerIDX: Word;
    procedure  SetActiveLayerIDX(AValue: Word);
    function   GetActiveLayerPenColor: TColor;
    procedure  SetActiveLayerPenColor(AColor: TColor);
    function   GetActiveLayerPenStyle: TPenStyle;
    procedure  SetActiveLayerPenStyle(APenStyle: TPenStyle);
    function   GetActiveLayerPenWidth: word;
    procedure  SetActiveLayerPenWidth(AValue: word);
    function   GetActiveLayerBrushColor: TColor;
    procedure  SetActiveLayerBrushColor(AColor: TColor);
    function   GetActiveLayerBrushStyle: TBrushStyle;
    procedure  SetActiveLayerBrushStyle(ABrushStyle: TBrushStyle);
    function   GetActiveLayerVisible: boolean;
    procedure  SetActiveLayerVisible(AValue: boolean);
    function   GetActiveLayerTransparent: boolean;
    procedure  SetActiveLayerTransparent(AValue: boolean);
    function   GetActiveLayerStreamable:  boolean;
    procedure  SetActiveLayerStreamable(AValue: boolean);
  public
    constructor create;
    destructor  destroy; override;

    property Drawing: TDrawing read fDrawing          write fDrawing;
    property Changed: boolean  read GetDrawingChanged write SetDrawingChanged;

    property EnableDragDrop: boolean read fEnableDragDrop write fEnableDragDrop;
    //CADViewport
    property  BackgroundColor: TColor    read  GetBackgroundColor write SetBackgroundColor;
    property  ShowGrid:   boolean        read  GetShowGrid   write SetShowGrid;
    property  ShowGridMainAxes: boolean  read  GetShowGridMainAxes write SetShowGridMainAxes;
    property  GridColor:  TColor         read  GetGridColor  write SetGridColor;
    property  RubberColor: TColor        read  GetRubberColor  write SetRubberColor;
    property  GridDeltaX: TRealType      read  GetGridDeltaX write SetGridDeltaX;
    property  GridDeltaY: TRealType      read  GetGridDeltaY write SetGridDeltaY;
    property  ShowControlPoints: boolean read  GetShowControlPoints write SetShowControlPoints;
    //CADPrg
    property ShowCrossCursor: boolean    read  GetShowCursorCross  write SetShowCursorCross;
    property CrossCursorColor: TColor    read  GetCursorCrossColor write SetCursorCrossColor;
    property UseOrto: boolean            read  GetUseOrto          write SetUseOrto;
    property UseSnap: boolean            read  GetUseSnap          write SetUseSnap;
    property XSnap: TRealType            read  GetXSnap            write SetXSnap;
    property YSnap: TRealType            read  GetYSnap            write SetYSnap;
    //CADCmp
    property ShowDirection: boolean        read GetShowDirection      write SetShowDirection;
    property PolarTracking: boolean        read GetPolarTracking      write SetPolarTracking;
    property PolarTrackingValue: TRealType read GetPolarTrackingValue write SetPolarTrackingValue;
    property DefaultLayersColor: TColor    read GetDefaultLayersColor write SetDefaultLayersColor;

    property ActiveLayerName:        TLayerName   read GetActiveLayerName        write SetActiveLayerName;
    property ActiveLayerIndex:       Word         read GetActiveLayerIDX         write SetActiveLayerIDX;
    property ActiveLayerPenColor:    TColor       read GetActiveLayerPenColor    write SetActiveLayerPenColor;
    property ActiveLayerPenStyle:    TPenStyle    read GetActiveLayerPenStyle    write SetActiveLayerPenStyle;
    Property ActiveLayerPenWidth:    word         read GetActiveLayerPenWidth    write SetActiveLayerPenWidth;
    property ActiveLayerBrushColor:  TColor       read GetActiveLayerBrushColor  write SetActiveLayerBrushColor;
    property ActiveLayerBrushStyle:  TBrushStyle  read GetActiveLayerBrushStyle  write SetActiveLayerBrushStyle;
    property ActiveLayerVisible:     boolean      read GetActiveLayerVisible     write SetActiveLayerVisible;
    property ActiveLayerTransparent: boolean      read GetActiveLayerTransparent write SetActiveLayerTransparent;
    property ActiveLayerStreamable:  boolean      read GetActiveLayerStreamable  write SetActiveLayerStreamable;
    property ShowRulerMarker:        boolean      read GetShowRulerMarker        write SetShowRulerMarker;

  published
    property FileName: string             read GetFileName; // write SetFileName;
    property Colors:      TColorsInsp     read fColorsInsp    write fColorsInsp;
    property Grid:        TGridInsp       read fGridInsp      write fGridInsp;
    property Commands:    TCmdsInsp       read fCmdsInsp      write fCmdsInsp;
    property ActiveLayer: TLayerInsp      read fLayerInsp     write fLayerInsp;

  end;

  TColorsInsp = class
  private
    fOwner: TComponentDrawing;
    function  GetBackgroundColor: TColor;
    procedure SetBackgroundColor(AColor: TColor);
    function  GetGridColor: TColor;
    procedure SetGridColor(AColor: TColor);
    function  GetCursorCrossColor: TColor;
    procedure SetCursorCrossColor(AColor: TColor);
    function  GetRubberColor: TColor;
    procedure SetRubberColor(AColor: TColor);
    function  GetDefaultLayersColor: TColor;
    procedure SetDefaultLayersColor(AColor: TColor);
  public
    constructor create(AOwner: TComponentDrawing);
    destructor  destroy; override;
  published
    property  BackgroundColor: TColor    read  GetBackgroundColor    write SetBackgroundColor;
    property  GridColor:  TColor         read  GetGridColor          write SetGridColor;
    property  RubberColor: TColor        read  GetRubberColor        write SetRubberColor;
    property  CrossCursorColor: TColor   read  GetCursorCrossColor   write SetCursorCrossColor;
    //property  DefaultLayersColor: TColor read  GetDefaultLayersColor write SetDefaultLayersColor;
  end;

  TGridInsp = class
  private
    fOwner: TComponentDrawing;
    function  GetShowGrid: boolean;
    function  GetShowGridMainAxes: boolean;
    procedure SetShowGridMainAxes(AValue: boolean);
    procedure SetShowGrid(AValue: boolean);
    function  GetGridColor: TColor;
    procedure SetGridColor(AColor: TColor);
    function  GetGridDeltaX: TRealType;
    procedure SetGridDeltaX(AValue: TRealType);
    function  GetGridDeltaY: TRealType;
    procedure SetGridDeltaY(AValue: TRealType);
  public
    constructor create(AOwner: TComponentDrawing);
    destructor  destroy; override;
  published
    property  ShowGrid:   boolean        read  GetShowGrid   write SetShowGrid;
    property  ShowGridMainAxes: boolean  read  GetShowGridMainAxes write SetShowGridMainAxes;
    property  GridColor:  TColor         read  GetGridColor  write SetGridColor;
    property  GridDeltaX: TRealType      read  GetGridDeltaX write SetGridDeltaX;
    property  GridDeltaY: TRealType      read  GetGridDeltaY write SetGridDeltaY;
  end;

  TCmdsInsp = class
  private
    fOwner:  TComponentDrawing;
    function  GetUseOrto: boolean;
    procedure SetUseOrto(AValue: boolean);
    function  GetUseSnap: boolean;
    procedure SetUseSnap(AValue: boolean);
    function  GetXSnap: TRealType;
    procedure SetXSnap(AValue: TRealType);
    function  GetYSnap: TRealType;
    procedure SetYSnap(AValue: TRealType);
    function  GetShowControlPoints: boolean;
    procedure SetShowControlPoints(AValue: boolean);
    function  GetShowDirection: boolean;
    procedure SetShowDirection(AValue: boolean);
    function  GetPolarTracking: boolean;
    procedure SetPolarTracking(AValue: boolean);
    function  GetPolarTrackingValue: TRealType;
    procedure SetPolarTrackingValue(AValue: TRealType);
    function  GetEnableDragDrop: boolean;
    procedure SetEnableDragDrop(AValue: boolean);
    function  GetShowRulerMarker: boolean;
    procedure SetShowRulerMarker(AValue: boolean);
  public
    constructor create(AOwner: TComponentDrawing);
    destructor  destroy; override;
  published
    property UseOrto: boolean              read  GetUseOrto             write SetUseOrto;
    property UseSnap: boolean              read  GetUseSnap             write SetUseSnap;
    property XSnap: TRealType              read  GetXSnap               write SetXSnap;
    property YSnap: TRealType              read  GetYSnap               write SetYSnap;
    property ShowDirection: boolean        read  GetShowDirection       write SetShowDirection;
    property PolarTracking: boolean        read  GetPolarTracking       write SetPolarTracking;
    property PolarTrackingValue: TRealType read  GetPolarTrackingValue  write SetPolarTrackingValue;
    property EnableDragDrop: boolean       read  GetEnableDragDrop      write SetEnableDragDrop;
    property ShowControlPoints:  boolean   read  GetShowControlPoints   write SetShowControlPoints;
    property ShowRulerMarker:    boolean   read  GetShowRulerMarker     write SetShowRulerMarker;
  end;

  TLayerInsp = class
    fOwner: TComponentDrawing;
    function   GetLayerName: TLayerName;
    procedure  SetLayerName(ALayerName: TLayerName);
    function   GetLayerIDX: Word;
    procedure  SetLayerIDX(AValue: Word);
    function   GetLayerPenColor: TColor;
    procedure  SetLayerPenColor(AColor: TColor);
    function   GetLayerPenStyle: TPenStyle;
    procedure  SetLayerPenStyle(APenStyle: TPenStyle);
    function   GetLayerPenWidth: word;
    procedure  SetLayerPenWidth(AValue: word);
    function   GetLayerBrushColor: TColor;
    procedure  SetLayerBrushColor(AColor: TColor);
    function   GetLayerBrushStyle: TBrushStyle;
    procedure  SetLayerBrushStyle(ABrushStyle: TBrushStyle);
    function   GetLayerVisible: boolean;
    procedure  SetLayerVisible(AValue: boolean);
    function   GetLayerTransparent: boolean;
    procedure  SetLayerTransparent(AValue: boolean);
    function   GetLayerStreamable:  boolean;
    procedure  SetLayerStreamable(AValue: boolean);
  public
    constructor create(AOwner: TComponentDrawing);
    destructor  destroy; override;
  published
    property Name:        TLayerName   read GetLayerName        write SetLayerName;
    property Index:       Word         read GetLayerIDX         write SetLayerIDX;
    property PenColor:    TColor       read GetLayerPenColor    write SetLayerPenColor;
    property PenStyle:    TPenStyle    read GetLayerPenStyle    write SetLayerPenStyle;
    Property PenWidth:    word         read GetLayerPenWidth    write SetLayerPenWidth;
    property BrushColor:  TColor       read GetLayerBrushColor  write SetLayerBrushColor;
    property BrushStyle:  TBrushStyle  read GetLayerBrushStyle  write SetLayerBrushStyle;
    property Visible:     boolean      read GetLayerVisible     write SetLayerVisible;
    property Transparent: boolean      read GetLayerTransparent write SetLayerTransparent;
    property Streamable:  boolean      read GetLayerStreamable  write SetLayerStreamable;
  end;

  TDrawing = class(TFrame)
    CADCmp2D: TCADCmp2D;
    CADPrg2D: TCADPrg2D;
    CADViewport2D: TCADViewport2D;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    mnuInverse: TMenuItem;
    mnuModifyReverse: TMenuItem;
    pnlLeft: TPanel;
    popupModify: TPopupMenu;
    RulerLeft: TRuler;
    Separator2: TMenuItem;
    pmnuSetPoint: TMenuItem;
    pmnuCancel: TMenuItem;
    pmnuAccept: TMenuItem;
    PageControl1: TPageControl;
    pnlBottom: TPanel;
    pnlCorner: TPanel;
    popupDrawing: TPopupMenu;
    popupSpaces: TPopupMenu;
    RulerBottom: TRuler;
    Separator1: TMenuItem;
    tsModel: TTabSheet;
    tsPaper: TTabSheet;
    procedure CADCmp2DAddObject(Sender: TObject; Obj: TGraphicObject);
    procedure CADCmp2DLoadProgress(Sender: TObject; ReadPercent: Byte);
    procedure CADPrg2DDescriptionChanged(Sender: TObject);
    procedure CADPrg2DEndOperation(Sender: TObject;
      const Operation: TCADStateClass; const Param: TCADPrgParam);
    procedure CADPrg2DExitState(Sender: TObject; const State: TCADState);
    procedure CADPrg2DIdle(Sender: TObject);
    procedure CADPrg2DMouseMoveFilter(Sender: TCADPrg2D;
      CurrentState: TCADState; var WPt: TPoint2D; X, Y: Integer);
    procedure CADPrg2DSnapFilter(Sender: TCADPrg2D; CurrentState: TCADState;
      const LastPt: TPoint2D; var CurrSnappedPt: TPoint2D);
    procedure CADPrg2DStartOperation(Sender: TObject;
      const Operation: TCADStateClass; const Param: TCADPrgParam);
    procedure CADPrg2DStopOperation(Sender: TObject;
      const Operation: TCADStateClass; const Param: TCADPrgParam);
    procedure CADViewport2DEndRedraw(Sender: TObject);
    procedure CADViewport2DMouseDown2D(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; WX, WY: TRealType; X, Y: Integer);
    procedure CADViewport2DMouseMove2D(Sender: TObject; Shift: TShiftState; WX,
      WY: TRealType; X, Y: Integer);
    procedure CADViewport2DMouseUp2D(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; WX, WY: TRealType; X, Y: Integer);
    procedure CADViewport2DPaint(Sender: TObject);
    procedure mnuModifyReverseClick(Sender: TObject);
    procedure mnuInverseClick(Sender: TObject);
    procedure pmnuAcceptClick(Sender: TObject);
    procedure pmnuCancelClick(Sender: TObject);
    procedure pmnuSetPointClick(Sender: TObject);
    procedure popupDrawingPopup(Sender: TObject);
  private
    fFileName: string;
    fChanged: boolean;
    fExistTemplateLayer: boolean;

    fSnapOption: word;
    fSnapProfilePointsNr: integer;

    procedure SetPoint;

  public
    UndoRedo : TUndoRedo; //StellaSoft

    procedure UndoSaveProcedure(var MemSt:TMemoryStream);
    procedure UndoRedoProcedure(MemSt:TMemoryStream);
    procedure UndoRedoChangeEvent(Sender:TObject; Undo,Redo:boolean);

    constructor Create(TheOwner: TComponent); override;
    destructor  Destroy; override;

    procedure   SaveToFile(AFileName: string);
    procedure   LoadFromFile(AFileName: string);

    function    LoadBlockLibraryFromFile(AFileName: string): boolean;
    function    SaveBlockLibraryToFile(AFileName: string): boolean;

    function    GetSnapPoint(ASnapOption: word; AObj: TObject2D; AAperture: word): TPoint2D;

    property    Changed: boolean  read  fChanged write fChanged;
    property    FileName: string  read  fFileName write fFileName;

    property    SnapOption: word read fSnapOption write fSnapOption;
  published
  end;

implementation

uses fMain;

{$R *.lfm}

constructor TComponentDrawing.create;
begin
  inherited;
  fColorsInsp := TColorsInsp.create(self);
  fGridInsp   := TGridInsp.Create(self);
  fCmdsInsp   := TCmdsInsp.create(self);
  fLayerInsp  := TLayerInsp.create(self);
end;

destructor  TComponentDrawing.destroy;
begin
  fColorsInsp.Free;
  fGridInsp.Free;
  fCmdsInsp.Free;
  fLayerInsp.Free;
  inherited;
end;

function TComponentDrawing.GetFileName: string;
begin
  result := fDrawing.FileName;
end;

procedure TComponentDrawing.SetFileName(AFileName: string);
begin
  if (AFileName <> fDrawing.FileName) then
  begin
    fDrawing.FileName := AFileName;
    //frmMain.ActivePage.Caption := AFileName;
    fDrawing.Changed := true;
  end;
end;

function  TComponentDrawing.GetDrawingChanged: boolean;
begin
  result := fDrawing.Changed;
end;

procedure TComponentDrawing.SetDrawingChanged(AChanged: boolean);
begin
  fDrawing.Changed := AChanged;
end;

function  TComponentDrawing.GetGridColor: TColor;
begin
  result := fDrawing.CADCmp2D.GridColor;
end;

procedure TComponentDrawing.SetGridColor(AColor: TColor);
begin
  fDrawing.CADViewport2D.GridColor := AColor; // xor BackGroundColor;
  fDrawing.CADCmp2D.GridColor := AColor;
  fDrawing.CADViewport2D.Repaint;
  fDrawing.Changed := true;
end;

function  TComponentDrawing.GetRubberColor: TColor;
begin
   result := fDrawing.CADViewport2D.RubberPen.Color;
end;

procedure TComponentDrawing.SetRubberColor(AColor: TColor);
begin
  fDrawing.CADViewport2D.RubberPen.Color := AColor; // xor fDrawing.CADCmp2D.BackgroundColor;
end;

function  TComponentDrawing.GetGridDeltaX: TRealType;
begin
  result := fDrawing.CADCmp2D.GridDeltaX;
end;

procedure TComponentDrawing.SetGridDeltaX(AValue: TRealType);
begin
  fDrawing.CADViewport2D.GridDeltaX := AValue;
  fDrawing.CADCmp2D.GridDeltaX      := AValue;
  fDrawing.Changed := true;
end;

function  TComponentDrawing.GetGridDeltaY: TRealType;
begin
   result := fDrawing.CADCmp2D.GridDeltaY;
end;

procedure TComponentDrawing.SetGridDeltaY(AValue: TRealType);
begin
  fDrawing.CADViewport2D.GridDeltaY := AValue;
  fDrawing.CADCmp2D.GridDeltaY      := AValue;
  fDrawing.Changed := true;
end;

function  TComponentDrawing.GetBackgroundColor: TColor;
begin
  result := fDrawing.CADCmp2D.BackgroundColor;
end;

procedure TComponentDrawing.SetBackgroundColor(AColor: TColor);
begin
  fDrawing.CADCmp2D.BackGroundColor      := AColor;
  fDrawing.CADViewport2D.BackGroundColor := AColor;
  fDrawing.Changed := true;
end;

function  TComponentDrawing.GetShowRulerMarker: boolean;
begin
  result := applicationh.fShowRulerMarker;
end;

procedure TComponentDrawing.SetShowRulerMarker(AValue: boolean);
begin
  applicationh.fShowRulerMarker := AValue;
  self.fDrawing.RulerLeft.ShowMarker := AValue;
  self.fDrawing.RulerBottom.ShowMarker := AValue;

  if applicationh.fShowRulerMarker then
    fIniFile.WriteString('Application',   'ShowRulerMarker', 'yes')
  else
    applicationh.fIniFile.WriteString('Application',   'ShowRulerMarker', 'no');
end;

function  TComponentDrawing.GetShowGrid: boolean;
begin
  result := fDrawing.CADCmp2D.ShowGrid;
end;

procedure TComponentDrawing.SetShowGrid(AValue: boolean);
begin
  fDrawing.CADViewport2D.ShowGrid := AValue;
  fDrawing.CADViewport2D.ShowGridMainAxes := AValue;
  fDrawing.CADCmp2D.ShowGrid := AValue;
  frmMain.acCMDShowGrid.Checked := AValue;   //
  fDrawing.CADViewport2D.Repaint;
  fDrawing.Changed := true;
end;

function  TComponentDrawing.GetUseOrto: boolean;
begin
  result := fDrawing.CADCmp2D.UseOrtho;
end;

procedure TComponentDrawing.SetUseOrto(AValue: boolean);
begin
  fDrawing.CADPrg2D.UseOrto     := AValue;
  fDrawing.CADCmp2D.UseOrtho    := AValue;
  frmMain.acCMDUseOrtho.Checked := AValue;   //
  fDrawing.Changed := true;
end;

function  TComponentDrawing.GetUseSnap: boolean;
begin
  result := fDrawing.CADCmp2D.UseSnap;
end;

procedure TComponentDrawing.SetUseSnap(AValue: boolean);
begin
  fDrawing.CADPrg2D.UseSnap    := AValue;
  fDrawing.CADCmp2D.UseSnap    := AValue;
  frmMain.acCMDUseSnap.Checked := AValue;
  fDrawing.Changed := true;
end;

function  TComponentDrawing.GetShowGridMainAxes: boolean;
begin
  result := fDrawing.CADViewport2D.ShowGridMainAxes;
end;

procedure TComponentDrawing.SetShowGridMainAxes(AValue: boolean);
begin
  fDrawing.CADViewport2D.ShowGridMainAxes := AValue;
  fDrawing.Changed := true;
  //frmMain.TIPropertyGrid1.Update;
  //frmMain.TIPropertyGrid1.Repaint;
end;

function  TComponentDrawing.GetShowControlPoints: boolean;
begin
  result := fDrawing.CADViewport2D.ShowControlPoints;
end;

procedure TComponentDrawing.SetShowControlPoints(AValue: boolean);
begin
  fDrawing.CADViewport2D.ShowControlPoints := AValue;
  fDrawing.Changed := true;
end;

function  TComponentDrawing.GetShowCursorCross: boolean;
begin
  result := fDrawing.CADPrg2D.ShowCursorCross;
end;

procedure TComponentDrawing.SetShowCursorCross(AValue: boolean);
begin
  fDrawing.CADPrg2D.ShowCursorCross := AValue;
  fDrawing.Changed := true;
end;

function  TComponentDrawing.GetCursorCrossColor: TColor;
begin
  result := fDrawing.CADPrg2D.CursorColor;
end;

procedure TComponentDrawing.SetCursorCrossColor(AColor: TColor);
begin
  fDrawing.CADPrg2D.CursorColor := AColor xor fDrawing.CADViewport2D.BackGroundColor;
  fDrawing.Changed := true;
end;

function  TComponentDrawing.GetXSnap: TRealType;
begin
  result := fDrawing.CADCmp2D.SnapX;
end;

procedure TComponentDrawing.SetXSnap(AValue: TRealType);
begin
  fDrawing.CADPrg2D.XSnap := AValue;
  fDrawing.CADCmp2D.SnapX := AValue;
  fDrawing.Changed := true;
end;

function  TComponentDrawing.GetYSnap: TRealType;
begin
  result := fDrawing.CADCmp2D.SnapY;
end;

procedure TComponentDrawing.SetYSnap(AValue: TRealType);
begin
  fDrawing.CADPrg2D.YSnap := AValue;
  fDrawing.CADCmp2D.SnapY := AValue;
  fDrawing.Changed := true;
end;

function  TComponentDrawing.GetPolarTracking: boolean;
begin
  result := fDrawing.CADCmp2D.PolarTracking;
end;

procedure TComponentDrawing.SetPolarTracking(AValue: boolean);
begin
  fDrawing.CADCmp2D.PolarTracking := AValue;
  frmMain.acCMDPolarTracking.Checked := AValue;
  fDrawing.Changed := true;
end;

function  TComponentDrawing.GetPolarTrackingValue: TRealType;
begin
  result := fDrawing.CADCmp2D.PolarTrackingValue;
end;

procedure TComponentDrawing.SetPolarTrackingValue(AValue: TRealType);
begin
  fDrawing.CADCmp2D.PolarTrackingValue := AValue;
end;

function  TComponentDrawing.GetDefaultLayersColor: TColor;
begin
  result := fDrawing.CADCmp2D.DefaultLayersColor;
end;

procedure TComponentDrawing.SetDefaultLayersColor(AColor: TColor);
begin
  fDrawing.CADCmp2D.DefaultLayersColor := AColor;
  fDrawing.Changed := true;
end;

function  TComponentDrawing.GetCurrentBlockLibrary: string;
begin
  result := applicationh.fDefaultBlockLibrary;
end;

procedure TComponentDrawing.SetCurrentBlockLibrary(AValue: string);
begin
  applicationh.fDefaultBlockLibrary := AValue;
  fDrawing.LoadBlockLibraryFromFile(AValue);
end;

function  TComponentDrawing.GetShowDirection: boolean;
begin
  result := fDrawing.CADCmp2D.ShowDirection;
end;

procedure TComponentDrawing.SetShowDirection(AValue: boolean);
var TmpIter: TExclusiveGraphicObjIterator;
begin
  fDrawing.CADCmp2D.ShowDirection := AValue;
  TmpIter := fDrawing.CADCmp2D.ObjectsExclusiveIterator;
  try
    TmpIter.First;
    while TmpIter.Current <> nil do
    begin
      if (TmpIter.Current.LayerName <> CAM_LAYER_STR_JUMPS) and (TmpIter.Current.LayerName <> LAYER_STR_TEMPLATE) then
      begin
        if  (TmpIter.Current is TSimplePrimitive2D) then
        begin
          TSimplePrimitive2D(TmpIter.Current).ShowDirection := AValue;
          fDrawing.CADCmp2D.RedrawObject(TSimplePrimitive2D(TmpIter.Current));
        end;
      end;
      TmpIter.Next;
    end;
  finally
    TmpIter.Free;
  end;
end;

function   TComponentDrawing.GetActiveLayerName: TLayerName;
begin
  result := fDrawing.CADCmp2D.Layers[fDrawing.CADCmp2D.CurrentLayer].Name;
end;

procedure  TComponentDrawing.SetActiveLayerName(ALayerName: TLayerName);
var TmpLayer: TLayer;
begin
  TmpLayer := fDrawing.CADCmp2D.Layers.LayerByName[ALayerName];
  if   TmpLayer <> nil then
    fDrawing.CADCmp2D.CurrentLayer := TmpLayer.LayerIndex
  else
    fDrawing.CADCmp2D.CurrentLayer := 0;
end;

function   TComponentDrawing.GetActiveLayerIDX: Word;
begin
  result := fDrawing.CADCmp2D.CurrentLayer;
end;

procedure  TComponentDrawing.SetActiveLayerIDX(AValue: Word);
begin
  fDrawing.CADCmp2D.CurrentLayer := AValue;
end;

function   TComponentDrawing.GetActiveLayerPenColor: TColor;
begin
  result := fDrawing.CADCmp2D.Layers[fDrawing.CADCmp2D.CurrentLayer].Pen.Color;
end;

procedure  TComponentDrawing.SetActiveLayerPenColor(AColor: TColor);
begin
  fDrawing.CADCmp2D.Layers[fDrawing.CADCmp2D.CurrentLayer].Pen.Color := AColor;
end;

function   TComponentDrawing.GetActiveLayerPenStyle: TPenStyle;
begin
  result := fDrawing.CADCmp2D.Layers[fDrawing.CADCmp2D.CurrentLayer].Pen.Style;
end;

procedure  TComponentDrawing.SetActiveLayerPenStyle(APenStyle: TPenStyle);
begin
  fDrawing.CADCmp2D.Layers[fDrawing.CADCmp2D.CurrentLayer].Pen.Style := APenStyle;
end;

function   TComponentDrawing.GetActiveLayerPenWidth: word;
begin
  result := fDrawing.CADCmp2D.Layers[fDrawing.CADCmp2D.CurrentLayer].Pen.Width;
end;

procedure  TComponentDrawing.SetActiveLayerPenWidth(AValue: word);
begin
  fDrawing.CADCmp2D.Layers[fDrawing.CADCmp2D.CurrentLayer].Pen.Width := AValue;
end;

function   TComponentDrawing.GetActiveLayerBrushColor: TColor;
begin
  result := fDrawing.CADCmp2D.Layers[fDrawing.CADCmp2D.CurrentLayer].Brush.Color;
end;

procedure  TComponentDrawing.SetActiveLayerBrushColor(AColor: TColor);
begin
  fDrawing.CADCmp2D.Layers[fDrawing.CADCmp2D.CurrentLayer].Brush.Color := AColor;
end;

function   TComponentDrawing.GetActiveLayerBrushStyle: TBrushStyle;
begin
  result := fDrawing.CADCmp2D.Layers[fDrawing.CADCmp2D.CurrentLayer].Brush.Style;
end;

procedure  TComponentDrawing.SetActiveLayerBrushStyle(ABrushStyle: TBrushStyle);
begin
  fDrawing.CADCmp2D.Layers[fDrawing.CADCmp2D.CurrentLayer].Brush.Style := ABrushStyle;
end;

function   TComponentDrawing.GetActiveLayerVisible: boolean;
begin
  result := fDrawing.CADCmp2D.Layers[fDrawing.CADCmp2D.CurrentLayer].Visible;
end;

procedure  TComponentDrawing.SetActiveLayerVisible(AValue: boolean);
begin
  fDrawing.CADCmp2D.Layers[fDrawing.CADCmp2D.CurrentLayer].Visible := AValue;
end;

function   TComponentDrawing.GetActiveLayerTransparent: boolean;
begin
  result := not fDrawing.CADCmp2D.Layers[fDrawing.CADCmp2D.CurrentLayer].Opaque;
end;

procedure  TComponentDrawing.SetActiveLayerTransparent(AValue: boolean);
begin
  fDrawing.CADCmp2D.Layers[fDrawing.CADCmp2D.CurrentLayer].Opaque := (not AValue);
end;

function   TComponentDrawing.GetActiveLayerStreamable:  boolean;
begin
  result := fDrawing.CADCmp2D.Layers[fDrawing.CADCmp2D.CurrentLayer].Streamable;
end;

procedure  TComponentDrawing.SetActiveLayerStreamable(AValue: boolean);
begin
  fDrawing.CADCmp2D.Layers[fDrawing.CADCmp2D.CurrentLayer].Streamable := AValue;
end;

///////////////////////////////////////////////////////////////////////////////
//TColorsIns //////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
constructor TColorsInsp.create(Aowner: TComponentDrawing);
begin
  fOwner := AOwner;
end;

destructor TColorsInsp.destroy;
begin
  inherited;
end;

function  TColorsInsp.GetBackgroundColor: TColor;
begin
  result := fOwner.BackgroundColor;
end;

procedure TColorsInsp.SetBackgroundColor(AColor: TColor);
begin
  fOwner.BackgroundColor := AColor;
end;

function  TColorsInsp.GetGridColor: TColor;
begin
  result := fOwner.GridColor;
end;

procedure TColorsInsp.SetGridColor(AColor: TColor);
begin
  fOwner.GridColor := AColor;
end;

function  TColorsInsp.GetCursorCrossColor: TColor;
begin
  result := fOwner.CrossCursorColor;
end;

procedure TColorsInsp.SetCursorCrossColor(AColor: TColor);
begin
  fOwner.CrossCursorColor := AColor;
end;

function  TColorsInsp.GetRubberColor: TColor;
begin
  result := fOwner.RubberColor;
end;

procedure TColorsInsp.SetRubberColor(AColor: TColor);
begin
  fOwner.RubberColor := AColor;
end;

function  TColorsInsp.GetDefaultLayersColor: TColor;
begin
  result := fOwner.DefaultLayersColor;
end;

procedure TColorsInsp.SetDefaultLayersColor(AColor: TColor);
begin
  fOwner.DefaultLayersColor := AColor;
end;

//TGridInsp/////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
constructor TGridInsp.create(AOwner: TComponentDrawing);
begin
  fOwner := AOwner;
end;

destructor  TGridInsp.destroy;
begin
  inherited;
end;

function  TGridInsp.GetShowGrid: boolean;
begin
  result := fOwner.ShowGrid;
end;

procedure TGridInsp.SetShowGrid(AValue: boolean);
begin
  fOwner.ShowGrid := AValue;
end;

procedure TGridInsp.SetShowGridMainAxes(AValue: boolean);
begin
  fOwner.ShowGrid := AValue;
end;

function  TGridInsp.GetShowGridMainAxes: boolean;
begin
  result := fOwner.ShowGridMainAxes;
end;

function  TGridInsp.GetGridColor: TColor;
begin
  result := fOwner.GridColor;
end;

procedure TGridInsp.SetGridColor(AColor: TColor);
begin
  fOwner.GridColor := AColor;
end;

function  TGridInsp.GetGridDeltaX: TRealType;
begin
  result := fOwner.GridDeltaX;
end;

procedure TGridInsp.SetGridDeltaX(AValue: TRealType);
begin
  fOwner.GridDeltaX := AValue;
end;

function  TGridInsp.GetGridDeltaY: TRealType;
begin
  result := fOwner.GridDeltaY;
end;

procedure TGridInsp.SetGridDeltaY(AValue: TRealType);
begin
  fOwner.GridDeltaY := AValue;
end;

////////////////////////////////////////////////////////////////////////////////
//TCmdsInsp = class/////////////////////////////////////////////////////////////
constructor TCmdsInsp.create(AOwner: TComponentDrawing);
begin
  fOwner := AOwner;
end;

destructor  TCmdsInsp.destroy;
begin
  inherited;
end;

function  TCmdsInsp.GetUseOrto: boolean;
begin
  result := fOwner.UseOrto;
end;

procedure TCmdsInsp.SetUseOrto(AValue: boolean);
begin
  fOwner.UseOrto := AValue;
end;

function  TCmdsInsp.GetUseSnap: boolean;
begin
  result := fOwner.UseSnap;
end;

procedure TCmdsInsp.SetUseSnap(AValue: boolean);
begin
  fOwner.UseSnap := AValue;
end;

function  TCmdsInsp.GetXSnap: TRealType;
begin
  result := fOwner.XSnap;
end;

procedure TCmdsInsp.SetXSnap(AValue: TRealType);
begin
  fOwner.XSnap := AValue;
end;

function  TCmdsInsp.GetYSnap: TRealType;
begin
  result := fOwner.YSnap;
end;

procedure TCmdsInsp.SetYSnap(AValue: TRealType);
begin
  fOwner.YSnap := AValue;
end;

function  TCmdsInsp.GetShowDirection: boolean;
begin
  result := fOwner.ShowDirection;
end;

procedure TCmdsInsp.SetShowDirection(AValue: boolean);
begin
  fOwner.ShowDirection := AValue;
end;

function  TCmdsInsp.GetPolarTracking: boolean;
begin
  result := fOwner.PolarTracking;
end;

procedure TCmdsInsp.SetPolarTracking(AValue: boolean);
begin
  fOwner.PolarTracking := AValue;
end;

function  TCmdsInsp.GetPolarTrackingValue: TRealType;
begin
  result := fOwner.PolarTrackingValue;
end;

procedure TCmdsInsp.SetPolarTrackingValue(AValue: TRealType);
begin
  fOwner.PolarTrackingValue := AValue;
end;

function  TCmdsInsp.GetShowControlPoints: boolean;
begin
  result := fOwner.ShowControlPoints;
end;

procedure TCmdsInsp.SetShowControlPoints(AValue: boolean);
begin
  fOwner.ShowControlPoints := AValue;
end;

function  TCmdsInsp.GetEnableDragDrop: boolean;
begin
  result := fOwner.EnableDragDrop;
end;

procedure TCmdsInsp.SetEnableDragDrop(AValue: boolean);
begin
  fOwner.EnableDragDrop := AValue;
end;

function  TCmdsInsp.GetShowRulerMarker: boolean;
begin
  result := fOwner.ShowRulerMarker;
end;

procedure TCmdsInsp.SetShowRulerMarker(AValue: boolean);
begin
  fOwner.ShowRulerMarker := AValue;
end;

////////////////////////////////////////////////////////////////////////////////
//TLayerInsp////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
constructor TLayerInsp.create(AOwner: TComponentDrawing);
begin
  fOwner :=  AOwner;
end;

destructor TLayerInsp.destroy;
begin
  inherited;
end;

function   TLayerInsp.GetLayerName: TLayerName;
begin
  result := fOwner.ActiveLayerName;
end;

procedure  TLayerInsp.SetLayerName(ALayerName: TLayerName);
begin
  fOwner.ActiveLayerName := ALayerName;
end;

function   TLayerInsp.GetLayerIDX: Word;
begin
  result := fOwner.ActiveLayerIndex;
end;

procedure  TLayerInsp.SetLayerIDX(AValue: Word);
begin
  fOwner.ActiveLayerIndex := AValue;
end;

function   TLayerInsp.GetLayerPenColor: TColor;
begin
  result := fOwner.ActiveLayerPenColor;
end;

procedure  TLayerInsp.SetLayerPenColor(AColor: TColor);
begin
  fOwner.ActiveLayerPenColor := AColor;
end;

function   TLayerInsp.GetLayerPenStyle: TPenStyle;
begin
  result := fOwner.ActiveLayerPenStyle;
end;

procedure  TLayerInsp.SetLayerPenStyle(APenStyle: TPenStyle);
begin
  fOwner.ActiveLayerPenStyle := APenStyle;
end;

function   TLayerInsp.GetLayerPenWidth: word;
begin
  result := fOwner.ActiveLayerPenWidth;
end;

procedure  TLayerInsp.SetLayerPenWidth(AValue: word);
begin
  fOwner.ActiveLayerPenWidth := AValue;
end;

function   TLayerInsp.GetLayerBrushColor: TColor;
begin
  result := fOwner.ActiveLayerBrushColor;
end;

procedure  TLayerInsp.SetLayerBrushColor(AColor: TColor);
begin
  fOwner.ActiveLayerBrushColor := AColor;
end;

function   TLayerInsp.GetLayerBrushStyle: TBrushStyle;
begin
  result := fOwner.ActiveLayerBrushStyle;
end;

procedure  TLayerInsp.SetLayerBrushStyle(ABrushStyle: TBrushStyle);
begin
  fOwner.ActiveLayerBrushStyle := ABrushStyle;
end;

function   TLayerInsp.GetLayerVisible: boolean;
begin
  result := fOwner.ActiveLayerVisible;
end;

procedure  TLayerInsp.SetLayerVisible(AValue: boolean);
begin
  fOwner.ActiveLayerVisible := AValue;
end;

function   TLayerInsp.GetLayerTransparent: boolean;
begin
  result := fOwner.ActiveLayerTransparent;
end;

procedure  TLayerInsp.SetLayerTransparent(AValue: boolean);
begin
  fOwner.ActiveLayerTransparent := AValue;
end;

function   TLayerInsp.GetLayerStreamable:  boolean;
begin
  result := fOwner.ActiveLayerStreamable;
end;

procedure  TLayerInsp.SetLayerStreamable(AValue: boolean);
begin
  fOwner.ActiveLayerStreamable := AValue;
end;

///////////////////////////////////////////////////////////////////////////////
//TDrawing ////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
procedure TDrawing.CADPrg2DEndOperation(Sender: TObject;
  const Operation: TCADStateClass; const Param: TCADPrgParam);
begin
  //SnapOption := 0;
  fChanged := true;
  fMain.frmMain.acCMDAccept.Checked := true;
  UndoRedo.UndoSave;
end;

procedure TDrawing.CADPrg2DExitState(Sender: TObject; const State: TCADState);
begin

end;

procedure TDrawing.CADPrg2DIdle(Sender: TObject);
begin

end;

procedure TDrawing.CADPrg2DStopOperation(Sender: TObject;
  const Operation: TCADStateClass; const Param: TCADPrgParam);
begin
  fChanged := true;
  //fMain.frmMain.acCMDAccept.Checked := true;
end;

function TDrawing.GetSnapPoint(ASnapOption: word; AObj: TObject2D; AAperture: word): TPoint2D;
var i: integer; TmpDist: TRealType;  TmpBool: boolean;  TmpPointsSet: TPointsSet2D;
begin
  AObj.UpdateExtension(nil);
  //if (not CADPrg2D.UseSnap) then exit;
  case  ASnapOption  of
    0:  result := CADPrg2D.CurrentViewportPoint;  //Grid
    1:  result := TPrimitive2D(AObj).LeftTop;
    2:  result := TPrimitive2D(AObj).TopCenter;
    3:  result := TPrimitive2D(AObj).RightTop;
    4:  result := TPrimitive2D(AObj).RightCenter;
    5:  result := TPrimitive2D(AObj).RightBottom;
    6:  result := TPrimitive2D(AObj).BottomCenter;
    7:  result := TPrimitive2D(AObj).LeftBottom;
    8:  result := TPrimitive2D(AObj).LeftCenter;
    9:  result := TPrimitive2D(AObj).MiddlePoint;
    10: result := TSimplePrimitive2D(AObj).StartPoint;
    11: result := TSimplePrimitive2D(AObj).EndPoint;
    12: begin //ProfilePoints
       if  AObj is TOutLine2D then
         if fSnapProfilePointsNr > TOutline2D(AObj).ProfilePoints.Count - 1   then
           result := TOutline2D(AObj).ProfilePoints[TOutline2D(AObj).ProfilePoints.Count - 1]
         else
           result := TOutline2D(AObj).ProfilePoints[fSnapProfilePointsNr]
       else
         if AObj is TLine2D  then
         begin
           if fSnapProfilePointsNr > 1 then result := TLine2D(AObj).Points[1]
           else                             result := TLine2D(AObj).Points[fSnapProfilePointsNr];
         end;
       end;
    13: begin //Naerest
         if  AObj is TOutLine2D then
         begin
           try
             TmpPointsSet := TPointsSet2D.Create(TOutline2D(AObj).ProfilePoints.Count
                                                 + TOutline2D(AObj).Points.Count
                                                 + 9);
             TmpPointsSet.Copy(TOutline2D(AObj).ProfilePoints, 0, TOutline2D(AObj).ProfilePoints.Count - 1);
             TmpPointsSet.Copy(TOutline2D(AObj).Points, 0, TOutline2D(AObj).Points.Count - 1);

             TmpPointsSet.Add(TOutLine2D(AObj).LeftTop);
             TmpPointsSet.Add(TOutLine2D(AObj).LeftBottom);
             TmpPointsSet.Add(TOutLine2D(AObj).RightBottom);
             TmpPointsSet.Add(TOutLine2D(AObj).RightTop);
             TmpPointsSet.Add(TOutLine2D(AObj).MiddlePoint);
             TmpPointsSet.Add(TOutLine2D(AObj).LeftCenter);
             TmpPointsSet.Add(TOutLine2D(AObj).BottomCenter);
             TmpPointsSet.Add(TOutLine2D(AObj).RightCenter);
             TmpPointsSet.Add(TOutLine2D(AObj).TopCenter);

             TmpBool := false;
             i := 0;
             TmpBool := NearPoint2D(TmpPointsSet[i], CADPrg2D.CurrentViewportPoint, AAperture, TmpDist);
             while ((not TmpBool) and (i < TmpPointsSet.Count - 1)) do
             begin
               inc(i);
               TmpBool := NearPoint2D(TmpPointsSet[i], CADPrg2D.CurrentViewportPoint, AAperture, TmpDist);
             end;
             if  TmpBool then
               result := TmpPointsSet[i]
             else
               result := CADPrg2D.CurrentViewportPoint;
           finally
             TmpPointsSet.Clear;
             TmpPointsSet.Free;
           end;
         end else
         if  AObj is TLine2D then
         begin
           try
             TmpPointsSet := TPointsSet2D.Create(TPrimitive2D(AObj).Points.Count + 9);
             TmpPointsSet.Copy(TPrimitive2D(AObj).Points, 0, TPrimitive2D(AObj).Points.Count - 1);

             TmpPointsSet.Add(TPrimitive2D(AObj).LeftTop);
             TmpPointsSet.Add(TPrimitive2D(AObj).LeftBottom);
             TmpPointsSet.Add(TPrimitive2D(AObj).RightBottom);
             TmpPointsSet.Add(TPrimitive2D(AObj).RightTop);
             TmpPointsSet.Add(TPrimitive2D(AObj).MiddlePoint);
             TmpPointsSet.Add(TPrimitive2D(AObj).LeftCenter);
             TmpPointsSet.Add(TPrimitive2D(AObj).BottomCenter);
             TmpPointsSet.Add(TPrimitive2D(AObj).RightCenter);
             TmpPointsSet.Add(TPrimitive2D(AObj).TopCenter);

             TmpBool := false;
             i := 0;
             TmpBool := NearPoint2D(TmpPointsSet[i], CADPrg2D.CurrentViewportPoint, AAperture, TmpDist);
             while ((not TmpBool) and (i < TmpPointsSet.Count - 1)) do
             begin
               inc(i);
               TmpBool := NearPoint2D(TmpPointsSet[i], CADPrg2D.CurrentViewportPoint, AAperture, TmpDist);
             end;
             if  TmpBool then
               result := TmpPointsSet[i]
             else
               result := CADPrg2D.CurrentViewportPoint;
             finally
               TmpPointsSet.Clear;
               TmpPointsSet.Free;
             end;
         end else
         begin
           result := CADPrg2D.CurrentViewportPoint;
         end;
       end;
  end;
end;

procedure TDrawing.CADPrg2DMouseMoveFilter(Sender: TCADPrg2D;
  CurrentState: TCADState; var WPt: TPoint2D; X, Y: Integer);
begin
  CADPrg2DSnapFilter(Sender, CurrentState, WPt, WPt);
end;

procedure TDrawing.CADPrg2DSnapFilter(Sender: TCADPrg2D;
  CurrentState: TCADState; const LastPt: TPoint2D;
  var CurrSnappedPt: TPoint2D);
var
  TmpPt: TPoint2D;
  TmpObj: TObject2D;
  TmpN,i,j: Integer;
  hPoint2D, CurrPt: TPoint2D;
  hMousePos: TPoint;
begin
  if fSnapOption = -1 then  //Snap OFF
  begin
    CADCmp2D.UseSnap := false;
    CADPrg2D.UseSnap := false;
    exit;
  end;

  if fSnapOption = 0 then  //Grid
    exit;

  TmpPt  := CADPrg2D.CurrentViewportPoint;
  if fSnapOption = 14 then //AngularSnap
  begin
    CurrPt := CADPrg2D.CurrentViewportPoint;
    Make45_2D(LastPt, CurrPt);
    CurrSnappedPt := CurrPt;
    exit;
  end;
  TmpObj := CADViewport2D.PickObject(TmpPt, 5, False, TmpN);
  if (TmpObj <> nil) then
  begin
    If (TmpN=PICK_ONOBJECT) or (TmpN>=0) then
    begin
      //CurrSnappedPt := TransformPoint2D(GetSnapPoint(fSnapOption, TmpObj, 5), TmpObj.ModelTransform);
      CurrSnappedPt := GetSnapPoint(fSnapOption, TmpObj, 5);
      //Screen.Cursor := crHandPoint;
    end else
    begin
      //Screen.Cursor := crDefault;
    end;
  end;
  CADPrg2D.CurrentViewportPoint := CurrSnappedPt;
end;

procedure TDrawing.CADPrg2DStartOperation(Sender: TObject;
  const Operation: TCADStateClass; const Param: TCADPrgParam);
begin
  fChanged := true;
  frmMain.TIPropertyGrid1.TIObject := nil;
end;

procedure TDrawing.CADPrg2DDescriptionChanged(Sender: TObject);
begin
  frmMain.StatusBarMain.Panels[1].Text := TCADState(Sender).Description;
end;

procedure TDrawing.CADCmp2DAddObject(Sender: TObject; Obj: TGraphicObject);
begin
  fChanged := true;
  TObject2D(Obj).InitializeAngle;
  if Obj is TSimplePrimitive2D then
    TSimplePrimitive2D(Obj).ShowDirection := CADCmp2D.ShowDirection;
end;

procedure TDrawing.CADCmp2DLoadProgress(Sender: TObject; ReadPercent: Byte);
begin
  frmMain.ProgressBarMain.StepIt;
  if frmMain.ProgressBarMain.Position >= frmMain.ProgressBarMain.Max then
  begin
    frmMain.ProgressBarMain.Position := 0;
  end;
  //frmMain.ProgressBarMain.Repaint;
  Application.ProcessMessages;
end;

procedure TDrawing.CADViewport2DEndRedraw(Sender: TObject);
begin
  //frmMain.TIPropertyGrid1.Update;
end;

procedure TDrawing.CADViewport2DMouseDown2D(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; WX, WY: TRealType; X, Y: Integer);
begin

end;

procedure TDrawing.CADViewport2DMouseMove2D(Sender: TObject;
  Shift: TShiftState; WX, WY: TRealType; X, Y: Integer);
begin
  {with CADPrg2D.CurrentViewportSnappedPoint do
  begin
    frmMain.StatusBarMain.Panels[0].Text := Format('X: %6.3f   Y: %6.3f', [X,Y]);
    RulerLeft.SetMark(Y);
    RulerBottom.SetMark(X);
  end; }
end;

procedure TDrawing.CADViewport2DMouseUp2D(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; WX, WY: TRealType; X, Y: Integer);
begin

end;

procedure TDrawing.CADViewport2DPaint(Sender: TObject);
begin
  CADViewport2D.BackGroundColor := CADCmp2D.BackgroundColor;
  CADViewport2D.ShowGrid        := CADCmp2D.ShowGrid;
  CADViewport2D.GridColor       := CADCmp2D.GridColor;
  CADViewport2D.GridDeltaX      := CADCmp2D.GridDeltaX;
  CADViewport2D.GridDeltaY      := CADCmp2D.GridDeltaY;
  CADPrg2D.UseOrto              := CADCmp2D.UseOrtho;
  CADPrg2D.UseSnap              := CADCmp2D.UseSnap;
  CADPrg2D.XSnap                := CADCmp2D.SnapX;
  CADPrg2D.YSnap                := CADCmp2D.SnapY;
  CADViewport2D.Enabled         := CADCmp2D.EditMode;

  RulerLeft.Repaint;
  RulerBottom.Repaint;
end;

procedure TDrawing.mnuModifyReverseClick(Sender: TObject);
var TmpPt: TPoint2D; TmpObj: TObject2D; TmpN: Integer;
begin
  TmpPt  := CADPrg2D.CurrentViewportSnappedPoint;
  TmpObj := CADViewport2D.PickObject(TmpPt, 5, false, TmpN);
  if TmpObj <> nil then
  begin
    TSimplePrimitive2D(TmpObj).Reverse;
    CADCmp2D.RepaintViewports;
    //TmpObj.UpdateExtension(nil)
  end else
    CADViewport2D.PopupMenu := popupDrawing;
end;

procedure TDrawing.mnuInverseClick(Sender: TObject);
var TmpPt: TPoint2D; TmpObj: TObject2D; TmpN: Integer;
begin
  TmpPt  := CADPrg2D.CurrentViewportSnappedPoint;
  TmpObj := CADViewport2D.PickObject(TmpPt, 5, false, TmpN);
  if TmpObj <> nil then
  begin
    TSimplePrimitive2D(TmpObj).Inverse;
    CADCmp2D.RepaintViewports;
    //TmpObj.UpdateExtension(nil)
  end else
    CADViewport2D.PopupMenu := popupDrawing;
end;

procedure TDrawing.pmnuAcceptClick(Sender: TObject);
begin
  CADPrg2D.SendUserEvent(CADPRG_ACCEPT);
end;

procedure TDrawing.pmnuCancelClick(Sender: TObject);
begin
  CADPrg2D.SendUserEvent(CADPRG_CANCEL);
end;

procedure TDrawing.SetPoint;
var
  TmpStrX, TmpStrY: String;
begin
  TmpStrX := Format('%6.3f', [CADPrg2D.CurrentViewportPoint.X]);
  TmpStrY := Format('%6.3f', [CADPrg2D.CurrentViewportPoint.Y]);
  if not InputQuery('Insert point', 'X', TmpStrX) then
   Exit;
  if not InputQuery('Insert point', 'Y', TmpStrY) then
   Exit;
  CADPrg2D.CurrentViewportPoint := Point2D(StrToFloat(TmpStrX), StrToFloat(TmpStrY));
  CADPrg2D.SendCADEvent(ceMouseDown, mbLeft, [], 0);
end;

procedure TDrawing.pmnuSetPointClick(Sender: TObject);
begin
  SetPoint;
end;

procedure TDrawing.popupDrawingPopup(Sender: TObject);
begin
  pmnuAccept.Visible    := CADPrg2D.IsBusy;
  pmnuCancel.Visible    := CADPrg2D.IsBusy;
  pmnuSetPoint.Visible  := CADPrg2D.IsBusy;
end;

constructor TDrawing.Create(TheOwner: TComponent);
var TmpStream: TFileStream;
begin
  inherited;
  RulerLeft.ShowMarker   := applicationh.fShowRulerMarker;
  RulerBottom.ShowMarker := applicationh.fShowRulerMarker;
  fFileName := '';
  fChanged  := false;

  UndoRedo := TUndoRedo.Create;     // Create an UndoRedo object instance
  UndoRedo.UndoInit;                // Initialize the UndoRedo streams
  UndoRedo.UndoSaveProcedure := @UndoSaveProcedure;  // Save procedure deff.
  UndoRedo.UndoRedoProcedure := @UndoRedoProcedure;  // Redo procedure deff.
  UndoRedo.OnUndoRedo        := @UndoRedoChangeEvent;// Event for undo/redo
  //UndoRedo.UndoSave;

  //self.CADViewport2D.RubberPen.Color := self.CADViewport2D.RubberPen.Color xor self.CADCmp2D.DefaultLayersColor;   ;
  //imgLogo.Picture.LoadFromFile('data\images\logos\laz_logo1.jpg');
end;

procedure TDrawing.UndoSaveProcedure(var MemSt: TMemoryStream);
begin
  CadCmp2D.SaveToStream(MemSt);
end;

procedure TDrawing.UndoRedoProcedure(MemSt: TMemoryStream);
begin
  //CadCmp2D.DeleteAllObjects;
  //CadCmp2D.DeleteSavedSourceBlocks;
  CadCmp2D.LoadFromStream(MemSt);
  CadCmp2D.RepaintViewports;
  CADViewPort2D.CADCmp2D.RefreshViewports;
end;

procedure TDrawing.UndoRedoChangeEvent(Sender: TObject; Undo, Redo: boolean);
begin
  fMain.frmMain.acEditUndo.Enabled := Undo;
  fMain.frmMain.acEditRedo.Enabled := Redo;
end;


procedure   TDrawing.SaveToFile(AFileName: string);
begin
  CADCmp2D.SaveToFile(AFileName);
end;

procedure   TDrawing.LoadFromFile(AFileName: string);
begin
  CADCmp2D.LoadFromFile(AFileName);
end;

function  TDrawing.LoadBlockLibraryFromFile(AFileName: string): boolean;
var TmpStr: TFileStream;
begin
  AFileName := AFileName;
  result := false;
  TmpStr := TFileStream.Create(AFileName, fmOpenRead);
  try
    CADCmp2D.DeleteLibrarySourceBlocks;
    CADCmp2D.LoadLibrary(TmpStr);
    result := true;
  finally
    TmpStr.Free;
  end;
end;

function  TDrawing.SaveBlockLibraryToFile(AFileName: string): boolean;
var TmpStr: TFileStream;
begin
  result := false;
  TmpStr := TFileStream.Create(AFileName, fmCreate);
  try
    CADCmp2D.SaveLibrary(TmpStr);
    result := true;
  finally
    TmpStr.Free;
  end;
end;

destructor TDrawing.Destroy;
begin
  UndoRedo.Free;
  inherited;
end;

end.

