unit fDrawing;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, Dialogs, Graphics, ComCtrls,
  Menus, Types,
  UndoRedo,
  applicationh,
  camh,
  CADSys4,
  CS4Shapes,
  CS4BaseTypes,
  CS4Tasks;

type



  { TDrawing }

   TDrawing = class;


  TComponentDrawing = class(TPersistent)
  private
    fDrawing: TDrawing;
    fEnableDragDrop: boolean;
    function  GetFileName: string;
    procedure SetFileName(AFileName: string);
    function  GetDrawingChanged: boolean;
    procedure SetDrawingChanged(AChanged: boolean);
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
  public
    property Drawing: TDrawing read fDrawing          write fDrawing;
    property Changed: boolean  read GetDrawingChanged write SetDrawingChanged;

  published
    property FileName: string            read GetFileName; // write SetFileName;

    property EnableDragDrop: boolean read fEnableDragDrop write fEnableDragDrop default true;

    //CADViewport
    property  BackgroundColor: TColor    read GetBackgroundColor write SetBackgroundColor;
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
    property DefaultLayersColor: TColor  read   GetDefaultLayersColor write SetDefaultLayersColor;
    property ShowDirection: boolean  read GetShowDirection write SetShowDirection;
    property PolarTracking: boolean read GetPolarTracking write SetPolarTracking;
    property PolarTrackingValue: TRealType read GetPolarTrackingValue write SetPolarTrackingValue;
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
    procedure CADViewport2DMouseMove2D(Sender: TObject; Shift: TShiftState; WX,
      WY: TRealType; X, Y: Integer);
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
  fDrawing.CADViewport2D.RubberPen.Color := AColor;// xor  fDrawing.CADCmp2D.DefaultLayersColor;
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
        TPrimitive2D(TmpIter.Current).ShowDirection := AValue;
        fDrawing.CADCmp2D.RedrawObject(TPrimitive2D(TmpIter.Current));
      end;
      TmpIter.Next;
    end;
  finally
    TmpIter.Free;
  end;
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
  //UndoRedo.UndoSave;
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
    9:  result := TPrimitive2D(AObj).CenterPoint;
    10: result := TPrimitive2D(AObj).StartPoint;
    11: result := TPrimitive2D(AObj).EndPoint;
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
             TmpPointsSet.Add(TOutLine2D(AObj).CenterPoint);
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
             TmpPointsSet.Add(TPrimitive2D(AObj).CenterPoint);
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
end;

procedure TDrawing.CADPrg2DDescriptionChanged(Sender: TObject);
begin
  frmMain.StatusBarMain.Panels[1].Text := TCADState(Sender).Description;
end;

procedure TDrawing.CADCmp2DAddObject(Sender: TObject; Obj: TGraphicObject);
begin
  if Obj is TPrimitive2D then
  begin
    TPrimitive2D(Obj).ShowDirection :=  CADCmp2D.ShowDirection;
    CADCmp2D.RedrawObject(TPrimitive2D(Obj));
  end;
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
    Reverse(TPrimitive2D(TmpObj));
    //CADViewport2D.Repaint;
    //frmMain.TIPropertyGrid1.Repaint;
  end;
  CADViewport2D.PopupMenu := popupDrawing;
end;

procedure TDrawing.mnuInverseClick(Sender: TObject);
var TmpPt: TPoint2D; TmpObj: TObject2D; TmpN: Integer;
begin
  TmpPt  := CADPrg2D.CurrentViewportSnappedPoint;
  TmpObj := CADViewport2D.PickObject(TmpPt, 5, false, TmpN);
  if TmpObj <> nil then
  begin
    Inverse(TPrimitive2D(TmpObj));
    //CADViewport2D.Repaint;
    //frmMain.TIPropertyGrid1.Repaint;
  end;
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
  AFileName := AFileName;
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

