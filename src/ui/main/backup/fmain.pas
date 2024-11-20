unit fMain;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF WINDOWS} Windows, ShellAPI, {$ENDIF}
  LCLType, Classes, SysUtils, Forms, Types, Controls, Graphics, Dialogs,
  ComCtrls, ExtCtrls, RTTIGrids, StdCtrls, ShellCtrls, ActnList, Buttons, Menus,
  ExtDlgs, SpkToolbar, spkt_Tab, spkt_Pane, spkt_Buttons, spkt_Appearance,
  spkt_Checkboxes, BCButtonFocus, AbUnzper, Interfaces, PropEdits,
  ObjectInspector, ClipBrd,
  IniFiles,

  CADSys4,
  CS4BaseTypes,
  CS4Shapes,
  CS4Tasks,

  CS4DXFModule,

  cImportEssi,

  applicationh,
  CADDocument,
  FiguresAsComponents,
  fAbout,
  fLibraryBlocks,
  fLayers,
  fSimulation,
  fDrawing,
  fttf2vector,
  ide_editor,
  camh,
  fHelpHtml;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    acAlign: TAction;
    acAlignBottom: TAction;
    acAlignCenter: TAction;
    acAlignCenterX: TAction;
    acAlignCenterY: TAction;
    acAlignLeft: TAction;
    acAlignRight: TAction;
    acAlignTop: TAction;
    acAppsDummy: TAction;
    acBarcode128: TAction;
    acBarcode39: TAction;
    acBarcode93: TAction;
    acBarcodeDMatrix: TAction;
    acBarcodeDMatrixRect: TAction;
    acBarcodeDMatrixRect1: TMenuItem;
    acBarcodeEAN13: TAction;
    acBarcodeEAN8: TAction;
    acBarcodeITF: TAction;
    acBarcodeMQR: TAction;
    acBarcodeQR: TAction;
    acBlockBasePoint: TAction;
    acBlocks: TAction;
    acBlocksCreate: TAction;
    acBlocksInsert: TAction;
    acBoundary: TAction;
    acCAMDrawingBrowser: TAction;
    acCAMMacroMan: TAction;
    acCAMPostprocessors: TAction;
    acCAMShowHideRapidMoves: TAction;
    acCAMStartStopEmul: TAction;
    acCMDAccept: TAction;
    acCMDCancel: TAction;
    acCMDPolarTracking: TAction;
    acCMDResetOperation: TAction;
    acCMDSetPoint: TAction;
    acCMDShowGrid: TAction;
    acCMDShowHideArealView: TAction;
    acCMDShowHideInspector: TAction;
    acCMDShowHideMagnifier: TAction;
    acCMDAreaSelectionMode: TAction;
    acCMDUseOrtho: TAction;
    acCMDUseSnap: TAction;
    acCMDEditMode: TAction;
    acDimAli: TAction;
    acDimAng: TAction;
    acDimCenterMark: TAction;
    acDimDia: TAction;
    acDimLeader: TAction;
    acDimLin: TAction;
    acDimOrd: TAction;
    acDimRad: TAction;
    acDimRot: TAction;
    acDrawEllipticalArc: TAction;
    acDrawArcCONT: TAction;
    acDrawArcCSE: TAction;
    acDrawArcSEC: TAction;
    acDrawArcSED: TAction;
    acDrawArcSEM: TAction;
    acDrawArcSME: TAction;
    acDrawArcText: TAction;
    acDrawBarcode: TAction;
    acDrawCircle0: TAction;
    acDrawCircle2Pts: TAction;
    acDrawCircle3Pts: TAction;
    acDrawCircleCR: TAction;
    acDrawConstructionLine: TAction;
    acDrawECW: TAction;
    acDrawEllipse: TAction;
    acDrawEllipseAxisEnd: TAction;
    acDrawEllipseByRectangle: TAction;
    acDrawEllipseCenter: TAction;
    acDrawHatch: TAction;
    acDrawLine: TAction;
    acDrawMultiLineText: TAction;
    acDrawPoint: TAction;
    acDrawPolygon: TAction;
    acDrawPolyLine: TAction;
    acDrawRay: TAction;
    acDrawRectangle0: TAction;
    acDrawRectangle2Pts: TAction;
    acDrawRectangle3Pts: TAction;
    acDrawRectangleCSA: TAction;
    acDrawSingleLineText: TAction;
    acDrawSketch: TAction;
    acDrawSpiral: TAction;
    acDrawSpline: TAction;
    acDrawTangentLine: TAction;
    acDrawVectorialText: TAction;
    acDrawViewport: TAction;
    acEditCopy: TAction;
    acEditCopyCB: TAction;
    acEditCut: TAction;
    acEditPaste: TAction;
    acEditRedo: TAction;
    acEditUndo: TAction;
    acFileClose: TAction;
    acFileExit: TAction;
    acFileNew: TAction;
    acFileOpen: TAction;
    acFileOpenRecent: TAction;
    acFileSave: TAction;
    acFileSaveAs: TAction;
    acFormatColors: TAction;
    acFormatDimensionStyles: TAction;
    acFormatExternalRefs: TAction;
    acFormatFillings: TAction;
    acFormatImages: TAction;
    acFormatLinetypes: TAction;
    acFormatLayerOrd: TAction;
    acFormatLayers: TAction;
    acFormatLayouts: TAction;
    acFormatLimits: TAction;
    acFormatLineweights: TAction;
    acFormatMultilineStyles: TAction;
    acFormatPointstyle: TAction;
    acFormatTextstyles: TAction;
    acFormatUnits: TAction;
    acHelpAbout: TAction;
    acHelpHtml: TAction;
    acInsertRasterImage: TAction;
    acLoadRuntimeApps: TAction;
    acModifyBreak: TAction;
    acModifyClearDrawing: TAction;
    acModifyErase: TAction;
    acModifyExplode: TAction;
    acModifyExplodeDrw: TAction;
    acModifyExtend: TAction;
    acModifyFillet: TAction;
    acModifyJoin: TAction;
    acModifyJoinAll: TAction;
    acModifyMirrorY: TAction;
    acModifyMove: TAction;
    acModifyOffset: TAction;
    acModifyRotate: TAction;
    acModifyScale: TAction;
    acModifySplit: TAction;
    acModifyTrim: TAction;
    acOpenCloseTraceWindow: TAction;
    acOrderSequentiel: TAction;
    acOrderBingForward: TAction;
    acOrderSendToBack: TAction;
    acOrderBringToFront: TAction;
    acOrderBringBackward: TAction;
    acOrderShortC: TAction;
    acOrderSwap: TAction;
    acOSnap: TAction;
    acOSnapCenter: TAction;
    acOSnapEndPoint: TAction;
    acOSnapInsertion: TAction;
    acOSnapIntersection: TAction;
    acOSnapMidPoint: TAction;
    acOSnapNearest: TAction;
    acOSnapNode: TAction;
    acOSnapPerpendicular: TAction;
    acOSnapQuadrant: TAction;
    acOSnapTangent: TAction;
    acFilePrint: TAction;
    acPrinterSetup: TAction;
    acPyImportDXF: TAction;
    acRequestPwd: TAction;
    acSaveTraceListToDisk: TAction;
    acSettingsDRWPRM: TAction;
    acSettingsEmulParams: TAction;
    acSettingsGRIDDlg: TAction;
    acSettingsKeyboardMoveDlg: TAction;
    acSettingsMagnifierParamsDlg: TAction;
    acSettingsOpenAppSettingsForm: TAction;
    acSettingsOpenCurrentTemplateFile: TAction;
    acSettingsOSNAPDlg: TAction;
    acSettingsPluginsDlg: TAction;
    acSettingsPTRACKDlg: TAction;
    acSettingsSelectionOptionsDlg: TAction;
    acSettingsSetBackgImage: TAction;
    acSettingsSetConnStrFile: TAction;
    acSettingsSetDefTemplate: TAction;
    acSettingsSetHackMode: TAction;
    acSettingsUnLockTemplateLayer: TAction;
    acShowHideCommandPanel: TAction;
    acShowHideToolBarDraw: TAction;
    acShowHideToolBarEdit: TAction;
    acShowHideToolBarFile: TAction;
    acShowHideToolBarFormat: TAction;
    acShowHideToolBarMDI: TAction;
    acShowHideToolBarMenu: TAction;
    acShowHideToolBarModify: TAction;
    acShowHideToolBarOrder: TAction;
    acShowHideToolBarPGMCommands: TAction;
    acShowHideToolBarTools: TAction;
    acShowHideToolBarZoom: TAction;
    acSimParams1: TMenuItem;
    acSystemChangePwd: TAction;
    acTestsTestLayers: TAction;
    acTINSwapTriangles1: TMenuItem;
    Action1: TAction;
    acCAMShowHideJumps: TAction;
    acModifyMirrorX: TAction;
    acDrawCircularEllipticalArc2D: TAction;
    acDrawCircularArc2D: TAction;
    acCMDShowHideCmdLine: TAction;
    acChangeUserInterface: TAction;
    acSnapTopLeft: TAction;
    acSnapRightCenter: TAction;
    acSnapBottomCenter: TAction;
    acSnapButtomRight: TAction;
    acSnapLeftCenter: TAction;
    acSnapMiddle: TAction;
    acSnapStartPoint: TAction;
    acSnapEndPoint: TAction;
    acSnapPolar: TAction;
    acSnapNaerest: TAction;
    acSnapGrid: TAction;
    acSnapTopRight: TAction;
    acSnapTopCenter: TAction;
    acSnapBottomLeft: TAction;
    acCMDSingleSelectionMode: TAction;
    acCMDExtendetSelectionMode: TAction;
    acModifyEdit: TAction;
    acClipperUnion: TAction;
    acClipperIntersection: TAction;
    acClipperXOR: TAction;
    acClipperDifference: TAction;
    acClipperOffset: TAction;
    acSettingsUseTemplates: TAction;
    acModifyReverse: TAction;
    acModifyInverse: TAction;
    acModifyMakeContainer: TAction;
    acFileImportEssi: TAction;
    acDrawSegment2D: TAction;
    acDrawSector2D: TAction;
    acDrawSymetricSymbol2D: TAction;
    acDrawASymetricSymbol2D: TAction;
    acFileCopyToClipboard: TAction;
    acModifyRotateEx: TAction;
    acToolsTTF2Vector: TAction;
    acToolsShowSimulator: TAction;
    ActionList: TActionList;
    acToolsArea: TAction;
    acToolsClippingRectangles: TAction;
    acToolsDistance: TAction;
    acToolsEmuStartStop: TAction;
    acToolsExecProject: TAction;
    acToolsScripter: TAction;
    acToolsRasterize: TAction;
    acToolsZip: TAction;
    acTraceON_OFF: TAction;
    actTransformMove: TAction;
    acZoomEXT: TAction;
    acZoomIN: TAction;
    acZoomLimits: TAction;
    acZoomOut: TAction;
    acZoomPan: TAction;
    acZoomPrev: TAction;
    acZoomRT: TAction;
    acZoomSelected: TAction;
    acZoomArea: TAction;
    AddPoints1: TMenuItem;
    Aligned1: TMenuItem;
    Angular1: TMenuItem;
    Arc1: TMenuItem;
    ArcText1: TMenuItem;
    Area1: TMenuItem;
    AutoBoundary1: TMenuItem;
    AxisEnd1: TMenuItem;
    Barcode1: TMenuItem;
    acBringToFront: TMenuItem;
    BCButtonFocus1: TBCButtonFocus;
    BCButtonFocus100: TBCButtonFocus;
    BCButtonFocus101: TBCButtonFocus;
    BCButtonFocus102: TBCButtonFocus;
    BCButtonFocus103: TBCButtonFocus;
    BCButtonFocus104: TBCButtonFocus;
    BCButtonFocus105: TBCButtonFocus;
    BCButtonFocus106: TBCButtonFocus;
    BCButtonFocus107: TBCButtonFocus;
    BCButtonFocus108: TBCButtonFocus;
    BCButtonFocus109: TBCButtonFocus;
    BCButtonFocus110: TBCButtonFocus;
    BCButtonFocus111: TBCButtonFocus;
    BCButtonFocus112: TBCButtonFocus;
    BCButtonFocus113: TBCButtonFocus;
    BCButtonFocus114: TBCButtonFocus;
    BCButtonFocus115: TBCButtonFocus;
    BCButtonFocus116: TBCButtonFocus;
    BCButtonFocus117: TBCButtonFocus;
    BCButtonFocus118: TBCButtonFocus;
    BCButtonFocus119: TBCButtonFocus;
    BCButtonFocus12: TBCButtonFocus;
    BCButtonFocus120: TBCButtonFocus;
    BCButtonFocus121: TBCButtonFocus;
    BCButtonFocus122: TBCButtonFocus;
    BCButtonFocus123: TBCButtonFocus;
    BCButtonFocus124: TBCButtonFocus;
    BCButtonFocus128: TBCButtonFocus;
    BCButtonFocus129: TBCButtonFocus;
    BCButtonFocus13: TBCButtonFocus;
    BCButtonFocus130: TBCButtonFocus;
    BCButtonFocus131: TBCButtonFocus;
    BCButtonFocus132: TBCButtonFocus;
    BCButtonFocus14: TBCButtonFocus;
    BCButtonFocus15: TBCButtonFocus;
    BCButtonFocus16: TBCButtonFocus;
    BCButtonFocus17: TBCButtonFocus;
    BCButtonFocus18: TBCButtonFocus;
    BCButtonFocus19: TBCButtonFocus;
    BCButtonFocus2: TBCButtonFocus;
    BCButtonFocus20: TBCButtonFocus;
    BCButtonFocus21: TBCButtonFocus;
    BCButtonFocus22: TBCButtonFocus;
    BCButtonFocus23: TBCButtonFocus;
    BCButtonFocus24: TBCButtonFocus;
    BCButtonFocus25: TBCButtonFocus;
    BCButtonFocus26: TBCButtonFocus;
    BCButtonFocus27: TBCButtonFocus;
    BCButtonFocus28: TBCButtonFocus;
    BCButtonFocus29: TBCButtonFocus;
    BCButtonFocus3: TBCButtonFocus;
    BCButtonFocus30: TBCButtonFocus;
    BCButtonFocus31: TBCButtonFocus;
    BCButtonFocus32: TBCButtonFocus;
    BCButtonFocus33: TBCButtonFocus;
    BCButtonFocus34: TBCButtonFocus;
    BCButtonFocus35: TBCButtonFocus;
    BCButtonFocus36: TBCButtonFocus;
    BCButtonFocus37: TBCButtonFocus;
    BCButtonFocus38: TBCButtonFocus;
    BCButtonFocus39: TBCButtonFocus;
    BCButtonFocus4: TBCButtonFocus;
    BCButtonFocus40: TBCButtonFocus;
    BCButtonFocus41: TBCButtonFocus;
    BCButtonFocus42: TBCButtonFocus;
    BCButtonFocus43: TBCButtonFocus;
    BCButtonFocus44: TBCButtonFocus;
    BCButtonFocus45: TBCButtonFocus;
    BCButtonFocus46: TBCButtonFocus;
    BCButtonFocus47: TBCButtonFocus;
    BCButtonFocus48: TBCButtonFocus;
    BCButtonFocus49: TBCButtonFocus;
    BCButtonFocus5: TBCButtonFocus;
    BCButtonFocus50: TBCButtonFocus;
    BCButtonFocus51: TBCButtonFocus;
    BCButtonFocus52: TBCButtonFocus;
    BCButtonFocus53: TBCButtonFocus;
    BCButtonFocus54: TBCButtonFocus;
    BCButtonFocus55: TBCButtonFocus;
    BCButtonFocus56: TBCButtonFocus;
    BCButtonFocus57: TBCButtonFocus;
    BCButtonFocus58: TBCButtonFocus;
    BCButtonFocus59: TBCButtonFocus;
    BCButtonFocus6: TBCButtonFocus;
    BCButtonFocus60: TBCButtonFocus;
    BCButtonFocus61: TBCButtonFocus;
    BCButtonFocus62: TBCButtonFocus;
    BCButtonFocus63: TBCButtonFocus;
    BCButtonFocus64: TBCButtonFocus;
    BCButtonFocus65: TBCButtonFocus;
    BCButtonFocus66: TBCButtonFocus;
    BCButtonFocus67: TBCButtonFocus;
    BCButtonFocus68: TBCButtonFocus;
    BCButtonFocus69: TBCButtonFocus;
    BCButtonFocus7: TBCButtonFocus;
    BCButtonFocus70: TBCButtonFocus;
    BCButtonFocus71: TBCButtonFocus;
    BCButtonFocus72: TBCButtonFocus;
    BCButtonFocus73: TBCButtonFocus;
    BCButtonFocus74: TBCButtonFocus;
    BCButtonFocus75: TBCButtonFocus;
    BCButtonFocus76: TBCButtonFocus;
    BCButtonFocus77: TBCButtonFocus;
    BCButtonFocus78: TBCButtonFocus;
    BCButtonFocus79: TBCButtonFocus;
    BCButtonFocus8: TBCButtonFocus;
    BCButtonFocus80: TBCButtonFocus;
    BCButtonFocus81: TBCButtonFocus;
    BCButtonFocus82: TBCButtonFocus;
    BCButtonFocus83: TBCButtonFocus;
    BCButtonFocus84: TBCButtonFocus;
    BCButtonFocus85: TBCButtonFocus;
    BCButtonFocus86: TBCButtonFocus;
    BCButtonFocus87: TBCButtonFocus;
    BCButtonFocus88: TBCButtonFocus;
    BCButtonFocus89: TBCButtonFocus;
    BCButtonFocus90: TBCButtonFocus;
    BCButtonFocus91: TBCButtonFocus;
    BCButtonFocus92: TBCButtonFocus;
    BCButtonFocus93: TBCButtonFocus;
    BCButtonFocus94: TBCButtonFocus;
    BCButtonFocus95: TBCButtonFocus;
    BCButtonFocus96: TBCButtonFocus;
    BCButtonFocus97: TBCButtonFocus;
    BCButtonFocus98: TBCButtonFocus;
    BCButtonFocus99: TBCButtonFocus;
    Bingaboveobject3: TMenuItem;
    BlockBasepoint1: TMenuItem;
    BlockBasepoint2: TMenuItem;
    Blocks1: TMenuItem;
    Blocks2: TMenuItem;
    Bottom2: TMenuItem;
    Break1: TMenuItem;
    FontDialog1: TFontDialog;
    HiResImages: TImageList;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    mnuCopyToClipBoard: TMenuItem;
    mnuTemplate: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    mnuMetroDark: TMenuItem;
    mnuMetroLight: TMenuItem;
    mnuOffice2007Blue: TMenuItem;
    mnuOffice2007Silver: TMenuItem;
    mnuOffice2007SilverTurquoise: TMenuItem;
    mnuView: TMenuItem;
    mnuSendToBack: TMenuItem;
    Bringunderobject2: TMenuItem;
    ByRectangle1: TMenuItem;
    Center2: TMenuItem;
    Center3: TMenuItem;
    CenterRadius1: TMenuItem;
    CenterRadius2: TMenuItem;
    CenterSizeAngle1: TMenuItem;
    CenterStartEnd1: TMenuItem;
    CenterX2: TMenuItem;
    CenterY2: TMenuItem;
    Changepassword1: TMenuItem;
    Clear1: TMenuItem;
    Clippingrectangles1: TMenuItem;
    Close1: TMenuItem;
    Code1281: TMenuItem;
    Code391: TMenuItem;
    Code931: TMenuItem;
    Colors1: TMenuItem;
    Constructionline1: TMenuItem;
    Continue1: TMenuItem;
    CoordinateGrid1: TMenuItem;
    Copy1: TMenuItem;
    CreateBlock1: TMenuItem;
    CreateBlock2: TMenuItem;
    DataMatrixsquare1: TMenuItem;
    DefineAttributes1: TMenuItem;
    DefineBoundary1: TMenuItem;
    DefineBoundary2: TMenuItem;
    DeleteDuplicatePoints1: TMenuItem;
    DeletePoints1: TMenuItem;
    DeletePoints2: TMenuItem;
    DeleteTriangle1: TMenuItem;
    DeveloperModus1: TMenuItem;
    Diameter1: TMenuItem;
    DimensionStyles1: TMenuItem;
    DimensionStyles2: TMenuItem;
    Distance1: TMenuItem;
    Draftingparameters1: TMenuItem;
    DrawingBrowser1: TMenuItem;
    EAN131: TMenuItem;
    EAN81: TMenuItem;
    ECWJPEG2000Image1: TMenuItem;
    EditTINPoints1: TMenuItem;
    Ellipse1: TMenuItem;
    Erase1: TMenuItem;
    Exe1: TMenuItem;
    Exit1: TMenuItem;
    Explode1: TMenuItem;
    ExplodeDrawing1: TMenuItem;
    Extend1: TMenuItem;
    extStyles1: TMenuItem;
    extWin1: TMenuItem;
    Fillet1: TMenuItem;
    Fillings1: TMenuItem;
    Hatch1: TMenuItem;
    HatchingImageList: TImageList;
    Help1: TMenuItem;
    ImageListClassic: TImageList;
    Images2: TMenuItem;
    IN1: TMenuItem;
    IN2: TMenuItem;
    InsertBlock1: TMenuItem;
    InsertBlock2: TMenuItem;
    IntegratedDevelopmentEnvironment1: TMenuItem;
    Interleaved2of5ITF1: TMenuItem;
    Join1: TMenuItem;
    JoinAll1: TMenuItem;
    KeyboardMove1: TMenuItem;
    Layers1: TMenuItem;
    Layouts1: TMenuItem;
    Leader1: TMenuItem;
    Left2: TMenuItem;
    Line1: TMenuItem;
    Line2: TMenuItem;
    Linear1: TMenuItem;
    LineTypes: TMenuItem;
    LoadPoints1: TMenuItem;
    LoadTINModel1: TMenuItem;
    MacroMananager1: TMenuItem;
    MagnifierParameters1: TMenuItem;
    MainMenu1: TMainMenu;
    MakeRaster1: TMenuItem;
    MarkCenter1: TMenuItem;
    Memo1: TMemo;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MicroQRCode1: TMenuItem;
    miMLText: TMenuItem;
    Mirror1: TMenuItem;
    miUnits: TMenuItem;
    mnuAlign: TMenuItem;
    mnuApplication: TMenuItem;
    mnuBlock: TMenuItem;
    mnuCAM: TMenuItem;
    mnuCAMPostprocessors: TMenuItem;
    mnuDimensions: TMenuItem;
    mnuDraw: TMenuItem;
    mnuEdit: TMenuItem;
    mnuEditCopy: TMenuItem;
    mnuEditCopyC: TMenuItem;
    mnuEditCut: TMenuItem;
    mnuEditErase: TMenuItem;
    mnuEditPaste: TMenuItem;
    mnuEditRedo: TMenuItem;
    mnuEditUndo: TMenuItem;
    mnuFile_OLD: TMenuItem;
    mnuFormat: TMenuItem;
    mnuHelp: TMenuItem;
    mnuModify: TMenuItem;
    mnuOrder: TMenuItem;
    mnuRTApps: TMenuItem;
    mnuSettings: TMenuItem;
    mnuTests: TMenuItem;
    mnuText: TMenuItem;
    mnuTools: TMenuItem;
    Move1: TMenuItem;
    N1: TMenuItem;
    N10: TMenuItem;
    N11: TMenuItem;
    N12: TMenuItem;
    N13: TMenuItem;
    N14: TMenuItem;
    N15: TMenuItem;
    N16: TMenuItem;
    N17: TMenuItem;
    N18: TMenuItem;
    N19: TMenuItem;
    N2: TMenuItem;
    N20: TMenuItem;
    N21: TMenuItem;
    N22: TMenuItem;
    N23: TMenuItem;
    N24: TMenuItem;
    N25: TMenuItem;
    N26: TMenuItem;
    N27: TMenuItem;
    N28: TMenuItem;
    N29: TMenuItem;
    N2Points1: TMenuItem;
    N3: TMenuItem;
    N30: TMenuItem;
    N3Points1: TMenuItem;
    N3Points2: TMenuItem;
    N4: TMenuItem;
    N5: TMenuItem;
    N6: TMenuItem;
    N7: TMenuItem;
    N8: TMenuItem;
    N9: TMenuItem;
    New1_OLD: TMenuItem;
    Offset1: TMenuItem;
    op2: TMenuItem;
    Open1_OLD: TMenuItem;
    OpenCADFileDialog: TOpenDialog;
    OpenPictureDialog1: TOpenPictureDialog;
    OrderLayers1: TMenuItem;
    OrderobjectsbyShortcut1: TMenuItem;
    Ordinate1: TMenuItem;
    OSNAP1: TMenuItem;
    PageControl1: TPageControl;
    PageControl2: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    pnlCAM: TPanel;
    pnlCAMGrip: TPanel;
    pnlClipper: TPanel;
    pnlClipperGrip: TPanel;
    pnlDrawGrip1: TPanel;
    pnlFormat: TPanel;
    pnlFormatsGrip: TPanel;
    pnlRightToolBars: TPanel;
    pnlSelectionMode: TPanel;
    pnlSelectionModeGrip: TPanel;
    pnlAlign: TPanel;
    pnlSnapGrip: TPanel;
    pnlSnap: TPanel;
    pnlAlignGrip: TPanel;
    pnlTools: TPanel;
    pnlZoomGrip: TPanel;
    pnlPgmCommands: TPanel;
    pnlPgmCmdsGrip: TPanel;
    pnlLeftToolBars: TPanel;
    pnlDraw: TPanel;
    pnlDrawGrip: TPanel;
    pnlOrder: TPanel;
    pnlOrderGrip: TPanel;
    pnlFile: TPanel;
    pnlEdit: TPanel;
    pnlFileModify: TPanel;
    pnlFileGrip: TPanel;
    pnlEditGrip: TPanel;
    pnlModifyGrip: TPanel;
    pnlLeft: TPanel;
    pnlZoom: TPanel;
    pnlTop: TPanel;
    pnlCMDLine: TPanel;
    Panel5: TPanel;
    Plugins1: TMenuItem;
    pnlBottomToolBars: TPanel;
    Point1: TMenuItem;
    PointStyle1: TMenuItem;
    PolarTracking1: TMenuItem;
    Polygon1: TMenuItem;
    PolyLine1: TMenuItem;
    popupRibbonMenu: TPopupMenu;
    Print1: TMenuItem;
    ProgressBarMain: TProgressBar;
    Properties1: TMenuItem;
    QRCode1: TMenuItem;
    Radius1: TMenuItem;
    Ray1: TMenuItem;
    Recent1_OLD: TMenuItem;
    Rectangle1: TMenuItem;
    Rectangle2P1: TMenuItem;
    RemoveBoundary1: TMenuItem;
    riangulate1: TMenuItem;
    Right2: TMenuItem;
    rim1: TMenuItem;
    Rotate1: TMenuItem;
    Rotated1: TMenuItem;
    Save1: TMenuItem;
    SaveAs1: TMenuItem;
    SaveCADSys4Dialog: TSaveDialog;
    SaveCADFileDialog: TSaveDialog;
    SaveTINModel1: TMenuItem;
    Scale1: TMenuItem;
    SelectionOptions1: TMenuItem;
    mnuSendToBack1: TMenuItem;
    SendToBack2: TMenuItem;
    SendToBack3: TMenuItem;
    SendToBack1: TMenuItem;
    Separator1: TMenuItem;
    SetBackgroundImage1: TMenuItem;
    SetConnectionFile1: TMenuItem;
    SetupPrinter1: TMenuItem;
    ShellTreeView1: TShellTreeView;
    ShowHideRapidMoves1: TMenuItem;
    SinglelineText1: TMenuItem;
    Sketch1: TMenuItem;
    Spiral1: TMenuItem;
    SpkCheckbox1: TSpkCheckbox;
    SpkLargeButton1: TSpkLargeButton;
    SpkLargeButton2: TSpkLargeButton;
    SpkPane1: TSpkPane;
    SpkPaneSnap: TSpkPane;
    SpkPanePgmCommands: TSpkPane;
    SpkPaneOrder: TSpkPane;
    SpkPaneAlign: TSpkPane;
    SpkPaneBlocks: TSpkPane;
    SpkPaneModify: TSpkPane;
    SpkPaneDraw: TSpkPane;
    SpkPaneEdit: TSpkPane;
    SpkPaneFile: TSpkPane;
    SpkSmallButton1: TSpkSmallButton;
    SpkSmallButton10: TSpkSmallButton;
    SpkSmallButton11: TSpkSmallButton;
    SpkSmallButton12: TSpkSmallButton;
    SpkSmallButton13: TSpkSmallButton;
    SpkSmallButton14: TSpkSmallButton;
    SpkSmallButton15: TSpkSmallButton;
    SpkSmallButton16: TSpkSmallButton;
    SpkSmallButton17: TSpkSmallButton;
    SpkSmallButton18: TSpkSmallButton;
    SpkSmallButton19: TSpkSmallButton;
    SpkSmallButton2: TSpkSmallButton;
    SpkSmallButton20: TSpkSmallButton;
    SpkSmallButton21: TSpkSmallButton;
    SpkSmallButton22: TSpkSmallButton;
    SpkSmallButton23: TSpkSmallButton;
    SpkSmallButton24: TSpkSmallButton;
    SpkSmallButton25: TSpkSmallButton;
    SpkSmallButton26: TSpkSmallButton;
    SpkSmallButton27: TSpkSmallButton;
    SpkSmallButton28: TSpkSmallButton;
    SpkSmallButton29: TSpkSmallButton;
    SpkSmallButton3: TSpkSmallButton;
    SpkSmallButton30: TSpkSmallButton;
    SpkSmallButton31: TSpkSmallButton;
    SpkSmallButton32: TSpkSmallButton;
    SpkSmallButton33: TSpkSmallButton;
    SpkSmallButton34: TSpkSmallButton;
    SpkSmallButton35: TSpkSmallButton;
    SpkSmallButton36: TSpkSmallButton;
    SpkSmallButton37: TSpkSmallButton;
    SpkSmallButton38: TSpkSmallButton;
    SpkSmallButton39: TSpkSmallButton;
    SpkSmallButton4: TSpkSmallButton;
    SpkSmallButton40: TSpkSmallButton;
    SpkSmallButton41: TSpkSmallButton;
    SpkSmallButton42: TSpkSmallButton;
    SpkSmallButton43: TSpkSmallButton;
    SpkSmallButton44: TSpkSmallButton;
    SpkSmallButton45: TSpkSmallButton;
    SpkSmallButton46: TSpkSmallButton;
    SpkSmallButton47: TSpkSmallButton;
    SpkSmallButton48: TSpkSmallButton;
    SpkSmallButton49: TSpkSmallButton;
    SpkSmallButton5: TSpkSmallButton;
    SpkSmallButton50: TSpkSmallButton;
    SpkSmallButton51: TSpkSmallButton;
    SpkSmallButton52: TSpkSmallButton;
    SpkSmallButton53: TSpkSmallButton;
    SpkSmallButton54: TSpkSmallButton;
    SpkSmallButton55: TSpkSmallButton;
    SpkSmallButton56: TSpkSmallButton;
    SpkSmallButton57: TSpkSmallButton;
    SpkSmallButton58: TSpkSmallButton;
    SpkSmallButton59: TSpkSmallButton;
    SpkSmallButton6: TSpkSmallButton;
    SpkSmallButton60: TSpkSmallButton;
    SpkSmallButton61: TSpkSmallButton;
    SpkSmallButton62: TSpkSmallButton;
    SpkSmallButton63: TSpkSmallButton;
    SpkSmallButton64: TSpkSmallButton;
    SpkSmallButton65: TSpkSmallButton;
    SpkSmallButton66: TSpkSmallButton;
    SpkSmallButton67: TSpkSmallButton;
    SpkSmallButton68: TSpkSmallButton;
    SpkSmallButton69: TSpkSmallButton;
    SpkSmallButton70: TSpkSmallButton;
    SpkSmallButton71: TSpkSmallButton;
    SpkSmallButton72: TSpkSmallButton;
    SpkSmallButton73: TSpkSmallButton;
    SpkSmallButton74: TSpkSmallButton;
    SpkSmallButton75: TSpkSmallButton;
    SpkSmallButton76: TSpkSmallButton;
    SpkSmallButton77: TSpkSmallButton;
    SpkSmallButton78: TSpkSmallButton;
    SpkSmallButton79: TSpkSmallButton;
    SpkSmallButton8: TSpkSmallButton;
    SpkSmallButton80: TSpkSmallButton;
    SpkSmallButton81: TSpkSmallButton;
    SpkSmallButton9: TSpkSmallButton;
    SpkTab1: TSpkTab;
    SpkTab2: TSpkTab;
    SpkTab3: TSpkTab;
    SpkTab4: TSpkTab;
    SpkTabView: TSpkTab;
    SpkTabHelp: TSpkTab;
    SpkTabModify: TSpkTab;
    SpkTabBlocks: TSpkTab;
    SpkTabEdit: TSpkTab;
    SpkTabFormat: TSpkTab;
    SpkTabDraw: TSpkTab;
    SpkTabFile: TSpkTab;
    SpkToolbar1: TSpkToolbar;
    Spline1: TMenuItem;
    Split1: TMenuItem;
    StartEndCenter1: TMenuItem;
    StartEndDirection1: TMenuItem;
    StartEndMiddle1: TMenuItem;
    StartMiddleEnd1: TMenuItem;
    StartStopSimulation1: TMenuItem;
    StatusBarMain: TStatusBar;
    Swapobjects2: TMenuItem;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    tsInspector: TTabSheet;
    tsShell: TTabSheet;
    TIPropertyGrid1: TTIPropertyGrid;
    Viewport1: TMenuItem;
    ZoomExtends1: TMenuItem;
    procedure acAlignBottomExecute(Sender: TObject);
    procedure acAlignCenterExecute(Sender: TObject);
    procedure acAlignLeftExecute(Sender: TObject);
    procedure acAlignRightExecute(Sender: TObject);
    procedure acAlignTopExecute(Sender: TObject);
    procedure acBlocksCreateExecute(Sender: TObject);
    procedure acBlocksExecute(Sender: TObject);
    procedure acBlocksInsertExecute(Sender: TObject);
    procedure acCAMShowHideRapidMovesExecute(Sender: TObject);
    procedure acCAMStartStopEmulExecute(Sender: TObject);
    procedure acClipperDifferenceExecute(Sender: TObject);
    procedure acClipperIntersectionExecute(Sender: TObject);
    procedure acClipperOffsetExecute(Sender: TObject);
    procedure acClipperUnionExecute(Sender: TObject);
    procedure acClipperXORExecute(Sender: TObject);
    procedure acCMDAcceptExecute(Sender: TObject);
    procedure acCMDCancelExecute(Sender: TObject);
    procedure acCMDEditModeExecute(Sender: TObject);
    procedure acCMDExtendetSelectionModeExecute(Sender: TObject);
    procedure acCMDPolarTrackingExecute(Sender: TObject);
    procedure acCMDShowGridExecute(Sender: TObject);
    procedure acCMDShowHideCmdLineExecute(Sender: TObject);
    procedure acCMDShowHideInspectorExecute(Sender: TObject);
    procedure acCMDAreaSelectionModeExecute(Sender: TObject);
    procedure acCMDSingleSelectionModeExecute(Sender: TObject);
    procedure acCMDUseOrthoExecute(Sender: TObject);
    procedure acCMDUseSnapExecute(Sender: TObject);
    procedure acDimLinExecute(Sender: TObject);
    procedure acDrawASymetricSymbol2DExecute(Sender: TObject);
    procedure acDrawCircle0Execute(Sender: TObject);
    procedure acDrawCircularArc2DExecute(Sender: TObject);
    procedure acDrawEllipticalArcExecute(Sender: TObject);
    procedure acDrawEllipseExecute(Sender: TObject);
    procedure acDrawLineExecute(Sender: TObject);
    procedure acDrawPointExecute(Sender: TObject);
    procedure acDrawPolygonExecute(Sender: TObject);
    procedure acDrawPolyLineExecute(Sender: TObject);
    procedure acDrawRectangle0Execute(Sender: TObject);
    procedure acDrawSector2DExecute(Sender: TObject);
    procedure acDrawSegment2DExecute(Sender: TObject);
    procedure acDrawSingleLineTextExecute(Sender: TObject);
    procedure acDrawSplineExecute(Sender: TObject);
    procedure acDrawVectorialTextExecute(Sender: TObject);
    procedure acEditCopyCBExecute(Sender: TObject);
    procedure acEditCopyExecute(Sender: TObject);
    procedure acEditCutExecute(Sender: TObject);
    procedure acEditPasteExecute(Sender: TObject);
    procedure acEditRedoExecute(Sender: TObject);
    procedure acEditUndoExecute(Sender: TObject);
    procedure acExitProgramExecute(Sender: TObject);
    procedure acFileCloseExecute(Sender: TObject);
    procedure acFileCopyToClipboardExecute(Sender: TObject);
    procedure acFileExitExecute(Sender: TObject);
    procedure acFileImportEssiExecute(Sender: TObject);
    procedure acFileNewExecute(Sender: TObject);
    procedure acFileOpenExecute(Sender: TObject);
    procedure acFileSaveAsExecute(Sender: TObject);
    procedure acFileSaveExecute(Sender: TObject);
    procedure acFormatLayersExecute(Sender: TObject);
    procedure acHelpAboutExecute(Sender: TObject);
    procedure acHelpHtmlExecute(Sender: TObject);
    procedure acInsertRasterImageExecute(Sender: TObject);
    procedure acCAMShowHideJumpsExecute(Sender: TObject);
    procedure acModifyEditExecute(Sender: TObject);
    procedure acModifyEraseExecute(Sender: TObject);
    procedure acModifyExplodeExecute(Sender: TObject);
    procedure acModifyInverseExecute(Sender: TObject);
    procedure acModifyMakeContainerExecute(Sender: TObject);
    procedure acModifyMirrorXExecute(Sender: TObject);
    procedure acModifyMirrorYExecute(Sender: TObject);
    procedure acModifyMoveExecute(Sender: TObject);
    procedure acModifyOffsetExecute(Sender: TObject);
    procedure acModifyReverseExecute(Sender: TObject);
    procedure acModifyRotateExecute(Sender: TObject);
    procedure acModifyRotateExExecute(Sender: TObject);
    procedure acModifyScaleExecute(Sender: TObject);
    procedure acOrderBingForwardExecute(Sender: TObject);
    procedure acOrderBringBackwardExecute(Sender: TObject);
    procedure acOrderSequentielExecute(Sender: TObject);
    procedure acOrderSendToBackExecute(Sender: TObject);
    procedure acOrderBringToFrontExecute(Sender: TObject);
    procedure acOrderSwapExecute(Sender: TObject);
    procedure acPrinterSetupExecute(Sender: TObject);
    procedure acChangeUserInterfaceExecute(Sender: TObject);
    procedure acRequestPwdExecute(Sender: TObject);
    procedure acSettingsOpenCurrentTemplateFileExecute(Sender: TObject);
    procedure acSettingsSetDefTemplateExecute(Sender: TObject);
    procedure acSettingsUnLockTemplateLayerExecute(Sender: TObject);
    procedure acSettingsUseTemplatesExecute(Sender: TObject);
    procedure acSnapBottomLeftExecute(Sender: TObject);
    procedure acSnapBottomCenterExecute(Sender: TObject);
    procedure acSnapButtomRightExecute(Sender: TObject);
    procedure acSnapMiddleExecute(Sender: TObject);
    procedure acSnapEndPointExecute(Sender: TObject);
    procedure acSnapGridExecute(Sender: TObject);
    procedure acSnapLeftCenterExecute(Sender: TObject);
    procedure acSnapNaerestExecute(Sender: TObject);
    procedure acSnapPolarExecute(Sender: TObject);
    procedure acSnapRightButtomExecute(Sender: TObject);
    procedure acSnapRightCenterExecute(Sender: TObject);
    procedure acSnapStartPointExecute(Sender: TObject);
    procedure acSnapTopCenterExecute(Sender: TObject);
    procedure acSnapTopLeftExecute(Sender: TObject);
    procedure acSnapTopRightExecute(Sender: TObject);
    procedure acTestsTestLayersExecute(Sender: TObject);
    procedure acDrawSymetricSymbol2DExecute(Sender: TObject);
    procedure acToolsScripterExecute(Sender: TObject);
    procedure acToolsShowSimulatorExecute(Sender: TObject);
    procedure acToolsTTF2VectorExecute(Sender: TObject);
    procedure acZoomAreaExecute(Sender: TObject);
    procedure acZoomEXTExecute(Sender: TObject);
    procedure acZoomINExecute(Sender: TObject);
    procedure acZoomOutExecute(Sender: TObject);
    procedure acZoomPanExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure mnuMetroDarkClick(Sender: TObject);
    procedure mnuMetroLightClick(Sender: TObject);
    procedure mnuOffice2007BlueClick(Sender: TObject);
    procedure mnuOffice2007SilverClick(Sender: TObject);
    procedure mnuOffice2007SilverTurquoiseClick(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure PageControl1Changing(Sender: TObject; var AllowChange: Boolean);
    procedure PageControl1CloseTabClicked(Sender: TObject);
    procedure TIPropertyGrid1Click(Sender: TObject);
    procedure TIPropertyGrid1Modified(Sender: TObject);
  private
    fMDICount: integer;

    ComponentDrawing: TComponentDrawing;
    fActivePage: TTabSheet;

    FMiddleMouseDown,
    FRightMouseDown,
    FLeftMouseDown: boolean;

    procedure RegisterDocument;
    procedure ChangeUserInterface;
    function  CreateNewDrawing: TTabSheet;
    function  GetDrawingFromPage(APage: TTabSheet): TDrawing;
    procedure CloseCADFile(APage: TTabSheet);
    function  CheckSave(APage: TTabSheet): boolean;
    function  TestCADSys4File(AFileName: string): boolean;

    procedure DisableControls;
    procedure EnableControls;

    procedure OnViewport2DMouseDown2D(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; WX, WY: TRealType; X, Y: Integer);
    procedure OnViewport2DMouseMove2D(Sender: TObject; Shift: TShiftState; WX, WY: TRealType; X, Y: Integer);
    procedure OnViewport2DMouseUp2D(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; WX, WY: TRealType; X, Y: Integer);

    procedure OnViewport2DMouseWheel(Sender: TObject; Shift: TShiftState;
        WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);

    procedure OnSelectObj(Sender: TCAD2DSelectObjectsParam; Obj: TObject2D; CtrlPt: Integer; Added: Boolean);

    procedure MoveObject(AObject: TObject2D);
    procedure EditObject(AObject: TObject2D; APointNr: integer);

    procedure SetSnapOption(AOption: Integer);

    procedure SetPropertyEditorForObjects(AObject: TObject2D);

    procedure OpenCS4File(AFileName: string);
    procedure OpenDXFFile(AFileName: string);
    procedure LoadDXFFile(AFileName: string; ACADCmp: TCADCmp2D);
    procedure OpenESSIFile(AFileName: string);
  public
    procedure SetupInspector(AControl: TPersistent);
    procedure UpdateUserInterface;

    procedure ImportFromCADCmp(ACADCmp: TCADCmp2D);
  end;


var
  frmMain: TfrmMain;
  FPickPosition: integer;
  MoveBasePoint: TPoint2D;
  IsEntityDragged: boolean;
  CancelEmulation: boolean;

procedure SimulObJ2D(AObj2D: TObject2D; AViewport: TCADViewport2D; T: TTransf2D; AColor: TColor);
procedure BresenhamDrawLine(X1, Y1, X2, Y2: TRealType; ACanvas: TCanvas; AColor: TColor; ASleepTime: word);


implementation

{$R *.lfm}

{ TfrmMain }

type
  EFileNotFound = class(Exception);


procedure TfrmMain.RegisterDocument;
var hDrawing: TDrawing;
begin
  hDrawing := GetDrawingFromPage(fActivePage);
  if  hDrawing <> nil then
    CADDocument.RegisterDocument(hDrawing.CADPrg2D, hDrawing.CADCmp2D, hDrawing.CADViewport2D, ProgressBarMain, TIPropertyGrid1)
  else
    CADDocument.RegisterDocument(nil, nil, nil, nil, nil);
end;


procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FMiddleMouseDown := false;
  FRightMouseDown  := false;
  FLeftMouseDown   := false;

  IsEntityDragged := false;
  fMDICount := 1;

  acSettingsUseTemplates.Checked   :=  (LowerCase(applicationh.fUseTemplates) = 'yes');
  ShellTreeView1.Root := GetAppDataPath;

  try
    CADSysRegisterFontFromFile(0, GetAppFontsFNTPath + 'verdana.fnt');
    CADSysRegisterFontFromFile(1, GetAppFontsFNTPath + 'monotxt.fnt');
    CADSysRegisterFontFromFile(2, GetAppFontsFNTPath + 'romanc.fnt');
    CADSysRegisterFontFromFile(3, GetAppFontsFNTPath + 'tms.fnt');
    DisableControls;
    ComponentDrawing := TComponentDrawing.Create;
    acFileNewExecute(nil);
  except
    EnableControls;
    raise;
  end;
end;

procedure TfrmMain.OnSelectObj(Sender: TCAD2DSelectObjectsParam; Obj: TObject2D; CtrlPt: Integer; Added: Boolean);
var hDrawing: TDrawing;
begin
  hDrawing := GetDrawingFromPage(fActivePage);
  if  hDrawing = nil then exit;

  if Assigned(Obj) then
    hDrawing.CADViewport2D.DrawObject2DWithRubber(Obj, True);
end;

function TfrmMain.GetDrawingFromPage(APage: TTabSheet): TDrawing;
var i: integer;
begin
  result := nil;
  for i := 0 to APage.ComponentCount - 1 do
    if (APage.Components[i] is TDrawing) then
    begin
      result := TDrawing(APage.Components[i]);
      exit;
    end;
end;

function TfrmMain.CreateNewDrawing: TTabSheet;
var TabSheet: TTabSheet;   hDrawing: TDrawing;
begin
  acSettingsUnLockTemplateLayer.Checked := false;
  TabSheet := TTabSheet.Create(PageControl1);
  TabSheet.Parent := PageControl1;
  PageControl1.ActivePage := TabSheet;

  hDrawing := TDrawing.Create(TabSheet);
  hDrawing.Parent := TabSheet;
  hDrawing.Align := alClient;

  hDrawing.CADViewport2D.OnMouseDown2D := @OnViewport2DMouseDown2D;
  hDrawing.CADViewport2D.OnMouseUp2D   := @OnViewport2DMouseUp2D;
  hDrawing.CADViewport2D.OnMouseMove2D := @OnViewport2DMouseMove2D;
  hDrawing.CADViewport2D.OnMouseWheel  := @OnViewport2DMouseWheel;

  //TabSheet.OnMouseWheel                := @OnTabSeetModelMouseWheel;

  //hDrawing.LoadBlockLibraryFromFile(hDrawing.CADCmp2D.CurrentBlockLibrary);

  hDrawing.CADViewport2D.ZoomWindow(Rect2D(-20, -20, 297, 210));
  result := TabSheet;
end;

procedure TfrmMain.OnViewport2DMouseUp2D(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; WX, WY: TRealType; X, Y: Integer);
var hDrawing: TDrawing;
begin
  FMiddleMouseDown := false;
  FRightMouseDown  := false;
  FLeftMouseDown   := false;

  hDrawing := GetDrawingFromPage(fActivePage);
  if  hDrawing = nil then exit;

  hDrawing.CADViewport2D.Cursor := crDefault;
  TIPropertyGrid1.Update;
  TIPropertyGrid1.Repaint;
  if IsEntityDragged then
  begin
    hDrawing.UndoRedo.UndoSave;
    IsEntityDragged := false;
  end;
end;

procedure TfrmMain.OnViewport2DMouseDown2D(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; WX, WY: TRealType; X, Y: Integer);
var hDrawing: TDrawing; TmpPar   : TCADPrgParam; TmpPt: TPoint2D; TmpObj: TObject2D; TmpN: Integer;
begin
  hDrawing := GetDrawingFromPage(fActivePage);
  if hDrawing = nil then exit;

  case Button of
    mbMiddle:begin
      FMiddleMouseDown := True;
        MoveBasePoint    := hDrawing.CADPrg2D.CurrentViewportSnappedPoint;
        MoveBasePoint    := hDrawing.CADViewport2D.ViewportToScreen(MoveBasePoint);
    end;
    mbRight:begin
      FRightMouseDown  := True;
    end;
    mbLeft:begin
      FLeftMouseDown   := True;
    end
  else
    // mbExtra1, mbExtra2
  end;

  if frmMain.acCAMShowHideJumps.Checked then
  begin
    frmMain.acCAMShowHideJumps.Checked := false;
    hDrawing.CADCmp2D.ClearLayer(CAM_LAYER_ID_JUMPS);
    hDrawing.CADViewport2D.Repaint;
  end;

  if hDrawing.CADPrg2D.IsBusy then exit;

  GlobalObject2D := nil;
  hDrawing.CADViewport2D.Repaint;

  TmpPt  := hDrawing.CADPrg2D.CurrentViewportSnappedPoint;
  TmpObj := hDrawing.CADViewport2D.PickObject(TmpPt, 5, false, TmpN);
  if  (TmpN = PICK_ONOBJECT) or (TmpN >= 0) then
  begin
    if Button = mbRight then
    begin
      hDrawing.CADViewport2D.PopupMenu :=  hDrawing.popupModify;
      exit;
    end;
    MoveBasePoint := hDrawing.CADPrg2D.CurrentViewportSnappedPoint;
    GlobalObject2D := TmpObj;
    FPickPosition  := TmpN;
    hDrawing.CadViewport2D.DrawObject2DWithRubber(TmpObj, true);
    SetPropertyEditorForObjects(TObject2D(TmpObj));
  end else
  begin
    ComponentDrawing.Drawing := hDrawing;
    SetupInspector(ComponentDrawing);
    GlobalObject2D := nil;
    hDrawing.CADViewport2D.PopupMenu :=  hDrawing.popupDrawing;
    if acCMDAreaSelectionMode.Checked then
    begin ///for UndoRedo disabled;
      //acModifyMoveExecute(nil);
    end;
  end;
end;

procedure TfrmMain.OnViewport2DMouseMove2D(Sender: TObject; Shift: TShiftState; WX, WY: TRealType; X, Y: Integer);
var TmpPt, TmpPt1: TPoint2D; TmpObj: TObject2D; TmpN: Integer; hDrawing: TDrawing; TmpSnapOpt: integer;
    DeltaX, DeltaY, NewStartX, NewStartY: TRealType;
begin
  hDrawing := GetDrawingFromPage(fActivePage);
  if  hDrawing = nil then exit;

  TmpPt := hDrawing.CADPrg2D.CurrentViewportSnappedPoint;

  if FMiddleMouseDown  then
  begin
    TmpPt  :=  hDrawing.CADViewport2D.ViewportToScreen(TmpPt);
    DeltaX :=  MoveBasePoint.X - TmpPt.X;
    DeltaY :=  MoveBasePoint.Y - TmpPt.Y;

    NewStartX := CADViewport2D.VisualRect.Left   + DeltaX;
    NewStartY := CADViewport2D.VisualRect.Bottom - DeltaY;

    NewStartX :=  NewStartX - (0.0008 * NewStartX);
    NewStartY :=  NewStartY - (0.0008 * NewStartY);

    hDrawing.CADViewport2D.MoveWindow(NewStartX, NewStartY);
    MoveBasePoint := TmpPt; //hDrawing.CADPrg2D.CurrentViewportSnappedPoint;
    exit;
  end;

  StatusBarMain.Panels[0].Text := Format('X: %6.3f   Y: %6.3f', [TmpPt.X, TmpPt.Y]);

  TmpPt1 := hDrawing.CADViewport2D.ViewportToScreen(TmpPt);
  if hDrawing.RulerLeft.ShowMarker then
  begin
    hDrawing.RulerLeft.MarkerPosition := TmpPt1.Y;  // Vertikale Position für die linke Ruler
    hDrawing.RulerLeft.Repaint;
  end;
  if hDrawing.RulerBottom.ShowMarker then
  begin
    hDrawing.RulerBottom.MarkerPosition := TmpPt1.X;  // Horizontale Position für die untere Ruler
    hDrawing.RulerBottom.Repaint;
  end;


  if GlobalObject2D = nil then exit;

  if GlobalObject2D is TBitmap2D then
    hDrawing.CADViewport2D.Repaint;

  if hDrawing.CADPrg2D.IsBusy then exit;

  if not ComponentDrawing.EnableDragDrop then exit;

  if SSLeft in Shift then
  begin
    if hDrawing.CADPrg2D.CurrentOperation = TCAD2DSelectObjects then
    begin
      hDrawing.CADPrg2D.SendUserEvent(CADPRG_ACCEPT);
      Exit;
    end;
    if hDrawing.CADPrg2D.CurrentOperation = TCADIdleState then
    begin
      if  (FPickPosition = PICK_ONOBJECT) then
      begin
        TmpSnapOpt := hDrawing.SnapOption;
        hDrawing.SnapOption := 0; //Grid
        hDrawing.CADViewport2D.Cursor := crDrag;
        MoveObject(GlobalObject2D);
        IsEntityDragged := true;
        hDrawing.SnapOption := TmpSnapOpt;
        hDrawing.CADViewport2D.Repaint;
        hDrawing.CadViewport2D.DrawObject2DWithRubber(GlobalObject2D, true);
      end else
      if  (FPickPosition >=0) then
      begin
        hDrawing.CADViewport2D.Cursor := crSize;
        EditObject(GlobalObject2D, FPickPosition);
        IsEntityDragged := true;
        hDrawing.CADViewport2D.Repaint;
        hDrawing.CadViewport2D.DrawObject2DWithRubber(GlobalObject2D, true);
      end;
    end;
  end;
end;

procedure TfrmMain.EditObject(AObject: TObject2D; APointNr: integer);
var TmpPrimitive2D: TPrimitive2D;   diffX, diffY: TRealType;
    hDrawing: TDrawing;
begin
  hDrawing := GetDrawingFromPage(fActivePage);
  if  hDrawing = nil then exit;
  if  AObject is TBlock2D then exit;
  TmpPrimitive2D := TPrimitive2D(AObject);
  TmpPrimitive2D.Points.DisableEvents := true;

  TmpPrimitive2D.Points[APointNr] := hDrawing.CADViewport2D.WorldToObject(TmpPrimitive2D, hDrawing.CADPrg2D.CurrentViewportSnappedPoint);

  TmpPrimitive2D.UpdateExtension(nil);
  TmpPrimitive2D.Points.DisableEvents := false;

  TIPropertyGrid1.Update;
  TIPropertyGrid1.Repaint;
  //IsEntityDragged := true;
end;

procedure TfrmMain.MoveObject(AObject: TObject2D);
var  hDrawing: TDrawing;  diffX, diffY: TRealType;
begin
  if (AObJect = nil) then exit;
  hDrawing := GetDrawingFromPage(fActivePage);
  if  hDrawing = nil then exit;

  diffX := hDrawing.CADPrg2D.CurrentViewportSnappedPoint.X - MoveBasePoint.X;
  diffY := hDrawing.CADPrg2D.CurrentViewportSnappedPoint.Y - MoveBasePoint.Y;

  AObject.Transform(Translate2D(diffX, diffY) );
  MoveBasePoint := hDrawing.CADPrg2D.CurrentViewportSnappedPoint;

  TIPropertyGrid1.Update;
  TIPropertyGrid1.Repaint;
end;

procedure TfrmMain.OnViewport2DMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var hDrawing: TDrawing;
begin
  hDrawing := GetDrawingFromPage(fActivePage);
  if  hDrawing = nil then exit;

  if  WheelDelta > 0 then
    hDrawing.CADViewport2D.ZoomIn
  else
     hDrawing.CADViewport2D.ZoomOut;
  hDrawing.RulerLeft.Repaint;
  hDrawing.RulerBottom.Repaint;
end;

procedure TfrmMain.SetPropertyEditorForObjects(AObject: TObject2D);
var hDrawing: TDrawing;
begin
  if (TIPropertyGrid1 <> nil) then
  begin
    {if (AObject is TLine2D) then
    begin
      CADSysLine2D.GraphicObject := TLine2D(GlobalObject2D);
      TIPropertyGrid1.TIObject := nil;
      TIPropertyGrid1.Clear;
      TIPropertyGrid1.TIObject := (CADSysLine2D);
      //TIPropertyGrid1.Update;
    end else
    if (AObject is TEllipticalArc2D) then
    begin
      CADSysEllipticalArc2D.GraphicObject := TEllipticalArc2D(GlobalObject2D);
      TIPropertyGrid1.TIObject := nil;
      TIPropertyGrid1.Clear;
      TIPropertyGrid1.TIObject := (CADSysEllipticalArc2D);
      //TIPropertyGrid1.Update;
    end else
    if (AObject is TPolygon2D) then
    begin
      CADSysPolygon2D.GraphicObject := TPolygon2D(GlobalObject2D);
      TIPropertyGrid1.TIObject := nil;
      TIPropertyGrid1.Clear;
      TIPropertyGrid1.TIObject := (CADSysPolygon2D);
      //TIPropertyGrid1.Update;
    end else
    if (AObject is TPolyLine2D) then
    begin
      CADSysPLine2D.GraphicObject := TPolyLine2D(GlobalObject2D);
      TIPropertyGrid1.TIObject := nil;
      TIPropertyGrid1.Clear;
      TIPropertyGrid1.TIObject := (CADSysPLine2D);
      //TIPropertyGrid1.Update;
    end else
    if (AObject is TEllipse2D) then
    begin
      CADSysEllipse2D.GraphicObject := TEllipse2D(GlobalObject2D);
      TIPropertyGrid1.TIObject := nil;
      TIPropertyGrid1.Clear;
      TIPropertyGrid1.TIObject := (CADSysEllipse2D);
      //TIPropertyGrid1.Update;
    end else
    if (AObject is TCircularArc2D) then
    begin
      CADSysCircularArc2D.GraphicObject := TCircularArc2D(GlobalObject2D);
      TIPropertyGrid1.TIObject := nil;
      TIPropertyGrid1.Clear;
      TIPropertyGrid1.TIObject := (CADSysCircularArc2D);
      //TIPropertyGrid1.Update;
    end else
    if (AObject is TCircle2D) then
    begin
      CADSysCircle2D.GraphicObject := TCircle2D(GlobalObject2D);
      TIPropertyGrid1.TIObject := nil;
      TIPropertyGrid1.Clear;
      TIPropertyGrid1.TIObject := (CADSysCircle2D);
      //TIPropertyGrid1.Update;
    end else
    if (AObject is TRectangle2D) then
    begin
      CADSysRectangle2D.GraphicObject := TRectangle2D(GlobalObject2D);
      TIPropertyGrid1.TIObject := nil;
      TIPropertyGrid1.Clear;
      TIPropertyGrid1.TIObject := (CADSysRectangle2D);
      //TIPropertyGrid1.Update;
    end else
    if (AObject is TFrame2D) then
    begin
      CADSysFrame2D.GraphicObject := TFrame2D(GlobalObject2D);
      TIPropertyGrid1.TIObject := nil;
      TIPropertyGrid1.Clear;
      TIPropertyGrid1.TIObject := (CADSysFrame2D);
      //TIPropertyGrid1.Update;
    end else
    if (AObject is TBSpline2D) then
    begin
      CADSysBSpline2D.GraphicObject := TBSpline2D(GlobalObject2D);
      TIPropertyGrid1.TIObject := nil;
      TIPropertyGrid1.Clear;
      TIPropertyGrid1.TIObject := (CADSysBSpline2D);
      //TIPropertyGrid1.Update;
    end else
    if (AObject is TSector2D) then
    begin
      CADSysSectort2D.GraphicObject := TSector2D(GlobalObject2D);
      TIPropertyGrid1.TIObject := nil;
      TIPropertyGrid1.Clear;
      TIPropertyGrid1.TIObject := (CADSysSectort2D);
    end else
    if (AObject is TSegment2D) then
    begin
      CADSysSegment2D.GraphicObject := TSegment2D(GlobalObject2D);
      TIPropertyGrid1.TIObject := nil;
      TIPropertyGrid1.Clear;
      TIPropertyGrid1.TIObject := (CADSysSegment2D);
    end else
    if (AObject is TClosedCurve2D) then
    begin
      CADSysClosedCurve2D.GraphicObject := TClosedCurve2D(GlobalObject2D);
      TIPropertyGrid1.TIObject := nil;
      TIPropertyGrid1.Clear;
      TIPropertyGrid1.TIObject := (CADSysClosedCurve2D);
      //TIPropertyGrid1.Update;
    end else
    if (AObject is TDirectionalCurve2D) then
    begin
      CADSysDirectionalCurve2D.DirectionalCurve2D := TDirectionalCurve2D(GlobalObject2D);
      TIPropertyGrid1.TIObject := nil;
      TIPropertyGrid1.Clear;
      TIPropertyGrid1.TIObject := (CADSysDirectionalCurve2D);
      //TIPropertyGrid1.Update;
    end else
    if (AObject is TContainer2D) then
    begin
      CADSysContainer2D.Container2D := TContainer2D(GlobalObject2D);
      TIPropertyGrid1.TIObject := nil;
      TIPropertyGrid1.Clear;
      TIPropertyGrid1.TIObject := CADSysContainer2D;
      //TIPropertyGrid1.Update;
    end else
    if (AObject is TClosedPolyline2D) then
    begin
      CADSysClosedPolyline2D.ClosedPolyline2D := TClosedPolyline2D(GlobalObject2D);
      TIPropertyGrid1.TIObject := nil;
      TIPropertyGrid1.Clear;
      TIPropertyGrid1.TIObject := (CADSysClosedPolyline2D);
      //TIPropertyGrid1.Update;
    end else

    if (AObject is TSimplePrimitive2D) then
    begin
      CADSysSimplePrimitive2D.GraphicObject := TSimplePrimitive2D(GlobalObject2D);
      TIPropertyGrid1.TIObject := nil;
      TIPropertyGrid1.Clear;
      TIPropertyGrid1.TIObject := (CADSysSimplePrimitive2D);
      //TIPropertyGrid1.Update;
    end else}
    if AObject is TGraphicObject then
    begin
      TIPropertyGrid1.TIObject := nil;
      TIPropertyGrid1.Clear;
      TIPropertyGrid1.TIObject := AObject;
    end else

    begin
      hDrawing := GetDrawingFromPage(fActivePage);
      ComponentDrawing.Drawing := hDrawing;
      SetupInspector(ComponentDrawing);
      FPickPosition := PICK_NOOBJECT;
      GlobalObject2D := nil;
    end;
    TIPropertyGrid1.Update;
  end;
end;

procedure TfrmMain.DisableControls;
var i: integer;
begin
   TIPropertyGrid1.TIObject := nil;
   TIPropertyGrid1.Repaint;

   StatusBarMain.Panels[0].Text := '';
   for i := 0 to ActionList.ActionCount - 1 do
     TAction(ActionList.Actions[i]).Enabled := false;

   acFileNew.Enabled  := true;
   acFileOpen.Enabled := true;
   acFileExit.Enabled := true;
end;

procedure TfrmMain.EnableControls;
var i: integer;
begin
  for i := 0 to ActionList.ActionCount - 1 do
    TAction(ActionList.Actions[i]).Enabled := true;
end;

procedure TfrmMain.SetupInspector(AControl: TPersistent);
begin
  TIPropertyGrid1.TIObject := nil;
  TIPropertyGrid1.TIObject := AControl;
  TIPropertyGrid1.Repaint;
end;

procedure TfrmMain.CloseCADFile(APage: TTabSheet);
var hDrawing: TDrawing;  TmpStr: TFileStream;
begin
  hDrawing := GetDrawingFromPage(fActivePage);
  if  hDrawing = nil then exit;

  if hDrawing.CADCmp2D.Layers.LayerByName[LAYER_STR_TEMPLATE] <> nil then
    hDrawing.CADCmp2D.Layers.LayerByName[LAYER_STR_TEMPLATE].Active := false;

  hDrawing.CADPrg2D.StopOperation;
  TIPropertyGrid1.TIObject := nil;
  TIPropertyGrid1.Repaint;
  GlobalObject2D := nil;

  case PageControl1.PageCount of
    0: exit;
    1: begin
        hDrawing.SaveBlockLibraryToFile(applicationh.fDefaultBlockLibrary);
        acSettingsUnLockTemplateLayer.Checked := false;
        DisableControls;
        fActivePage := nil;
        APage.Free;
        APage := nil;
      end
    else begin
      hDrawing.SaveBlockLibraryToFile(applicationh.fDefaultBlockLibrary);
      PageControl1.ActivePage := PageControl1.Pages[PageControl1.PageCount - 1];
      PageControl1Change(nil);
      fActivePage := PageControl1.ActivePage;
      hDrawing := GetDrawingFromPage(fActivePage);
      if hDrawing.CADCmp2D.Layers.LayerByName[LAYER_STR_TEMPLATE] <> nil then
        acSettingsUnLockTemplateLayer.Checked := hDrawing.CADCmp2D.Layers.LayerByName[LAYER_STR_TEMPLATE].Active;

      ComponentDrawing.Drawing := hDrawing;
      SetupInspector(ComponentDrawing);
      APage.Free;
      APage := nil;
    end;
  end;
  PageControl1.Refresh;
end;

procedure TfrmMain.acFileCloseExecute(Sender: TObject);
begin
  if PageControl1.PageCount > 0 then
    if CheckSave(fActivePage) then
      CloseCADFile(fActivePage);
  RegisterDocument;
end;

procedure TfrmMain.PageControl1CloseTabClicked(Sender: TObject);
begin
  acFileCloseExecute(nil);
  {if PageControl1.PageCount > 0 then
    if CheckSave(fActivePage) then
      CloseCADFile(fActivePage);
  }
end;

procedure TfrmMain.acFileCopyToClipboardExecute(Sender: TObject);
var hDrawing: TDrawing;  TmpColor: TColor;
begin
  hDrawing := GetDrawingFromPage(fActivePage);
  if  hDrawing = nil then exit;
  TmpColor :=  hDrawing.CADViewport2D.BackGroundColor;
  hDrawing.CADViewport2D.BackGroundColor := clWhite;
  hDrawing.CADViewport2D.CopyToClipboard(Clipboard);
  hDrawing.CADViewport2D.BackGroundColor := TmpColor;
end;

procedure TfrmMain.acExitProgramExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.acDrawLineExecute(Sender: TObject);
var TmpLine2D: TLine2D;   hDrawing: TDrawing;
begin
  if PageControl1.PageCount = 0 then exit;
  hDrawing := GetDrawingFromPage(fActivePage);
  if hDrawing <> nil then
  begin
    with hDrawing.CADPrg2D do
    begin
      if IsBusy then
        StopOperation;
      //fCurrentOpBtn := InsertLineBtn;
       TmpLine2D := TLine2D.Create(-1, Point2D(0, 0), Point2D(0, 0));
       StartOperation(TCAD2DDrawSizedPrimitive, TCAD2DDrawSizedPrimitiveParam.Create(nil,
          TmpLine2D, 0, True));
       //SetDefaultObjEntitys(TmpLine2D);
    end;
  end;
end;

procedure TfrmMain.acDrawPointExecute(Sender: TObject);
begin
  //
end;

procedure TfrmMain.acDrawPolygonExecute(Sender: TObject);
var TmpPolygon2D: TPolygon2D;   hDrawing: TDrawing;
begin
  if PageControl1.PageCount = 0 then exit;
  hDrawing := GetDrawingFromPage(fActivePage);
  if hDrawing <> nil then
  begin
    with hDrawing.CADPrg2D do
    begin
      if IsBusy then
      StopOperation;
      TmpPolygon2D := TPolygon2D.Create(-1, []);
     //fCurrentOpBtn := InsertPolylineBtn;
     StartOperation(TCAD2DDrawUnsizedPrimitive, TCAD2DDrawUnsizedPrimitiveParam.Create(nil,
        TmpPolygon2D, 0, True));
    end;
  end;
end;

procedure TfrmMain.acDrawPolyLineExecute(Sender: TObject);
var TmpPolyline2D: TPolyline2D;   hDrawing: TDrawing;
begin
  if PageControl1.PageCount = 0 then exit;
  hDrawing := GetDrawingFromPage(fActivePage);
  if hDrawing <> nil then
  begin
    with hDrawing.CADPrg2D do
    begin
      if IsBusy then
      StopOperation;
      TmpPolyline2D := TPolyline2D.Create(-1, []);
     //fCurrentOpBtn := InsertPolylineBtn;
     StartOperation(TCAD2DDrawUnsizedPrimitive, TCAD2DDrawUnsizedPrimitiveParam.Create(nil,
        TmpPolyline2D, 0, True));
    end;
  end;
end;

procedure TfrmMain.acDrawRectangle0Execute(Sender: TObject);
var TmpFrame2D: TFrame2D;   hDrawing: TDrawing;
begin
  if PageControl1.PageCount = 0 then exit;
  hDrawing := GetDrawingFromPage(fActivePage);
  if hDrawing <> nil then
  begin
    with hDrawing.CADPrg2D do
    begin
      if IsBusy then
      StopOperation;
      TmpFrame2D := TFrame2D.Create(-1, Point2D(0, 0), Point2D(0, 0));
      StartOperation(TCAD2DDrawSizedPrimitive, TCAD2DDrawSizedPrimitiveParam.Create(nil,
        TmpFrame2D, 0, True));
    end;
  end;
end;

procedure TfrmMain.acDrawSector2DExecute(Sender: TObject);
var TmpSector2D: TSector2D;   hDrawing: TDrawing;
begin
  if PageControl1.PageCount = 0 then exit;
  hDrawing := GetDrawingFromPage(fActivePage);
  if hDrawing <> nil then
  begin
    with hDrawing.CADPrg2D do
    begin
      if IsBusy then
      StopOperation;
      TmpSector2D := TSector2D.Create(-1, Point2D(0, 0), 0, 0, 0);
      StartOperation(TCAD2DDrawSizedPrimitive, TCAD2DDrawSizedPrimitiveParam.Create(nil, TmpSector2D, 0, false));
    end;
  end;
end;

procedure TfrmMain.acDrawSegment2DExecute(Sender: TObject);
var TmpSegment2D: TSegment2D;   hDrawing: TDrawing;
begin
  if PageControl1.PageCount = 0 then exit;
  hDrawing := GetDrawingFromPage(fActivePage);
  if hDrawing <> nil then
  begin
    with hDrawing.CADPrg2D do
    begin
      if IsBusy then
      StopOperation;
      TmpSegment2D := TSegment2D.Create(-1, Point2D(0, 0), 0, 0, 0);
     StartOperation(TCAD2DDrawSizedPrimitive, TCAD2DDrawSizedPrimitiveParam.Create(nil, TmpSegment2D, 0, false));
    end;
  end;
end;

procedure TfrmMain.acDrawSymetricSymbol2DExecute(Sender: TObject);
var TmpSymetricSymbol2D: TSymetricSymbol2D;   hDrawing: TDrawing;
begin
  if PageControl1.PageCount = 0 then exit;
  hDrawing := GetDrawingFromPage(fActivePage);
  if hDrawing <> nil then
  begin
    with hDrawing.CADPrg2D do
    begin
      if IsBusy then
      StopOperation;
      TmpSymetricSymbol2D := TSymetricSymbol2D.Create(-1, Point2D(0, 0), 0);
      TmpSymetricSymbol2D.CurvePrecision := 5;
     StartOperation(TCAD2DDrawSizedPrimitive, TCAD2DDrawSizedPrimitiveParam.Create(nil, TmpSymetricSymbol2D, 0, false));
    end;
  end;
end;

procedure TfrmMain.acToolsScripterExecute(Sender: TObject);
begin
  IDE.Visible := acToolsScripter.Checked;
end;

procedure TfrmMain.acDrawASymetricSymbol2DExecute(Sender: TObject);
var TmpASymetricSymbol2D: TASymetricSymbol2D;   hDrawing: TDrawing;
begin
  if PageControl1.PageCount = 0 then exit;
  hDrawing := GetDrawingFromPage(fActivePage);
  if hDrawing <> nil then
  begin
    with hDrawing.CADPrg2D do
    begin
      if IsBusy then
      StopOperation;
      TmpASymetricSymbol2D := TASymetricSymbol2D.Create(-1, Point2D(0, 0),  Point2D(0, 0));
      TmpASymetricSymbol2D.CurvePrecision := 4;
     StartOperation(TCAD2DDrawSizedPrimitive, TCAD2DDrawSizedPrimitiveParam.Create(nil, TmpASymetricSymbol2D, 0, false));
    end;
  end;
end;

procedure TfrmMain.acDrawSingleLineTextExecute(Sender: TObject);
var
  hDrawing: TDrawing;
  TmpText: TText2D;
  TmpStr: String;
  TmpH: TRealType;
begin
  if PageControl1.PageCount = 0 then exit;
  hDrawing := GetDrawingFromPage(fActivePage);
  if hDrawing = nil then exit;

  if (not FontDialog1.Execute) then
    exit;

  TmpStr := 'String' + #13#10 + 'Test';
  if not InputQuery('Add Text', 'String', TmpStr) then
   Exit;

  TmpText := TText2D.Create(0, Rect2D(1, 70, 99, 99), FontDialog1.Font.Height, TmpStr);
  TmpText.LogFont.FaceName := FontDialog1.Font.Name;
  TmpText.LogFont.Height   := FontDialog1.Font.Height;

  if (fsItalic in FontDialog1.Font.Style) then
    TmpText.LogFont.Italic := 1;
  if (fsStrikeOut in FontDialog1.Font.Style) then
    TmpText.LogFont.StrikeOut := 1;
  if (fsUnderline in FontDialog1.Font.Style) then
    TmpText.LogFont.Underline := 1;

  if (fsBold in FontDialog1.Font.Style) then
    TmpText.LogFont.Weight := 700
  else
    TmpText.LogFont.Weight := 400;

    hDrawing.CADPrg2D.StartOperation(TCAD2DPositionObject,
         TCAD2DPositionObjectParam.Create(nil, TmpText));

end;

procedure TfrmMain.acDrawSplineExecute(Sender: TObject);
var TmpBSpline2D: TBSpline2D;   hDrawing: TDrawing;
begin
  if PageControl1.PageCount = 0 then exit;
  hDrawing := GetDrawingFromPage(fActivePage);
  if hDrawing <> nil then
  begin
    with hDrawing.CADPrg2D do
    begin
      if IsBusy then
      StopOperation;
      TmpBSpline2D := TBSpline2D.Create(-1, []);
     //fCurrentOpBtn := InsertPolylineBtn;
     StartOperation(TCAD2DDrawUnsizedPrimitive, TCAD2DDrawUnsizedPrimitiveParam.Create(nil,
        TmpBSpline2D, 0, True));
    end;
  end;
end;

procedure TfrmMain.acDrawVectorialTextExecute(Sender: TObject);
var hDrawing: TDrawing;
    TmpText: TJustifiedVectText2D;
    TmpStr: String;
    TmpH: TRealType;
begin
  if PageControl1.PageCount = 0 then exit;
  hDrawing := GetDrawingFromPage(fActivePage);
  if hDrawing = nil then exit;

  if not InputQuery('Add Text', 'Height', TmpStr) then
   Exit;
  TmpH := StrToFloat(TmpStr);
  if not InputQuery('Add Text', 'String', TmpStr) then
   Exit;
  TmpText := TJustifiedVectText2D.Create(-1, CADSysFindFontByIndex(0), Rect2D(0, 0, 0, 0), TmpH, TmpStr);
  //if Left1.Checked then
   //TmpText.HorizontalJust := jhLeft
  //else if Right1.Checked then
   //TmpText.HorizontalJust := jhRight
  //else if Center1.Checked then
  TmpText.HorizontalJust := jhCenter;
  hDrawing.CADPrg2D.StartOperation(TCAD2DPositionObject, TCAD2DPositionObjectParam.Create(nil, TmpText));
end;

procedure TfrmMain.acEditCopyCBExecute(Sender: TObject);
var hDrawing: TDrawing; TmpPar: TCADPrgParam;
begin
  if PageControl1.PageCount = 0 then exit;
  hDrawing := GetDrawingFromPage(fActivePage);
  if hDrawing = nil then exit;

  if acCMDAreaSelectionMode.Checked then
  begin
    TmpPar := TCAD2DSelectObjectsInAreaParam.Create(gmAllInside, TCAD2DCopyObjectsToCADClipboard);
    TCAD2DSelectObjectsInAreaParam(TmpPar).OnObjectSelected := @OnSelectObj;
    hDrawing.CADPrg2D.StartOperation(TCAD2DSelectObjectsInArea, TmpPar);
  end else
  begin
    TmpPar := TCAD2DSelectObjectsParam.Create(5, TCAD2DCopyObjectsToCADClipboard);
    TCAD2DSelectObjectsParam(TmpPar).OnObjectSelected := @OnSelectObj;
    hDrawing.CADPrg2D.StartOperation(TCAD2DSelectObjects, TmpPar);
  end;
end;

procedure TfrmMain.acEditCopyExecute(Sender: TObject);
var hDrawing: TDrawing; TmpPar: TCADPrgParam;
begin
  if PageControl1.PageCount = 0 then exit;
  hDrawing := GetDrawingFromPage(fActivePage);
  if hDrawing = nil then exit;

  if acCMDAreaSelectionMode.Checked then
  begin
    TmpPar := TCAD2DSelectObjectsInAreaParam.Create(gmAllInside, TCAD2DCopyObjectsToCADClipboard);
    TCAD2DSelectObjectsInAreaParam(TmpPar).OnObjectSelected := @OnSelectObj;
    hDrawing.CADPrg2D.StartOperation(TCAD2DSelectObjectsInArea, TmpPar);
  end else
  begin
    TmpPar := TCAD2DSelectObjectsParam.Create(5, TCAD2DCopyObjectsToCADClipboard);
    TCAD2DSelectObjectsParam(TmpPar).OnObjectSelected := @OnSelectObj;
    hDrawing.CADPrg2D.StartOperation(TCAD2DSelectObjects, TmpPar);
  end;
end;

procedure TfrmMain.acEditCutExecute(Sender: TObject);
var hDrawing: TDrawing; TmpPar: TCADPrgParam;
begin
  if PageControl1.PageCount = 0 then exit;
  hDrawing := GetDrawingFromPage(fActivePage);
  if hDrawing = nil then exit;

  if acCMDAreaSelectionMode.Checked then
  begin
    TmpPar := TCAD2DSelectObjectsInAreaParam.Create(gmAllInside, TCAD2DCutObjects);
    //TCAD2DSelectObjectsInAreaParam(TmpPar).OnObjectSelected := @OnSelectObj;
    hDrawing.CADPrg2D.StartOperation(TCAD2DSelectObjectsInArea, TmpPar);
  end else
  begin
    TmpPar := TCAD2DSelectObjectsParam.Create(5, TCAD2DCutObjects);
    TCAD2DSelectObjectsParam(TmpPar).OnObjectSelected := @OnSelectObj;
    hDrawing.CADPrg2D.StartOperation(TCAD2DSelectObjects, TmpPar);
  end;
end;

procedure TfrmMain.acEditPasteExecute(Sender: TObject);
var hDrawing: TDrawing; TmpContainer2D: TContainer2D;
begin
  if PageControl1.PageCount = 0 then exit;
  hDrawing := GetDrawingFromPage(fActivePage);
  if hDrawing = nil then exit;

  if  CADClipboard2D = nil then
  begin
    ShowMessage('CAD Clipboard ist empty!');
    acEditPaste.Checked := false;
    exit;
  end;

  if (CADClipboard2D.Objects.Count > 0) then
  begin
    TmpContainer2D := TContainer2D.Create(-1, [nil]);
    TmpContainer2D.Assign(CADClipboard2D);
    hDrawing.CADPrg2D.StartOperation(TCAD2DPositionObject,
       TCAD2DPositionObjectParam.Create(nil, TmpContainer2D));
    //ExplodeContainer(TmpContainer2D);
  end;
end;

procedure TfrmMain.acEditRedoExecute(Sender: TObject);
var hDrawing: TDrawing;
begin
  if PageControl1.PageCount = 0 then exit;
  hDrawing := GetDrawingFromPage(fActivePage);
  if hDrawing = nil then exit;

  hDrawing.UndoRedo.Redo;
end;

procedure TfrmMain.acEditUndoExecute(Sender: TObject);
var hDrawing: TDrawing;
begin
  if PageControl1.PageCount = 0 then exit;
  hDrawing := GetDrawingFromPage(fActivePage);
  if hDrawing = nil then exit;
  SetPropertyEditorForObjects(nil);  //for Drawing
  GlobalObject2D := nil;
  hDrawing.UndoRedo.Undo;
end;

procedure TfrmMain.acCMDShowHideInspectorExecute(Sender: TObject);
begin
  pnlLeft.Visible := not pnlLeft.Visible;
end;

procedure TfrmMain.acCMDAreaSelectionModeExecute(Sender: TObject);
var hDrawing: TDrawing;
begin
  if PageControl1.PageCount = 0 then exit;
  hDrawing := GetDrawingFromPage(fActivePage);
  if hDrawing = nil then exit;
  if acCMDAreaSelectionMode.Checked then
  begin
    hDrawing.CADCmp2D.SelectionMode := smArea;
    acModifyMoveExecute(nil);
  end else
  begin
    hDrawing.CADPrg2D.SendUserEvent(CADPRG_CANCEL);
    acCMDSingleSelectionMode.Checked := true;
  end;
end;

procedure TfrmMain.acCMDSingleSelectionModeExecute(Sender: TObject);
var hDrawing: TDrawing;
begin
  if PageControl1.PageCount = 0 then exit;
  hDrawing := GetDrawingFromPage(fActivePage);
  if hDrawing = nil then exit;

  if acCMDSingleSelectionMode.Checked then
  begin
    hDrawing.CADCmp2D.SelectionMode := smSingle;
    hDrawing.CADPrg2D.SendUserEvent(CADPRG_CANCEL);
  end;
end;

procedure TfrmMain.acCMDExtendetSelectionModeExecute(Sender: TObject);
var hDrawing: TDrawing;
begin
  if PageControl1.PageCount = 0 then exit;
  hDrawing := GetDrawingFromPage(fActivePage);
  if hDrawing = nil then exit;
  if  acCMDExtendetSelectionMode.Checked then
  begin
    hDrawing.CADCmp2D.SelectionMode := smExtendet;
    //acCMDAreaSelectionMode.Checked := true;
    //acModifyMoveExecute(nil);
    //acCMDAreaSelectionModeExecute(nil);
  end else
    hDrawing.CADPrg2D.SendUserEvent(CADPRG_CANCEL);
end;


procedure TfrmMain.acDrawCircle0Execute(Sender: TObject);
var TmpCircle2D: TCircle2D;   hDrawing: TDrawing;
begin
  if PageControl1.PageCount = 0 then exit;
  hDrawing := GetDrawingFromPage(fActivePage);
  if hDrawing <> nil then
  begin
    with hDrawing.CADPrg2D do
    begin
      if IsBusy then
      StopOperation;
                                             // (ID: Longint; const CP: TPoint2D; R, SA, EA: TRealType);
      TmpCircle2D := TCircle2D.Create(-1, Point2D(0, 0), 0);
     //fCurrentOpBtn := InsertPolylineBtn;
     StartOperation(TCAD2DDrawSizedPrimitive, TCAD2DDrawSizedPrimitiveParam.Create(nil, TmpCircle2D, 0, false));
    end;
  end;
end;

procedure TfrmMain.acCMDAcceptExecute(Sender: TObject);
var hDrawing: TDrawing;
begin
  if PageControl1.PageCount = 0 then exit;
  hDrawing := GetDrawingFromPage(fActivePage);
  if hDrawing = nil then exit;
  hDrawing.CADPrg2D.SendUserEvent(CADPRG_ACCEPT);
end;

procedure TfrmMain.acBlocksExecute(Sender: TObject);
var frmLibraryBlocks: TfrmLibraryBlocks; hDrawing: TDrawing;
begin
  hDrawing := GetDrawingFromPage(fActivePage);
  if hDrawing = nil then exit;

  frmLibraryBlocks := TfrmLibraryBlocks.create(Self, hDrawing);
  frmLibraryBlocks.Show;
  //frmLibraryBlocks.Free;
end;

procedure IterateLibraryBlocks(ACADCmp2D: TCADCmp2D);
var hSourceBlock: TSourceBlock2D; hStr: string; TmpIter: TGraphicObjIterator;
begin
  TmpIter := ACADCmp2D.SourceBlocksIterator;
  ShowMessage(IntToStr(TmpIter.Count));
  hSourceBlock := TmpIter.First as TSourceBlock2D;
  try
    while hSourceBlock <> nil do
    begin
      hStr := BlockNameToString(hSourceBlock.Name);
      ShowMessage(hStr);
      hSourceBlock := TmpIter.Next as TSourceBlock2D;
    end;
  finally
    TmpIter.Free;
  end;
end;

procedure TfrmMain.acBlocksInsertExecute(Sender: TObject);
var hDrawing: TDrawing;  TmpStr: String; SrcBlk: TSourceBlock2D;
begin
  hDrawing := GetDrawingFromPage(fActivePage);
  if hDrawing = nil then exit;

  IterateLibraryBlocks(hDrawing.CADCmp2D);

  if not InputQuery('Add block', 'Name', TmpStr) then
    Exit;

  try
    SrcBlk := hDrawing.CADCmp2D.FindSourceBlock(StringToBlockName(TmpStr));
    if SrcBlk <> nil then
      hDrawing.CADPrg2D.StartOperation(TCAD2DPositionObject,
         TCAD2DPositionObjectParam.Create(nil, TBlock2D.Create(-1, SrcBlk)));
  except
    raise;
  end;
end;

procedure BresenhamDrawLine(X1, Y1, X2, Y2: TRealType; ACanvas: TCanvas; AColor: TColor; ASleepTime: word);
var HL, VL, L: TRealType; dX, dY, X, Y: Real; Count: Integer;
const hConst = 5;
begin
  HL := X2 - X1;
  VL := Y2 - Y1;
  L := Abs(HL) + Abs(VL);
  dX := HL / L;
  dY := VL / L;
  X := X1;
  Y := Y1;
  for Count := 0 to Round(L) do
  begin
    if CancelEmulation then exit;
    //ACanvas.Pixels[Round(X-1), Round(Y-1)] := AColor;
    //ACanvas.Pixels[Round(X), Round(Y)] := AColor;
    ACanvas.Ellipse(Round(X-hConst), Round(Y-hConst), Round(X+hConst), Round(Y+hConst));
    //ACanvas.Pixels[Round(X+1), Round(Y+1)] := AColor;
    if ASleepTime > 0 then
    begin
      //ACanvas.Ellipse(Round(X-hConst), Round(Y-hConst), Round(X+hConst), Round(Y+hConst));
      Sleep(ASleepTime);
    end;
    Application.ProcessMessages;
    X := X + dX;
    Y := Y + dY
  end;
end;

procedure SimulObJ2D(AObj2D: TObject2D; AViewport: TCADViewport2D; T: TTransf2D; AColor: TColor);
var i: integer; PointsSet2D: TPointsSet2D;   P0, P1: TPoint2D;   count: integer;
begin
  if  AObj2D is TOutline2D then
    PointsSet2D := TOutline2D(AObj2D).ProfilePoints
  else if  AObj2D is TLine2D then
    PointsSet2D := TLine2D(AObj2D).Points
  else exit;

  if  Assigned(PointsSet2D) then
  begin
    for i := 0 to   PointsSet2D.Count - 2 do
    begin
      if CancelEmulation then break;
      P0 := AViewport.ViewportToScreen(TransformPoint2D(PointsSet2D[i], AObj2D.ModelTransform));
      P1 := AViewport.ViewportToScreen(TransformPoint2D(PointsSet2D[i+1], AObj2D.ModelTransform));
      BresenhamDrawLine(P0.X, P0.Y, P1.X, P1.Y, AViewport.Canvas, AColor, 1);
    end;
    if AObj2D is TPolygon2D then
    begin
      P0 := P1;
      P1 := AViewport.ViewportToScreen(TransformPoint2D(PointsSet2D[0], AObj2D.ModelTransform));
      BresenhamDrawLine(P0.X, P0.Y, P1.X, P1.Y, AViewport.Canvas, AColor, 1);
    end;
  end;
end;

procedure TfrmMain.acCAMStartStopEmulExecute(Sender: TObject);
var hDrawing: TDrawing; TmpIter: TExclusiveGraphicObjIterator; P0, P1: TPoint2D;
    hColor: TColor;
begin
  if not acCAMStartStopEmul.Checked  then
  begin
    CancelEmulation := true;
    exit;
  end;

  if PageControl1.PageCount = 0 then exit;
  hDrawing := GetDrawingFromPage(fActivePage);
  if hDrawing = nil then exit;

  TmpIter := hDrawing.CADCmp2D.ObjectsExclusiveIterator;
  try
    CancelEmulation := false;
    hDrawing.CADViewport2D.Enabled := false;;
    //hColor := clBlue xor hDrawing.CADViewport2D.BackGroundColor;
    hColor := clBlue xor clBackground;
    P0 := Point2D(0, 0);
    P0 := hDrawing.CADViewport2D.ViewportToScreen(P0);
    TmpIter.First;
    while TmpIter.Current <> nil do
    begin
      if (TmpIter.Current.LayerName <> CAM_LAYER_STR_JUMPS) and (TmpIter.Current.LayerName <> LAYER_STR_TEMPLATE) then
      begin
        if TmpIter.Current is TLine2D then
          P1 := TLine2D(TmpIter.Current).Points[0]
        else
          P1 := TOutline2D(TmpIter.Current).ProfilePoints[0];
        P1 := hDrawing.CADViewport2D.ViewportToScreen(TransformPoint2D(P1, TObject2D(TmpIter.Current).ModelTransform));

        //RapidMove
        // hDrawing.CADViewport2D.Canvas.Brush.Color := clYellow;
        BresenhamDrawLine(P0.X, P0.Y, P1.X, P1.Y, hDrawing.CADViewport2D.Canvas, hColor, 1);
        //Primitive
        hDrawing.CADViewport2D.Canvas.Brush.Color := clRed xor hDrawing.CADViewport2D.BackGroundColor;
        SimulObJ2D(TObject2D(TmpIter.Current), hDrawing.CADViewport2D, TObject2D(TmpIter.Current).ModelTransform, hColor);

        if TmpIter.Current is TLine2D then P0 := TLine2D(TmpIter.Current).Points[1]
        else P0 := TOutline2D(TmpIter.Current).ProfilePoints[TOutline2D(TmpIter.Current).ProfilePoints.Count - 1];
        P0 := hDrawing.CADViewport2D.ViewportToScreen(TransformPoint2D(P0, TObject2D(TmpIter.Current).ModelTransform));
      end;
      TmpIter.Next;
    end;
  finally
    TmpIter.Free;
    hDrawing.CADViewport2D.Enabled := true;
    hDrawing.CADCmp2D.RefreshViewports;
    acCAMStartStopEmul.Checked := false;
  end;
end;

procedure TfrmMain.acClipperDifferenceExecute(Sender: TObject);
var hDrawing: TDrawing;
  TmpPar1: TCAD2DSelectObjectsParam;
  TmpPar2: TCAD2DSelectObjectsInAreaParam;
begin
  if PageControl1.PageCount = 0 then exit;
  hDrawing := GetDrawingFromPage(fActivePage);
  if hDrawing = nil then exit;

  if acCMDAreaSelectionMode.Checked then  // Use Area
  begin
    TmpPar2 := TCAD2DSelectObjectsInAreaParam.Create(gmCrossFrame, TCAD2DClipperDifferenceObjects);
    TmpPar2.OnObjectSelected := @OnSelectObj;
    hDrawing.CADPrg2D.StartOperation(TCAD2DSelectObjectsInArea, TmpPar2);
  end else
  begin
    TmpPar1 := TCAD2DSelectObjectsParam.Create(5, TCAD2DClipperDifferenceObjects);
    TmpPar1.OnObjectSelected := @OnSelectObj;
    hDrawing.CADPrg2D.StartOperation(TCAD2DSelectObjects, TmpPar1);
  end;
end;

procedure TfrmMain.acClipperIntersectionExecute(Sender: TObject);
var hDrawing: TDrawing;
  TmpPar1: TCAD2DSelectObjectsParam;
  TmpPar2: TCAD2DSelectObjectsInAreaParam;
begin
  if PageControl1.PageCount = 0 then exit;
  hDrawing := GetDrawingFromPage(fActivePage);
  if hDrawing = nil then exit;

  if acCMDAreaSelectionMode.Checked then  // Use Area
  begin
    TmpPar2 := TCAD2DSelectObjectsInAreaParam.Create(gmCrossFrame, TCAD2DClipperIntersectionObjects);
    TmpPar2.OnObjectSelected := @OnSelectObj;
    hDrawing.CADPrg2D.StartOperation(TCAD2DSelectObjectsInArea, TmpPar2);
  end else
  begin
    TmpPar1 := TCAD2DSelectObjectsParam.Create(5, TCAD2DClipperIntersectionObjects);
    TmpPar1.OnObjectSelected := @OnSelectObj;
    hDrawing.CADPrg2D.StartOperation(TCAD2DSelectObjects, TmpPar1);
  end;
end;

procedure TfrmMain.acClipperOffsetExecute(Sender: TObject);
var hDrawing: TDrawing;
  TmpPar1: TCAD2DSelectObjectsParam;
  TmpPar2: TCAD2DSelectObjectsInAreaParam;
begin
  if PageControl1.PageCount = 0 then exit;
  hDrawing := GetDrawingFromPage(fActivePage);
  if hDrawing = nil then exit;

  if acCMDAreaSelectionMode.Checked then  // Use Area
  begin
    TmpPar2 := TCAD2DSelectObjectsInAreaParam.Create(gmCrossFrame, TCAD2DClipperOffsetObjects);
    TmpPar2.OnObjectSelected := @OnSelectObj;
    hDrawing.CADPrg2D.StartOperation(TCAD2DSelectObjectsInArea, TmpPar2);
  end else
  begin
    TmpPar1 := TCAD2DSelectObjectsParam.Create(5, TCAD2DClipperOffsetObjects);
    TmpPar1.OnObjectSelected := @OnSelectObj;
    hDrawing.CADPrg2D.StartOperation(TCAD2DSelectObjects, TmpPar1);
  end;
end;

procedure TfrmMain.acClipperUnionExecute(Sender: TObject);
var hDrawing: TDrawing;
  TmpPar1: TCAD2DSelectObjectsParam;
  TmpPar2: TCAD2DSelectObjectsInAreaParam;
begin
  if PageControl1.PageCount = 0 then exit;
  hDrawing := GetDrawingFromPage(fActivePage);
  if hDrawing = nil then exit;

  if acCMDAreaSelectionMode.Checked then  // Use Area
  begin
    TmpPar2 := TCAD2DSelectObjectsInAreaParam.Create(gmCrossFrame, TCAD2DClipperUnionObjects);
    TmpPar2.OnObjectSelected := @OnSelectObj;
    hDrawing.CADPrg2D.StartOperation(TCAD2DSelectObjectsInArea, TmpPar2);
  end else
  begin
    TmpPar1 := TCAD2DSelectObjectsParam.Create(5, TCAD2DClipperUnionObjects);
    TmpPar1.OnObjectSelected := @OnSelectObj;
    hDrawing.CADPrg2D.StartOperation(TCAD2DSelectObjects, TmpPar1);
  end;
end;

procedure TfrmMain.acClipperXORExecute(Sender: TObject);
var hDrawing: TDrawing;
  TmpPar1: TCAD2DSelectObjectsParam;
  TmpPar2: TCAD2DSelectObjectsInAreaParam;
begin
  if PageControl1.PageCount = 0 then exit;
  hDrawing := GetDrawingFromPage(fActivePage);
  if hDrawing = nil then exit;

  if acCMDAreaSelectionMode.Checked then  // Use Area
  begin
    TmpPar2 := TCAD2DSelectObjectsInAreaParam.Create(gmCrossFrame, TCAD2DClipperXOrObjects);
    TmpPar2.OnObjectSelected := @OnSelectObj;
    hDrawing.CADPrg2D.StartOperation(TCAD2DSelectObjectsInArea, TmpPar2);
  end else
  begin
    TmpPar1 := TCAD2DSelectObjectsParam.Create(5, TCAD2DClipperXOrObjects);
    TmpPar1.OnObjectSelected := @OnSelectObj;
    hDrawing.CADPrg2D.StartOperation(TCAD2DSelectObjects, TmpPar1);
  end;
end;

procedure TfrmMain.acBlocksCreateExecute(Sender: TObject);
var hDrawing: TDrawing;
    TmpPar1: TCAD2DSelectObjectsParam;
    TmpPar2: TCAD2DSelectObjectsInAreaParam;   hResult: boolean;
begin
  hDrawing := GetDrawingFromPage(fActivePage);
  if hDrawing = nil then exit;

  if acCMDAreaSelectionMode.Checked then
  begin
    TmpPar2 := TCAD2DSelectObjectsInAreaParam.Create(gmAllInside, TCAD2DCreateSourceBlock);
    TmpPar2.OnObjectSelected := @OnSelectObj;
    hResult := hDrawing.CADPrg2D.StartOperation(TCAD2DSelectObjectsInArea, TmpPar2);
  end else
  begin
    TmpPar1 := TCAD2DSelectObjectsParam.Create(5, TCAD2DCreateSourceBlock);
    TmpPar1.OnObjectSelected := @OnSelectObj;
    hResult := hDrawing.CADPrg2D.StartOperation(TCAD2DSelectObjects, TmpPar1);
  end;
  if hResult then
    hDrawing.SaveBlockLibraryToFile(applicationh.fDefaultBlockLibrary);
end;

procedure TfrmMain.acAlignLeftExecute(Sender: TObject);
var hDrawing: TDrawing;
    TmpPar1: TCAD2DSelectObjectsParam;
    TmpPar2: TCAD2DSelectObjectsInAreaParam;
begin
  hDrawing := GetDrawingFromPage(fActivePage);
  if hDrawing = nil then exit;

  if acCMDAreaSelectionMode.Checked then
  begin
    TmpPar2 := TCAD2DSelectObjectsInAreaParam.Create(gmAllInside, TCAD2DAlignLeft);
    TmpPar2.OnObjectSelected := @OnSelectObj;
    hDrawing.CADPrg2D.StartOperation(TCAD2DSelectObjectsInArea, TmpPar2);
  end else
  begin
    TmpPar1 := TCAD2DSelectObjectsParam.Create(5, TCAD2DAlignLeft);
    TmpPar1.OnObjectSelected := @OnSelectObj;
    hDrawing.CADPrg2D.StartOperation(TCAD2DSelectObjects, TmpPar1);
  end;
end;

procedure TfrmMain.acAlignBottomExecute(Sender: TObject);
var hDrawing: TDrawing;
    TmpPar1: TCAD2DSelectObjectsParam;
    TmpPar2: TCAD2DSelectObjectsInAreaParam;
begin
  hDrawing := GetDrawingFromPage(fActivePage);
  if hDrawing = nil then exit;

  if acCMDAreaSelectionMode.Checked then
  begin
    TmpPar2 := TCAD2DSelectObjectsInAreaParam.Create(gmAllInside, TCAD2DAlignBottom);
    TmpPar2.OnObjectSelected := @OnSelectObj;
    hDrawing.CADPrg2D.StartOperation(TCAD2DSelectObjectsInArea, TmpPar2);
  end else
  begin
    TmpPar1 := TCAD2DSelectObjectsParam.Create(5, TCAD2DAlignBottom);
    TmpPar1.OnObjectSelected := @OnSelectObj;
    hDrawing.CADPrg2D.StartOperation(TCAD2DSelectObjects, TmpPar1);
  end;
end;

procedure TfrmMain.acAlignCenterExecute(Sender: TObject);
begin
end;

procedure TfrmMain.acAlignRightExecute(Sender: TObject);
var hDrawing: TDrawing;
    TmpPar1: TCAD2DSelectObjectsParam;
    TmpPar2: TCAD2DSelectObjectsInAreaParam;
begin
  hDrawing := GetDrawingFromPage(fActivePage);
  if hDrawing = nil then exit;

  if acCMDAreaSelectionMode.Checked then
  begin
    TmpPar2 := TCAD2DSelectObjectsInAreaParam.Create(gmAllInside, TCAD2DAlignRight);
    TmpPar2.OnObjectSelected := @OnSelectObj;
    hDrawing.CADPrg2D.StartOperation(TCAD2DSelectObjectsInArea, TmpPar2);
  end else
  begin
    TmpPar1 := TCAD2DSelectObjectsParam.Create(5, TCAD2DAlignRight);
    TmpPar1.OnObjectSelected := @OnSelectObj;
    hDrawing.CADPrg2D.StartOperation(TCAD2DSelectObjects, TmpPar1);
  end;
end;

procedure TfrmMain.acAlignTopExecute(Sender: TObject);
var hDrawing: TDrawing;
    TmpPar1: TCAD2DSelectObjectsParam;
    TmpPar2: TCAD2DSelectObjectsInAreaParam;
begin
  hDrawing := GetDrawingFromPage(fActivePage);
  if hDrawing = nil then exit;

  if acCMDAreaSelectionMode.Checked then
  begin
    TmpPar2 := TCAD2DSelectObjectsInAreaParam.Create(gmAllInside, TCAD2DAlignTop);
    TmpPar2.OnObjectSelected := @OnSelectObj;
    hDrawing.CADPrg2D.StartOperation(TCAD2DSelectObjectsInArea, TmpPar2);
  end else
  begin
    TmpPar1 := TCAD2DSelectObjectsParam.Create(5, TCAD2DAlignTop);
    TmpPar1.OnObjectSelected := @OnSelectObj;
    hDrawing.CADPrg2D.StartOperation(TCAD2DSelectObjects, TmpPar1);
  end;
end;

procedure TfrmMain.acCMDCancelExecute(Sender: TObject);
var hDrawing: TDrawing;
begin
  if PageControl1.PageCount = 0 then exit;
  hDrawing := GetDrawingFromPage(fActivePage);
  if hDrawing = nil then exit;
  hDrawing.CADPrg2D.SendUserEvent(CADPRG_CANCEL);
  acCMDAccept.Checked := true;
end;

procedure TfrmMain.UpdateUserInterface;
var hDrawing: TDrawing;
begin
  if PageControl1.PageCount = 0 then exit;
  hDrawing := GetDrawingFromPage(fActivePage);
  if hDrawing = nil then exit;

  acCMDShowGrid.Checked       := hDrawing.CADCmp2D.ShowGrid;
  acCMDUseOrtho.Checked       := hDrawing.CADCmp2D.UseOrtho;
  acCMDEditMode.Checked       := hDrawing.CADCmp2D.EditMode;
  acCMDUseSnap.Checked        := hDrawing.CADCmp2D.UseSnap;
  acCMDPolarTracking.Checked  := hDrawing.CADCmp2D.PolarTracking;

  case  hDrawing.CADCmp2D.SelectionMode of
    smSingle  : acCMDSingleSelectionMode.Checked   := true;
    smArea    : acCMDAreaSelectionMode.Checked     := true;
    smExtendet: acCMDExtendetSelectionMode.Checked := true;
  end;

  TIPropertyGrid1.Update;
  TIPropertyGrid1.Repaint;
  hDrawing.CADViewport2D.Repaint;
end;

procedure TfrmMain.acCMDShowGridExecute(Sender: TObject);
var hDrawing: TDrawing;
begin
  if PageControl1.PageCount = 0 then exit;
  hDrawing := GetDrawingFromPage(fActivePage);
  if hDrawing = nil then exit;
  //hDrawing.CADViewport2D.ShowGrid :=  acCMDShowGrid.Checked;
  hDrawing.CADCmp2D.ShowGrid :=  acCMDShowGrid.Checked;
  TIPropertyGrid1.Repaint;
  hDrawing.CADViewport2D.Refresh;
  hDrawing.CADViewport2D.SetFocus;
end;

procedure TfrmMain.acCMDUseOrthoExecute(Sender: TObject);
var hDrawing: TDrawing;
begin
  if PageControl1.PageCount = 0 then exit;
  hDrawing := GetDrawingFromPage(fActivePage);
  if hDrawing = nil then exit;

  hDrawing.CADPrg2D.UseOrto  := acCMDUseOrtho.Checked;
  hDrawing.CADCmp2D.UseOrtho := acCMDUseOrtho.Checked;
  TIPropertyGrid1.Repaint;
end;

procedure TfrmMain.acCMDUseSnapExecute(Sender: TObject);
var hDrawing: TDrawing;
begin
  if PageControl1.PageCount = 0 then exit;
  hDrawing := GetDrawingFromPage(fActivePage);
  if hDrawing = nil then exit;

  hDrawing.CADPrg2D.UseSnap := acCMDUseSnap.Checked;
  hDrawing.CADCmp2D.UseSnap := acCMDUseSnap.Checked;
  if not acCMDUseSnap.Checked then
    self.SetSnapOption(-1);
  TIPropertyGrid1.Repaint;
end;

procedure TfrmMain.acDimLinExecute(Sender: TObject);
var TmpDimLine2D: TDimension2D;   hDrawing: TDrawing;
begin
  if PageControl1.PageCount = 0 then exit;
  hDrawing := GetDrawingFromPage(fActivePage);
  if hDrawing <> nil then
  begin
    with hDrawing.CADPrg2D do
    begin
      if IsBusy then
        StopOperation;
      //fCurrentOpBtn := InsertLineBtn;
       TmpDimLine2D := TDimension2D.Create(-1, Point2D(0, 0), Point2D(0, 0));
       StartOperation(TCAD2DDrawSizedPrimitive, TCAD2DDrawSizedPrimitiveParam.Create(nil,
          TmpDimLine2D, 0, True));
       //SetDefaultObjEntitys(TmpLine2D);
    end;
  end;
end;

procedure TfrmMain.acCMDPolarTrackingExecute(Sender: TObject);
var hDrawing: TDrawing;
begin
  if PageControl1.PageCount = 0 then exit;
  hDrawing := GetDrawingFromPage(fActivePage);
  if hDrawing = nil then exit;

  //hDrawing.CADPrg2D.UseSnap := acCMDUseSnap.Checked;
  hDrawing.CADCmp2D.PolarTracking := acCMDPolarTracking.Checked;
  TIPropertyGrid1.Repaint;
end;

procedure TfrmMain.acCMDEditModeExecute(Sender: TObject);
var hDrawing: TDrawing;
begin
  if PageControl1.PageCount = 0 then exit;
  hDrawing := GetDrawingFromPage(fActivePage);
  if hDrawing = nil then exit;

  hDrawing.CADViewport2D.Enabled  := acCMDEditMode.Checked;
  hDrawing.CADCmp2D.EditMode      := acCMDEditMode.Checked;
  hDrawing.CADViewport2D.Enabled  := acCMDEditMode.Checked;
  TIPropertyGrid1.Repaint;
end;


procedure TfrmMain.acCMDShowHideCmdLineExecute(Sender: TObject);
begin
  pnlCMDLine.Visible := acCMDShowHideCmdLine.Checked;
end;

procedure TfrmMain.acDrawEllipticalArcExecute(Sender: TObject);
var TmpEllipticalArc2D: TEllipticalArc2D;   hDrawing: TDrawing;
begin
  if PageControl1.PageCount = 0 then exit;
  hDrawing := GetDrawingFromPage(fActivePage);
  if hDrawing <> nil then
  begin
    with hDrawing.CADPrg2D do
    begin
      if IsBusy then
      StopOperation;
      TmpEllipticalArc2D := TEllipticalArc2D.Create(-1, Point2D(0, 0), Point2D(0, 0), 0, 0);
     //fCurrentOpBtn := InsertPolylineBtn;
     StartOperation(TCAD2DDrawSizedPrimitive, TCAD2DDrawSizedPrimitiveParam.Create(nil,
        TmpEllipticalArc2D, 0, True));
    end;
  end;
end;

procedure TfrmMain.acDrawCircularArc2DExecute(Sender: TObject);
var TmpCircularArc2D: TCircularArc2D;   hDrawing: TDrawing;
begin
  if PageControl1.PageCount = 0 then exit;
  hDrawing := GetDrawingFromPage(fActivePage);
  if hDrawing <> nil then
  begin
    with hDrawing.CADPrg2D do
    begin
      if IsBusy then
      StopOperation;
                                             // (ID: Longint; const CP: TPoint2D; R, SA, EA: TRealType);
      TmpCircularArc2D := TCircularArc2D.Create(-1, Point2D(0, 0), 0, 0, 0);
     //fCurrentOpBtn := InsertPolylineBtn;
     StartOperation(TCAD2DDrawSizedPrimitive, TCAD2DDrawSizedPrimitiveParam.Create(nil, TmpCircularArc2D, 0, false));
    end;
  end;
end;

procedure TfrmMain.acDrawEllipseExecute(Sender: TObject);
var TmpEllipse2D: TEllipse2D;   hDrawing: TDrawing;
begin
  if PageControl1.PageCount = 0 then exit;
  hDrawing := GetDrawingFromPage(fActivePage);
  if hDrawing <> nil then
  begin
    with hDrawing.CADPrg2D do
    begin
      if IsBusy then
      StopOperation;
      TmpEllipse2D := TEllipse2D.Create(-1, Point2D(0, 0), Point2D(0, 0));
     //fCurrentOpBtn := InsertPolylineBtn;
     StartOperation(TCAD2DDrawSizedPrimitive, TCAD2DDrawSizedPrimitiveParam.Create(nil,
        TmpEllipse2D, 0, True));
    end;
  end;
end;

procedure TfrmMain.acFileExitExecute(Sender: TObject);
begin
  close;
end;

procedure TfrmMain.acFileImportEssiExecute(Sender: TObject);
var TabSheet: TTabSheet; hDrawing: TDrawing;  ImportEssi: TImportEssi;  TmpFilter: string;
begin
  TmpFilter :=  OpenCADFileDialog.Filter;
  OpenCADFileDialog.Filter := '*.*';
  if OpenCADFileDialog.Execute then
  try
    OpenESSIFile(OpenCADFileDialog.FileName);
  finally
    OpenCADFileDialog.Filter := TmpFilter;
  end;
end;

function  TfrmMain.TestCADSys4File(AFileName: string): boolean;
var TmpCADCmp2D: TCADCmp2D; var TabSheet: TTabSheet; hDrawing: TDrawing;
begin
  result := true;
  TmpCADCmp2D := TCADCmp2D.Create(nil);
  try
    TmpCADCmp2D.LoadFromFile(AFileName);
  except
    result := false;
    TmpCADCmp2D.Free;
    TmpCADCmp2D := nil;
    raise;
  end;
  if TmpCADCmp2D <> nil then
    TmpCADCmp2D.Free;
end;

procedure TfrmMain.OpenCS4File(AFileName: string);
var TabSheet: TTabSheet; hDrawing: TDrawing;
begin
  //if TestCADSys4File(AFileName) then
  //begin
    TabSheet := CreateNewDrawing;
    fActivePage := TabSheet;
    TabSheet.Caption := ExtractFileName(AFileName);
    hDrawing := GetDrawingFromPage(TabSheet);

    if  FileExists(applicationh.fDefaultBlockLibrary) then
      hDrawing.LoadBlockLibraryFromFile(applicationh.fDefaultBlockLibrary)
    else
      MessageDlg('The default BlocksLibrary file ' +
                   ' could be found.' + #13#10 +
                 'Please check the file paths in the LazCAD.ini file.', mtError, [mbOK], 0);

    hDrawing.LoadFromFile(AFileName);
    acSettingsUnLockTemplateLayer.Visible := hDrawing.CADCmp2D.Layers.LayerByName[LAYER_STR_TEMPLATE] <> nil;
    hDrawing.FileName := AFileName;
    hDrawing.CADViewport2D.ZoomToExtension;
    ComponentDrawing.Drawing := hDrawing;
    SetupInspector(ComponentDrawing);

    if PageControl1.PageCount = 1 then
      EnableControls;
    hDrawing.UndoRedo.UndoSave;
  //end;
end;

procedure TfrmMain.LoadDXFFile(AFileName: string; ACADCmp: TCADCmp2D);
var DXF2DImport: TDXF2DImport;
begin
  DXF2DImport := TDXF2DImport.Create(AFileName, ACADCmp);
  try
    DXF2DImport.SetTextFont(CADSysFindFontByIndex(3));
    DXF2DImport.ReadDXF;
  finally
    DXF2DImport.Free;
  end;
end;

procedure TfrmMain.OpenDXFFile(AFileName: string);
var TabSheet: TTabSheet; hDrawing: TDrawing; TmpColor: TColor;
begin
  TabSheet := CreateNewDrawing;
  fActivePage := TabSheet;
  TabSheet.Caption := ExtractFileName(AFileName);
  hDrawing := GetDrawingFromPage(TabSheet);

  LoadDXFFile(AFileName, hDrawing.CADCmp2D);
  acSettingsUnLockTemplateLayer.Visible := hDrawing.CADCmp2D.Layers.LayerByName[LAYER_STR_TEMPLATE] <> nil;

  if hDrawing.CADCmp2D.Layers[hDrawing.CADCmp2D.CurrentLayer].Pen.Color = clBlack
  then  hDrawing.CADCmp2D.BackgroundColor :=  clWhite
  else  hDrawing.CADCmp2D.BackgroundColor :=  clBlack;

  if  FileExists(applicationh.fDefaultBlockLibrary) then
    hDrawing.LoadBlockLibraryFromFile(applicationh.fDefaultBlockLibrary)
  else
    MessageDlg('The DefaultLibrary file could not be found.' + #13#10 +
               'Please check the file paths in the Inifile.', mtWarning, [mbOK], 0);

  hDrawing.CADViewport2D.ZoomToExtension;

  ComponentDrawing.Drawing := hDrawing;
  SetupInspector(ComponentDrawing);

  if PageControl1.PageCount = 1 then
    EnableControls;
  hDrawing.UndoRedo.UndoSave;
end;

procedure TfrmMain.OpenESSIFile(AFileName: string);
var TabSheet: TTabSheet; hDrawing: TDrawing;  ImportEssi: TImportEssi;  TmpFilter: string;
begin
  try
    TabSheet := CreateNewDrawing;
    fActivePage := TabSheet;
    TabSheet.Caption := ExtractFileName(AFileName);
    hDrawing := GetDrawingFromPage(TabSheet);

    ImportEssi := TImportEssi.create(hDrawing.CADCmp2D, hDrawing.CADViewport2D, ProgressBarMain);
    ImportEssi.LoadFromFile(AFileName);
    ImportEssi.ImportMoves := false;
    ImportEssi.Import;

    if  FileExists(applicationh.fDefaultBlockLibrary) then
      hDrawing.LoadBlockLibraryFromFile(applicationh.fDefaultBlockLibrary)
    else
      MessageDlg('The DefaultLibrary file could not be found.' + #13#10 +
                 'Please check the file paths in the LazCAD.ini file.', mtWarning, [mbOK], 0);

    acSettingsUnLockTemplateLayer.Visible := hDrawing.CADCmp2D.Layers.LayerByName[LAYER_STR_TEMPLATE] <> nil;
    hDrawing.FileName := AFileName;
    hDrawing.CADViewport2D.ZoomToExtension;
    ComponentDrawing.Drawing := hDrawing;
    SetupInspector(ComponentDrawing);

    if PageControl1.PageCount = 1 then
      EnableControls;
    hDrawing.UndoRedo.UndoSave;
  finally
    ImportEssi.Free;
  end;
end;

procedure TfrmMain.acFileOpenExecute(Sender: TObject);
var hExt: string; hDrawing: TDrawing;
begin
  OpenCADFileDialog.InitialDir := applicationh.GetAppDrawingsPath;

  if OpenCADFileDialog.Execute then
  begin
    IsEntityDragged := false;

    ProgressBarMain.Position := 0;
    //ProgressBarMain.Visible := true;
    Application.ProcessMessages;
    hExt := ExtractFileExt(OpenCADFileDialog.FileName);
    if      LowerCase(hExt) = '.cs4' then OpenCS4File(OpenCADFileDialog.FileName)
    else if LowerCase(hExt) = '.dxf' then OpenDXFFile(OpenCADFileDialog.FileName)
    else if LowerCase(hExt) = '.dat' then OpenESSIFile(OpenCADFileDialog.FileName);
    //ProgressBarMain.Visible := false;

    Application.ProcessMessages;
    ProgressBarMain.Position := 0;
    UpdateUserInterface;
    RegisterDocument;
  end;
end;

procedure TfrmMain.acFileNewExecute(Sender: TObject);
var TabSheet: TTabSheet;   hDrawing: TDrawing;
begin
  //Save the Library
  if PageControl1.PageCount > 0 then
  begin
    hDrawing := GetDrawingFromPage(fActivePage);
    if hDrawing <> nil then
      hDrawing.SaveBlockLibraryToFile(applicationh.fDefaultBlockLibrary);
  end;

  IsEntityDragged := false;

  SetupInspector(nil);
  TabSheet := CreateNewDrawing;
  fActivePage := TabSheet;
  TabSheet.Name := 'Noname' + IntToStr(fMDICount);
  inc(fMDICount);
  TabSheet.Caption := TabSheet.Name;
  hDrawing := GetDrawingFromPage(TabSheet);
  if hDrawing = nil then  exit;

  if  FileExists(applicationh.fDefaultBlockLibrary) then
    hDrawing.LoadBlockLibraryFromFile(applicationh.fDefaultBlockLibrary)
  else
    MessageDlg('The default block library ' + applicationh.fDefaultBlockLibrary + ' does not exist.' + #13#10 +
           'Please change it or disable it' + #13#10 +
           'via the Settings/BlockLibrary menu.', mtWarning, [mbOK], 0);


  if LowerCase(applicationh.fUseTemplates) = 'yes' then
  begin
     applicationh.ReadIniFile;
    if FileExists(fStdTemplate) then
    begin
      if   LowerCase(ExtractFileExt(fStdTemplate)) = '.dxf' then
        LoadDXFFile(applicationh.fStdTemplate, hDrawing.CADCmp2D)
      else
        hDrawing.CADCmp2D.LoadFromFile(applicationh.fStdTemplate);
    end else
    MessageDlg('The default template file ' + fStdTemplate + ' does not exist.' + #13#10 +
               'Please change it or disable it' + #13#10 +
               'via the Settings/Template menu.', mtWarning, [mbOK], 0);
  end;

  acSettingsUnLockTemplateLayer.Visible := hDrawing.CADCmp2D.Layers.LayerByName[LAYER_STR_TEMPLATE] <> nil;
  //hDrawing.CADViewport2D.ZoomWindow(Rect2D(-20, -20, 480, 210));
  hDrawing.FileName := TabSheet.Name;

  ComponentDrawing.Drawing := hDrawing;
  SetupInspector(ComponentDrawing);
  if PageControl1.PageCount = 1 then
    EnableControls;

  hDrawing.UndoRedo.UndoSave;

  UpdateUserInterface;
  RegisterDocument;
end;

procedure SaveCS4File(var ADrawing: TDrawing;  AFileName: string);
begin
  ADrawing.SaveToFile(AFileName);
  ADrawing.FileName := AFileName;
    //fActivePage.Caption := ExtractFileName(SaveCADSys4Dialog.FileName);
  ADrawing.Changed := false;
end;

procedure SaveDXFFile(var ADrawing: TDrawing;  AFileName: string);
var DXFExport: TDXF2DExport;
begin
  DXFExport := TDXF2DExport.Create(AFileName, ADrawing.CADCmp2D);
  try
    DXFExport.WriteDXF;
    ADrawing.FileName := AFileName;
    ADrawing.Changed := false;
  finally
    DXFExport.Free;
  end;
end;

procedure SaveESSIFile(var ADrawing: TDrawing;  AFileName: string);
begin
  {ADrawing.SaveToFile(AFileName);
  ADrawing.FileName := AFileName;
  fActivePage.Caption := ExtractFileName(SaveCADSys4Dialog.FileName);
  }
end;


procedure TfrmMain.acFileSaveExecute(Sender: TObject);
var hDrawing: TDrawing;   hExt: string; hFileName: string;
begin
  if PageControl1.PageCount = 0 then exit;
  hDrawing := GetDrawingFromPage(fActivePage);
  if  hDrawing = nil then exit;

  if FileExists(hDrawing.FileName) then
    hFileName := hDrawing.FileName
  else  begin
    SaveCADFileDialog.InitialDir := applicationh.GetAppDrawingsPath;
    if SaveCADFileDialog.Execute then
    begin
      hFileName := SaveCADFileDialog.FileName;
      hDrawing.SaveBlockLibraryToFile(applicationh.fDefaultBlockLibrary);
    end else
      exit;
  end;

  hExt := LowerCase(ExtractFileExt(hFileName));

  if      hExt = '.cs4' then SaveCS4File(hDrawing,  hFileName)
  else if hExt = '.dxf' then SaveDXFFile(hDrawing,  hFileName)
  else if hExt = '.dat' then SaveESSIFile(hDrawing, hFileName);

  fActivePage.Caption := ExtractFileName(hFileName);

  TIPropertyGrid1.Update;
  TIPropertyGrid1.Repaint;
end;

procedure TfrmMain.acFileSaveAsExecute(Sender: TObject);
var hDrawing: TDrawing;  hExt: string; hFileName: string;
begin
  if PageControl1.PageCount = 0 then exit;
  hDrawing := GetDrawingFromPage(fActivePage);
  if  hDrawing = nil then exit;

  SaveCADFileDialog.InitialDir := applicationh.GetAppDrawingsPath;
  If SaveCADFileDialog.Execute then
  begin
    hExt := LowerCase(ExtractFileExt(SaveCADFileDialog.FileName));
    if      hExt = '.cs4' then SaveCS4File(hDrawing,  SaveCADFileDialog.FileName)
    else if hExt = '.dxf' then SaveDXFFile(hDrawing,  SaveCADFileDialog.FileName)
    else if hExt = '.dat' then SaveESSIFile(hDrawing, SaveCADFileDialog.FileName);

    fActivePage.Caption := ExtractFileName(SaveCADFileDialog.FileName);
  end;
end;


procedure TfrmMain.acFormatLayersExecute(Sender: TObject);
var hDrawing: TDrawing;
begin
  if PageControl1.PageCount = 0 then exit;
  hDrawing := GetDrawingFromPage(fActivePage);
  if  hDrawing = nil then exit;

  frmLayers := TfrmLayers.Create(Self);
  try
    frmLayers.Execute(hDrawing.CADCmp2D);
  finally
    frmLayers.Free;
    hDrawing.CADViewport2D.Repaint;
  end;
end;

procedure TfrmMain.acHelpAboutExecute(Sender: TObject);
var frmAbout: TfrmAbout;
begin
  frmAbout := TfrmAbout.Create(self);
  frmAbout.ShowModal;
end;

procedure TfrmMain.acHelpHtmlExecute(Sender: TObject);
var frmHelpHtml: TfrmHelpHtml; HtmlIndexFile: string;
begin
  fIniFile := TIniFile.Create(fIniFileName);
  applicationh.fLanguage := applicationh.fIniFile.ReadString('UserInterface',  'Language', 'fr');
  HtmlIndexFile := applicationh.GetAppDocPath + applicationh.fLanguage + '/help/html/index.html';
  try
    if not FileExists(HtmlIndexFile) then
    begin
      ShowMessage('The file "' + HtmlIndexFile + '" was not found. Please check your installation.');
      Exit;
    end;

    {$IFDEF WINDOWS}
    try
      ShellExecute(0, 'open', PChar(HtmlIndexFile), nil, nil, SW_SHOWNORMAL);
    except
      ShowMessage('Failed to open the browser. Please ensure a default browser is installed and configured.');
    end;
    {$ELSE}
    if FileExists('/usr/bin/xdg-open') then
    begin
      try
        ExecuteProcess('/usr/bin/xdg-open', [HtmlIndexFile], []);
      except
        ShowMessage('Failed to open the browser. Please ensure a default browser is installed and configured.');
      end;
    end
    {$IFDEF DARWIN}
    else if FileExists('/usr/bin/open') then
    begin
      try
        ExecuteProcess('/usr/bin/open', [HtmlIndexFile], []);
      except
        ShowMessage('Failed to open the browser. Please ensure a default browser is installed and configured.');
      end;
    end
    {$ENDIF}
    else
    begin
      ShowMessage('No browser detected. Please ensure a default browser is installed and configured.');
    end;
    {$ENDIF}
  finally
    //frmHelpHtml.Free;
    fIniFile.Free;
    fIniFile := nil;
  end;
end;

procedure TfrmMain.acInsertRasterImageExecute(Sender: TObject);
var hDrawing: TDrawing; TmpBmp: TBitmap; TmpObject2D: TObject2D;
begin
  hDrawing := GetDrawingFromPage(fActivePage);
  if hDrawing = nil then exit;

  if OpenPictureDialog1.Execute then
   begin
     TmpBmp := TBitmap.Create;
     try
       TmpBmp.LoadFromFile(OpenPictureDialog1.FileName);
       TmpObject2D := TBitmap2D.Create(-1, Point2D(0, 0), Point2D(100, 100), TmpBmp);
       hDrawing.CADPrg2D.StartOperation(TCAD2DPositionObject, TCAD2DPositionObjectParam.Create(nil, TmpObject2D));
       hDrawing.CADViewport2D.Repaint;
     finally
       TmpBmp.Free;
     end;
   end;
end;

procedure TfrmMain.acCAMShowHideRapidMovesExecute(Sender: TObject);
var hDrawing: TDrawing; TmpIter: TExclusiveGraphicObjIterator; P0, P1: TPoint2D; hColor: TColor;
begin
  if PageControl1.PageCount = 0 then exit;
  hDrawing := GetDrawingFromPage(fActivePage);
  if hDrawing = nil then exit;

  hDrawing.CADCmp2D.ClearLayer(CAM_LAYER_ID_JUMPS);

  hColor := clBlue; // xor clBackground;
  P0.X := 0;
  P0.Y := 0;
  p0.W := 1;
  P1.X := 0;
  P1.Y := 0;
  P1.W := 1;
  TmpIter := hDrawing.CADCmp2D.ObjectsExclusiveIterator;
  try
    TmpIter.First;
    while TmpIter.Current <> nil do
    begin
      if (TmpIter.Current.LayerName <> CAM_LAYER_STR_JUMPS) and (TmpIter.Current.LayerName <> LAYER_STR_TEMPLATE)  then
      begin
        if TmpIter.Current is TOutline2D then
          P1 := TransformPoint2D(TOutline2D(TmpIter.Current).ProfilePoints[0], TOutline2D(TmpIter.Current).ModelTransform)
        else
          P1 :=TransformPoint2D(TPrimitive2D(TmpIter.Current).Points[0], TPrimitive2D(TmpIter.Current).ModelTransform);
        P0 := hDrawing.CADViewport2D.ViewportToScreen(P0);
        P1 := hDrawing.CADViewport2D.ViewportToScreen(P1);
        BresenhamDrawLine(P0.X, P0.Y, P1.X, P1.Y, hDrawing.CADViewport2D.Canvas, hColor, 0);

        if TmpIter.Current is TOutline2D then
          P0 := TransformPoint2D(TOutline2D(TmpIter.Current).ProfilePoints[TOutline2D(TmpIter.Current).ProfilePoints.Count - 1], TOutline2D(TmpIter.Current).ModelTransform)
        else
          P0 :=TransformPoint2D(TPrimitive2D(TmpIter.Current).Points[TOutline2D(TmpIter.Current).Points.Count - 1], TPrimitive2D(TmpIter.Current).ModelTransform);
        end;
      TmpIter.Next;
    end;
  finally
    TmpIter.Free;
  end;
end;

procedure TfrmMain.acCAMShowHideJumpsExecute(Sender: TObject);
var hDrawing: TDrawing; TmpIter1: TGraphicObjIterator; P0, P1: TPoint2D;
    TmpLine2D: TLine2D;  hPenStyle: TPenStyle;  hPenColor: TColor;
    TmpLayer: word;     TmpGraphicObjList: TGraphicObjList;  i: integer;
begin
  if PageControl1.PageCount = 0 then exit;
  hDrawing := GetDrawingFromPage(fActivePage);
  if hDrawing = nil then exit;

  hDrawing.CADCmp2D.ClearLayer(CAM_LAYER_ID_JUMPS);
  if (not acCAMShowHideJumps.Checked) then
   begin
     hDrawing.CADViewport2D.Repaint;
     exit;
   end;

  P0.X := 0;
  P0.Y := 0;
  p0.W := 1;
  P1.X := 0;
  P1.Y := 0;
  P1.W := 1;

  TmpLayer := hDrawing.CADCmp2D.CurrentLayer;
  hDrawing.CADCmp2D.CurrentLayer := CAM_LAYER_ID_JUMPS;

  TmpGraphicObjList := TGraphicObjList.Create;

  TmpIter1 := hDrawing.CADCmp2D.ObjectsIterator;
  try
    TmpIter1.First;
    while TmpIter1.Current <> nil do
    begin
      if (TmpIter1.Current.LayerName <> CAM_LAYER_STR_JUMPS) and (TmpIter1.Current.LayerName <> LAYER_STR_TEMPLATE)  then
      begin
        if TmpIter1.Current is TOutline2D then
          P1 := TransformPoint2D(TOutline2D(TmpIter1.Current).ProfilePoints[0], TOutline2D(TmpIter1.Current).ModelTransform)
        else
          P1 :=TransformPoint2D(TPrimitive2D(TmpIter1.Current).Points[0], TPrimitive2D(TmpIter1.Current).ModelTransform);

        TmpLine2D := TLine2D.Create(-1, P0, P1);
        TmpGraphicObjList.Add(TmpLine2D);

        if TmpIter1.Current is TOutline2D then
          P0 := TransformPoint2D(TOutline2D(TmpIter1.Current).ProfilePoints[TOutline2D(TmpIter1.Current).ProfilePoints.Count - 1], TOutline2D(TmpIter1.Current).ModelTransform)
        else
          P0 :=TransformPoint2D(TPrimitive2D(TmpIter1.Current).Points[TOutline2D(TmpIter1.Current).Points.Count - 1], TPrimitive2D(TmpIter1.Current).ModelTransform);
      end;
      TmpIter1.Next;
    end;
    finally
      TmpIter1.Free;
      TmpIter1 := nil;
    end;

    TmpIter1 := TmpGraphicObjList.GetIterator;
    try
      TmpIter1.First;
      while TmpIter1.Current <> nil do
      begin
        TmpLine2D := TLine2D.Create(-1, P0, P1);
        TmpLine2D.Assign(TLine2D(TmpIter1.Current));
        hDrawing.CADCmp2D.AddObject(-1, TmpLine2D);
        TmpLine2D.ShowDirection := true;
        TmpIter1.Next;
      end;
    finally
      TmpIter1.Free;
      TmpIter1 := nil;
      TmpGraphicObjList.RemoveAllIterators;
      TmpGraphicObjList.Free;
      TmpGraphicObjList := nil;
    end;

    hDrawing.CADCmp2D.CurrentLayer := TmpLayer;
    hDrawing.CADViewport2D.Repaint;
end;

procedure TfrmMain.acModifyEditExecute(Sender: TObject);
var hDrawing: TDrawing;
begin
  if PageControl1.PageCount = 0 then exit;
  hDrawing := GetDrawingFromPage(fActivePage);
  if hDrawing = nil then exit;

  hDrawing.CADPrg2D.StartOperation(TCAD2DSelectObject, TCAD2DSelectObjectsParam.Create(5, TCAD2DEditSelectedPrimitive));
end;

procedure TfrmMain.acModifyEraseExecute(Sender: TObject);
var hDrawing: TDrawing; TmpPar: TCADPrgParam;
begin
  if PageControl1.PageCount = 0 then exit;
  hDrawing := GetDrawingFromPage(fActivePage);
  if hDrawing = nil then exit;
  case hDrawing.CADCmp2D.SelectionMode of
    smSingle: begin
      TmpPar := TCAD2DSelectObjectsParam.Create(5, TCAD2DDeleteObjects);
      TCAD2DSelectObjectsParam(TmpPar).OnObjectSelected := @OnSelectObj;
      hDrawing.CADPrg2D.StartOperation(TCAD2DSelectObjects, TmpPar);
    end;
    smArea: begin
      TmpPar := TCAD2DSelectObjectsInAreaParam.Create(gmAllInside, TCAD2DDeleteObjects);
      TCAD2DSelectObjectsInAreaParam(TmpPar).OnObjectSelected := @OnSelectObj;
      hDrawing.CADPrg2D.StartOperation(TCAD2DSelectObjectsInArea, TmpPar);
    end;
    smExtendet: begin
      // single is temporary. extended is being implemented

      TmpPar := TCAD2DSelectObjectsParam.Create(5, TCAD2DDeleteObjects);
      TCAD2DSelectObjectsParam(TmpPar).OnObjectSelected := @OnSelectObj;
      //hDrawing.CADPrg2D.StartOperation(TCAD2DSelectObjects, TmpPar);
      hDrawing.CADPrg2D.StartOperation(TCAD2DExtendedSelectObjects, TmpPar);
    end;
  end;
end;

procedure TfrmMain.acModifyScaleExecute(Sender: TObject);
var hDrawing: TDrawing; TmpPar: TCADPrgParam;
begin
  if PageControl1.PageCount = 0 then exit;
  hDrawing := GetDrawingFromPage(fActivePage);
  if hDrawing = nil then exit;

  if acCMDAreaSelectionMode.Checked then
  begin
    TmpPar := TCAD2DSelectObjectsInAreaParam.Create(gmAllInside, TCAD2DScaleObjects);
    TCAD2DSelectObjectsInAreaParam(TmpPar).OnObjectSelected := @OnSelectObj;
    hDrawing.CADPrg2D.StartOperation(TCAD2DSelectObjectsInArea, TmpPar);
  end else
  begin
    TmpPar := TCAD2DSelectObjectsParam.Create(5, TCAD2DScaleObjects);
    TCAD2DSelectObjectsParam(TmpPar).OnObjectSelected := @OnSelectObj;
    hDrawing.CADPrg2D.StartOperation(TCAD2DSelectObjects, TmpPar);
  end;
end;

procedure TfrmMain.acModifyExplodeExecute(Sender: TObject);
var hDrawing: TDrawing; TmpPar: TCADPrgParam;
begin
  if PageControl1.PageCount = 0 then exit;
  hDrawing := GetDrawingFromPage(fActivePage);
  if hDrawing = nil then exit;

  if acCMDAreaSelectionMode.Checked then
  begin
    TmpPar := TCAD2DSelectObjectsInAreaParam.Create(gmAllInside, TCAD2DExplodeObjects);
    TCAD2DSelectObjectsInAreaParam(TmpPar).OnObjectSelected := @OnSelectObj;
    hDrawing.CADPrg2D.StartOperation(TCAD2DSelectObjectsInArea, TmpPar);
  end else
  begin
    TmpPar := TCAD2DSelectObjectsParam.Create(5, TCAD2DExplodeObjects);
    TCAD2DSelectObjectsParam(TmpPar).OnObjectSelected := @OnSelectObj;
    hDrawing.CADPrg2D.StartOperation(TCAD2DSelectObjects, TmpPar);
  end;
end;

procedure TfrmMain.acModifyMirrorXExecute(Sender: TObject);
var hDrawing: TDrawing; TmpPar: TCADPrgParam;
begin
  if PageControl1.PageCount = 0 then exit;
  hDrawing := GetDrawingFromPage(fActivePage);
  if hDrawing = nil then exit;

  if acCMDAreaSelectionMode.Checked then
  begin
    TmpPar := TCAD2DSelectObjectsInAreaParam.Create(gmAllInside, TCAD2DMirrorXObjects);
    TCAD2DSelectObjectsInAreaParam(TmpPar).OnObjectSelected := @OnSelectObj;
    hDrawing.CADPrg2D.StartOperation(TCAD2DSelectObjectsInArea, TmpPar);
  end else
  begin
    TmpPar := TCAD2DSelectObjectsParam.Create(5, TCAD2DMirrorXObjects);
    TCAD2DSelectObjectsParam(TmpPar).OnObjectSelected := @OnSelectObj;
    hDrawing.CADPrg2D.StartOperation(TCAD2DSelectObjects, TmpPar);
  end;
end;

procedure TfrmMain.acModifyMirrorYExecute(Sender: TObject);
var hDrawing: TDrawing; TmpPar: TCADPrgParam;
begin
  if PageControl1.PageCount = 0 then exit;
  hDrawing := GetDrawingFromPage(fActivePage);
  if hDrawing = nil then exit;

  if acCMDAreaSelectionMode.Checked then
  begin
    TmpPar := TCAD2DSelectObjectsInAreaParam.Create(gmAllInside, TCAD2DMirrorYObjects);
    TCAD2DSelectObjectsInAreaParam(TmpPar).OnObjectSelected := @OnSelectObj;
    hDrawing.CADPrg2D.StartOperation(TCAD2DSelectObjectsInArea, TmpPar);
  end else
  begin
    TmpPar := TCAD2DSelectObjectsParam.Create(5, TCAD2DMirrorYObjects);
    TCAD2DSelectObjectsParam(TmpPar).OnObjectSelected := @OnSelectObj;
    hDrawing.CADPrg2D.StartOperation(TCAD2DSelectObjects, TmpPar);
  end;
end;

procedure TfrmMain.acModifyMoveExecute(Sender: TObject);
var TmpPar: TCADPrgParam;  hDrawing: TDrawing;  i: integer;   TmpIter: TExclusiveGraphicObjIterator;
begin
  if PageControl1.PageCount = 0 then exit;
  hDrawing := GetDrawingFromPage(fActivePage);
  if hDrawing = nil then exit;

  if acCMDAreaSelectionMode.Checked then
   begin
     TmpPar := TCAD2DSelectObjectsInAreaParam.Create(gmAllInside, TCAD2DMoveSelectedObjects);
     //TCAD2DSelectObjectsInAreaParam(TmpPar).OnObjectSelected := @OnSelectedObj;
     hDrawing.CADPrg2D.StartOperation(TCAD2DSelectObjectsInArea, TmpPar);
   end else
   begin
     TmpPar := TCAD2DSelectObjectsParam.Create(5, TCAD2DMoveSelectedObjects);
     TCAD2DSelectObjectsParam(TmpPar).OnObjectSelected := @OnSelectObj;
     hDrawing.CADPrg2D.StartOperation(TCAD2DSelectObjects, TmpPar);
   end;
end;

procedure TfrmMain.acModifyOffsetExecute(Sender: TObject);
var TmpPar: TCADPrgParam; hDrawing: TDrawing;
begin
  if PageControl1.PageCount = 0 then exit;
  hDrawing := GetDrawingFromPage(fActivePage);
  if hDrawing = nil then exit;

  if acCMDAreaSelectionMode.Checked then
   begin
     TmpPar := TCAD2DSelectObjectsInAreaParam.Create(gmAllInside, TCAD2DOffsetObjects);
     with hDrawing.CADPrg2D do
       StartOperation(TCAD2DSelectObjectsInArea, TmpPar);
   end else
   begin
     TmpPar := TCAD2DSelectObjectsParam.Create(5, TCAD2DOffsetObjects);
     TCAD2DSelectObjectsParam(TmpPar).OnObjectSelected := @OnSelectObj;
     with hDrawing.CADPrg2D do
      StartOperation(TCAD2DSelectObjects, TmpPar);
   end;
end;

procedure TfrmMain.acModifyReverseExecute(Sender: TObject);
var TmpPar: TCADPrgParam; hDrawing: TDrawing;
begin
  if PageControl1.PageCount = 0 then exit;
  hDrawing := GetDrawingFromPage(fActivePage);
  if hDrawing = nil then exit;

  if acCMDAreaSelectionMode.Checked then
   begin
     TmpPar := TCAD2DSelectObjectsInAreaParam.Create(gmAllInside, TCAD2DReverse);
     with hDrawing.CADPrg2D do
       StartOperation(TCAD2DSelectObjectsInArea, TmpPar);
   end else
   begin
     TmpPar := TCAD2DSelectObjectsParam.Create(5, TCAD2DReverse);
     TCAD2DSelectObjectsParam(TmpPar).OnObjectSelected := @OnSelectObj;
     with hDrawing.CADPrg2D do
      StartOperation(TCAD2DSelectObjects, TmpPar);
   end;
end;

procedure TfrmMain.acModifyInverseExecute(Sender: TObject);
var TmpPar: TCADPrgParam; hDrawing: TDrawing;
begin
  if PageControl1.PageCount = 0 then exit;
  hDrawing := GetDrawingFromPage(fActivePage);
  if hDrawing = nil then exit;

  if acCMDAreaSelectionMode.Checked then
   begin
     TmpPar := TCAD2DSelectObjectsInAreaParam.Create(gmAllInside, TCAD2DInverse);
     with hDrawing.CADPrg2D do
       StartOperation(TCAD2DSelectObjectsInArea, TmpPar);
   end else
   begin
     TmpPar := TCAD2DSelectObjectsParam.Create(5, TCAD2DInverse);
     TCAD2DSelectObjectsParam(TmpPar).OnObjectSelected := @OnSelectObj;
     with hDrawing.CADPrg2D do
      StartOperation(TCAD2DSelectObjects, TmpPar);
   end;
end;

procedure TfrmMain.acModifyMakeContainerExecute(Sender: TObject);
var TmpPar: TCADPrgParam; hDrawing: TDrawing;
begin
  if PageControl1.PageCount = 0 then exit;
  hDrawing := GetDrawingFromPage(fActivePage);
  if hDrawing = nil then exit;

  if acCMDAreaSelectionMode.Checked then
   begin
     TmpPar := TCAD2DSelectObjectsInAreaParam.Create(gmAllInside, TCAD2DMakeContainer);
     with hDrawing.CADPrg2D do
       StartOperation(TCAD2DSelectObjectsInArea, TmpPar);
   end else
   begin
     TmpPar := TCAD2DSelectObjectsParam.Create(5, TCAD2DMakeContainer);
     TCAD2DSelectObjectsParam(TmpPar).OnObjectSelected := @OnSelectObj;
     with hDrawing.CADPrg2D do
      StartOperation(TCAD2DSelectObjects, TmpPar);
   end;
end;

procedure TfrmMain.acModifyRotateExecute(Sender: TObject);
var TmpPar: TCADPrgParam; hDrawing: TDrawing;
begin
  if PageControl1.PageCount = 0 then exit;
  hDrawing := GetDrawingFromPage(fActivePage);
  if hDrawing = nil then exit;

  if acCMDAreaSelectionMode.Checked then
   begin
     TmpPar := TCAD2DSelectObjectsInAreaParam.Create(gmAllInside, TCAD2DRotateSelectedObjects);
     with hDrawing.CADPrg2D do
       StartOperation(TCAD2DSelectObjectsInArea, TmpPar);
   end else
   begin
     TmpPar := TCAD2DSelectObjectsParam.Create(5, TCAD2DRotateSelectedObjects);
     TCAD2DSelectObjectsParam(TmpPar).OnObjectSelected := @OnSelectObj;
     with hDrawing.CADPrg2D do
      StartOperation(TCAD2DSelectObjects, TmpPar);
   end;
end;

procedure TfrmMain.acModifyRotateExExecute(Sender: TObject);
var TmpPar: TCADPrgParam; hDrawing: TDrawing;
begin
  if PageControl1.PageCount = 0 then exit;
  hDrawing := GetDrawingFromPage(fActivePage);
  if hDrawing = nil then exit;

  if acCMDAreaSelectionMode.Checked then
   begin
     TmpPar := TCAD2DSelectObjectsInAreaParam.Create(gmAllInside, TCAD2DRotateExSelectedObjects);
     with hDrawing.CADPrg2D do
       StartOperation(TCAD2DSelectObjectsInArea, TmpPar);
   end else
   begin
     TmpPar := TCAD2DSelectObjectsParam.Create(5, TCAD2DRotateExSelectedObjects);
     TCAD2DSelectObjectsParam(TmpPar).OnObjectSelected := @OnSelectObj;
     with hDrawing.CADPrg2D do
      StartOperation(TCAD2DSelectObjects, TmpPar);
   end;
end;

procedure TfrmMain.acOrderBingForwardExecute(Sender: TObject);
var hDrawing: TDrawing;  TmpPar: TCADPrgParam;
begin
  if PageControl1.PageCount = 0 then exit;
  hDrawing := GetDrawingFromPage(fActivePage);
  if hDrawing = nil then exit;
  if acCMDAreaSelectionMode.Checked then
  begin
    TmpPar := TCAD2DSelectObjectsInAreaParam.Create(gmAllInside, TCAD2DBringForward);
    TCAD2DSelectObjectsInAreaParam(TmpPar).OnObjectSelected := @OnSelectObj;
    hDrawing.CADPrg2D.StartOperation(TCAD2DSelectObjectsInArea, TmpPar);
  end else
  begin
    TmpPar := TCAD2DSelectObjectsParam.Create(5, TCAD2DBringForward);
    TCAD2DSelectObjectsParam(TmpPar).OnObjectSelected := @OnSelectObj;
    hDrawing.CADPrg2D.StartOperation(TCAD2DSelectObjects, TmpPar);
  end;
end;

procedure TfrmMain.acOrderBringBackwardExecute(Sender: TObject);
var hDrawing: TDrawing;  TmpPar: TCADPrgParam;
begin
  if PageControl1.PageCount = 0 then exit;
  hDrawing := GetDrawingFromPage(fActivePage);
  if hDrawing = nil then exit;
  if acCMDAreaSelectionMode.Checked then
  begin
    TmpPar := TCAD2DSelectObjectsInAreaParam.Create(gmAllInside, TCAD2DSendBackwards);
    TCAD2DSelectObjectsInAreaParam(TmpPar).OnObjectSelected := @OnSelectObj;
    hDrawing.CADPrg2D.StartOperation(TCAD2DSelectObjectsInArea, TmpPar);
  end else
  begin
    TmpPar := TCAD2DSelectObjectsParam.Create(5, TCAD2DSendBackwards);
    TCAD2DSelectObjectsParam(TmpPar).OnObjectSelected := @OnSelectObj;
    hDrawing.CADPrg2D.StartOperation(TCAD2DSelectObjects, TmpPar);
  end;
end;

procedure TfrmMain.acOrderSequentielExecute(Sender: TObject);
var hDrawing: TDrawing;  TmpPar: TCADPrgParam;
begin
  if PageControl1.PageCount = 0 then exit;
  hDrawing := GetDrawingFromPage(fActivePage);
  if hDrawing = nil then exit;

  TmpPar := TCAD2DSelectObjectsParam.Create(5, TCAD2DSendToBack);
  TCAD2DSelectObjectsParam(TmpPar).OnObjectSelected := @OnSelectObj;
  hDrawing.CADPrg2D.StartOperation(TCAD2DSelectObjects, TmpPar);
end;

procedure TfrmMain.acOrderSendToBackExecute(Sender: TObject);
var hDrawing: TDrawing;  TmpPar: TCADPrgParam;
begin
  if PageControl1.PageCount = 0 then exit;
  hDrawing := GetDrawingFromPage(fActivePage);
  if hDrawing = nil then exit;
  if acCMDAreaSelectionMode.Checked then
  begin
    TmpPar := TCAD2DSelectObjectsInAreaParam.Create(gmAllInside, TCAD2DSendToBack);
    TCAD2DSelectObjectsInAreaParam(TmpPar).OnObjectSelected := @OnSelectObj;
    hDrawing.CADPrg2D.StartOperation(TCAD2DSelectObjectsInArea, TmpPar);
  end else
  begin
    TmpPar := TCAD2DSelectObjectsParam.Create(5, TCAD2DSendToBack);
    TCAD2DSelectObjectsParam(TmpPar).OnObjectSelected := @OnSelectObj;
    hDrawing.CADPrg2D.StartOperation(TCAD2DSelectObjects, TmpPar);
  end;
end;

procedure TfrmMain.acOrderBringToFrontExecute(Sender: TObject);
var hDrawing: TDrawing;  TmpPar: TCADPrgParam;
begin
  if PageControl1.PageCount = 0 then exit;
  hDrawing := GetDrawingFromPage(fActivePage);
  if hDrawing = nil then exit;
  if acCMDAreaSelectionMode.Checked then
  begin
    TmpPar := TCAD2DSelectObjectsInAreaParam.Create(gmAllInside, TCAD2DBringToFront);
    TCAD2DSelectObjectsInAreaParam(TmpPar).OnObjectSelected := @OnSelectObj;
    hDrawing.CADPrg2D.StartOperation(TCAD2DSelectObjectsInArea, TmpPar);
  end else
  begin
    TmpPar := TCAD2DSelectObjectsParam.Create(5, TCAD2DBringToFront);
    TCAD2DSelectObjectsParam(TmpPar).OnObjectSelected := @OnSelectObj;
    hDrawing.CADPrg2D.StartOperation(TCAD2DSelectObjects, TmpPar);
  end;
end;

procedure TfrmMain.acOrderSwapExecute(Sender: TObject);
var hDrawing: TDrawing;  TmpPar: TCADPrgParam;
begin
  if PageControl1.PageCount = 0 then exit;
  hDrawing := GetDrawingFromPage(fActivePage);
  if hDrawing = nil then exit;
  if acCMDAreaSelectionMode.Checked then
  begin
    TmpPar := TCAD2DSelectObjectsInAreaParam.Create(gmAllInside, TCAD2DSwapObjects);
    TCAD2DSelectObjectsInAreaParam(TmpPar).OnObjectSelected := @OnSelectObj;
    hDrawing.CADPrg2D.StartOperation(TCAD2DSelectObjectsInArea, TmpPar);
  end else
  begin
    TmpPar := TCAD2DSelectObjectsParam.Create(5, TCAD2DSwapObjects);
    TCAD2DSelectObjectsParam(TmpPar).OnObjectSelected := @OnSelectObj;
    hDrawing.CADPrg2D.StartOperation(TCAD2DSelectObjects, TmpPar);
  end;
end;

procedure TfrmMain.acPrinterSetupExecute(Sender: TObject);
var hDrawing: TDrawing;
begin
  if PageControl1.PageCount = 0 then exit;
  hDrawing := GetDrawingFromPage(fActivePage);
  if hDrawing = nil then exit;

  hDrawing.CADCmp2D.DeleteAllSourceBlocks;
  hDrawing.CADCmp2D.DeleteLibrarySourceBlocks;
  hDrawing.CADCmp2D.DeleteAllObjects;
  hDrawing.CADViewport2D.Repaint;
end;

procedure TfrmMain.acChangeUserInterfaceExecute(Sender: TObject);
begin
  self.Hide;
  ChangeUserInterface;
  self.Show;
end;

procedure TfrmMain.acRequestPwdExecute(Sender: TObject);
begin
end;

procedure TfrmMain.acSettingsOpenCurrentTemplateFileExecute(Sender: TObject);
var hExt: string;
begin
  hExt := ExtractFileExt(fStdTemplate);
  if      LowerCase(hExt) = '.cs4' then OpenCS4File(fStdTemplate)
  else if LowerCase(hExt) = '.dxf' then OpenDXFFile(fStdTemplate);
end;

procedure TfrmMain.acSettingsSetDefTemplateExecute(Sender: TObject);
begin
  OpenCADFileDialog.InitialDir := applicationh.GetAppTemplatesPath;
  if OpenCADFileDialog.Execute then
  begin
    applicationh.fStdTemplate := OpenCADFileDialog.FileName;
    applicationh.WriteIniFile;
  end;
end;

procedure TfrmMain.acSettingsUnLockTemplateLayerExecute(Sender: TObject);
var hDrawing: TDrawing;
begin
  if PageControl1.PageCount = 0 then exit;
  hDrawing := GetDrawingFromPage(fActivePage);
  if hDrawing = nil then exit;

  if hDrawing.CADCmp2D.Layers.LayerByName[LAYER_STR_TEMPLATE] <> nil then
    hDrawing.CADCmp2D.Layers.LayerByName[LAYER_STR_TEMPLATE].Active := acSettingsUnLockTemplateLayer.Checked;
end;

procedure TfrmMain.acSettingsUseTemplatesExecute(Sender: TObject);
begin
  if acSettingsUseTemplates.Checked then
    applicationh.fUseTemplates := 'yes'
  else
    applicationh.fUseTemplates := 'no';

  applicationh.WriteIniFile;
end;

procedure TfrmMain.acSnapBottomLeftExecute(Sender: TObject);
begin
   SetSnapOption(7);//LeftBottom
end;

procedure TfrmMain.acSnapBottomCenterExecute(Sender: TObject);
begin
  SetSnapOption(6);//BottomCenter
end;

procedure TfrmMain.acSnapButtomRightExecute(Sender: TObject);
begin
  SetSnapOption(5);//RightBottom
end;

procedure TfrmMain.acSnapMiddleExecute(Sender: TObject);
begin
  SetSnapOption(9);////CenterPoint
end;

procedure TfrmMain.acSnapEndPointExecute(Sender: TObject);
begin
  SetSnapOption(11);//EndPoint
end;

procedure TfrmMain.acSnapGridExecute(Sender: TObject);
begin
  SetSnapOption(0); //Grid
end;

procedure TfrmMain.acSnapLeftCenterExecute(Sender: TObject);
begin
  SetSnapOption(8);//LeftCenter
end;

procedure TfrmMain.acSnapNaerestExecute(Sender: TObject);
begin
  SetSnapOption(13);//Naerest
end;

procedure TfrmMain.acSnapPolarExecute(Sender: TObject);
begin
  SetSnapOption(10);//StartPoint
end;

procedure TfrmMain.acSnapRightButtomExecute(Sender: TObject);
begin
  SetSnapOption(5);//RightBottom
end;

procedure TfrmMain.acSnapRightCenterExecute(Sender: TObject);
begin
  SetSnapOption(4); //RightCenter
end;

procedure TfrmMain.acSnapStartPointExecute(Sender: TObject);
begin
  SetSnapOption(10);//StartPoint
end;

procedure TfrmMain.acSnapTopCenterExecute(Sender: TObject);
begin
  SetSnapOption(2); //TopCenter
end;

procedure TfrmMain.acSnapTopLeftExecute(Sender: TObject);
begin
  SetSnapOption(1); //LeftTop
end;

procedure TfrmMain.acSnapTopRightExecute(Sender: TObject);
begin
  SetSnapOption(3); //RightTop
end;

procedure TfrmMain.acTestsTestLayersExecute(Sender: TObject);
begin

end;

procedure TfrmMain.acToolsShowSimulatorExecute(Sender: TObject);
var frmSimulation: TfrmSimulation;  hDrawing: TDrawing;
begin
  if PageControl1.PageCount = 0 then exit;
  hDrawing := GetDrawingFromPage(fActivePage);
  if hDrawing = nil then exit;

  //hDrawing.CADCmp2D.SaveToFile(applicationh.GetAppTempPath + 'tmp.cs4');

  hDrawing.CADCmp2D.SaveToFile(GetAppTempPath + 'tmp.cs4');
  self.Hide;
  frmSimulation := TfrmSimulation.Create(self);
  frmSimulation.CADCmp2D1.LoadFromFile(GetAppTempPath + 'tmp.cs4');
  frmSimulation.CADViewport2D1.Repaint;
  frmSimulation.CADViewport2D1.ZoomToExtension;
  frmSimulation.ShowModal;
  self.Show;
end;

procedure TfrmMain.acToolsTTF2VectorExecute(Sender: TObject);
var frmTTF2Vector: TfrmTTF2Vector;
begin
  frmTTF2Vector := TfrmTTF2Vector.Create(self);
  frmTTF2Vector.Show;
end;

procedure TfrmMain.ImportFromCADCmp(ACADCmp: TCADCmp2D);
var hDrawing: TDrawing;  TmpContainer2D: TContainer2D; TmpIter: TGraphicObjIterator;
    TmpClass: TGraphicObjectClass; TmpObj: TGraphicObject;
begin
  if PageControl1.PageCount = 0 then exit;
  hDrawing := GetDrawingFromPage(fActivePage);
  if hDrawing = nil then exit;

  TmpContainer2D := TContainer2D.Create(-1, [nil]);
  TmpIter := ACADCmp.ObjectsIterator;
  try
    TmpIter.First;
    while TmpIter.Current <> nil do
    begin
      TmpClass := TGraphicObjectClass(TmpIter.Current.ClassType);
      TmpObj := TmpClass.Create(-1);
      TmpObj.Assign(TmpIter.Current);

      TmpContainer2D.Objects.Add(TGraphicObject(TmpObj));
      TmpIter.Next;
    end;
  finally
    TmpIter.Free;
  end;
  hDrawing.CADPrg2D.StartOperation(TCAD2DPositionObject,
     TCAD2DPositionObjectParam.Create(nil, TmpContainer2D));
end;

procedure TfrmMain.acZoomAreaExecute(Sender: TObject);
var hDrawing: TDrawing;
begin
  if PageControl1.PageCount = 0 then exit;
  hDrawing := GetDrawingFromPage(fActivePage);
  if hDrawing = nil then exit;

  with hDrawing.CADPrg2D do
    SuspendOperation(TCADPrgZoomArea, nil);
end;

procedure TfrmMain.acZoomEXTExecute(Sender: TObject);
var hDrawing: TDrawing;
begin
  if PageControl1.PageCount = 0 then exit;
  hDrawing := GetDrawingFromPage(fActivePage);
  if hDrawing = nil then exit;
  hDrawing.CADViewport2D.ZoomToExtension;
end;

procedure TfrmMain.acZoomINExecute(Sender: TObject);
var hDrawing: TDrawing;
begin
  if PageControl1.PageCount = 0 then exit;
  hDrawing := GetDrawingFromPage(fActivePage);
  if hDrawing = nil then exit;
  hDrawing.CADViewport2D.ZoomIn;
end;

procedure TfrmMain.acZoomOutExecute(Sender: TObject);
var hDrawing: TDrawing;
begin
  if PageControl1.PageCount = 0 then exit;
  hDrawing := GetDrawingFromPage(fActivePage);
  if hDrawing = nil then exit;
  hDrawing.CADViewport2D.ZoomOut;
end;

procedure TfrmMain.acZoomPanExecute(Sender: TObject);
var hDrawing: TDrawing;
begin
  if PageControl1.PageCount = 0 then exit;
  hDrawing := GetDrawingFromPage(fActivePage);
  if hDrawing = nil then exit;
  hDrawing.CADPrg2D.SuspendOperation(TCADPrgRealTimePan, nil);
end;

function TfrmMain.CheckSave(APage: TTabSheet): boolean;
var hDrawing: TDrawing; Reply: TModalResult;
begin
  result := false;
  hDrawing := GetDrawingFromPage(APage);
  if  hDrawing = nil then exit;

  if hDrawing.Changed then
  begin
    Reply := MessageDlg('File ' + QuotedStr(hDrawing.FileName) + ' changed!', 'Do you wish to Save?', mtConfirmation, [mbYes, mbNo, mbCancel],0);
    if Reply = mrCancel then exit;
    if Reply = mrNo then
    begin
      result := true;
      exit;
    end;
    if Reply = mrYes then
      if FileExists(hDrawing.FileName) then
      begin
        hDrawing.SaveToFile(hDrawing.FileName);
        result := true;
      end else
        if SaveCADSys4Dialog.Execute then
        begin
          hDrawing.SaveToFile(SaveCADSys4Dialog.FileName);
          result := true;
        end;
  end else
    result := true;
end;

procedure TfrmMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var i: integer;
begin
  for i := PageControl1.PageCount -1 downto 0 do
    if CheckSave(PageControl1.Pages[i]) then
      CloseCADFile(PageControl1.Pages[i])
    else begin
      CloseAction := caNone;
      exit;
    end;

  if GlobalObject2D <> nil then
    GlobalObject2D := nil;

  if ComponentDrawing <> nil then
    ComponentDrawing := nil;
end;

procedure TfrmMain.FormResize(Sender: TObject);
begin
  ProgressBarMain.Color := StatusBarMain.Color;
  ProgressBarMain.Min := 0;
  ProgressBarMain.Max := 100;
  ProgressBarMain.Position := 0;
  ProgressBarMain.Top := StatusBarMain.Top;
  ProgressBarMain.Height := StatusBarMain.Height - 2;
  ProgressBarMain.Left := StatusBarMain.Panels[0].Width  + StatusBarMain.Panels[1].Width;
  ProgressBarMain.Width := StatusBarMain.Panels[2].Width;
end;

procedure TfrmMain.mnuMetroDarkClick(Sender: TObject);
begin
  SpkToolbar1.Style := spkMetroDark;
end;

procedure TfrmMain.mnuMetroLightClick(Sender: TObject);
begin
  SpkToolbar1.Style := spkMetroLight;
  TIPropertyGrid1.BackgroundColor := clDefault;
end;

procedure TfrmMain.mnuOffice2007BlueClick(Sender: TObject);
begin
  SpkToolbar1.Style := spkOffice2007Blue;
  TIPropertyGrid1.BackgroundColor := clSkyBlue;
end;

procedure TfrmMain.mnuOffice2007SilverClick(Sender: TObject);
begin
  SpkToolbar1.Style := spkOffice2007Silver;
  TIPropertyGrid1.BackgroundColor := clDefault;
end;

procedure TfrmMain.mnuOffice2007SilverTurquoiseClick(Sender: TObject);
begin
  SpkToolbar1.Style := spkOffice2007SilverTurquoise;
  TIPropertyGrid1.BackgroundColor := clDefault;
end;

procedure TfrmMain.PageControl1Change(Sender: TObject);
var hDrawing: TDrawing;
begin
  SetupInspector(nil);
  TIPropertyGrid1.Update;
  TIPropertyGrid1.Repaint;
  fActivePage := PageControl1.ActivePage;
  hDrawing := GetDrawingFromPage(fActivePage);
  if hDrawing <> nil then
  begin

    hDrawing.LoadBlockLibraryFromFile (applicationh.fDefaultBlockLibrary);

    if hDrawing.CADCmp2D.Layers.LayerByName[LAYER_STR_TEMPLATE] <> nil then
    begin
      acSettingsUnLockTemplateLayer.Visible := true;
      acSettingsUnLockTemplateLayer.Checked := hDrawing.CADCmp2D.Layers.LayerByName[LAYER_STR_TEMPLATE].Active;
    end else
    begin
      acSettingsUnLockTemplateLayer.Visible  := false;
      acSettingsUnLockTemplateLayer.Checked  := false;
    end;
  end;
  ComponentDrawing.Drawing := hDrawing;
  SetupInspector(ComponentDrawing);
  UpdateUserInterface;
  RegisterDocument;
end;

procedure TfrmMain.PageControl1Changing(Sender: TObject; var AllowChange: Boolean);
var hDrawing: TDrawing;
begin
  hDrawing := GetDrawingFromPage(fActivePage);
  if hDrawing <>  nil then
    hDrawing.SaveBlockLibraryToFile(applicationh.fDefaultBlockLibrary);
end;

procedure TfrmMain.ChangeUserInterface;
begin
  if self.Menu = nil then
  begin
    pnlTop.Visible         := true;
    pnlLeftToolBars.Visible := true;
    pnlBottomToolBars.Visible := true;
    pnlFormat.Visible     := true;

    self.menu := MainMenu1;
    TIPropertyGrid1.BackgroundColor := clDefault;
    SpkToolbar1.Visible := false;
  end else
  begin
    pnlTop.Visible         := false;
    pnlLeftToolBars.Visible  := false;
    pnlBottomToolBars.Visible := false;
    pnlFormat.Visible     := false;

    self.menu := nil;
    SpkToolbar1.Visible := true;
  end;
end;

procedure TfrmMain.TIPropertyGrid1Click(Sender: TObject);
var hDrawing: TDrawing;
begin
  hDrawing := GetDrawingFromPage(fActivePage);
  if hDrawing <> nil then
    if GlobalObject2D <> nil then
      hDrawing.CADViewport2D.DrawObject2D(GlobalObject2D, true);
end;

procedure TfrmMain.TIPropertyGrid1Modified(Sender: TObject);
var hDrawing: TDrawing;
begin
  hDrawing := GetDrawingFromPage(fActivePage);
  if hDrawing = nil then  exit;
  hDrawing.CADViewport2D.Repaint;
  TIPropertyGrid1.Refresh;
  hDrawing.UndoRedo.UndoSave;
  if GlobalObject2D <> nil then
    hDrawing.CADViewport2D.DrawObject2D(GlobalObject2D, true);
end;

procedure TfrmMain.SetSnapOption(AOption: Integer);
var hDrawing: TDrawing;
begin
  hDrawing := GetDrawingFromPage(fActivePage);
  if hDrawing <> nil then
    if hDrawing.CADPrg2D.UseSnap then
      hDrawing.SnapOption := AOption;
end;

end.

