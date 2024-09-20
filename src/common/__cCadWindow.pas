unit cCadWindow;

interface

uses Classes, SysUtils, Dialogs,
     LiteCAD,
     CADDocument,
     CADUtils,
     applicationh,
     CAM;

type


  TCadWindow = class
  public
    ParentHandle: integer;
    hLcWnd, hLcDrw, hFirstBlock, hView: integer;

    constructor CreateWin(AParentHandle: integer; AFileName: PWideChar; ACopyFromFile: boolean);
    destructor Destroy;
    procedure   Resize(ALeft, ATop, AWidth, AHeight: integer);
    procedure   AddTemplateLayer;
  end;


implementation

constructor TCadWindow.CreateWin(AParentHandle: integer; AFileName: PWideChar; ACopyFromFile: boolean);
var NoName: string; TempName: PWideChar;
begin
  inherited create;
  ParentHandle :=  AParentHandle;
  self.hLcWnd := lcCreateWindow(AParentHandle, LC_WS2_STATBAR or LC_WS2_DEFAULT or LC_WS_BORDER or LC_WS_RULERS or LC_WS_VIEWTABS);
  lcPropPutBool( self.hLcWnd, LC_PROP_WND_COORDS, 1 );
  //lcPropPutBool( self.hLcWnd, LC_PROP_WND_CRECTS_VISIBLE, 1 );
  //lcWndResize(self.hLcWnd, 0, 0, self.Width, self.Height);
  self.hLcDrw := lcCreateDrawing();
  //lcPropPutStr( hLcDrw, LC_PROP_DRW_COLORBACKM, '20,120,140' );
  lcPropPutBool( self.hLcWnd, LC_PROP_WND_SELECT, 1 );
  lcPropPutBool( self.hLcWnd, LC_PROP_WND_SELBYRECT, 1 );
  if FileExists(AFileName) then
  begin
    if ACopyFromFile then
    begin
      lcDrwLoad(self.hLcDrw, AFileName, hLcWnd);
      NoName   := WideCharToString(lcStrGet('APP_NONAME'));
      TempName := PWideChar(WideString(NoName + IntToStr(fFakeMDIChildCount + 1)));
      lcPropPutStr(self.hLcDrw, LC_PROP_DRW_FILENAME, TempName);
    end else
    begin
      lcDrwLoad(self.hLcDrw, AFileName, hLcWnd)
    end;
  end else
  begin
    lcDrwNew( self.hLcDrw, '', self.hLcWnd );
    lcPropPutStr(self.hLcDrw, LC_PROP_DRW_FILENAME, AFileName);
  end;

    //{$IFDEF TEMPLATE_LAYER}
    AddTemplateLayer;
  //{$ENDIF}

  //{$IFDEF CAM}
     AddCamLayers(self.hLcDrw);
  //{$ENDIF}
end;

destructor TCadWindow.Destroy;
begin
  if hLcWnd > 0 then
    lcDeleteWindow (hLcWnd);
  if hLcDrw > 0 then
    lcDeleteDrawing (hLcDrw);
  inherited destroy;
end;

procedure   TCadWindow.Resize(ALeft, ATop, AWidth, AHeight: integer);
begin
  lcWndResize(self.hLcWnd, ALeft, ATop, AWidth, AHeight);
end;

procedure TCadWindow.AddTemplateLayer;
var hLayer: integer; LayerName: string;
begin
  LayerName := CAD_LAYER_TEMPLATE;
  hLayer := lcDrwGetObjectByName (hLcDrw, LC_OBJ_LAYER, cuStrToWChar(LayerName));
  if   (hLayer = 0) then
    hLayer :=  lcDrwAddLayer (hLcDrw,  cuStrToWChar(LayerName), nil, 0, 0);
  if (hLayer <> 0) then
  begin
    lcPropPutBool(hLayer, LC_PROP_LAYER_NODLG, 1);
    lcPropPutBool(hLayer, LC_PROP_LAYER_LOCKED, 1);
    //lcPropPutBool(hLayer, LC_PROP_LAYER_VISIBLE, 1);
  end else
    ShowMessage('Cant add Layer ' + LayerName);
end;



end.
