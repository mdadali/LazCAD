unit cCADObject;

interface

uses Windows, Classes, SysUtils, Controls, Dialogs,
     LiteCAD,
     applicationh,
     CommonUtils,
     CAM;

type


  TCADObject = class
  private
    function  GetFileName: string;
    procedure LoadExistingFile(AFileName: string);
    procedure CreateFromTemplate;
    procedure CreateNewDrawing;
  public
    ParentHandle: integer;
    ParentControl: TWinControl;
    ParentControlClassType: TClass;
    ParentControlClassName: string;
    hLcWnd, hLcDrw, hFirstBlock, hView: integer;

    constructor Create(AParentHandle: integer; AFileName: string);
    destructor  Destroy; override;
    procedure   Resize(ALeft, ATop, AWidth, AHeight: integer);
    procedure   AddTemplateLayer;

    property    FileName: string read GetFileName;
  end;


implementation


function TCADObject.GetFileName: string;
begin
  result := WideCharToString(lcPropGetStr(self.hLcDrw, LC_PROP_DRW_FILENAME));
end;

procedure TCADObject.LoadExistingFile(AFileName: string);
var PAFileName: PWideChar;
begin
  try
    PAFileName := cuStrToWChar(AFileName) ;
    lcDrwLoad(self.hLcDrw, PAFileName, hLcWnd);
    hFirstBlock := lcDrwGetFirstObject(self.hLcDrw, LC_OBJ_BLOCK );
  finally
   dispose(PAFileName);
  end;
end;

procedure TCADObject.CreateFromTemplate;
var   PAFileName, TempName: PWideChar; NoName,  StdTemplate: string;
begin
  try
    PAFileName := cuStrToWChar(applicationh.StdTemplate);
    lcDrwLoad(self.hLcDrw, PAFileName, hLcWnd);
    NoName := lcStrGet('APP_NONAME');
    TempName := cuStrToWChar(NoName +IntToStr(fFakeMDIChildCount + 1));
    lcPropPutStr(self.hLcDrw, LC_PROP_DRW_FILENAME, TempName);
    hFirstBlock := lcDrwGetFirstObject(self.hLcDrw, LC_OBJ_BLOCK );
  finally
    dispose(TempName);
    dispose(PAFileName);
  end;
end;

procedure TCADObject.CreateNewDrawing;
var  TempName: PWideChar;  NoName: string;
begin
  try
    lcDrwNew(self.hLcDrw, '', hLcWnd);
    NoName := lcStrGet('APP_NONAME');
    TempName := cuStrToWChar(NoName + IntToStr(fFakeMDIChildCount + 1));
    lcPropPutStr(self.hLcDrw, LC_PROP_DRW_FILENAME, TempName);
    hFirstBlock := lcDrwGetFirstObject(self.hLcDrw, LC_OBJ_BLOCK );
  finally
    dispose(TempName);
  end;
end;

constructor TCADObject.Create(AParentHandle: integer; AFileName: string);
begin
  inherited create;
  ParentHandle :=  AParentHandle;
  ParentControl := FindControl(ParentHandle);
  ParentControlClassType :=  ParentControl.ClassType;
  ParentControlClassName := ParentControl.ClassName;
  ParentControl.Visible := false;
  self.hLcWnd := lcCreateWindow(AParentHandle, {LC_WS2_STATBAR or} {LC_WS2_DEFAULT or LC_WS_BORDER or} LC_WS_RULERS or LC_WS_VIEWTABS);

  with ParentControl do
  begin
    self.Resize(Left, Top, Width, Height);
  end;

   ParentControl.Visible := true;

  lcPropPutBool( self.hLcWnd, LC_PROP_WND_COORDS, 1 );
  self.hLcDrw := lcCreateDrawing();
  lcPropPutBool( self.hLcWnd, LC_PROP_WND_SELECT, 1 );
  lcPropPutBool( self.hLcWnd, LC_PROP_WND_SELBYRECT, 1 );


  if FileExists(AFileName) then
    LoadExistingFile(AFileName)
  else
    if (applicationh.StdTemplate <> '') then
      CreateFromTemplate
    else
      CreateNewDrawing;

    //{$IFDEF TEMPLATE_LAYER}
    AddTemplateLayer;
  //{$ENDIF}

  {$IFDEF CAM}
     CAM_AddCamLayers(self.hLcDrw);
  {$ENDIF}
end;

destructor TCADObject.Destroy;
begin
  lcWndExeCommand(hLcWnd, LC_CMD_RESET, 0 );
  //lcWndOnClose(hLcWnd);

  if self.hLcDrw > 0 then
  begin
    lcDeleteDrawing (self.hLcDrw);
    self.hLcDrw := 0;
  end;

  {if self.hLcWnd > 0 then
  begin
    lcDeleteWindow (self.hLcWnd);
    self.hLcWnd := 0;
  end; }

  inherited destroy;
end;

procedure TCADObject.Resize(ALeft, ATop, AWidth, AHeight: integer);
begin
  lcWndResize(self.hLcWnd, ALeft, ATop, AWidth, AHeight);
  //lcWndResize(self.hLcWnd, ParentControl.Left, ParentControl.Top, ParentControl.Width, ParentControl.Height);
end;

procedure TCADObject.AddTemplateLayer;
var hLayer: integer; LayerName: PWideChar;
begin
  LayerName := cuStrToWChar(CAD_LAYER_TEMPLATE);
  hLayer := lcDrwGetObjectByName (self.hLcDrw, LC_OBJ_LAYER, LayerName);
  if   (hLayer = 0) then
    hLayer :=  lcDrwAddLayer (self.hLcDrw,  LayerName, nil, 0, 0);
  if (hLayer <> 0) then
  begin
    lcPropPutBool(hLayer, LC_PROP_LAYER_NODLG, 1);
    lcPropPutBool(hLayer, LC_PROP_LAYER_LOCKED, 1);
    //lcPropPutBool(hLayer, LC_PROP_LAYER_VISIBLE, 1);
  end else
    ShowMessage('Cant add Layer ' + LayerName);

  dispose(LayerName);
end;

end.
