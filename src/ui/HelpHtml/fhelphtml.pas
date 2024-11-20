unit fHelpHtml;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, Forms, Controls, Graphics, Dialogs, StdCtrls, HtmlView, HTMLUn2,
  HtmlGlobals, FramBrwz, applicationh,

{$IFDEF WINDOWS} Windows, ShellAPI, {$ENDIF}
  {$IFDEF UNIX} BaseUnix, {$ENDIF}
  Process, SysUtils;

type

  { TfrmHelpHtml }

  TfrmHelpHtml = class(TForm)
    procedure FormCreate(Sender: TObject);
  private

  public
    WebView: THtmlViewer;
    procedure WebView_OnLink(Sender: TObject; const Rel, Rev, Href: ThtString);
    function  IsExternalLink(const URL: string): Boolean;
    procedure OpenURL(const URL: string);
  end;

//var
  //frmHelpHtml: TfrmHelpHtml;

implementation

{$R *.lfm}

{ TfrmHelpHtml }

procedure TfrmHelpHtml.FormCreate(Sender: TObject);
var FrameBrowser1: TFrameBrowser;
begin
  FrameBrowser1 := FrameBrowser1.Create(self);
  FrameBrowser1.Parent := Self;
  FrameBrowser1.Align := alClient;
  //FrameBrowser1.OnLink  := @WebView_OnLink;
  //FrameBrowser1.OnClick := @WebView_OnLink;
  ShowMessage('FrameBrowser1 initialisiert');  // Debugging-Nachricht
end;

procedure  TfrmHelpHtml.OpenURL(const URL: string);
begin
  {$IFDEF WINDOWS}
    // Windows: ShellExecute verwenden
    ShellExecute(0, 'open', PChar(URL), nil, nil, SW_SHOWNORMAL);
  {$ELSE}
    // Linux und macOS: mit ExecuteProcess
    if FileExists('/usr/bin/xdg-open') then
      ExecuteProcess('/usr/bin/xdg-open', [URL], []);
    {$IFDEF DARWIN} // macOS
    else if FileExists('/usr/bin/open') then
      ExecuteProcess('/usr/bin/open', [URL], []);
    {$ENDIF}
  {$ENDIF}
end;
function TfrmHelpHtml.IsExternalLink(const URL: string): Boolean;
var
  BaseURL: string;
begin
  // Die Basis-URL deiner Anwendung (z. B. lokale Hilfe-Seiten).
  BaseURL := applicationh.GetAppDocPath;

  // Prüfen, ob der Link außerhalb der Basis-URL liegt.
  Result := not URL.StartsWith(BaseURL, True); // True für case-insensitive Vergleich
end;

procedure TfrmHelpHtml.WebView_OnLink(Sender: TObject; const Rel, Rev, Href: ThtString);
begin
  ShowMessage('Link clicked: ' + Href);
  if IsExternalLink(Href) then
  begin
    // Externe Links im Standardbrowser öffnen
    OpenURL(Href);
  end
  else
  begin
    // Interne Links innerhalb des Viewers laden
    WebView.LoadFromFile(Href);
  end;
end;

end.

