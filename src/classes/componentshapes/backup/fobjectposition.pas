unit fobjectposition;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  CADSys4,
  CS4Shapes,
  CS4BaseTypes;

{$M+}
type


  TObjectPosition2D = class(TPersistent)
  private
    fOnChange: TNotifyEvent; // Event für Benachrichtigung
    fTop,
    fLeft,
    fBottom,
    fRight: TRealType;
    {procedure SetTop(AValue: TRealType);
    procedure SetLeft(AValue: TRealType);
    procedure SetBottom(AValue: TRealType);
    procedure SetRight(AValue: TRealType);}
  protected
    procedure DoChange; // Methode, um das Event auszulösen
  public
    constructor create;
  published
    property Top: TRealType read fTop write fTop; //SetTop;
    property Left: TRealType read fLeft write fLeft; //SetLeft;
    property Bottom: TRealType read fBottom write fBottom; //SetBottom;
    property Right: TRealType read fRight write fRight; //SetRight;
    property OnChange: TNotifyEvent read fOnChange write fOnChange; // Event verfügbar machen
  end;


implementation

constructor TObjectPosition2D.create;
begin
  inherited create;
end;

procedure TObjectPosition2D.DoChange;
begin
  if Assigned(fOnChange) then
    fOnChange(Self);  // Ruft die Benachrichtigungsprozedur auf
end;

{procedure TObjectPosition2D.SetTop(AValue: TRealType);
begin
  if fTop <> AValue then
  begin
    fTop := AValue;
    DoChange;  // Event auslösen, wenn sich etwas ändert
  end;
end;

procedure TObjectPosition2D.SetLeft(AValue: TRealType);
begin
  if fLeft <> AValue then
  begin
    fLeft := AValue;
    DoChange;
  end;
end;

procedure TObjectPosition2D.SetBottom(AValue: TRealType);
begin
  if fBottom <> AValue then
  begin
    fBottom := AValue;
    DoChange;
  end;
end;

procedure TObjectPosition2D.SetRight(AValue: TRealType);
begin
  if fRight <> AValue then
  begin
    fRight := AValue;
    DoChange;
  end;
end; }

initialization
  //RegisterClass(TObjectPosition2D);

finalization

end.
