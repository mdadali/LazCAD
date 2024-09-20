unit mImport;

uses Classes, SysUtils, Dialogs, fCADIntrf2D, tools;

type
  TSinglePoint = record
    X,
    Y: single;
  end;

var
  OpenDialog1: TOpenDialog;
  Factor: single;
  ImportFileName: string;
  InputList: TStringList;

  LayerNr    : integer;
  PenWidth   : integer;
  PenStyle   : integer;
  PenColor   : Integer;
  BrushStyle : integer;
  BrushColor : integer;
  RowStyle   : integer;
  Selected   : boolean;
  ArcStyle   : integer;

  //Essi
  CommentOn : boolean;
  incremental: boolean;
  MoveON     : boolean;
  lastposx,
  lastposy: single;

procedure  CheckRecords;
var i, hHF: integer; hStr: string;
begin
  lastposx := 0;
  lastposy := 0;
  I_ProgressBarReset(0, InputList.count, 1);
  for i:= 0 to (InputList.Count -1) do
  begin
    hStr := InputList[i];
    
    if (hStr <> '') then
    begin
      if isLine(hStr)
        then ProcessLine(hStr, i-1)
      else  if isArc(hStr)
        then ProcessArc(hStr, i-1)
      else  if ((hStr = '5') or (hStr = '6'))
        then MoveON := true
      else  if (hStr = '7')
        then MoveON := false;
      //else  if (isHF = 1) then  FillHF(hStr);
      //else FillErrRec(hStr);
      I_ProgressBarStep;
    end;
    hStr := '';
  end;
  I_ProgressBarReset(0, InputList.count, 1);
 end;

function isLine(AStr: string) : boolean;
var nodeIdx, idx1, idx2, SignCount, AStrLen: integer; val1, val2 : string;
begin
  result := false;
  AStrLen     := StrLen(AStr);
  SignCount   := I_CharCount(AStr, '+') + I_CharCount(AStr, '-');
  if SignCount <> 2 then exit;
  idx1        := 1;
  idx2        := I_SignIdx(I_SubString(AStr, 2, AStrLen)) + idx1;
  val1        := I_SubString(AStr, idx1, (idx2 - 1));
  val2        := I_SubString(AStr, idx2, AStrLen);
  if ((val1 = '+') or (val1 = '-')) then val1 := val1 + '0';
  if ((val2 = '+') or (val2 = '-')) then val2 := val2 + '0';

  result :=  (
                 ( (AStr[1] = '+') or (AStr[1] = '-' ))                              and
                 ((val1='+') or (val1='-') or (StrToIntDef(val1, -9999999) <> -9999999))  and
                 ((val2='+') or (val2='-') or (StrToIntDef(val2, -9999999) <> -9999999))
              );
end;

function isArc(AStr: string): boolean;
var idx1, idx2, idx3, idx4, idx5, SignCount, AStrLen: integer;
    val1, val2, val3, val4: string;
begin
  result := false;
  AStrLen     := StrLen(AStr);
  SignCount   := I_CharCount(AStr, '+') + I_CharCount(AStr, '-');
  if SignCount <> 5 then exit;
  idx1        := 1;
  idx2        := I_SignIdx(I_SubString(AStr, 2, AStrLen)) + idx1;
  idx3        := I_SignIdx(I_SubString(AStr, idx2 + 1, AStrLen)) + idx2;
  idx4        := I_SignIdx(I_SubString(AStr, idx3 + 1, AStrLen)) + idx3;
  idx5        := AStrLen;
  val1        := I_SubString(AStr, idx1, (idx2 - 1));
  val2        := I_SubString(AStr, idx2, (idx3 - 1));
  val3        := I_SubString(AStr, idx3, (idx4 - 1));
  val4        := I_SubString(AStr, idx4, (idx5 - 1));
  if ((val1 = '+') or (val1 = '-')) then val1 := val1 + '0';
  if ((val2 = '+') or (val2 = '-')) then val2 := val2 + '0';
  if ((val3 = '+') or (val3 = '-')) then val3 := val3 + '0';
  if ((val4 = '+') or (val4 = '-')) then val4 := val4 + '0';

  result :=  (   ((AStr[1] = '+') or (AStr[1] = '-' ))       and
                 ((val1='+') or (val1='-') or (StrToIntDef(val1, -9999999) <> -9999999))  and
                 ((val2='+') or (val2='-') or (StrToIntDef(val2, -9999999) <> -9999999))  and
                 ((val3='+') or (val3='-') or (StrToIntDef(val3, -9999999) <> -9999999))  and
                 ((val4='+') or (val4='-') or (StrToIntDef(val4, -9999999) <> -9999999))
              );
end;

procedure ProcessArc(AStr: string; ALineNum: integer);
var idx1, idx2, idx3, idx4, idx5, SignCount, AStrLen: integer;
    val1, val2, val3, val4, hDirection: string; hDirectionInt: word;  hRowStyle: integer;
    A,B,C,D: TsinglePoint;
    a1, a2, x1, y1, x2, y2, cx, cy, radius: single;
begin
  AStrLen     := StrLen(AStr);
  idx1        := 1;
  idx2        := I_SignIdx(I_SubString(AStr, idx1 + 1, AStrLen)) + idx1;
  idx3        := I_SignIdx(I_SubString(AStr, idx2 + 1, AStrLen)) + idx2;
  idx4        := I_SignIdx(I_SubString(AStr, idx3 + 1, AStrLen)) + idx3;
  idx5        := AStrLen;
  hDirection  := AStr[idx5];
  val1        := I_SubString(AStr, idx1, (idx2 - 1));
  val2        := I_SubString(AStr, idx2, (idx3 - 1));
  val3        := I_SubString(AStr, idx3, (idx4 - 1));
  val4        := I_SubString(AStr, idx4, (idx5 - 1));
  if ((val1 = '+') or (val1 = '-')) then val1 := '0';
  if ((val2 = '+') or (val2 = '-')) then val2 := '0';
  if ((val3 = '+') or (val3 = '-')) then val3 := '0';
  if ((val4 = '+') or (val4 = '-')) then val4 := '0';
  x1           := LastPosX; //StartPunktX
  y1           := LastPosY; //StartPunktY
  cx           := StrToFloat(val3);  //MittelPunktX
  cy           := StrToFloat(val4);  //MittelPunktY
  x2           := StrToFloat(val1);  //+ x1; //EndPunktX
  y2           := StrToFloat(val2);  // + y1; //EndPunktY

  radius := I_sqrt(sqr(cx) + sqr(cy));

  //Koordinaten von Rechteck ermitteln.
  A.x :=  cx - radius; //round(cx - radius);
  A.y :=  cy - radius;
  B.x :=  cx + radius;
  B.y :=  cy + radius;

  C.x := 0;  //bogen Startpunkt.x
  C.y := 0;  //bogen Startpunkt.y
  D.x :=  x2;         //bogen EndPunkt.x
  D.y :=  y2;         //bogen EndPunkt.y

  if Incremental then
  begin
    cx  :=  cx  + LastPosX;
    cy  :=  cy  + LastPosY;
    A.x :=  A.x + LastPosX;
    A.y :=  A.y + LastPosY;
    B.x :=  B.x + LastPosX;
    B.y :=  B.y + LastPosY;
    C.x :=  C.x + LastPosX;
    C.y :=  C.y + LastPosY;
    D.x :=  D.x + LastPosX;
    D.y :=  D.y + LastPosY;
  end;
  a1 := I_GetAngleFromPoints(cx, cy, C.X, C.Y);
  a2 := I_GetAngleFromPoints(cx, cy, D.X, D.Y);
  if hDirection = '-' then hDirectionInt := 1
  else hDirectionInt := 0;
  if (MoveON = false) then
    I_DrawArc((Factor * A.x), (Factor * A.y), (Factor * B.x), (Factor * B.y), a1, a2, hDirectionInt);
  LastPosX := D.X;
  LastPosY := D.Y;
end;

procedure ProcessLine(AStr: string; ALineNum: integer);
var idx1, idx2, SignCount, AStrLen: integer; val1, val2 : string;
    x1, y1, x2, y2: single;
begin
  AStrLen     := StrLen(AStr);
  idx1        := 1;
  idx2        := I_SignIdx(I_SubString(AStr, idx1 + 1, AStrLen)) + idx1;
  val1        := I_SubString(AStr, idx1, (idx2 - 1));
  val2        := I_SubString(AStr, idx2, AStrLen);
  if ((val1 = '+') or (val1 = '-')) then val1 := '0';
  if ((val2 = '+') or (val2 = '-')) then val2 := '0';

  x1           := LastPosX;
  y1           := LastPosY;
  x2           := StrToFloat(val1) + x1;
  y2           := StrToFloat(val2) + y1;
  if (MoveON = false) then
    I_DrawLine((x1*Factor), (y1*Factor), (x2*Factor), (y2*Factor));
  LastPosX := x2;
  LastPosY := y2;
end;

procedure Init;
begin
  if (ImportFileName = '') then
  begin
    OpenDialog1 := TOpenDialog.Create(nil);
    OpenDialog1.Title := 'Import Essi-File';
    if OpenDialog1.Execute then
       ImportFileName := OpenDialog1.FileName;
    OpenDialog1.free;
  end;
  if  (ImportFileName <> '') then
  begin
    //SetWorkHeight(3000);
    //SetWorkWidth(12000);
    InputList  :=  TStringList.Create;
    InputList.LoadFromFile(ImportFileName);
    Factor     := 0.1;
    LayerNr    := 0;
    PenWidth   := 1;
    PenStyle   := 0; //psSolid;
    //PenColor   := clWhite;
    RowStyle   := 4;
    BrushStyle := 0;
    //BrushColor := clWhite;
    Selected   := false;
    ArcStyle   := 0;
    CommentOn  := false;
    incremental:= true;
    MoveON     := false;
    CheckRecords;
    InputList.Free;
  end;
end;

begin
  ImportFileName := '';
  Init;
  I_Refresh;
end.

