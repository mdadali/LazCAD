{
Reads AvisoCNC G-Code

License: The same modified LGPL as the Free Pascal RTL
         See the file COPYING.modifiedLGPL for more details

AUTHORS: Felipe Monteiro de Carvalho
         Pedro Sol Pegorini L de Lima
}
unit cImportGCode;

//{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls, Graphics,
  CADSys4,
  CS4BaseTypes,
  CS4Shapes,
  CS4Tasks,
  CS4DXFModule;

type

  T10Strings      = array[0..9] of shortstring;
  TLine2DCoords   = array[0..1] of shortstring;
  TArc2DCoords_1  = array[0..3] of shortstring;   // X Y I J
  TArc2DCoords_2  = array[0..2] of shortstring;   // X Y R (R=radius)

  { TImportGCode }

  TImportGCode = class
  private
    fCADCmp2D: TCADCmp2D;
    fCADViewport2D: TCADViewport2D;
    fProgressBar: TProgressBar;

    fLastCommand: string;
    fInputList: TStringList;
    Factor: single;
    CommentOn  : boolean;
    fImportsRapidMove: boolean;
    fIncremental: boolean;
    fArcCenterREL:  boolean;
    RapidMoveON: boolean;

    LastX, LastY, LastZ: Double;
    function  SeparateString(AString: string; ASeparator: Char): T10Strings;
    function  GetFirstCommandIDX(AArray: T10Strings): integer;
    procedure ReadString(AStr: string);

    function  GetCoordinate(AStr: shortstring): Integer;
    function  GetCoordinateValue(AStr: shortstring): Double;

    procedure ProcessRapidMove(ACoords: TLine2DCoords);
    function  ProcessLinearMove(ACoords: TLine2DCoords): integer;

    //function  GetArcMode

  public
    constructor  create(ACADCmp2D: TCADCmp2D; ACADViewport2D: TCADViewport2D ;AProgressBar: TProgressBar);
    destructor  destroy;

    procedure LoadFromFile(AFile: string);
    procedure Import;
    function  AddLineToCAD(AX, AY, AZ: single): integer;
  end;

var CommandsFirstCase: set of char = ['G','M', 'F'];

implementation

uses fMain;

const
  { Coordinate constants }

  INT_COORDINATE_NONE = 0;
  INT_COORDINATE_X = 1;
  INT_COORDINATE_Y = 2;
  INT_COORDINATE_Z = 3;
  
  INT_COORDINATE_I = 4;
  INT_COORDINATE_J = 5;
  INT_COORDINATE_K = 6;

  INT_COORDINATE_R = 7;
  INT_COORDINATE_P = 8;

  INT_ARC_MODE_0   = 0;
  INT_ARC_MODE_1   = 1;

  INT_ARC_DIRECTION_CW  = 0;
  INT_ARC_DIRECTION_CCW = 1;

  { GCode constants }

  STR_GCODE_RAPID_LINEAR_MOVE    = 'G00';
  STR_GCODE_LINEAR_MOVE          = 'G01';
  STR_GCODE_HELICAL_CW_MOVE      = 'G02';
  STR_GCODE_HELICAL_CCW_MOVE     = 'G03';

  STR_GCODE_UNIT_INCHES          = 'G20';
  STR_GCODE_UNIT_MILLIMETERS     = 'G21';

  STR_GCODE_RADIUS_COMP_CANCEL   = 'G40';
  STR_GCODE_RADIUS_COMP_LEFT     = 'G41';
  STR_GCODE_RADIUS_COMP_RIGHT    = 'G42';

  STR_GCODE_ABS_DIST_MODE        = 'G90';
  STR_GCODE_REL_DIST_MODE        = 'G91';

  STR_GCODE_ARC_CENTER_MODE_ABS  = 'G90.1';
  STR_GCODE_ARC_CENTER_MODE_REL  = 'G91.1';     //Relativ to Arc-startpoint

  STR_GCODE_PGM_PAUSE            = 'M0';
  STR_GCODE_PGM_END              = 'M2';
  STR_GCODE_PGM_END2             = 'M30';

  STR_GCODE_TOOL_DOWN            = 'M10';
  STR_GCODE_TOOL_UP              = 'M11';

{ TImportGCode }

constructor  TImportGCode.create(ACADCmp2D: TCADCmp2D; ACADViewport2D: TCADViewport2D ;AProgressBar: TProgressBar);
begin
  inherited create;
  fCADCmp2D              := ACADCmp2D;
  fCADViewport2D         := ACADViewport2D;
  fProgressBar           := AProgressBar;
  fInputList             := TStringList.Create;
  fIncremental           := false;
  fArcCenterREL          := true;
  fImportsRapidMove      := false;
end;

destructor  TImportGCode.destroy;
begin
  fInputList.Free;
  inherited free;
end;

procedure TImportGCode.LoadFromFile(AFile: string);
begin
  fInputList.LoadFromFile(AFile);
end;
{@@
  Reads a string and separates it in substring
  using ASeparator to delimite them.

  Limits:

  Number of substrings: 10 (indexed 0 to 9)
  Length of each substring: 255 (they are shortstrings)
}

function  TImportGCode.GetFirstCommandIDX(AArray: T10Strings): integer;
var i: integer; found: boolean;
begin
  i := 0;
  found := false;
  while ((i <= 9) and (not found)) do
  begin
    if (AArray[i][1] in CommandsFirstCase) then
      found := true;
    inc(i);
  end;
  if found then
    result := i -1
  else
    result := -1
end;

function TImportGCode.SeparateString(AString: string; ASeparator: Char): T10Strings;
var i, CurrentPart: Integer; hStr: T10Strings;  x: integer;
begin
  CurrentPart := 0;   x := 0;
  for i := 0 to 9 do Result[i] := '';  { Clears the result }
  for i := 1 to Length(AString) do  { Iterates througth the string, filling strings }
  begin
    if Copy(AString, i, 1) = ASeparator then
    begin
      Inc(CurrentPart);
      if CurrentPart > 9 then Exit; { Verifies if the string capacity wasn't exceeded }
    end
    else
      Result[CurrentPart] := Result[CurrentPart] + Copy(AString, i, 1);
  end;
  for i := 0 to 9 do  hStr[i] := '';
  for i := 0 to 9 do
  begin
    if   (Result[i] <> ASeparator) and (Result[i] <> '') then
    begin
      hStr[x] := Result[i];
      Inc(x);
    end;
  end;
  Result := hStr;
end;

procedure TImportGCode.ReadString(AStr: string);
var AParams: T10Strings; i: Integer; hLineCoords: TLine2DCoords;
    DestX, DestY, DestZ: Double;  FirstCommandIDX: integer;
begin
  AParams := SeparateString(AStr, ' ');
  FirstCommandIDX := GetFirstCommandIDX(AParams);
  //if FirstCommandIDX = -1 then
    //exit;

  if AParams[FirstCommandIDX] = STR_GCODE_REL_DIST_MODE  then
    fIncremental := true

  else if AParams[FirstCommandIDX] = STR_GCODE_ABS_DIST_MODE  then
    fIncremental := false

  else if AParams[FirstCommandIDX] = STR_GCODE_ARC_CENTER_MODE_ABS  then
     fArcCenterREL := false

  else if AParams[FirstCommandIDX] = STR_GCODE_ARC_CENTER_MODE_REL  then
     fArcCenterREL := true

  else if AParams[FirstCommandIDX] = STR_GCODE_TOOL_UP then
    LastZ := 0

  else if AParams[FirstCommandIDX] = STR_GCODE_TOOL_DOWN then
    LastZ := 50

  else if AParams[1] = STR_GCODE_RAPID_LINEAR_MOVE  then
  begin
    hLineCoords[0] := AParams[2];
    hLineCoords[1] := AParams[3];
    ProcessRapidMove(hLineCoords);
  end

  else if AParams[FirstCommandIDX] = STR_GCODE_LINEAR_MOVE then
  begin
    hLineCoords[0] := AParams[2];
    hLineCoords[1] := AParams[3];
    ProcessLinearMove(hLineCoords);
  end

  else if AParams[FirstCommandIDX] = STR_GCODE_HELICAL_CW_MOVE then
  begin
    {AddBezierToCAD(
      GetCoordinateValue(AParams[1]),
      GetCoordinateValue(AParams[2]),
      GetCoordinateValue(AParams[3]),
      GetCoordinateValue(AParams[4]),
      GetCoordinateValue(AParams[5]),
      GetCoordinateValue(AParams[6])
      );
    LastX := GetCoordinateValue(AParams[5]);
    LastY := GetCoordinateValue(AParams[6]);  }
  end
  else if AParams[FirstCommandIDX] = STR_GCODE_HELICAL_CCW_MOVE then
  begin
    {AData.AddBezierToPath(
      GetCoordinateValue(AParams[1]),
      GetCoordinateValue(AParams[2]),
      GetCoordinateValue(AParams[3]),
      GetCoordinateValue(AParams[4]),
      GetCoordinateValue(AParams[5]),
      GetCoordinateValue(AParams[6]),
      GetCoordinateValue(AParams[7]),
      GetCoordinateValue(AParams[8]),
      GetCoordinateValue(AParams[9])
      );

    LastX := GetCoordinateValue(AParams[7]);
    LastY := GetCoordinateValue(AParams[8]);
    LastZ := GetCoordinateValue(AParams[9]); }
  end else
  if fLastCommand = STR_GCODE_LINEAR_MOVE then
  begin
    hLineCoords[0] := AParams[1];
    hLineCoords[1] := AParams[2];
    ProcessLinearMove(hLineCoords);
  end;
end;

{@@
  The information of each separate path is lost in G-Code files
  Only one path uniting all of them is created when reading G-Code
}

function TImportGCode.GetCoordinate(AStr: shortstring): Integer;
begin
  Result := INT_COORDINATE_NONE;

  if AStr = '' then Exit
  else if AStr[1] = 'X' then Result := INT_COORDINATE_X
  else if AStr[1] = 'Y' then Result := INT_COORDINATE_Y
  else if AStr[1] = 'Z' then Result := INT_COORDINATE_Z;
end;

function TImportGCode.GetCoordinateValue(AStr: shortstring): Double;
begin
  Result := 0.0;
  if Length(AStr) <= 1 then Exit;
  Result := StrToFloat(Copy(AStr, 2, Length(AStr) - 1));
end;

procedure TImportGCode.Import;
var i: Integer;
begin
  for i := 0 to fInputList.Count - 1 do
    ReadString(fInputList[i]);
end;

procedure TImportGCode.ProcessRapidMove(ACoords: TLine2DCoords);
var i: integer; DestX, DestY, DestZ: Double;  hID: integer;
    x,y: double;
begin
  if fImportsRapidMove then
  begin
    hID := ProcessLinearMove(ACoords);
    TLine2D(fCADCmp2D.GetObject(hID)).PenStyle := psDOT;
  end else
  begin
    x := GetCoordinateValue(ACoords[0]);
    y := GetCoordinateValue(ACoords[1]);
    if fIncremental then
    begin
      LastX := LastX + x;
      LastY := LastY + y;
    end else
    begin
      LastX := x;
      LastY := y;
    end;
  end;
  fLastCommand := STR_GCODE_RAPID_LINEAR_MOVE;
end;

function TImportGCode.ProcessLinearMove(ACoords: TLine2DCoords): integer;
var i: integer; DestX, DestY, DestZ: Double;  hRes: integer;  XP, YP: boolean;
begin
  XP := false; YP := false;
    fLastCommand := STR_GCODE_LINEAR_MOVE;
    DestX := LastX; DestY := LastY; DestZ := LastZ;
    for i := 0 to 1 do
    begin
      case GetCoordinate(ACoords[i]) of
        INT_COORDINATE_X:
          begin
            DestX := GetCoordinateValue(ACoords[i]);
            XP := true;
          end;
        INT_COORDINATE_Y:
          begin
            DestY := GetCoordinateValue(ACoords[i]);
            YP := true;
          end;
      else
        //exit;  //error
      end;
    end;
    if not XP then   DestX :=  LastX;
    if not YP then   DestY :=  LastY;
    if fIncremental then
    begin
      if XP then DestX := DestX + LastX;
      if YP then DestY := DestY + LastY;
    end;
    hRes := AddLineToCAD(DestX, DestY, DestZ);
    LastX := DestX; LastY := DestY; LastZ := DestZ;
    result := hRes;
end;

function TImportGCode.AddLineToCAD(AX, AY, AZ: single): integer;
var TmpLine2D: TLine2D; P0, P1: TPoint2D;
begin
  P0.X := LastX;
  P0.Y := LastY;
  P0.W := 1;
  P1.X := AX;
  P1.Y := AY;
  P1.W := 1;
  TmpLine2D := TLine2D.Create(-1, P0, P1);
  fCADCmp2D.AddObject(-1, TmpLine2D);
  fCADCmp2D.RepaintViewports;
  result :=  TmpLine2D.ID;
end;

initialization

end.





