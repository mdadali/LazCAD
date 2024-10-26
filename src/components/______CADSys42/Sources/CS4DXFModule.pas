{: This help file explain all the classes defined for DXF handling
   for the CADSys 4.0 library for both the 2D and 3D use.

   These classes are defined in the CS4DXFModule unit file
   that you must include in the <B=uses> clause in all of your units
   that access the types mentioned here.

   <B=Note>: The DXF module given here is not yet completed
   for the DXF documentation is lack to be completed and
   easy to understant (at least to me). Thanks to
   <Code=Vincenzo Siviero (esiviero@@tin.it)> for his help.
   Thanks also to <Code=Giuseppe Staltieri (sgs@@elios.net)>
   for his invaluable support and beta testing.
}
unit CS4DXFModule;

Interface

uses SysUtils, Classes, Graphics, ComCtrls,
     CADSys4, CS4BaseTypes, CS4Shapes, LCLIntf;

type
// -----===== Starting Cs4DXFReadWrite.pas =====-----
  TSections = (scHeader, scClasses, scObjects, scThumbinalImage, scTables, scBlocks, scEntities, scUnknow);

  { Used to store readed groups from 0 group to 0 group. }
  TGroupTable = array[0..512] of Variant;

  TDXFRead = class(TObject)
  private
    fStream: TextFile;
    fCurrentSection: TSections;
    fGroupCode: Word;
    fGroupValue: Variant;
    fProgressBar: TProgressBar;

    procedure SetProgressBar(PB: TProgressBar);
  public
    constructor Create(FileName: String);
    destructor Destroy; override;
    procedure Rewind;
    { Return False if EOF. }
    function ConsumeGroup: Boolean;
    procedure NextSection;
    function ReadAnEntry(GroupDel: Word; var Values: TGroupTable): Word;

    property GroupCode: Word read FGroupCode;
    property GroupValue: Variant read FGroupValue;
    property CurrentSection: TSections read FCurrentSection;
    property PositionBar: TProgressBar read fProgressBar write SetProgressBar;
  end;

  TDXFWrite = class(TObject)
  private
    FStream: TextFile;
  public
    constructor Create(FileName: String);
    destructor Destroy; override;

    procedure Reset;
    procedure BeginSection(Sect: TSections);
    procedure EndSection(Sect: TSections);
    procedure WriteGroup(GroupCode: Word; GroupValue: Variant);
    procedure WriteAnEntry(var Values: TGroupTable);
  end;

  EDXFException = Exception;
  EDXFEndOfFile = EAbort;
  EDXFInvalidDXF = EDXFException;

// -----===== Starting Cs4DXF2DConverter.pas =====-----

  TDXF2DImport = class(TObject)
  private
    fTextFont: TVectFont;
    fDXFRead: TDXFRead;
    fScale: TRealType;
    fHasExtension, fUnableToReadAll, fVerbose: Boolean;
    fSetLayers: Boolean; // Se True i layers letti modificano quelli del CAD in base all'ordine di recupero.
    fCADCmp2D: TCADCmp2D;
    fAngleDir: TArcDirection;
    fExtension: TRect2D;
    fLayerList: TStringList; { Contain the name of the layers and the layer itself. }
    fBlockList: TStringList; { Contain the name of the blocks and the block itself. }

    procedure ReadEntitiesAsContainer(const Container: TContainer2D);
    { Read the DXF blocks. }
    procedure ReadBlocks;
    { Read the DXF entities. }
    procedure ReadEntities;
  protected
    // Leggono le entità
    function ReadLine2D(Entry: TGroupTable): TLine2D;
    function ReadCircle2D(Entry: TGroupTable): TCircle2D;
    function ReadCircularArc2D(Entry: TGroupTable): TCircularArc2D;
    function ReadEllipse2D(Entry: TGroupTable): TCurve2D;
    function ReadTrace2D(Entry: TGroupTable): TPolyline2D;
    function ReadSolid2D(Entry: TGroupTable): TPolyline2D;
    function ReadPolyline2D(Entry: TGroupTable): TPolyline2D;
    function ReadText2D(Entry: TGroupTable): TJustifiedVectText2D;
    function ReadSourceBlock(Entry: TGroupTable): TSourceBlock2D;
    function ReadBlock(Entry: TGroupTable): TBlock2D;
    function ReadEntity(IgnoreBlock: Boolean): TObject2D;

    function GoToSection(Sect: TSections): Boolean;

    property DXFRead: TDXFRead read FDXFRead;
  public
    constructor Create(const FileName: String; const CAD: TCADCmp2D);
    destructor Destroy; override;

    procedure SetTextFont(F: TVectFont);

    { Read the DXF header. }
    procedure ReadHeader;
    { Read the DXF tables. }
    procedure ReadTables;

    { Read the DXF informations. }
    procedure ReadDXF;
    { Read the DXF informations as a block. }
    function ReadDXFAsSourceBlock(const Name: TSourceBlockName): TSourceBlock2D;
    { Read the DXF informations as a container. }
    function ReadDXFAsContainer: TContainer2D;

    property HasExtension: Boolean read fHasExtension;
    property Extension: TRect2D read FExtension;
    property BlockList: TStringList read FBlockList;
    property LayerList: TStringList read FLayerList;
    property Scale: TRealType read fScale write fScale;
    property SetLayers: Boolean read fSetLayers write fSetLayers;
    property UnableToReadAllTheFile: Boolean read fUnableToReadAll;
    property Verbose: Boolean read fVerbose write fVerbose;
  end;

  TDXF2DExport = class(TObject)
  private
    FDXFWrite: TDXFWrite;
    FCADCmp2D: TCADCmp2D;
  protected
    procedure WriteLine2D(ALine: TLine2D);
    procedure WriteCircle2D(ACircle2D: TCircle2D);
    procedure WriteCircularArc2D(ACircularArc2D: TCircularArc2D);
    procedure WriteEllipticalArc2D(AEllipticalArc2D: TEllipticalArc2D);
    procedure WriteEllipse2D(AEllipse2D: TEllipse2D);
    procedure WriteFrame2D(AFrm: TFrame2D);
    procedure WriteCurve2D(ACurve: TCurve2D);
    procedure WriteOutline2D(APoly: TOutline2D);
    procedure WriteText2D(AText: TJustifiedVectText2D);
    procedure WriteBlock(ABlock: TBlock2D);
    procedure WriteEntity(AObj: TObject2D);
  public
    constructor Create(const FileName: String; const CAD: TCADCmp2D);
    destructor Destroy; override;

    { Write the DXF headers. }
    procedure WriteHeader;
    { Write the DXF tables. }
    procedure WriteTables;
    { Write the DXF blocks. }
    procedure WriteBlocks;
    { Write the DXF entities. }
    procedure WriteEntities;
    { Write the DXF informations. }
    procedure WriteDXF;
  end;

var
  Colors : array[0..255] of TColor;

  Implementation

uses Math, Dialogs, Forms, Variants;

// function body

// -----===== Starting Cs4DXFReadWrite.pas =====-----

function ColorToIndex(Col: TColor; Active: Boolean): Integer;
begin
  Result := 7;
  case Col of
   clBlack,
   clWhite: Result := 7;   //original from Piero.
   //clWhite: Result := 0;
   clRed: Result := 1;
   clYellow: Result := 2;
   clLime: Result := 3;
   clAqua: Result := 4;
   clBlue: Result := 5;
   clFuchsia: Result := 6;
   clGray: Result := 8;
   clLtGray: Result := 9;
  end;
  if not Active then Result := -1 * Result;
end;

procedure TDXFRead.SetProgressBar(PB: TProgressBar);
begin
  fProgressBar := PB;
  if Assigned(fProgressBar) then
   begin
     fProgressBar.Min := 0;
     //fProgressBar.Max := FileSize(fStream);
     //fProgressBar.Position := FilePos(fStream);
   end;
end;

constructor TDXFRead.Create(FileName: String);
begin
  inherited Create;
  fProgressBar := nil;
  AssignFile(FStream, FileName);
  Reset(FStream);
  ConsumeGroup;
  NextSection;
end;

destructor TDXFRead.Destroy;
begin
  CloseFile(FStream);
  inherited Destroy;
end;

procedure TDXFRead.Rewind;
begin
  Reset(FStream);
  if Assigned(fProgressBar) then
   fProgressBar.Position := 0;
  ConsumeGroup;
  NextSection;
end;

function TDXFRead.ConsumeGroup;
var
  TxtLine: String;
  LastSep: Char;
begin
  LastSep := DecimalSeparator;
  try
    ReadLn(FStream, FGroupCode);
    ReadLn(FStream, TxtLine);
    if EOF(FStream) then
     begin
       Result := False;
       Exit;
     end;
    case FGroupCode of
      0..9,
      999,
      1000..1009: FGroupValue := Trim(TxtLine);
      10..59,
      140..147,
      210..239,
      1010..1059: begin
        if Pos('.', TxtLine) > 0 then
         DecimalSeparator := '.'
        else
         DecimalSeparator := ',';
        try
         FGroupValue := StrToFloat(Trim(TxtLine));
        except
         FGroupValue := varEmpty;
        end;
      end;
      60..79,
      170..175,
      1060..1079 : begin
        if Pos('.', TxtLine) > 0 then
         DecimalSeparator := '.'
        else
         DecimalSeparator := ',';
        try
         FGroupValue := StrToInt(Trim(TxtLine));
        except
         FGroupValue := varEmpty;
        end;
      end;
    else
      FGroupValue := TxtLine;
    end;
    Result := True;
  finally
    DecimalSeparator := LastSep;
  end;
//  if Assigned(fProgressBar) then
//    fProgressBar.Position := FilePos(fStream);
end;

procedure TDXFRead.NextSection;
begin
  FCurrentSection := scUnknow;
  while not ((FGroupCode = 0) and (FGroupValue = 'SECTION')) do
   if ConsumeGroup = False then Exit;
  ConsumeGroup;
  if FGroupValue = 'HEADER' then
   FCurrentSection := scHeader
  else if FGroupValue = 'CLASSES' then
   FCurrentSection := scClasses
  else if FGroupValue = 'OBJECTS' then
   FCurrentSection := scObjects
  else if FGroupValue = 'THUMBNAILIMAGE' then
   FCurrentSection := scThumbinalImage
  else if FGroupValue = 'TABLES' then
   FCurrentSection := scTables
  else if FGroupValue = 'BLOCKS' then
   FCurrentSection := scBlocks
  else if FGroupValue = 'ENTITIES' then
   FCurrentSection := scEntities;
  ConsumeGroup;
end;

{ GroupDel = Group delimiter }
function TDXFRead.ReadAnEntry(GroupDel: Word; var Values: TGroupTable): Word;
begin
  Result := 0;
  { Find the start of the entry. }
  while (FGroupCode <> GroupDel) and (FGroupCode <> 0) do
   if ConsumeGroup = False then Exit;
  if (FGroupCode = 0) and (FGroupCode <> GroupDel) then Exit;
  { Read all values. }
  repeat
   if FGroupCode < 256 then
    Values[FGroupCode] := FGroupValue
   else if FGroupCode > 999 then
    { The extended data types are remapped from 256=1000. }
    Values[FGroupCode - 744] := FGroupValue;
   ConsumeGroup;
  until (FGroupCode = GroupDel) or (FGroupCode = 0);
  Result := GroupDel;
end;

{ --================== DXFWriter ==================-- }

constructor TDXFWrite.Create(FileName: String);
begin
  inherited Create;
  AssignFile(FStream, FileName);
  Rewrite(FStream);
end;

destructor TDXFWrite.Destroy;
begin
  CloseFile(FStream);
  inherited Destroy;
end;

procedure TDXFWrite.Reset;
begin
  Rewrite(FStream);
end;

procedure TDXFWrite.WriteGroup(GroupCode: Word; GroupValue: Variant);
var
  TxtLine: String;
  LastSep: Char;
begin
  LastSep := DecimalSeparator;
  try
    DecimalSeparator := '.';
    WriteLn(FStream, Format('%3d', [GroupCode]));
    case GroupCode of
      0..9,
      999,
      1000..1009: TxtLine := Copy(GroupValue, 1, 255);
      10..59,
      140..147,
      210..239,
      1010..1059: TxtLine := Format('%.6f', [Double(GroupValue)]);
      60..79,
      170..175,
      1060..1079 : TxtLine := Format('%6d', [Integer(GroupValue)]);
    else
      TxtLine := '';
    end;
    WriteLn(FStream, TxtLine);
  finally
    DecimalSeparator := LastSep;
  end;
end;

procedure TDXFWrite.BeginSection(Sect: TSections);
begin
  WriteGroup(0, 'SECTION');
  case Sect of
   scHeader: WriteGroup(2, 'HEADER');
   scTables: WriteGroup(2, 'TABLES');
   scBlocks: WriteGroup(2, 'BLOCKS');
   scEntities: WriteGroup(2, 'ENTITIES');
  end;
end;

procedure TDXFWrite.EndSection(Sect: TSections);
begin
  WriteGroup(0, 'ENDSEC');
end;

{ Table Index => DXF file group value
  0-255       => 0-255
  256-512     => 1000-1255 }
procedure TDXFWrite.WriteAnEntry(var Values: TGroupTable);
var
  Cont: Word;
begin
  for Cont := 0 to 255 do
   if VarType(Values[Cont]) > 0 then
    WriteGroup(Cont, Values[Cont]);
  for Cont := 1000 to 1255 do
   if VarType(Values[Cont - 744]) > 0 then
    WriteGroup(Cont, Values[Cont - 744]);
end;

// -----===== Starting Cs4DXF2DConverter.pas =====-----

{ --================ DXF2DImport ==================-- }

procedure TDXF2DImport.SetTextFont(F: TVectFont);
begin
  fTextFont := F;
end;

function TDXF2DImport.ReadLine2D(Entry: TGroupTable): TLine2D;
begin
  Result := TLine2D.Create(0, Point2D(Entry[10], Entry[20]),
                              Point2D(Entry[11], Entry[21]));
end;

function TDXF2DImport.ReadCircle2D(Entry: TGroupTable): TCircle2D; //TEllipticalArc2D;
var P0, P1: TPoint2D; r, sa, ea: TrealType;
begin
  //P0 := Point2D(Entry[10] - Entry[40], Entry[20] - Entry[40]);
  //P1 := Point2D(Entry[10] + Entry[40], Entry[20] + Entry[40]);
  P0 := Point2D(Entry[10] , Entry[20]);
  r :=  Entry[40];
  result := TCircle2D.Create(-1, P0, r);
  TCircle2D(result).Direction := FAngleDir;
  //TCircle2D(result).Points[1] := P1;
  // result := TEllipticalArc2D.Create(-1, P0, P1, 0.000001, 0.000001);
  //TEllipticalArc2D(result).Direction := FDirAngle;
end;

function TDXF2DImport.ReadCircularArc2D(Entry: TGroupTable): TCircularArc2D;
var SA, EA: TRealType;
begin
  SA := DegToRad(Entry[50]);  if SA = 0 then SA := 0.000001;
  EA := DegToRad(Entry[51]);  if EA = 0 then EA := 0.000001;
  //result := TCircularArc2D.Create(-1, Point2D(Entry[10], Entry[10]), Entry[40], SA, EA);
  Result := TCircularArc2D.Create(-1, Point2D(Entry[10], Entry[20]), Entry[40], SA, EA);

  //result := TCircularArc2D.Create(-1, Point2D(Entry[10], Entry[10]), Entry[40], SA, EA);

  Result.Direction := FAngleDir;
end;


function TDXF2DImport.ReadEllipse2D(Entry: TGroupTable): TCurve2D;
var
  CenterPt, MajorAx: TPoint2D;
  MinorLen, MajorLen, SA, EA, RotA: TRealType;
begin
  CenterPt := Point2D(Entry[10], Entry[20]);
  MajorAx := Point2D(Entry[11], Entry[21]);
  MinorLen := Entry[40];
  MajorLen := PointDistance2D(CenterPt, MajorAx);
  SA := Entry[41];
  EA := Entry[42];
  //SA := DegToRad(SA);
  //EA := DegToRad(EA);
  RotA := ArcTan2(MajorAx.Y - CenterPt.Y, MajorAx.X - CenterPt.X);

  if (SA = 0) and (EA >= 2 * Pi) then
   // Ellisse completa
   Result := TEllipse2D.Create(0, CenterPt, CenterPt)
  else
   // Arco di ellisse
   Result := TEllipticalArc2D.Create(0, CenterPt, CenterPt, SA, EA);
  with Result do
   begin
     Points[0] := Point2D(CenterPt.X - MajorLen, CenterPt.X - MinorLen);
     Points[1] := Point2D(CenterPt.X + MajorLen, CenterPt.X + MinorLen);
     if RotA <> 0 then
      ModelTransform := Rotate2D(RotA);
   end;
end;

{function TDXF2DImport.ReadEllipse2D(Entry: TGroupTable): TCurve2D;
var
  Center: TPoint2D;
  MajorAxisVector: TPoint2D;
  MajorAxisLength, MinorAxisLength: Double;
  SA, EA: TRealType;
  Direction: TArcDirection;
  AngleDifference: TRealType;
  TopLeft, BottomRight: TPoint2D;

  // Konvertiert Grad in Radiant
  function DegToRad(Deg: TRealType): TRealType;
  begin
    Result := Deg * (Pi / 180);
  end;

  // Berechnet die Position eines Punktes entlang der Haupt- und Nebenachsen
  function CalculateCorner(const Center: TPoint2D; const AxisVector: TPoint2D;
                           MajorLength, MinorLength: Double; const SignMajor, SignMinor: Integer): TPoint2D;
  var
    Perpendicular: TPoint2D;
    NormalizedAxis: TPoint2D;
  begin
    // Normalisieren des Hauptachsenvektors
    NormalizedAxis.X := AxisVector.Y / Hypot(AxisVector.X, AxisVector.Y);
    NormalizedAxis.Y := -AxisVector.X / Hypot(AxisVector.X, AxisVector.Y);

    Perpendicular.X := NormalizedAxis.X * MinorLength * SignMinor;
    Perpendicular.Y := NormalizedAxis.Y * MinorLength * SignMinor;

    Result.X := Center.X + AxisVector.X * MajorLength * SignMajor + Perpendicular.X;
    Result.Y := Center.Y + AxisVector.Y * MajorLength * SignMajor + Perpendicular.Y;
  end;

begin
  // Mittelpunkt der Ellipse
  Center.X := Entry[10];
  Center.Y := Entry[20];

  // Hauptachse Vektor
  MajorAxisVector.X := Entry[11];
  MajorAxisVector.Y := Entry[21];

  // Berechnung der Länge der Hauptachse und der Nebenachse
  MajorAxisLength := Hypot(MajorAxisVector.X, MajorAxisVector.Y);
  MinorAxisLength := Entry[40];

  // Normierung des Hauptachsenvektors
  MajorAxisVector.X := MajorAxisVector.X / MajorAxisLength;
  MajorAxisVector.Y := MajorAxisVector.Y / MajorAxisLength;

  // Berechnung der Ecken des umrahmenden Rechtecks
  TopLeft := CalculateCorner(Center, MajorAxisVector, MajorAxisLength / 2, MinorAxisLength / 2, 1, 1);
  BottomRight := CalculateCorner(Center, MajorAxisVector, MajorAxisLength / 2, MinorAxisLength / 2, -1, -1);

  // Start- und Endwinkel in Grad, dann Umwandlung in Bogenmaß
  //SA := DegToRad(Entry[41]);
  //EA := DegToRad(Entry[42]);
  SA := Entry[41];
  EA := Entry[42];

  // Überprüfen, ob der Bogen eine komplette Ellipse ist
  //if Abs(EA - SA) >= 2 * Pi then
  if (SA = 0) and (EA >= 2 * Pi) then
  begin
    // Setze den Endwinkel für eine geschlossene Ellipse
    EA := SA + 2 * Pi;
    Direction := CounterClockwise; // Standardmäßig gegen den Uhrzeigersinn für eine vollständige Ellipse
  end
  else
  begin
    // Normalisieren des Endwinkels, wenn EA kleiner als SA ist
    if EA < SA then
      EA := EA + 2 * Pi;

    // Berechnung der Richtung für einen unvollständigen Bogen
    AngleDifference := EA - SA;
    if AngleDifference <= Pi then
      Direction := CounterClockwise
    else
      Direction := Clockwise;
  end;

  // Erstellen der Ellipse
  Result := TEllipticalArc2D.Create(
    -1,               // ID, falls benötigt
    TopLeft,          // Obere linke Ecke des umrahmenden Rechtecks
    BottomRight,      // Untere rechte Ecke des umrahmenden Rechtecks
    SA,               // Startwinkel in Radiant
    EA                // Endwinkel in Radiant
  );

  // Setze die Richtung der Ellipse
  Result.Direction := Direction;
end;}

function TDXF2DImport.ReadTrace2D(Entry: TGroupTable): TPolyline2D;
begin
  Result := TPolyline2D.Create(0, [Point2D(Entry[10], Entry[20]),
                                   Point2D(Entry[11], Entry[21]),
                                   Point2D(Entry[12], Entry[22]),
                                   Point2D(Entry[13], Entry[23])]);
end;

function TDXF2DImport.ReadSolid2D(Entry: TGroupTable): TPolyline2D;
begin
  if VarType(Entry[13]) = varEmpty then
   Result := TPolyline2D.Create(0, [Point2D(Entry[10], Entry[20]),
                                    Point2D(Entry[11], Entry[21]),
                                    Point2D(Entry[12], Entry[22]),
                                    Point2D(Entry[10], Entry[20])])
  else
   Result := TPolyline2D.Create(0, [Point2D(Entry[10], Entry[20]),
                                    Point2D(Entry[11], Entry[21]),
                                    Point2D(Entry[12], Entry[22]),
                                    Point2D(Entry[13], Entry[23]),
                                    Point2D(Entry[10], Entry[20])]);
end;

function TDXF2DImport.ReadText2D(Entry: TGroupTable): TJustifiedVectText2D;
var
  TmpRect: TRect2D;
begin
  Result := nil;
  if fTextFont = nil then
   Exit;
  TmpRect.FirstEdge := Point2D(Entry[10], Entry[20]);
  TmpRect.SecondEdge := Point2D(Entry[10], Entry[20] + Entry[40]);
  Result := TJustifiedVectText2D.Create(0, fTextFont, TmpRect, Entry[40], Entry[1]);
  if Entry[1] = 'F' then
  begin
   Result.DrawBox := True;
  end;
  if VarType(Entry[72]) <> 0 then
   begin
     case Entry[72] of
      1: Result.HorizontalJust := jhCenter;
      2: Result.HorizontalJust := jhRight;
     end;
     case Entry[73] of
      1: Result.VerticalJust := jvBottom;
      2: Result.VerticalJust := jvCenter;
     end;
     if (Entry[72] > 0) or (Entry[73] > 0) then
      Result.Points[1] := Point2D(Entry[11], Entry[21]);
   end;
  if VarType(Entry[50]) <> 0 then
   begin
     Result.Transform(Translate2D(-Result.Points[1].X, -Result.Points[1].Y));
     Result.Transform(Rotate2D(DegToRad(Entry[50])));
     Result.Transform(Translate2D(Result.Points[1].X, Result.Points[1].Y));
   end;
  Result.DrawBox := False;
end;

function TDXF2DImport.ReadPolyline2D(Entry: TGroupTable): TPolyline2D;
var
  LocalEntry: TGroupTable;
  IsClosedInM, IsSpline2D: Boolean;
begin
  // Considera le polilinee 2D e 3D alla stessa maniera.
  // Determina i flags della polilinea.
  if VarType(Entry[70]) <> varEmpty then
   begin
     IsClosedInM := Entry[70] and 1 = 1;
     IsSpline2D := (Entry[70] and 2 = 2) or (Entry[70] and 4 = 4);
   end
  else
   begin
     IsClosedInM := False;
     IsSpline2D := False;
   end;

  if IsSpline2D then
   begin // Leggo la spline2D
     Result := TPolyline2D.Create(0, [Point2D(0, 0)]);
     Result.Points.Delete(0);
     // Leggo soli punti aggiunti, quindi gruppo 70 con bit 8.
     fDXFRead.ReadAnEntry(0, LocalEntry);
     while LocalEntry[0] = 'VERTEX' do
      begin
        if (VarType(LocalEntry[70]) <> varEmpty)
           and (LocalEntry[70] and 8 = 8) then
         Result.Points.Add(Point2D(LocalEntry[10], LocalEntry[20]));
        FDXFRead.ReadAnEntry(0, LocalEntry);
      end;
     if Result.Points.Count = 0 then
      begin // La spline è stata creata senza i punti aggiuntivi.
        Result.Free;
        Result := nil;
        fUnableToReadAll := True;
        if fVerbose then
         ShowMessage('Spline without spline-fitting isn''t supported.');
      end;
   end
  else
   begin // Leggo la polilinea2D
     Result := TPolyline2D.Create(0, [Point2D(0, 0)]);
     Result.Points.Delete(0);
     // Leggo tutti i punti.
     fDXFRead.ReadAnEntry(0, LocalEntry);
     while LocalEntry[0] = 'VERTEX' do
      begin
        Result.Points.Add(Point2D(LocalEntry[10], LocalEntry[20]));
        FDXFRead.ReadAnEntry(0, LocalEntry);
      end;
     if IsClosedInM then // la chiudo.
      Result.Points.Add(Result.Points[0]);
   end;
  if (LocalEntry[0] <> 'SEQEND') then
   begin
     fUnableToReadAll := True;
     Raise EDXFInvalidDXF.Create('Invalid DXF file.');
   end;
end;

function TDXF2DImport.ReadEntity(IgnoreBlock: Boolean): TObject2D;
var
  Entry: TGroupTable;
  NLayer: Integer;
begin
  Result := nil;
  FDXFRead.ReadAnEntry(0, Entry);
  NLayer := FLayerList.IndexOf(Entry[8]);
  if NLayer > -1 then
   FCADCmp2D.CurrentLayer := NLayer;
  if Entry[0] = 'LINE' then
   Result := ReadLine2D(Entry)
  else if Entry[0] = 'ARC' then
   Result := ReadCircularArc2D(Entry)
  else if Entry[0] = 'CIRCLE' then
   Result := ReadCircle2D(Entry)
  else if Entry[0] = 'ELLIPSE' then // Ellipse and Elliptical_ARC
   Result := ReadEllipse2D(Entry)
  else if Entry[0] = 'TRACE' then
   Result := ReadTrace2D(Entry)
  else if Entry[0] = 'SOLID' then
   Result := ReadSolid2D(Entry)
   //Result := ReadEllipticalArc2D(Entry)
  else if Entry[0] = 'POLYLINE' then
   Result := ReadPolyline2D(Entry)
  else if Entry[0] = 'TEXT' then
   Result := ReadText2D(Entry)
  else if (not IgnoreBlock) and (Entry[0] = 'INSERT') then
   Result := ReadBlock(Entry);
end;

function TDXF2DImport.ReadBlock(Entry: TGroupTable): TBlock2D;
var
  TmpSource: TSourceBlock2D;
  TmpTransf, ScaleTransf, RotTransf: TTransf2D;
begin
  Result := nil;
  try
    TmpSource := FCADCmp2D.FindSourceBlock(StringToBlockName(Entry[2]));
  except
   fUnableToReadAll := True;
   if fVerbose then
    ShowMessage('Some blocks cannot be read.');
   Exit;
  end;
  if TmpSource <> nil then
   begin
     Result := TBlock2D.Create(0, TmpSource);
     // Rotation
     if VarType(Entry[50]) <> varEmpty then
      RotTransf := Rotate2D(DegToRad(Entry[50]))
     else
      RotTransf := IdentityTransf2D;
     // Scale.
     ScaleTransf := IdentityTransf2D;
     if VarType(Entry[41]) <> varEmpty then
      ScaleTransf[1, 1] := Entry[41];
     if VarType(Entry[42]) <> varEmpty then
      ScaleTransf[2, 2] := Entry[42];
     TmpTransf := MultiplyTransform2D(ScaleTransf, RotTransf);
     TmpTransf := MultiplyTransform2D(TmpTransf, Translate2D(Entry[10], Entry[20]));
     Result.ModelTransform := TmpTransf;
     Result.ApplyTransform;
     Result.UpdateExtension(Self);
   end;
end;

procedure TDXF2DImport.ReadEntitiesAsContainer(const Container: TContainer2D);
var
 Tmp: TObject2D;
 ID: Integer;
begin
  ID := 0;
  while DXFRead.GroupValue <> 'ENDSEC' do
   begin
     Tmp := ReadEntity(True);
     if Assigned(Tmp) then
      begin
        Tmp.Transform(Scale2D(fScale, fScale));
        Tmp.ApplyTransform;
        Tmp.ID := ID;
        Container.Objects.Add(Tmp);
        Inc(ID)
      end
     else
      fUnableToReadAll := True;
   end;
  Container.UpdateExtension(Self);
end;

constructor TDXF2DImport.Create(const FileName: String; const CAD: TCADCmp2D);
begin
  inherited Create;
  fDXFRead := TDXFRead.Create(FileName);
  fCADCmp2D := CAD;
  fLayerList := TStringList.Create;
  fBlockList := TStringList.Create;
  fSetLayers := true;
  fScale := 1.0;
  fHasExtension := False;
  fUnableToReadAll := False;
end;

destructor TDXF2DImport.Destroy;
begin
  fDXFRead.Free;
  fLayerList.Free;
  fBlockList.Free;
  inherited Destroy;
end;

function TDXF2DImport.GoToSection(Sect: TSections): Boolean;
begin
  FDXFRead.Rewind;
  while (FDXFRead.CurrentSection <> scUnknow) and
        (FDXFRead.CurrentSection <> Sect) do
   FDXFRead.NextSection;
  Result := FDXFRead.CurrentSection = Sect;
end;

procedure TDXF2DImport.ReadDXF;
begin
  if fCADCmp2D = nil then Exit;
  fUnableToReadAll := False;
  FDXFRead.Rewind;
  while (FDXFRead.CurrentSection <> scUnknow) do
   begin
     case FDXFRead.CurrentSection of
      scHeader: ReadHeader;
      scTables: ReadTables;
      scBlocks: ReadBlocks;
      scEntities: ReadEntities;
     end;
     FDXFRead.NextSection;
   end;
end;

function TDXF2DImport.ReadDXFAsSourceBlock(const Name: TSourceBlockName): TSourceBlock2D;
begin
  fUnableToReadAll := False;
  ReadHeader;
  ReadBlocks;
  Result := TSourceBlock2D.Create(0, Name, [nil]);
  try
   FDXFRead.Rewind;
   while (FDXFRead.CurrentSection <> scUnknow) do
    begin
      if FDXFRead.CurrentSection = scEntities then
       ReadEntitiesAsContainer(Result);
      FDXFRead.NextSection;
    end;
   Result.UpdateExtension(Self);
  except
   on Exception do
    begin
      Result.Free;
      Result := nil;
    end;
  end;
end;

function TDXF2DImport.ReadDXFAsContainer: TContainer2D;
begin
  fUnableToReadAll := False;
  ReadHeader;
  ReadBlocks;
  Result := TContainer2D.Create(0, [nil]);
  try
   FDXFRead.Rewind;
   while (FDXFRead.CurrentSection <> scUnknow) do
    begin
      if FDXFRead.CurrentSection = scEntities then
       ReadEntitiesAsContainer(Result);
      FDXFRead.NextSection;
    end;
   Result.UpdateExtension(Self);
  except
   on Exception do
    begin
      Result.Free;
      Result := nil;
    end;
  end;
end;

procedure TDXF2DImport.ReadHeader;
var
  Entry: TGroupTable;
begin
  fHasExtension := False;
  fExtension.Right := 1000;
  fExtension.Top := 1000;
  fExtension.W1 := 1.0;
  fExtension.Left := -1000;
  fExtension.Bottom := -1000;
  fExtension.W2 := 1.0;
  fAngleDir := adCounterClockwise;
  while fDXFRead.ReadAnEntry(9, Entry) <> 0 do
   begin
     if Entry[9] = '$ANGDIR' then
      begin
        if Entry[70] = 1 then
         fAngleDir := adClockwise
        else
         fAngleDir := adCounterClockwise;
      end
     else if Entry[9] = '$EXTMAX' then
      begin
        fExtension.Right := Entry[10];
        fExtension.Top := Entry[20];
        fHasExtension := True;
      end
     else if Entry[9] = '$EXTMIN' then
      begin
        fExtension.Left := Entry[10];
        fExtension.Bottom := Entry[20];
        fHasExtension := True;
      end;
   end;
  if fHasExtension then
   FExtension := ReOrderRect2D(FExtension);
end;

procedure TDXF2DImport.ReadTables;
var
  Entry: TGroupTable;
begin
  if fCADCmp2D = nil then Exit;
  fDXFRead.ReadAnEntry(0, Entry);
  while fDXFRead.GroupValue <> 'ENDSEC' do
  begin
     if Entry[0] = 'LAYER' then
     begin
       if fSetLayers then
         with fCADCmp2D.Layers[fLayerList.Count] do
         begin
           Pen.Color := Colors[Abs(Round(Float(Entry[62])))];
           Brush.Style := bsClear;
           Active := Entry[62] >= 0;
           Name := Entry[2];
           //ShowMessage(Name);
         end;
       fLayerList.AddObject(Entry[2], fCADCmp2D.Layers[fLayerList.Count]);
     end;
     fDXFRead.ReadAnEntry(0, Entry);
  end;
end;

procedure TDXF2DImport.ReadEntities;
var
 Tmp: TObject2D;
begin
  if fCADCmp2D = nil then Exit;
  while DXFRead.GroupValue <> 'ENDSEC' do
   begin
     Tmp := ReadEntity(False);
     if Assigned(Tmp) then
      begin
        Tmp.Transform(Scale2D(fScale, fScale));
        Tmp.ApplyTransform;
        FCADCmp2D.AddObject(-1, Tmp);
      end
     else
      fUnableToReadAll := True;
   end;
end;

procedure TDXF2DImport.ReadBlocks;
var
  Entry: TGroupTable;
  Tmp: TSourceBlock2D;
  NLayer: Integer;
begin
  if fCADCmp2D = nil then Exit;
  while DXFRead.GroupValue <> 'ENDSEC' do
   begin
     FDXFRead.ReadAnEntry(0, Entry);
     NLayer := FLayerList.IndexOf(Entry[8]);
     if NLayer > -1 then
      FCADCmp2D.CurrentLayer := NLayer;
     if Entry[0] = 'BLOCK' then
      begin
        Tmp := ReadSourceBlock(Entry);
        if Assigned(Tmp) then
         begin
           FCADCmp2D.AddSourceBlock(Tmp);
           { Entry[2] contains the name of the block for future ref. }
           fBlockList.AddObject(Entry[2], Tmp);
         end;
      end;
   end;
end;

function TDXF2DImport.ReadSourceBlock(Entry: TGroupTable): TSourceBlock2D;
var
  BasePoint: TPoint2D;
  Tmp: TObject2D;
  TmpName: TSourceBlockName;
begin
  Result := nil;
  if (Entry[70] and $4) or (Entry[70] and $1) then
   { XRef and anonymous are not allowed. }
   Exit;
  BasePoint.X := Entry[10];
  BasePoint.Y := Entry[20];
  BasePoint.W := 1.0;
  if Entry[2] = '' then
   TmpName := StringToBlockName(Format('BLOCK%d', [FCADCmp2D.SourceBlocksCount]))
  else
   TmpName := StringToBlockName(Entry[2]);
  Result := TSourceBlock2D.Create(0, TmpName, [nil]);
  while FDXFRead.GroupValue <> 'ENDBLK' do
   begin
     Tmp := ReadEntity(False);
     if Assigned(Tmp) then
      Result.Objects.Add(Tmp);
   end;
  if Result.Objects.Count > 0 then
   begin
     Result.Transform(Translate2D(-BasePoint.X, -BasePoint.Y));
     Result.ApplyTransform;
     Result.UpdateExtension(Self);
   end
  else
   begin
     Result.Free;
     Result := nil;
   end;
end;

{ --================ DXF2DExport ==================-- }

constructor TDXF2DExport.Create(const FileName: String; const CAD: TCADCmp2D);
begin
  inherited Create;
  FDXFWrite := TDXFWrite.Create(FileName);
  FCADCmp2D := CAD;
end;

destructor TDXF2DExport.Destroy;
begin
  FDXFWrite.WriteGroup(0, 'EOF');
  FDXFWrite.Free;
  inherited Destroy;
end;

procedure TDXF2DExport.WriteDXF;
begin
  if fCADCmp2D = nil then Exit;
  FDXFWrite.Reset;
  WriteHeader;
  WriteTables;
  WriteBlocks;
  WriteEntities;
end;

procedure TDXF2DExport.WriteHeader;
var
  TmpInt: Integer;
begin
  if fCADCmp2D = nil then Exit;
  with FDXFWrite do
   begin
     BeginSection(scHeader);
     { Angle direction, Default CounterClockWise }
     WriteGroup(9, '$ANGDIR');
     TmpInt := 0;
     WriteGroup(70, TmpInt);
     { Extension of the drawing }
     WriteGroup(9, '$EXTMAX');
     WriteGroup(10, FCADCmp2D.DrawingExtension.Right);
     WriteGroup(20, FCADCmp2D.DrawingExtension.Top);
     WriteGroup(30, 0);
     WriteGroup(9, '$EXTMIN');
     WriteGroup(10, FCADCmp2D.DrawingExtension.Left);
     WriteGroup(20, FCADCmp2D.DrawingExtension.Bottom);
     WriteGroup(30, 0);
     WriteGroup(9, '$CECOLOR');
     WriteGroup(62, 256);
     WriteGroup(9, '$CELTYPE');
     WriteGroup(6, 'BYLAYER');
     { End Header Section }
     EndSection(scHeader);
   end;
end;

procedure TDXF2DExport.WriteTables;
var
  Count: Integer;
begin
  if fCADCmp2D = nil then Exit;
  with FDXFWrite do
   begin
     { Begin Tables Section }
     BeginSection(scTables);
     WriteGroup(0, 'TABLE');
     WriteGroup(2, 'LTYPE');
     WriteGroup(70, 1);
     WriteGroup(0, 'LTYPE');
     WriteGroup(2, 'CONTINUOUS');
     WriteGroup(70, 64);
     WriteGroup(3, 'Solid line');
     WriteGroup(72, 65);
     WriteGroup(73, 0);
     WriteGroup(40, 0.0);
     WriteGroup(0, 'ENDTAB');
     WriteGroup(0, 'TABLE');
     WriteGroup(2, 'LAYER');
     WriteGroup(70, 257);
     WriteGroup(0, 'LAYER');
     WriteGroup(2, '0');
     WriteGroup(70, 64);
     WriteGroup(62, 7);
     WriteGroup(6, 'CONTINUOUS');
     for Count := 0 to MAX_LAYER_NUMBER do
      with FCADCmp2D.Layers[Count] do
       if Modified then
        begin
          WriteGroup(0, 'LAYER');
          WriteGroup(2, Name);
          WriteGroup(70, 64);
          WriteGroup(62, ColorToIndex(Pen.Color, Active));
          WriteGroup(6, 'CONTINUOUS');
        end;
     WriteGroup(0, 'ENDTAB');
     { End Tables Section }
     EndSection(scTables);
   end;
end;

procedure TDXF2DExport.WriteEntities;
var
  TmpIter: TGraphicObjIterator;
  TmpObj: TObject2D;
begin
  if fCADCmp2D = nil then Exit;
  FDXFWrite.BeginSection(scEntities);
  TmpIter := fCADCmp2D.ObjectsIterator;
  try
    TmpObj := TmpIter.First as TObject2D;
    while TmpObj <> nil do
     begin
       WriteEntity(TmpObj);
       TmpObj := TmpIter.Next as TObject2D;
     end;
  finally
    TmpIter.Free;
    FDXFWrite.EndSection(scEntities);
  end;
end;

procedure TDXF2DExport.WriteBlocks;
var
  TmpIter: TGraphicObjIterator;
  TmpObj: TObject2D;
begin
  if fCADCmp2D = nil then Exit;
  FDXFWrite.BeginSection(scBlocks);
  TmpIter := fCADCmp2D.SourceBlocksIterator;
  try
    TmpObj := TmpIter.First as TObject2D;
    while TmpObj <> nil do
     begin
       WriteEntity(TmpObj);
       TmpObj := TmpIter.Next as TObject2D;
     end;
  finally
    TmpIter.Free;
    FDXFWrite.EndSection(scBlocks);
  end;
end;

procedure TDXF2DExport.WriteLine2D(ALine: TLine2D);
var
 TmpPnt: TPoint2D;
begin
  with FDXFWrite, ALine do
   begin
     TmpPnt := TransformPoint2D(Points[0], ModelTransform);
     WriteGroup(10, TmpPnt.X);
     WriteGroup(20, TmpPnt.Y);
     WriteGroup(30, 0);
     TmpPnt := TransformPoint2D(Points[1], ModelTransform);
     WriteGroup(11, TmpPnt.X);
     WriteGroup(21, TmpPnt.Y);
     WriteGroup(31, 0);
   end;
end;

procedure TDXF2DExport.WriteCircle2D(ACircle2D: TCircle2D);
var TmpPnt: TPoint2D;
begin
  with FDXFWrite, ACircle2D do
   begin
     TmpPnt := TransformPoint2D(Points[0], ModelTransform);
     WriteGroup(10, TmpPnt.X);
     WriteGroup(20, TmpPnt.Y);
     WriteGroup(40, ACircle2D.Radius);
   end;
end;

procedure TDXF2DExport.WriteCircularArc2D(ACircularArc2D: TCircularArc2D);
var TmpPnt: TPoint2D;
begin
  with FDXFWrite, ACircularArc2D do
   begin
     TmpPnt := TransformPoint2D(Points[0], ModelTransform);
     WriteGroup(10, TmpPnt.X);
     WriteGroup(20, TmpPnt.Y);
     WriteGroup(40, ACircularArc2D.Radius);
     WriteGroup(50, RadToDeg(ACircularArc2D.StartAngle));
     WriteGroup(51, RadToDeg(ACircularArc2D.EndAngle));
   end;
end;

procedure TDXF2DExport.WriteEllipticalArc2D(AEllipticalArc2D: TEllipticalArc2D);
var TmpPnt, P0, P1: TPoint2D; Center: TPoint2D; SA, EA, MajorAxis, MinorAxis: TRealType;
begin
  with FDXFWrite, AEllipticalArc2D do
  begin
    P0 := AEllipticalArc2D.Points[0];
    P1 := AEllipticalArc2D.Points[1];
    P0 := TransformPoint2D(P0, AEllipticalArc2D.ModelTransform);
    P1 := TransformPoint2D(P1, AEllipticalArc2D.ModelTransform);

    SA := AEllipticalArc2D.StartAngle;
    EA := AEllipticalArc2D.EndAngle;
    SA := RadToDeg(SA);
    EA := RadToDeg(EA);

    // Berechne den Mittelpunkt der Ellipse
    Center.X := (P0.X + P1.X) / 2;
    Center.Y := (P0.Y + P1.Y) / 2;

    // Berechne die Achsen der Ellipse
    MajorAxis := Abs(P1.X - P0.X) / 2;
    MinorAxis := Abs(P1.Y - P0.Y) / 2;

    // Transformiere den Mittelpunkt
    TmpPnt := TransformPoint2D(Center, AEllipticalArc2D.ModelTransform);

    // Schreiben der Daten in die DXF-Datei
    WriteGroup(10, TmpPnt.X);            // Zentrum X
    WriteGroup(20, TmpPnt.Y);            // Zentrum Y
    WriteGroup(40, MajorAxis);           // Major Axis
    WriteGroup(41, MinorAxis);           // Minor Axis
    WriteGroup(50, SA);                  // Startwinkel
    WriteGroup(51, EA);                  // Endwinkel
  end;
end;

procedure TDXF2DExport.WriteEllipse2D(AEllipse2D: TEllipse2D);
var TmpPnt: TPoint2D; Center, P0, P1: TPoint2D; MajorAxis, MinorAxis: Double;
begin
  with FDXFWrite, AEllipse2D do
  begin
    P0 := AEllipse2D.Points[0];
    P0 := TransformPoint2D(P0, AEllipse2D.ModelTransform);
    P1 := AEllipse2D.Points[1];
    P1 := TransformPoint2D(P1, AEllipse2D.ModelTransform);

    // Berechne den Mittelpunkt der Ellipse
    Center.X := (P0.X + P1.X) / 2;
    Center.Y := (P0.Y + P1.Y) / 2;

    // Berechne die Achsen der Ellipse
    MajorAxis := Abs(P1.X - P0.X) / 2;
    MinorAxis := Abs(P1.Y - P0.Y) / 2;

    // Transformiere den Mittelpunkt
    TmpPnt := TransformPoint2D(Center, ModelTransform);

    // Schreiben der Daten in die DXF-Datei
    WriteGroup(10, TmpPnt.X);            // Zentrum X
    WriteGroup(20, TmpPnt.Y);            // Zentrum Y
    WriteGroup(40, MajorAxis);           // Major Axis
    WriteGroup(41, MinorAxis);           // Minor Axis
    WriteGroup(42, 0);                   // Exzentrizität (0 für Kreise/Standardellipsen)
  end;
end;

procedure TDXF2DExport.WriteFrame2D(AFrm: TFrame2D);
var
 TmpPnt: TPoint2D;
begin
  with FDXFWrite, AFrm do
   begin
     WriteGroup(66, 1);
     WriteGroup(10, 0);
     WriteGroup(20, 0);
     WriteGroup(30, 0); // Questo setta l'elevazione.
     WriteGroup(0, 'VERTEX');
     WriteGroup(8, FCADCmp2D.Layers[AFrm.LayerIndex].Name);
     TmpPnt := TransformPoint2D(Points[0], ModelTransform);
     WriteGroup(10, TmpPnt.X);
     WriteGroup(20, TmpPnt.Y);
     WriteGroup(30, 0);
     WriteGroup(0, 'VERTEX');
     WriteGroup(8, FCADCmp2D.Layers[AFrm.LayerIndex].Name);
     TmpPnt := TransformPoint2D(Point2D(Points[0].X, Points[1].Y), ModelTransform);
     WriteGroup(10, TmpPnt.X);
     WriteGroup(20, TmpPnt.Y);
     WriteGroup(30, 0);
     WriteGroup(0, 'VERTEX');
     WriteGroup(8, FCADCmp2D.Layers[AFrm.LayerIndex].Name);
     TmpPnt := TransformPoint2D(Points[1], ModelTransform);
     WriteGroup(10, TmpPnt.X);
     WriteGroup(20, TmpPnt.Y);
     WriteGroup(30, 0);
     WriteGroup(0, 'VERTEX');
     WriteGroup(8, FCADCmp2D.Layers[AFrm.LayerIndex].Name);
     TmpPnt := TransformPoint2D(Point2D(Points[1].X, Points[0].Y), ModelTransform);
     WriteGroup(10, TmpPnt.X);
     WriteGroup(20, TmpPnt.Y);
     WriteGroup(30, 0);
     WriteGroup(0, 'VERTEX');
     WriteGroup(8, FCADCmp2D.Layers[AFrm.LayerIndex].Name);
     TmpPnt := TransformPoint2D(Points[0], ModelTransform);
     WriteGroup(10, TmpPnt.X);
     WriteGroup(20, TmpPnt.Y);
     WriteGroup(30, 0);
     WriteGroup(0, 'SEQEND');
   end;
end;

procedure TDXF2DExport.WriteText2D(AText: TJustifiedVectText2D);
var
  TmpPnt, TmpPnt1: TPoint2D;
  InsPnt: TPoint2D;
  AllPnt: TPoint2D;
  TmpAngle: TRealType;
begin
  with FDXFWrite, AText do
   begin
     WriteGroup(1, Text);
     WriteGroup(40, Height);
     TmpPnt := TransformPoint2D(Points[0], ModelTransform);
     AllPnt := TransformPoint2D(Points[1], ModelTransform);
     InsPnt := TransformPoint2D(Point2D(Points[0].X, Points[1].Y - Height), ModelTransform);
     WriteGroup(10, InsPnt.X);
     WriteGroup(20, InsPnt.Y);
     WriteGroup(30, 0);
     // I compute the angle of the Text from the rotation of it.
     TmpPnt := TransformPoint2D(Point2D(0, 0), ModelTransform);
     TmpPnt1 := TransformPoint2D(Point2D(10, 0), ModelTransform);
     TmpAngle := ArcTan2(TmpPnt1.Y - TmpPnt.Y, TmpPnt1.X - TmpPnt.X);
     WriteGroup(50, RadToDeg(TmpAngle));
     case HorizontalJust of
       jhCenter: begin
         WriteGroup(11, AllPnt.X - InsPnt.X);
         WriteGroup(21, AllPnt.Y - InsPnt.Y);
         WriteGroup(31, 0);
         WriteGroup(72, 1);
       end;
       jhRight: begin
         WriteGroup(11, AllPnt.X);
         WriteGroup(21, AllPnt.Y);
         WriteGroup(31, 0);
         WriteGroup(72, 2);
       end;
     end;
  end;
end;

procedure TDXF2DExport.WriteCurve2D(ACurve: TCurve2D);
var
  Count: Integer;
  TmpPnt: TPoint2D;
begin
  with FDXFWrite, ACurve do
   begin
     BeginUseProfilePoints;
     try
       WriteGroup(66, 1);
       WriteGroup(10, 0);
       WriteGroup(20, 0);
       WriteGroup(30, 0); // Questo setta l'elevazione.
       for Count := 0 to ProfilePoints.Count - 1 do
        begin
          WriteGroup(0, 'VERTEX');
          WriteGroup(8, FCADCmp2D.Layers[ACurve.LayerIndex].Name);
          TmpPnt := TransformPoint2D(ProfilePoints[Count], ModelTransform);
          WriteGroup(10, TmpPnt.X);
          WriteGroup(20, TmpPnt.Y);
          WriteGroup(30, 0);
        end;
       WriteGroup(0, 'SEQEND');
     finally
       EndUseProfilePoints;
     end;
   end;
end;

procedure TDXF2DExport.WriteOutline2D(APoly: TOutline2D);
var
  Count: Integer;
  TmpPnt: TPoint2D;
begin
  with FDXFWrite, APoly do
   begin
     WriteGroup(66, 1);
     WriteGroup(10, 0);
     WriteGroup(20, 0);
     WriteGroup(30, 0); // Questo setta l'elevazione.
     for Count := 0 to Points.Count - 1 do
      begin
        WriteGroup(0, 'VERTEX');
        WriteGroup(8, FCADCmp2D.Layers[APoly.LayerIndex].Name);
        TmpPnt := TransformPoint2D(Points[Count], ModelTransform);
        WriteGroup(10, TmpPnt.X);
        WriteGroup(20, TmpPnt.Y);
        WriteGroup(30, 0);
      end;
     WriteGroup(0, 'SEQEND');
   end;
end;

procedure TDXF2DExport.WriteBlock(ABlock: TBlock2D);
begin
  with FDXFWrite, ABlock do
   begin
     WriteGroup(2, Format('%s', [SourceName]));
     WriteGroup(10, ModelTransform[3,1]);
     WriteGroup(20, ModelTransform[3,2]);
     WriteGroup(30, 0);
     WriteGroup(41, ModelTransform[1,1]);
     WriteGroup(42, ModelTransform[2,2]);
   end;
end;

procedure TDXF2DExport.WriteEntity(AObj: TObject2D);
begin
  with FDXFWrite do
   if AObj is TLine2D then
    begin
      WriteGroup(0, 'LINE');
      WriteGroup(8, FCADCmp2D.Layers[AObj.LayerIndex].Name);
      WriteLine2D(AObj as TLine2D);
    end

  else if AObj is TCircle2D then
   begin
     WriteGroup(0, 'CIRCLE');
     WriteGroup(8, FCADCmp2D.Layers[AObj.LayerIndex].Name);
     WriteCircle2D(AObj as TCircle2D);
   end

  else if AObj is TCircularArc2D then
   begin
     WriteGroup(0, 'ARC');
     WriteGroup(8, FCADCmp2D.Layers[AObj.LayerIndex].Name);
     WriteCircularArc2D(AObj as TCircularArc2D);
   end

  else if AObj is TEllipticalArc2D then
   begin
     WriteGroup(0, 'ELLIPSE');
     WriteGroup(8, FCADCmp2D.Layers[AObj.LayerIndex].Name);
     WriteEllipticalArc2D(AObj as TEllipticalArc2D);
   end

  else if AObj is TEllipse2D then
   begin
     WriteGroup(0, 'ELLIPSE');
     WriteGroup(8, FCADCmp2D.Layers[AObj.LayerIndex].Name);
     WriteEllipse2D(AObj as TEllipse2D);
   end

   else if AObj is TFrame2D then
    begin
      WriteGroup(0, 'POLYLINE');
      WriteGroup(8, FCADCmp2D.Layers[AObj.LayerIndex].Name);
      WriteFrame2D(AObj as TFrame2D);
    end
   else if AObj is TCurve2D then
    begin
      WriteGroup(0, 'POLYLINE');
      WriteGroup(8, FCADCmp2D.Layers[AObj.LayerIndex].Name);
      WriteCurve2D(AObj as TCurve2D);
    end
   else if AObj is TOutline2D then
    begin
      WriteGroup(0, 'POLYLINE');
      WriteGroup(8, FCADCmp2D.Layers[AObj.LayerIndex].Name);
      WriteOutline2D(AObj as TOutline2D);
    end
   else if AObj is TJustifiedVectText2D then
    begin
      WriteGroup(0, 'TEXT');
      WriteGroup(8, FCADCmp2D.Layers[AObj.LayerIndex].Name);
      WriteText2D(AObj as TJustifiedVectText2D);
    end
   else if AObj is TBlock2D then
    begin
      WriteGroup(0, 'INSERT');
      WriteGroup(8, FCADCmp2D.Layers[AObj.LayerIndex].Name);
      WriteBlock(AObj as TBlock2D);
    end;
end;

const
 ColArray1: array[1..4] of Byte = (0, 63, 127, 191);
 ColArray2: array[1..4] of Byte = (127, 159, 191, 223);
var
 Cont: Byte;
initialization
// Settaggio colori per DXF.
  // Colori base.
  Colors[0] := RGB(255, 255, 255);
  Colors[1] := RGB(255, 0, 0);
  Colors[2] := RGB(255, 255, 0);
  Colors[3] := RGB(0, 255, 0);
  Colors[4] := RGB(0, 255, 255);
  Colors[5] := RGB(0, 0, 255);
  Colors[6] := RGB(255, 0, 255);
  Colors[7] := RGB(0, 0, 0);
  Colors[8] := RGB(134, 134, 134);
  Colors[9] := RGB(187, 187, 187);
  // Toni di grigio.
  Colors[250] := RGB(0, 0, 0);
  Colors[251] := RGB(45, 45, 45);
  Colors[252] := RGB(91, 91, 91);
  Colors[253] := RGB(137, 137, 137);
  Colors[254] := RGB(183, 183, 183);
  Colors[255] := RGB(179, 179, 179);
  // Altre tonalità
  for Cont := 1 to 4 do
   begin
     Colors[Cont * 10] := RGB(255, ColArray1[Cont], 0);
     Colors[40 + Cont * 10] := RGB(ColArray1[5 - Cont], 255, 0);
     Colors[80 + Cont * 10] := RGB(0, 255, ColArray1[Cont]);
     Colors[120 + Cont * 10] := RGB(0, ColArray1[5 - Cont], 255);
     Colors[160 + Cont * 10] := RGB(ColArray1[Cont], 0, 255);
   end;
  Colors[210] := RGB(255, 0, 255);
  Colors[220] := RGB(255, 0, 191);
  Colors[230] := RGB(255, 0, 127);
  Colors[240] := RGB(255, 0, 63);
  for Cont := 1 to 4 do
   begin
     Colors[Cont * 10 + 1] := RGB(255, ColArray2[Cont], 127);
     Colors[41 + Cont * 10] := RGB(ColArray2[5 - Cont], 255, 127);
     Colors[81 + Cont * 10] := RGB(127, 255, ColArray2[Cont]);
     Colors[121 + Cont * 10] := RGB(127, ColArray2[5 - Cont], 255);
     Colors[161 + Cont * 10] := RGB(ColArray2[Cont], 127, 255);
   end;
  Colors[211] := RGB(255, 127, 255);
  Colors[221] := RGB(255, 127, 223);
  Colors[231] := RGB(255, 127, 191);
  Colors[241] := RGB(255, 127, 159);
  // Gli altri sono tutti zero per ora.
end.

