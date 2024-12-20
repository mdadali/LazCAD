unit CommonUtils;

interface

uses  Classes, SysUtils, Forms, Math, Dialogs, Graphics, {$IFDEF WINDOWS} Windows, {$ENDIF}
      LCLType, LCLVersion, versiontypes, versionresource,
      interfaces, LCLPlatformDef;

const ErrStrToInt         = -999999;
const ErrCharIdxNotFound  = -500;
const ErrSubStr           = 'SubStringError';

////////////////////////////////////////////////////////////////////////////////
//Colors////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
function  cuRGBToColor(R, G, B: byte): TColor;

////////////////////////////////////////////////////////////////////////////////
//StringUtils///////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
function  cuEncodeStr(AStr: string): string;
function  cuDecodeStr(AStr: string): string;
function  cuStrToWChar(ASource: string): PWideChar;
function  cuFileIsNamed(AFileName, InvalidFileName: string): boolean;
function  cuSubString(AStr : string; startPos, endPos: integer): string;
function  cuConvStrToInt(AStr: string): Integer;
function  cuStrLength(AStr: string): Integer;
function  cuCharIndex(AStr: string; AChar: char) : integer;
function  cuCharCount(AStr: string; AChar: char): integer;
function  cuSignIdx(AStr: string): integer;
////////////////////////////////////////////////////////////////////////////////
//Math//////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
function cuSqrt(AValue: double): double;
function cuSqr(AValue: double): double;
function cuPi:  double;
function cuDegToRad(AAngle: double): double;
function cuRadToDeg(AAngle: double): double;
function cuGetAngleFromPoints(Ax1, Ay1, Ax2, Ay2: double): double;
function cuPointsDistance2D(ex, sx, ey, sy: double): double;
////////////////////////////////////////////////////////////////////////////////
//Utils/////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
procedure cuSwapPoints(var Ax1:double; var Ay1:double; var Ax2:double; var Ay2: double);
procedure cuSwapAngle(var AAngle1:double; var AAngle2:double);
////////////////////////////////////////////////////////////////////////////////
//Files/////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
procedure cuGetDirList(Path: string; DirList: TStrings);
procedure cuListFileDir(Path: string; Ext: string; FileList: TStrings);

function  cuFileExt(AFileName: string): string;    //FileExtention
function  cuFileName(AFileName: string): string;   //Filename without FileExtention
function  cuFileNameFileExt(AFileName: string): string;   //Filename+FileExtention
function  cuFilePath(AFileName: string): string;   //FilePath without Filename

function  cuAppFileName: string;         //AppFilename-FileExtention
function  cuAppFileNameFileExt: string;  //AppFilename+FileExtention
function  cuAppPath: string;             //AppFilePath-Filename
function  cuAppFullPath: string;         //Delphi's Application.ExeName
////////////////////////////////////////////////////////////////////////////////
//LCL/VCL///////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
function cuOpenFileDialog(AFilter, AInitialDir: string): string;
function cuSaveFileDialog(AFilter, AInitialDir: string): string;

////////////////////////////////////////////////////////////////////////////////
//Application/OS///////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
function GetProgramVersion: string;
function GetLazarusVersion: string;
function GetFPCVersion: string;
function ReadOSReleaseFile: string;
function ReadFileToString(const AFileName: string): string;
function GetOSInfo: string;
function GetLCLWidgetSet: string;

implementation

////////////////////////////////////////////////////////////////////////////////
//Colors////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
function  cuRGBToColor(R, G, B: byte): TColor;
begin
  result := RGBToColor(R, G, B);
end;
////////////////////////////////////////////////////////////////////////////////
//StringUtils///////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
function cuEncodeStr(AStr: string): string;
var c: array[0..255] of Byte absolute AStr; i: Integer;
begin
  for i := 1 to Ord(AStr[1]) do
    c[i] := 23 xor c[i];
  result := AStr;
end;

function cuDecodeStr(AStr: string): string;
var c: array[0..255] of Byte absolute AStr; i: Integer;
begin
  for i := 1 to Length(AStr) do AStr[i] := AnsiChar(23 xor Ord(c[i]));
  result := AStr;
end;

function cuStrToWChar(ASource: string): PWideChar;
var hRet: PWideChar;
begin
  GetMem(hRet, sizeof(WideChar) * Succ(Length(ASource)));
  StringToWideChar(ASource, hRet, Succ(Length(ASource)));
  result := hRet;
   //result := PWideChar(ASource);
end;

function cuFileIsNamed(AFileName, InvalidFileName: string): boolean;
var a, b: boolean;
begin
  a :=  ((AFileName <> '')  and (AFileName <> ' '));
  AFileName        := LowerCase(AFileName);
  InvalidFileName  := LowerCase(InvalidFileName);
  b :=  (Pos(InvalidFileName, AFileName) = 0);
  result := (a and b);
end;

function cuSubString(AStr : string; startPos, endPos: integer):    string;
var i, AStrLen: integer; hStr, hRet: string; gueltig: boolean;
begin
  hRet := '';
  gueltig := true;
  AStrLen := Length(AStr);
  if (startPos <= 0)         then startPos := 1;
  if (endPos   > AStrLen)    then endPos   := AStrLen;
  if (startPos > endPos)     then gueltig  := false;
  if (startPos > AStrLen)    then gueltig  := false;
  if (gueltig) then
  begin
    for i := startPos to endPos do hStr := hStr + AStr[i];
    hRet := hStr;
  end else hRet := ErrSubStr;
  result := hStr;
end;

function cuCharIndex(AStr: string; AChar: char): integer;
var i, hRet, AStrLen: integer;
begin
  i := 0;
  AStrLen := Length(AStr);
  repeat
    inc(i);
  until  (AStr[i] = AChar) or (i > AStrLen) ;
  if (AStr[i] = AChar)
  then hRet := i
  else hRet := ErrCharIdxNotFound;
  result := hRet;
end;

function cuCharCount(AStr: string; AChar: char): integer;
var i, AStrLen, hRet: integer;
begin
  hRet        := 0;
  AStrLen     := Length(AStr);
  if AStrLen > 0 then
    for i:= 1 to AStrLen do
      if AStr[i] = AChar then
        hRet := hRet + 1;
  result := hRet;
end;

function  cuSignIdx(AStr: string): integer;
var i, hRet, AStrLen: integer;
begin
  i := 0;
  AStrLen := Length(AStr);
  hRet := -1;
  if AStrLen > 0 then
  begin
    repeat
      i := i + 1;
    until  (AStr[i] = '+') or (AStr[i] = '-') or (i > AStrLen) ;
    if ((AStr[i] = '+') or (AStr[i] = '-')) then hRet := i
  end;
  result := hRet;
end;

function cuConvStrToInt(AStr: string): Integer;
begin
  result := (StrToIntDef(AStr, ErrStrToInt));
end;

function cuStrLength(AStr: string): Integer;
begin
  result := Length(AStr);
end;

////////////////////////////////////////////////////////////////////////////////
//Math///////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
function cuSqrt(AValue: double): double;
begin
  result := sqrt(AValue);
end;

function cuSqr(AValue: double): double;
begin
  result := sqr(AValue);
end;

function  cuPi:  double;
begin
  result := pi;
end;

function cuDegToRad(AAngle: double): double;
begin
  result := DegToRad(AAngle);
end;

function cuRadToDeg(AAngle: double): double;
begin
   result := RadToDeg(AAngle);
end;

function cuGetAngleFromPoints(Ax1, Ay1, Ax2, Ay2: double): double;
var x,y, angle: double;
begin
  x := Ax2 - Ax1;
  y := Ay2 - Ay1;
  angle := ArcTan2(y, x);

  if angle < 0 then
    angle := angle + (2 * pi);
  result := angle;
end;

function cuPointsDistance2D(ex, sx, ey, sy: double): double;
begin
  result := sqrt(sqr(ex-sx) + sqr(ey-sy));
end;

////////////////////////////////////////////////////////////////////////////////
//Utils///////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
procedure cuSwapPoints(var Ax1:double; var Ay1:double; var Ax2:double; var Ay2: double);
var tempX, tempY: double;
begin
  tempX := Ax1;
  tempY := Ay1;
  Ax1   := Ax2;
  Ay1   := Ay2;
  Ax2   := tempX;
  Ay2   := tempY;
end;

procedure cuSwapAngle(var AAngle1:double; var AAngle2:double);
var tempAngle: double;
begin
  tempAngle := AAngle1;
  AAngle1   := AAngle2;
  AAngle2   := tempAngle;
end;

////////////////////////////////////////////////////////////////////////////////
//Files///////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
{procedure cuGetDirList(Path: string; DirList: TStrings);
var SR: TSearchRec;  temppath: string;
begin
  temppath := Path;
  if FindFirst(Path, faDirectory, SR) = 0 then
  begin
    repeat
      if (SR.Attr = faDirectory) then
      begin
        DirList.Add(SR.Name);
      end;
    until FindNext(SR) <> 0;
    FindClose(SR);
  end;
end;
}

procedure cuGetDirList(Path: string; DirList: TStrings);
var SR: TSearchRec; TempPath: string;
begin
  TempPath := IncludeTrailingPathDelimiter(Path); // Sicherstellen, dass der Pfad das richtige Format hat
  if FindFirst(TempPath + '*.*', faDirectory, SR) = 0 then
  begin
    try
      repeat
        if (SR.Attr and faDirectory <> 0) and (SR.Name <> '.') and (SR.Name <> '..') then
        begin
          DirList.Add(SR.Name);
        end;
      until FindNext(SR) <> 0;
    finally
      {$IFDEF WINDOWS} FindClose(SR.FindHandle);  {$ENDIF}
      {$IFDEF LINUX} FindClose(SR);  {$ENDIF}
      {$IFDEF DARWIN} FindClose(SR);  {$ENDIF} //???
    end;
  end;
end;

procedure cuListFileDir(Path: string; Ext: string; FileList: TStrings);
var SR: TSearchRec;
begin
  if FindFirst(Path + '*' + Ext, faAnyFile, SR) = 0 then
  begin
    repeat
      if (SR.Attr <> faDirectory) then
      begin
        FileList.Add(SR.Name);
      end;
    until FindNext(SR) <> 0;
    {$IFDEF WINDOWS} FindClose(SR.FindHandle);  {$ENDIF}
    {$IFDEF LINUX} FindClose(SR);  {$ENDIF}
    {$IFDEF DARWIN} FindClose(SR);  {$ENDIF} //???
  end;
end;

function cuFileExt(AFileName: string): string;  //FileExtention
begin
  result := ExtractFileExt(AFileName);
end;

function cuFileName(AFileName: string): string;   //Filename-FileExtention
var hStr: string; Idx: integer;
begin
  hStr := ExtractFileName(AFileName);
  Idx  := cuCharIndex(hStr, '.');
  hStr := cuSubString(hStr, 1, Idx-1);
  result := hStr;
end;

function cuFileNameFileExt(AFileName: string): string; //Filename+FileExtention
begin
  result := ExtractFileName(AFileName);
end;

function  cuFilePath(AFileName: string): string;  //FilePath-Filename
begin
  result := ExtractFilePath(AFileName);
end;

function cuAppFileName: string;                  //AppFilename-FileExtention
begin
  result := cuFileName(Application.ExeName);
end;

function cuAppFileNameFileExt: string;           //AppFilename+FileExtention
begin
  result := ExtractFileName(Application.ExeName);
end;

function cuAppPath: string;
begin
  result := ExtractFilePath(Application.ExeName);  //AppFilePath-Filename
end;

function cuAppFullPath: string;
begin
  result := Application.ExeName;
end;

////////////////////////////////////////////////////////////////////////////////
//LCL/VCL///////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
function cuOpenFileDialog(AFilter, AInitialDir: string): string;
var OpenDialog: TOpenDialog;  hRet: string;
begin
  try
    hRet := '';
    OpenDialog := TOpenDialog.Create(nil);
    OpenDialog.Filter := AFilter;
    OpenDialog.InitialDir := AInitialDir;
    if OpenDialog.Execute then
      hRet := OpenDialog.FileName;
  finally
    OpenDialog.Free;
    OpenDialog := nil;
    result := hRet;
  end;
end;

function cuSaveFileDialog(AFilter, AInitialDir: string): string;
var SaveDialog: TSaveDialog;  hRet: string;
begin
  try
    hRet := '';
    SaveDialog := TSaveDialog.Create(nil);
    SaveDialog.Filter := AFilter;
    SaveDialog.InitialDir := AInitialDir;
    if SaveDialog.Execute then
      hRet := SaveDialog.FileName;
  finally
    SaveDialog.Free;
    SaveDialog := nil;
    result := hRet;
  end;
end;

function GetProgramVersion: string;
var
  vr: TVersionResource;
  rs: TResourceStream;
begin
  Result := 'unknow';
  vr := TVersionResource.Create;
  try
    rs := TResourceStream.CreateFromID(HINSTANCE, 1, PChar(RT_VERSION));
    try
      vr.SetCustomRawDataStream(rs);
      Result := Format('%d.%d.%d.%d', [vr.FixedInfo.FileVersion[0],
                                       vr.FixedInfo.FileVersion[1],
                                       vr.FixedInfo.FileVersion[2],
                                       vr.FixedInfo.FileVersion[3]]);
    finally
      rs.Free;
    end;
  finally
    vr.Free;
  end;
end;

function GetLazarusVersion: string;
begin
  result := lcl_version;
end;

function GetFPCVersion: string;
begin
  Result := {$I %FPCVERSION%};
end;

function ReadFileToString(const AFileName: string): string;
var
  FileStream: TFileStream;
  StringStream: TStringStream;
begin
  if not FileExists(AFileName) then
    raise Exception.CreateFmt('File "%s" not found!', [AFileName]);

  FileStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
  try
    StringStream := TStringStream.Create('');
    try
      StringStream.CopyFrom(FileStream, FileStream.Size);
      Result := StringStream.DataString;
    finally
      StringStream.Free;
    end;
  finally
    FileStream.Free;
  end;
end;

function ReadOSReleaseFile: string;
var
  OSReleaseFile: TextFile;
  Line, OSVersion: string;
begin
  OSVersion := 'Unknown Linux Version';
  if FileExists('/etc/os-release') then
  begin
    AssignFile(OSReleaseFile, '/etc/os-release');
    try
      Reset(OSReleaseFile);
      while not EOF(OSReleaseFile) do
      begin
        ReadLn(OSReleaseFile, Line);
        if Pos('PRETTY_NAME=', Line) = 1 then
        begin
          OSVersion := Copy(Line, Pos('=', Line) + 1, Length(Line));
          OSVersion := StringReplace(OSVersion, '"', '', [rfReplaceAll]);
          Break;
        end;
      end;
    finally
      CloseFile(OSReleaseFile);
    end;
  end
  else if FileExists('/proc/version') then
  begin
    // Fallback: read from /proc/version, if /etc/os-release don't exists
    OSVersion := Trim(StringReplace(ReadFileToString('/proc/version'), 'Linux version ', '', []));
  end;
  Result := OSVersion;
end;

function GetOSInfo: string;
var OSName, OSType, OSVersion: string; Buffer: array[0..255] of Char;
begin
  // Betriebssystem bestimmen
  {$IFDEF WINDOWS}
  OSName := 'Windows';
  if GetEnvironmentVariable('OS', Buffer, SizeOf(Buffer)) > 0 then
    OSVersion := Buffer
  else
    OSVersion := 'unknow';
  {$ENDIF}

  {$IFDEF LINUX}
  OSName := 'Linux';
  OSVersion := ReadOSReleaseFile;
  {$ENDIF}

  {$IFDEF DARWIN}
  OSName := 'Mac OS';
  OSVersion := GetEnvironmentVariable('OSTYPE'); // Alternativ mit `uname -r` die Mac-Version auslesen
  {$ENDIF}

  // 32-Bit or 64-Bit
  {$IFDEF CPU32}
  OSType := '32-bit';
  {$ENDIF}
  {$IFDEF CPU64}
  OSType := '64-bit';
  {$ENDIF}

  //Result := Format('OS: %s' + sLineBreak +
  //                 '%s' + sLineBreak +
  //                 'Architecture: %s', [OSName, OSVersion, OSType]);

  Result := Format('%s' + sLineBreak +
                   '%s' + sLineBreak +
                   'Architecture: %s', [OSName, OSVersion, OSType]);
end;

function GetLCLWidgetSet: string;
begin
  {$IFDEF LCLQT}
  Result := 'Qt4';
  {$ENDIF}

  {$IFDEF LCLQT5}
  Result := 'Qt5';
  {$ENDIF}

  {$IFDEF LCLQT6}
  Result := 'Qt6';
  {$ENDIF}

  {$IFDEF LCLGTK2}
  Result := 'GTK2';
  {$ENDIF}

  {$IFDEF LCLGTK3}
  Result := 'GTK3';
  {$ENDIF}

  {$IFDEF LCLCOCOA}
  Result := 'Cocoa (Mac OS)';
  {$ENDIF}

  {$IFDEF LCLCARBON}
  Result := 'Carbon (Mac OS)';
  {$ENDIF}

  {$IFDEF MSWINDOWS}
  Result := 'Windows';
  {$ENDIF}

  if Result = '' then
    Result := 'unknow Widgetset';
end;

end.
