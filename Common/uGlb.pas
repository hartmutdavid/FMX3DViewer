unit uGlb;

interface

uses IniFiles, FMX.MaterialSources, System.SysUtils, System.IOUtils, uStrUtil;

var
 g_sAppName:       String;
 g_sAppVersion:    String;
 g_sAppFileName:   String;

 g_cExtensionSeparatorChar: Char;
 g_cDirectorySeparatorChar: Char;
 g_cPathSeparator: Char;
 g_cVolumeSeparatorChar: Char;

 g_sRunDir:        String;     // <- Mit DirectorySeparator am Schluss!
 g_sHomePath:      String;     // <- Mit DirectorySeparator am Schluss!
 g_sDocPath:       String;     // <- Mit DirectorySeparator am Schluss!
 g_sCurDir:        String;     // <- Mit DirectorySeparator am Schluss!
 g_sWinDirPath:    String;     // <- Mit DirectorySeparator am Schluss!
 g_sTempDirPath:   String;     // <- Mit DirectorySeparator am Schluss!
 g_sCacheDirPath:  String;     // <- Mit DirectorySeparator am Schluss!
 g_sIniFilePath:   String;
 g_sIniFileName:   String;

 g_sModelPath:     String;

 // CTM-Werte
 g_lWidthSimplifyMesh: Boolean = False;
 g_iAgress : Integer = 5;
 g_fRatio  : Real = 0.25;

implementation
//------------------------------------------------------------------------------

function CheckPathByDir(aPathValue: String; var aFullPath: String): Boolean;
var
 l_sStr: String;
begin
 Result := False;
 aFullPath := '';
 if Length(aPathValue) > 0 then begin
   if Pos(':',aPathValue) = 2 then begin
     aFullPath := aPathValue;
   end
   else begin
     if Pos('.\',aPathValue) = 1 then
       l_sStr := Copy(aPathValue,3,999)
     else if Pos('\',l_sStr) = 1 then
       l_sStr := Copy(aPathValue,2,999)
     else
       l_sStr := aPathValue;
     aFullPath := g_sRunDir + l_sStr;
   end;
   if DirectoryExists(aFullPath) then
     Result := True
   else
     aFullPath := '';
 end;
end;

function CheckPathByFile(aPathValue: String; var aFullPath: String): Boolean;
var
 l_sStr: String;
begin
 Result := False;
 aFullPath := '';
 if Length(aPathValue) > 0 then begin
   if Pos(':',aPathValue) = 2 then begin
     aFullPath := aPathValue;
   end
   else begin
     if Pos('.\',aPathValue) = 1 then
       l_sStr := Copy(aPathValue,3,999)
     else if Pos('\',l_sStr) = 1 then
       l_sStr := Copy(aPathValue,2,999)
     else
       l_sStr := aPathValue;
     aFullPath := g_sRunDir + l_sStr;
   end;
   if FileExists(aFullPath) then
     Result := True
   else
     aFullPath := '';
 end;
end;

//------------------------------------------------------------------------------


procedure ReadIniFile;
var
 l_lErrorSw: Boolean;
 n: Integer;
 l_sStr, l_sMsg, l_sDriverId, l_sFileName, l_sPathName: String;
 l_oIniFile: TIniFile;
begin
{$ifdef CODESITE}
 CodeSite.Clear;
{$endif}
 { Ini-Datei auslesen }
 l_lErrorSw := False;      { <= Fehler Switch init. }
 if not FileExists(g_sIniFilePath) then begin
   l_sMsg := 'Exception: File "' + g_sIniFilePath + '" not found!' + #13#10 +
             '- Full Path: ' + g_sIniFilePath;
{$ifdef CODESITE}
   CodeSite.Send(l_sMsg);
{$endif}
   raise Exception.Create(l_sMsg);
   l_lErrorSw := True;
 end;
 //
 l_oIniFile := Nil;
 try
   l_oIniFile := TIniFile.Create(g_sIniFilePath);
 except
   l_lErrorSw := True;
 end;
 if not l_lErrorSw then begin
   try
     l_sStr := l_oIniFile.ReadString('Path','ModelPath','');
   except
     l_sStr := '';
   end;
   if not CheckPathByDir(l_sStr, l_sPathName) then begin
     l_sMsg := 'Wrong Ini-Value! Key=Path';
{$ifdef CODESITE}
     CodeSite.Send(l_sMsg);
{$endif}
     l_lErrorSw := True;
   end;
   g_sModelPath := l_sPathName;
 end;
 if not l_lErrorSw then begin
   try
     l_sStr := l_oIniFile.ReadString('CTM','WidthSimplifyMesh','');
   except
     l_sStr := '';
   end;
   if Length(l_sStr) > 0 then begin
     g_lWidthSimplifyMesh := False;
     if UpperCase(l_sStr) = 'TRUE' then
       g_lWidthSimplifyMesh := True;
   end;
   try
     l_sStr := l_oIniFile.ReadString('CTM','Agress','');
   except
     l_sStr := '';
   end;
   if Length(l_sStr) > 0 then begin
     tryStrToInt(l_sStr,g_iAgress);
   end;
   try
     l_sStr := l_oIniFile.ReadString('CTM','Ratio','');
   except
     l_sStr := '';
   end;
   if Length(l_sStr) > 0 then begin
     g_fRatio := uStrUtil.StrToFloatX(l_sStr);
   end;
 end;
 if Assigned(l_oIniFile) then
   l_oIniFile.Free;
end;

var
 n, idx: Integer;
 l_sStr: STring;
initialization
begin
 g_cExtensionSeparatorChar := TPath.ExtensionSeparatorChar;
 g_cDirectorySeparatorChar := TPath.DirectorySeparatorChar;
 g_cPathSeparator          := TPath.PathSeparator;
 g_cVolumeSeparatorChar    := TPath.VolumeSeparatorChar;
 g_sAppFileName:= ParamStr(0);
 g_sIniFilePath := TPath.ChangeExtension(g_sAppFileName, 'ini');
 l_sStr := uStrutil.GetFileVersionsString(g_sAppFileName);
 n := Pos('Produktversion:', g_sIniFilePath);
 if n > 0 then
   g_sAppVersion := Trim(Copy(g_sIniFilePath,n+15,999));
 g_sRunDir     := IncludeTrailingPathDelimiter(TDirectory.GetCurrentDirectory);
 g_sHomePath   := IncludeTrailingPathDelimiter(TPath.GetHomePath);
 g_sDocPath    := IncludeTrailingPathDelimiter(TPath.GetDocumentsPath);
 g_sWinDirPath := IncludeTrailingPathDelimiter(uStrUtil.GetWinDir);
 g_sTempDirPath := IncludeTrailingPathDelimiter(TPath.GetTempPath);
 g_sIniFileName := TPath.ChangeExtension(g_sAppFileName, 'ini');
{$IFNDEF ISAPI}
 if System.ParamCount > 1 then begin
   for idx := 1 to ParamCount do begin
     if UpperCase(ParamStr(idx)) = '-INI' then
       g_sIniFileName := Trim(ParamStr(idx+1));
   end;
 end;
{$ENDIF}
 g_sIniFilePath := g_sCurDir + g_sIniFileName;
 //
 // Lesen der Ini-Datei
 ReadIniFile;
 //
end;

finalization
begin
end;

end.
