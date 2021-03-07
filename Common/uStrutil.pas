unit uStrUtil;


interface

uses
  SysUtils, DB, System.StrUtils,
  Windows, Messages, Classes, ZLib, System.IOUtils,
  Winapi.ShellAPI, IdHashMessageDigest,
  WinSock, TlHelp32;

function _PAnsiChar(const str: AnsiString): PAnsiChar; overload;
function _PAnsiChar(const str: String; var strBuf: AnsiString): PAnsiChar; overload;

function StrToFloatX(Const str : String) : Real;

Procedure linksb(var str: String);
   { Macht ein String linksbuendig. }

Procedure replace(var str: String; oldChar, newChar: Char);
   { Ersetzt ein Zeichen durch ein anderes }

function DatumOk(DateStr: ShortString; var DateValue: TDateTime) : Boolean;
   { Ueberpruefen und Konvertieren eines Strings in Datum/Zeit }

function  GetDBTypeString(FieldType: TFieldType): String;
   { Liefert zum Feldtyp den zugehoerigen String }

function GetInsertedText(sTrenn: String; var iInd: Integer; sFullText: String;  var sGetStr: String): Integer;
   { Dieser Operator extrahiert einen String aus einem anderern ab der Position iInd.
     Die Strings sind durch 'sTrenn' voneinander getrennt. }

procedure fill_up (Maxlen: Integer; FillChar: Char; var str: String);
   { Fuellt den String ab der akt. Laenge bis 'maxlen' mit FillChar auf. }

function GetLastPos(cDelimiter: Char; sText: String): Integer;
   { Ermittelt die letzte Position im String }

procedure Compress (FillChar: Char; var str: String);
   { Komprimiert den String, suchend nach 'FillChar', d.h. alle
     'FillChar's gehen verloren. }

procedure TrimByChar (TrimChar: Char; var str: String);
   { Komprimiert den String, suchend nach 'TrimChar'. Ab dieses Zeichen wird der
     String gekuerzt. }

function DaysPerMonth(AYear, AMonth: Integer): Integer;
   { Anzahl Tage fuer einen Monat }

function FormatFields(dataSet: TDataSet; sFormat, sFields: String; var sResult: String): Boolean;
   { Formatiert DB-Fields zu einem String }

function GetTextFromFormatedFields(sFormat, sFields: String;
             var iInd: Integer;
             sFullText: String; var sName, sVal: String): Integer;
   { Extrahiert formatierte Felder }

function CheckFieldsByDataSet(cTrenn: String; dsDataSet: TDataSet; var sFormat, sFields: String): Boolean;
  { Ueberpruefen, ob alle Felder in der akt. Tabelle vorhanden sind.
    Wenn nicht, so werden "sFormat" und "sFields" um die fehlenden Felder verkuerzt ! }

{ Debug - Funktionen }
procedure DebugStr(const DebugMsg: String);

function StringToDateTime(const sStrSQLDateTime : String) : TDateTime;

{ }
function GetLastPathPart(sPath: String): String;
function GetAusgFileName(sAusgPathString: String): String;


function GetWinDir: String;
function GetTempDirectory: String;
function GetTempFile(const sExtension: String): string;

function LocalComputerName: String;
function GetLocalIPs(const Lines: TStrings):Boolean;
function GetIpAddressByName(const Lines:TStrings; const AHost: String): Boolean;

function ProcessExists(sExeFileName: string): Boolean;

function SaveStringToFile(const sStr, sFileName: String): Boolean;
     { Speichert ein String in einer Datei }
function  FileSize(const sFileName: String): Integer;
     { Groesse der Datei }
procedure FileCopy(sVonFile, sNachFile: String);
     { Kopiert ein File }
function AddURLSeparatorChar(sStr: String): String;
     { Haengt das Zeichen '/' hinten an, wenn nocht nicht vorhanden ! }
function AddDirectorySeparatorChar(sStr: String): String;
     { Haengt das Zeichen 'DirectorySeparatorChar' hinten an, wenn nocht nicht vorhanden ! }
function DelDirectorySeparatorChar(sStr: String): String;
     { Loescht das hintere Zeichen 'DirectorySeparatorChar', wenn noch vorhanden ! }
function  GetLastRelDir(const sDir: String): String; overload;
function  GetLastRelDir(const sDir: String; var sRootDir: String): String; overload;
     { Liefert den Name der letzten relativen Directory aus einem Directory-Namen }

function  ShellExecAndWait(const aRunFileName: string;
                           const aRunInDirectory: String;
                           const aParameters: string;
                           const aVerb: string;         // '', 'open', 'opennew',
                           aCmdShow: Integer            // SW_HIDE, SW_SHOW , SW_SHOWDEFAULT,  SW_SHOWNORMAL
                           ): Boolean;
function PCharOrNil(const aString: string): PChar;

function GetHashMD5Key(const Str: String): String;

function BuildExcelExportFileName(const sRefName: String): String;

function FormatDateTimeSQLPart(dt: TDateTime): String;
function FormatDateSQLPart(dt: TDateTime): String;
function BuildDateTimeSQLPart(dt: TDateTime): String;
function BuildDateSQLPart(dt: TDateTime): String;

function GetFilterPartForString(const sFieldName, sValueText: String): String;

function CorrectMailAddresses(const sMailAddresses: String): String;

function GetFileVersionsString(const p_sFilepath : string) : string;


implementation

uses uGlb;

function StrToFloatX(Const str : String) : Real;
//like StrToFloat but accepts either decimal separator: '1.23' or '1,23'
var
 f: Single;
 fmt: TFormatSettings;
begin
 Result := 0.0;
 fmt := FormatSettings;
 fmt.DecimalSeparator := '.';
 if TryStrToFloat(str, f, fmt) then begin
   Result := f;
   exit;
 end;
 fmt.DecimalSeparator := ',';
 if TryStrToFloat(str, f, fmt) then begin
   Result := f;
 end;
end;

function _PAnsiChar(const str: AnsiString): PAnsiChar;
begin
 if Length(str) > 0 then begin
   Result := PAnsiChar(str);
 end
 else
   Result := Nil;
end;

function _PAnsiChar(const str: String; var strBuf: AnsiString): PAnsiChar;
begin
 if Length(str) > 0 then begin
   strBuf := AnsiString(str);
   Result := PAnsiChar(strBuf);
 end
 else begin
   strBuf := '';
   Result := Nil;
 end;
end;

Procedure linksb(var str: String);
{
Operatorname    : linksb

Art             : Extern

Funktion        : Dieser Operator verschiebt eine Zeichenkette links-
                  buendig.

Input-Parameter : var str string
                     Urspruengliche Zeichenkette

Output-Parameter: var str string
                     Verschobene Zeichenkette
}
var
 i, j, l: Integer;
begin
 l := Length(str);
 if l > 0 then
   begin
    { Suche nach dem ersten Zeichen ungleich Blank }
    for i := 1 to l do
      begin
       if str[i] <> ' ' then
          break;
      end;
    if i > l then
       SetLength(str,0)    { Nur Blanks }
    else
      begin
       if i > 1 then
         begin
          for j := 1 to l do
            begin
             str[j] := str[i];
             inc(i);
             if i > l then
                break;
            end;
         end
       else
          j := l;
       while j > 0 do
         begin
          if str[j] <> ' ' then
             break;
          dec(j);
         end;
       SetLength(str,j);
     end;
   end;
end;

Procedure replace(var str: String; oldChar, newChar: Char);
{ Ersetzt ein Zeichen durch ein anderes }
var
 i, lng: Integer;
begin
 lng := Length(str);
 for i := 1 to lng do begin
   if str[i] = oldChar then
     str[i] := newChar;
 end;
end;

function DatumOk(DateStr: ShortString; var DateValue: TDateTime) : Boolean;
var
 d: TDateTime;
 l, n: Integer;
 ret: Boolean;
begin
 ret := True;
 l   := Length(DateStr);
 if l > 0 then
   begin
    n := Pos('.',DateStr);
    if n > 1 then
      begin
       if (DateStr[n-1] >= '0') and (DateStr[n-1] <= '9') then
         begin
          try
             d := StrToDate(DateStr);
          except
             ret := False;
          end;
         end
       else
         ret := False;
      end
    else
       ret := False;
   end
 else
    ret := False;
 if ret then
    DateValue := d;
 DatumOk := ret;
end;

function GetDBTypeString(FieldType: TFieldType): String;
var
 str: string[255];
begin
 case FieldType of
    ftUnknown:  str := 'ftUnknown';
    ftString:   str := 'ftString';
    ftSmallint: str := 'ftSmallint';
    ftInteger:  str := 'ftInteger';
    ftWord:     str := 'ftWord';
    ftBoolean:  str := 'ftBoolean';
    ftFloat:    str := 'ftFloat';
    ftCurrency: str := 'ftCurrency';
    ftBCD:      str := 'ftBCD';
    ftDate:     str := 'ftDate';
    ftTime:     str := 'ftTime';
    ftDateTime: str := 'ftDateTime';
    ftBytes:    str := 'ftBytes';
    ftVarBytes: str := 'ftVarBytes';
    ftBlob:     str := 'ftBlob';
    ftMemo:     str := 'ftMemo';
    ftGraphic:  str := 'ftGraphic';
 else
    str:= 'ftUnknown';
 end;
 GetDBTypeString := str;
end;

function GetInsertedText(sTrenn: String; var iInd: Integer; sFullText: String;  var sGetStr: String): Integer;
{ Dieser Operator extrahiert einen String aus einem anderern ab der Position iInd.
  Die Strings sind durch 'cChar' voneinander getrennt. }
var
 i, j, k, iLng, iL, iPos: Integer;
 bv: Boolean;
begin
 sGetStr := '';
 if iInd < 1 then
    iInd := 1;
 iLng    := Length(sFullText);
 if iInd < iLng then
   begin
    k := Length(sTrenn);
    iPos    := 0;
    for i := iInd to iLng do
      begin
       if sFullText[i] = sTrenn[1] then
         begin
          if k > 1 then
            begin
             bv := True;
             for j := 2 to k do
               begin
                if sFullText[i+j-1] <> sTrenn[1] then
                  begin
                   bv := False;
                   break;
                  end;
               end;
             if bv then
               begin
                iPos := i - 1;
                break;
               end;
            end
          else
            begin
             iPos := i - 1;
             break;
            end;
         end;
      end;
    if iPos = 0 then
       iPos := iLng;
    iL := iPos-iInd+1;
    sGetStr := Copy(sFullText,iInd, iL);
    iInd := iInd + iL + k;
    if iInd < iLng then
      begin
       for i := iInd to iLng do
          if sFullText[i] <> ' ' then
             break;
       iInd := i;
      end;
   end
 else if iInd = iLng then
   begin   { 1 Zeichen ! }
    SetLength(sGetStr,1);
    sGetStr[1] := sFullText[iInd];
    Inc(iInd);
    iL := 1;
   end
 else
    iL := 0;
 GetInsertedText := iL;
end;

procedure fill_up (Maxlen: Integer; FillChar: Char; var str: String);
{ Fuellt den String ab der akt. Laenge bis 'Maxlen' mit 'FillChar' auf. }
var
 i, iLng: Integer;
begin
 iLng := Length(str);
 SetLength(str,Maxlen);
 for i := (iLng+1) to Maxlen do
    str[i] := FillChar;
end;

function GetLastPos(cDelimiter: Char; sText: String): Integer;
   { Ermittelt die letzte Position im String }
var
 n: Integer;
begin
 Result := 0;
 n := Pos(cDelimiter,sText);
 while n > 0 do begin
   Result := n;
   n := PosEx(cDelimiter,sText,n+1);
 end;
end;

procedure Compress (FillChar: Char; var str: String);
{ Komprimiert den String suchend nach 'FillChar' auf, d.h. alle
  'FillChar's gehen verloren. }
var
 i, j, iLng: Integer;
begin
 iLng := Length(str);
 { Besteht der String evtl. nur aus 'FillChar's }
 for i := 1 to iLng do
    if str[i] <> FillChar then
       break;
 if i > iLng then
    iLng := 0;
 i := 1;
 while i < iLng do
   begin
    if str[i] = FillChar then
      begin
       for j := i to iLng - 1 do
          str[j] := str[j+1];
       Dec(iLng);
      end
    else
       Inc(i);
   end;
 SetLength(str,iLng);
end;

procedure TrimByChar (TrimChar: Char; var str: String);
{ Komprimiert den String, suchend nach 'TrimChar'. Ab dieses Zeichen wird der
  String gekuerzt. }
var
 i, n, iLng: Integer;
begin
 n := -1;
 iLng := Length(str);
 { Besteht der String evtl. nur aus 'FillChar's }
 for i := 1 to iLng do
    if str[i] = TrimChar then
      begin
       n := i - 1;
       break;
      end;
 if n >= 0 then
    SetLength(str,n);
end;

function DaysPerMonth(AYear, AMonth: Integer): Integer;
{ Anzahl Tage fuer einen Monat }
const
 DaysInMonth: array[1..12] of Integer = (31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);
begin
 Result := DaysInMonth[AMonth];
 if (AMonth = 2) and IsLeapYear(AYear) then Inc(Result); { leap-year Feb is special }
end;

function FormatFields(dataSet: TDataSet; sFormat, sFields: String; var sResult: String): Boolean;
{ Formatiert Felder }
var
 lOk, lEnd, lFillNulls: Boolean;
 i, j, iProz, iEnd, iInd, iIndText, iL: Integer;
 sVal, sHstr1, sHstr2, sName: String;
 iVal: LongInt;
 rVal: Real;
 FieldType: TFieldType;
begin
 lOk  := True;
 lEnd := False;
 iInd := -1;
 iIndText := 0;
 sResult := sFormat;
 while not lEnd do
   begin
    if sFields <> '' then
      begin  { Die Namen der Felder sind vorgegeben -> Suchen ! }
       iL := GetInsertedText(',', iIndText, sFields, sName);
       if iL > 0 then
         begin
          iInd := -1;
          for i := 0 to (dataSet.FieldCount -1) do
            begin
             if sName = dataSet.Fields[i].FieldName then
               begin
                iInd := i;
                break;
               end;
            end;
          if iInd < 0 then
            begin
             lOk := False;
             break;
            end;
         end
       else
         begin
          lEnd := True;  { <= Es gibt nichts mehr !!! }
          break;
         end;
      end
    else
      begin
       Inc(iInd);
       if iInd >= dataSet.FieldCount then
          break;
      end;
    FieldType := dataSet.Fields[iInd].DataType;
    try
       if FieldType = ftString then
          sVal := dataSet.Fields[iInd].AsString
       else if (FieldType = ftSmallint) or (FieldType = ftInteger) or
               (FieldType = ftWord) then
         begin
          iVal := dataSet.Fields[iInd].AsInteger;
          FieldType := ftInteger;
         end
       else if FieldType = ftFloat then
          rVal := dataSet.Fields[iInd].AsFloat
       else
          lOk := False;
    except
       lOk := False;
    end;
    if lOk then
      begin
       iProz := 0;
       iEnd  := 0;
       for j := 1 to Length(sResult) do
         begin
          if sResult[j] = '%' then
             iProz := j
          else if iProz > 0 then
            begin
             if (FieldType = ftString) and (sResult[j] = 's') then
                iEnd := j
             else if (FieldType = ftInteger) and (sResult[j] = 'd') then
                iEnd := j
             else if (FieldType = ftFloat) and (sResult[j] = 'f') then
                iEnd := j;
            end;
          if iEnd > 0 then
             break;
         end;
       if (iProz = 0) or (iEnd = 0) then
          lOk := False
       else
         begin
          sHstr1 := Copy(sResult,1,iEnd);
          if FieldType = ftString then
             sHstr2 := Format(sHstr1,[sVal])
          else if FieldType = ftInteger then
            begin
             if (sHstr1[iProz+1] >= '0') and (sHstr1[iProz+1] <= '9') then
                lFillNulls := True
             else
                lFillNulls := False;
             sHstr2 := Format(sHstr1,[iVal]);
             while sHstr2[iProz] = ' ' do
               begin
                sHstr2[iProz] := '0';
                Inc(iProz);
               end;
            end
          else if FieldType = ftFloat then
             sHstr2 := Format(sHstr1,[rVal]);
          sHstr1 := sHstr2 + Copy(sResult,iEnd+1,Length(sResult));
          sResult := sHstr1;
         end;
      end;
    if not lOk then
       break;
   end;
 FormatFields := lOk;
end;


var
 iIndFormat: Integer;
 iIndFullText: Integer;

function GetTextFromFormatedFields(sFormat, sFields: String;
             var iInd: Integer;
             sFullText: String; var sName, sVal: String): Integer;
{ Extrahiert formatierte Felder }
var
 i1, i2, j, iPos, iProz, iEnd, iL, iLng: Integer;
 sTrenner: String;
begin
 if iInd = 0 then
   begin
    iIndFormat   := 1;
    iIndFullText := 1;
   end;
 iProz := 0;
 iEnd  := 0;
 iLng  := 0;
 iL    := 0;
 sName := '';
 sVal  := '';
 for j := iIndFormat to Length(sFormat) do
   begin
    if (sFormat[j] = '%') or (sFormat[j] = '?') then
      begin
       if iLng > 0 then
         begin
          for i1 := iIndFullText to Length(sFullText) do
            begin
             iPos := 0;
             for i2 := 1 to Length(sTrenner) do
               begin
                if sFullText[i1+i2-1] <> sTrenner[i2] then
                  begin
                   iPos := 0;
                   break;
                  end
                else if iPos = 0 then
                   iPos := i1;
               end;
             if iPos > 0 then
                break;
            end;
          if iPos > 0 then
            begin
             iL := GetInsertedText(',',iInd,sFields,sName);
             if iL > 0 then
               begin
                sVal := Copy(sFullText,iIndFullText,iPos-iIndFullText);
                iIndFullText := iPos + Length(sTrenner);
                iIndFormat := iEnd + 1;
               end;
             break;
            end;
          iEnd  := 0;
          iLng  := 0;
         end;
       iProz := j
      end
    else if (iProz > 0) and (iEnd = 0) then
      begin
       if sFormat[j] = 's' then
          iEnd := j
       else if sFormat[j] = 'd' then
          iEnd := j
       else if sFormat[j] = 'f' then
          iEnd := j;
       if iEnd > 0 then
          Continue;
      end;
    if iEnd > 0 then
       begin
        Inc(iLng);
        SetLength(sTrenner,iLng);
        sTrenner[iLng] := sFormat[j];
       end;
   end;
 if (sName = '') and (iInd > 0) then
   begin
    iL := GetInsertedText(',',iInd,sFields,sName);
    if iL > 0 then
      begin
       sVal := Copy(sFullText,iIndFullText,Length(sFullText));
       iIndFullText := Length(sFullText) + 1;
      end;
   end;
 GetTextFromFormatedFields := iL;
end;


function CheckFieldsByDataSet(cTrenn: String; dsDataSet: TDataSet;
               var sFormat, sFields: String): Boolean;
{ Ueberpruefen, ob alle Felder in der akt. Tabelle vorhanden sind.
  Wenn nicht, so werden "sFormat" und "sFields" um die fehlenden Felder
  verkuerzt ! }
var
 lOk, lEx: Boolean;
 i, iL, iInd, iProz, iEnd:  Integer;
 sName, sNewFormat, sNewFields: String;
begin
 lOk := True;
 sNewFormat := sFormat;
 sNewFields := '';
 iInd := 0;
 iIndFormat := 1;
 iL := GetInsertedText(cTrenn, iInd, sFields, sName);
 while iL > 0 do
   begin
    { Ermitteln des naechsten Formats }
    iProz := 0;
    iEnd  := 0;
    for i := iIndFormat to Length(sNewFormat) do
      begin
       if sNewFormat[i] = '%' then
          iProz := i
       else if (iProz > 0) and (iEnd = 0) then
         begin
          if sNewFormat[i] = 's' then
             iEnd := i
          else if sNewFormat[i] = 'd' then
             iEnd := i
          else if sNewFormat[i] = 'f' then
             iEnd := i;
          if iEnd > 0 then
            begin
             iIndFormat := iEnd + 1;
             break;
            end;
         end;
      end;
    lEx := False;
    for i := 0 to (dsDataSet.FieldCount - 1) do
      if sName = dsDataSet.Fields[i].FieldName then
        begin
         if dsDataSet.Fields[i].IsIndexField then
            lEx := True;
         break;
        end;
    if lEx then
      begin
       if sNewFields = '' then
          sNewFields := sName
       else
          sNewFields := sNewFields + cTrenn + sName;
      end
    else
      begin  { Format-String mit ? fuellen }
       sNewFormat[iProz] := '?';
       for i := (iProz+1) to iEnd do
          sNewFormat[i] := ' ';
      end;
    iL := GetInsertedText(cTrenn, iInd, sFields, sName);
   end;
 sFormat := sNewFormat;
 sFields := sNewFields;
 CheckFieldsByDataSet := lOk;
end;


function GetGlsParts(sFullGls: String; var sHBz, sUBz, sGlsNr: String): Boolean;
{ Liefert zu einer vollstaendigen Gleisangabe die einzelnen Teile, wenn
  alle Teile korrekt sind. }
var
 lOk: Boolean;
begin
 if (Length(sFullGls) = 4) and (Pos(';',sFullGls) = 0) then
   begin
    lOk := True;
    sHBz  := Copy(sFullGls,1,1);
    sUBz  := Copy(sFullGls,2,1);
    sGlsNr:= Copy(sFullGls,3,2);
   end
 else
   begin
    sHBz  := '';
    sUBz  := '';
    sGlsNr:= '';
    lOk := False;
   end;
 GetGlsParts := lOk;
end;

procedure DebugStr(const DebugMsg: String);
var
 CDS    : TCopyDataStruct;
 DbWin  : hWnd;
 Msg    : PAnsiChar;
 LenStr : Integer;
begin
 DbWin := FindWindow('TDbWinScrn', nil);
 if DbWin <> 0 then
   begin
    LenStr := Length(DebugMsg) + 1;
    CDS.cbData := LenStr;
    GetMem(Msg,LenStr);
    Try
       StrPCopy(Msg,DebugMsg);
       CDS.lpData := Msg;
       SendMessage(DbWin, WM_COPYDATA, 0, LParam(@CDS));
    Finally
       FreeMem(Msg,LenStr);
    end;
  end;
end;

// Wandelt einen String im Format "yyyy-mm-dd hh:mm:ss" in DateTime-Wert um
function StringToDateTime(const sStrSQLDateTime : String) : TDateTime;
begin
 if Length(sStrSQLDateTime) = 19 then begin
   if ((sStrSQLDateTime[5] = '-') and (sStrSQLDateTime[8] = '-')) then begin
     Result := StrToDateTime(copy(sStrSQLDateTime,9,2) + '.' + // Day
                             copy(sStrSQLDateTime,6,2) + '.' + // Month
                             copy(sStrSQLDateTime,1,4) + ' ' + // Year
                             copy(sStrSQLDateTime,12,8)) // Time
   end
   else begin
     Result := StrToDateTime(sStrSQLDateTime);
   end;
 end
 else
   raise Exception.Create('Datumumwandlungsfehler von ' + sStrSQLDateTime);
end;

function GetLastPathPart(sPath: String): String;
{ Ermittelt den letzten Teilpfad einer Pfadangabe }
var
 i, iLng: Integer;
 sStr: String;
begin
 sStr := '';
 iLng := Length(sPath);
 if sPath[iLng] = '\' then
   begin
    Dec(iLng);
    SetLength(sPath,iLng);
   end;
 for i := iLng downto 1 do
   begin
    if sPath[i] = '\' then
      begin
       sStr := Copy(sPath,i+1,iLng);
       break;
      end;
   end;
 GetLastPathPart := sStr;
end;

function getAusgFileName(sAusgPathString: String): String;
var
 l_sPath, l_sPostfix, l_sFileName, l_sFileNameExt, l_sDateiNameOhneExt: String;
 n, i, l_iLaenge: Integer;
 l_dtNow: TDateTime;
 l_wYear, l_wMonth, l_wDay, l_wHour, l_wMin, l_wSec, l_wMSec: Word;
begin
 {Datei oeffnen!}
 n := 0;
 l_sFileName := sAusgPathString;
 l_sPath := ExtractFilePath(l_sFileName);
 l_iLaenge := Length(l_sPath);
 if (l_iLaenge > 0) and (l_sPath[l_iLaenge]  <> '\') then begin
   l_sPath := l_sPath + '\';
 end;
 l_sDateiNameOhneExt := ExtractFileName(l_sFileName);
 l_sFileNameExt := ExtractFileExt(l_sDateiNameOhneExt);
 l_iLaenge := Length(l_sDateiNameOhneExt);
 for i := l_iLaenge downto 1 do begin
   if (l_sDateiNameOhneExt[i] = '.') then begin
     SetLength(l_sDateiNameOhneExt, i-1);
     break;
   end;
 end;
 l_dtNow := Now;
 DecodeDate(l_dtNow, l_wYear, l_wMonth, l_wDay);
 DecodeTime(l_dtNow, l_wHour, l_wMin,   l_wSec, l_wMSec);
 l_sDateiNameOhneExt := l_sDateiNameOhneExt + IntToStr(l_wYear);
 if l_wMonth < 10 then
   l_sDateiNameOhneExt := l_sDateiNameOhneExt + '0';
 l_sDateiNameOhneExt := l_sDateiNameOhneExt + IntToStr(l_wMonth);
 if l_wDay < 10 then
   l_sDateiNameOhneExt := l_sDateiNameOhneExt + '0';
 l_sDateiNameOhneExt := l_sDateiNameOhneExt + IntToStr(l_wDay);
 if l_wHour < 10 then
   l_sDateiNameOhneExt := l_sDateiNameOhneExt + '0';
 l_sDateiNameOhneExt := l_sDateiNameOhneExt + IntToStr(l_wHour);
 if l_wMin < 10 then
   l_sDateiNameOhneExt := l_sDateiNameOhneExt + '0';
 l_sDateiNameOhneExt := l_sDateiNameOhneExt + IntToStr(l_wMin);
 if l_wSec < 10 then
   l_sDateiNameOhneExt := l_sDateiNameOhneExt + '0';
 l_sDateiNameOhneExt := l_sDateiNameOhneExt + IntToStr(l_wSec);
 if l_wMSec < 10 then
   l_sDateiNameOhneExt := l_sDateiNameOhneExt + '0';
 l_sDateiNameOhneExt := l_sDateiNameOhneExt + IntToStr(l_wMSec);
 l_sFileName := l_sPath + l_sDateiNameOhneExt + '_' + IntToStr(n) + l_sFileNameExt;
 while (FileExists(l_sFileName)) do begin
   Inc(n);
   l_sFileName := l_sPath + '\' + l_sDateiNameOhneExt + '_' + IntToStr(n) + l_sFileNameExt;
 end;
 Result := l_sFileName;
end;

function GetWinDir: String;
var
 l_iLen: DWord;
 l_sDir: array[0..MAX_PATH] of Char;
begin
 l_iLen := GetWindowsDirectory(@l_sDir, MAX_PATH);
 if l_iLen > 0 then
   result := StrPas(l_sDir)
 else
   RaiseLastOSError;
end;

function GetTempDirectory: String;
var
  tempFolder: array[0..MAX_PATH] of Char;
begin
  GetTempPath(MAX_PATH, @tempFolder);
  result := StrPas(tempFolder);
end;

function GetTempFile(const sExtension: String): string;
var
 l_iNullPos: Integer;
 l_sFileName, l_sExt: string;
begin
 l_sFileName := TPath.GetTempFileName;
 l_iNullPos := Pos(#0,l_sFileName);
 if l_iNullPos > 0 then
   l_sFileName := LeftStr( l_sFileName, l_iNullPos - 1 );
 l_sExt := TPath.GetExtension(l_sFileName);
 if Length(l_sExt) > 0 then
   l_sFileName := TPath.ChangeExtension(l_sFileName,sExtension)
 else
   l_sFileName := l_sFileName + sExtension;
 Result := l_sFileName;
end;

function LocalComputerName: String;
var
 Buffer : array[0..255] of char;
 Size : DWORD;
begin
 if GetComputerName(Buffer, Size) then
   Result := Buffer
 else
   Result := '';
end;

function GetLocalIPs(const Lines:TStrings):Boolean;
type
 PPInAddr= ^PInAddr;
var
 wsaData: TWSAData;
 HostInfo: PHostEnt;
 HostName: Array[0..255] of AnsiChar;
 Addr: PPInAddr;
begin
 Result:=False;
 Lines.Clear;
 if WSAStartup($0102, wsaData)=0 then begin
   try
     if gethostname(HostName, SizeOf(HostName)) = 0 then Begin
       HostInfo:= gethostbyname(HostName);
       if HostInfo<>nil then begin
         Addr := Pointer(HostInfo^.h_addr_list);
         if (Addr<>nil) AND (Addr^<>nil) then
           Repeat
             Lines.Add(StrPas(inet_ntoa(Addr^^)));
             inc(Addr);
           Until Addr^=nil;
       end;
     end;
     Result:=True;
   finally
     WSACleanup;
   end;
 end;
end;

function GetIpAddressByName(const Lines:TStrings; const AHost: String): Boolean;
type
 PPInAddr= ^PInAddr;
var
 WSA: TWSAData;
 HostInfo: PHostEnt;
 Addr: PPInAddr;
begin
 Result:=False;
 if WSAStartUp($101, WSA) = 0 then begin
   try
     HostInfo:= getHostByName(PAnsiChar(AHost));
     Result:=HostInfo<>nil;
     if Result then begin
       Addr := Pointer(HostInfo^.h_addr_list);
       if (Addr<>nil) AND (Addr^<>nil) then begin
         Repeat
           Lines.Add(StrPas(inet_ntoa(Addr^^)) ) ;
           inc(Addr);
         Until Addr^=nil;
       end;
     end;
   finally
     WSACleanup;
   end;
 end;
end;

function ProcessExists(sExeFileName: string): Boolean;
var
 ContinueLoop: BOOL;
 FSnapshotHandle: THandle;
 FProcessEntry32: TProcessEntry32;
begin
 FSnapshotHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
 FProcessEntry32.dwSize := SizeOf(FProcessEntry32);
 ContinueLoop := Process32First(FSnapshotHandle, FProcessEntry32);
 Result := False;
 while Integer(ContinueLoop) <> 0 do begin
   if ((UpperCase(ExtractFileName(FProcessEntry32.szExeFile)) =
        UpperCase(sExeFileName)) or (UpperCase(FProcessEntry32.szExeFile) =
        UpperCase(sExeFileName))) then begin
     Result := True;
   end;
   ContinueLoop := Process32Next(FSnapshotHandle, FProcessEntry32);
 end;
 CloseHandle(FSnapshotHandle);
end;

function SaveStringToFile(const sStr, sFileName: String): Boolean;
var
 l_oFd: TextFile;
begin
 try
   System.AssignFile(l_oFd, sFileName);
   Rewrite(l_oFd);
   Write(l_oFd,sStr);
   System.CloseFile(l_oFd);
   Result := True;
 except
   Result := False;
 end;
end;

function FileSize(const sFileName: String): Integer;
var
 l_oFd: file of Byte;
begin
 try
   System.AssignFile(l_oFd, sFileName);
   System.Reset(l_oFd);
   Result := System.FileSize(l_oFd);
   System.CloseFile(l_oFd);
 except
   Result := 0;
 end;
end;

procedure FileCopy(sVonFile, sNachFile: String);
{ Kopiert ein File }
var
 fd1, fd2: File;
 Buf: array[1..2048] of Byte;
 NumRead, NumWritten: Integer;
begin
 try
    AssignFile(fd1, sVonFile);
    AssignFile(fd2, sNachFile);
    Reset(fd1, 1);
    Rewrite(fd2, 1);
    repeat
       BlockRead(fd1,  Buf, SizeOf(Buf), NumRead);
       BlockWrite(fd2, Buf, NumRead, NumWritten);
    until (NumRead = 0) or (NumWritten <> NumRead);
    CloseFile(fd1);
    CloseFile(fd2);
  except end;
end;

function AddURLSeparatorChar(sStr: String): String;
{ Haengt das Zeichen '/' hinten an, wenn noch nicht vorhanden ! }
var
 iLng: Integer;
begin
 iLng := Length(sStr);
 if (iLng > 0) and (sStr[iLng] <> '/') then
    sStr := sStr + '/';
 Result := sStr;
end;

function AddDirectorySeparatorChar(sStr: String): String;
{ Haengt das Zeichen 'DirectorySeparatorChar' hinten an, wenn noch nicht vorhanden ! }
var
 iLng: Integer;
begin
 iLng := Length(sStr);
 if (iLng > 0) and (sStr[iLng] <> TPath.DirectorySeparatorChar) then
    sStr := sStr + TPath.DirectorySeparatorChar;
 Result := sStr;
end;

function DelDirectorySeparatorChar(sStr: String): String;
{ Loescht das hintere Zeichen 'DirectorySeparatorChar', wenn noch vorhanden ! }
var
 iLng: Integer;
begin
 iLng := Length(sStr);
 if (iLng > 0) and (sStr[iLng] = TPath.DirectorySeparatorChar) then
    SetLength(sStr,iLng-1);
 Result := sStr;
end;

function GetLastRelDir(const sDir: String): String;
{ Liefert den letzten Directory-Anteil }
var
 sRoot: String;
begin
 Result := GetLastRelDir(sDir,sRoot);
end;

function GetLastRelDir(const sDir: String; var sRootDir: String): String;
var
 i, lng, offset: Integer;
begin
 Result   := '';
 sRootDir := '';
 lng := Length(sDir);
 if lng > 0 then begin
   offset := 0;
   for i := lng downto 1 do begin
     if sDir[i] = TPath.DirectorySeparatorChar then begin
       if i = lng then
         offset := -1
       else
         break;
     end;
   end;
   Result   := Copy(sDir,i+1,lng-i+offset);
   sRootDir := Copy(sDir,1,i);
 end;
end;

function ShellExecAndWait(const aRunFileName: string;
                          const aRunInDirectory: String;
                          const aParameters: string;
                          const aVerb: string;         // '', 'open', 'opennew',
                          aCmdShow: Integer            // SW_HIDE, SW_SHOW , SW_SHOWDEFAULT,  SW_SHOWNORMAL
                          ): Boolean;
var
 Sei: TShellExecuteInfo;
begin
 FillChar(Sei, SizeOf(Sei), #0);
 Sei.cbSize := SizeOf(Sei);
 Sei.fMask := SEE_MASK_DOENVSUBST or SEE_MASK_FLAG_NO_UI or SEE_MASK_NOCLOSEPROCESS;
 Sei.lpFile := PChar(aRunFileName);
 Sei.lpDirectory  := PCharOrNil(aRunInDirectory);
 Sei.lpParameters := PCharOrNil(aParameters);
 Sei.lpVerb := PCharOrNil(aVerb);
 Sei.nShow := aCmdShow;
 Result := ShellExecuteEx(@Sei);
 if Result then begin
   WaitForInputIdle(Sei.hProcess, INFINITE);
   WaitForSingleObject(Sei.hProcess, INFINITE);
   CloseHandle(Sei.hProcess);
 end;
end;

function PCharOrNil(const aString: string): PChar;
begin
 if aString = '' then
   Result := nil
 else
   Result := PChar(aString);
end;

function GetHashMD5Key(const Str: String): String;
var
 MD5: TIdHashMessageDigest5;
begin
 MD5 := TIdHashMessageDigest5.Create;
 try
   Result := LowerCase(MD5.HashStringAsHex(Str))
 finally
   MD5.Free
 end
end;

function BuildExcelExportFileName(const sRefName: String): String;
var
 l_iYear, l_iMonth, l_iDay: Word;
 l_sFilename: String;
begin
 DecodeDate(Now, l_iYear, l_iMonth, l_iDay);
 l_sFilename := sRefName + '_' + IntToStr(l_iYear);
 if l_iMonth < 10 then
   l_sFilename := l_sFilename + '0';
 l_sFilename := l_sFilename + IntToStr(l_iMonth);
 if l_iDay < 10 then
   l_sFilename := l_sFilename + '0';
 l_sFilename := l_sFilename + IntToStr(l_iDay) + '.xls';
 Result := l_sFilename;
end;


function FormatDateTimeSQLPart(dt: TDateTime): String;
begin
 Result := FormatDateTime('yyyy-MM-dd hh:nn:ss',dt)
end;

function FormatDateSQLPart(dt: TDateTime): String;
begin
 Result := FormatDateTime('yyyy-MM-dd',dt)
end;

function BuildDateTimeSQLPart(dt: TDateTime): String;
begin
 Result := 'datetime(''' + FormatDateTime('yyyy-MM-dd hh:nn:ss',dt) + ''')'
end;

function BuildDateSQLPart(dt: TDateTime): String;
begin
 Result := 'date(''' + FormatDateTime('yyyy-MM-dd',dt) + ''')'
end;

function GetFilterPartForString(const sFieldName, sValueText: String): String;
var
 l_sStr: String;
begin
 l_sStr := sValueText;
 if Pos('*',l_sStr) > 0 then begin
   uStrUtil.replace(l_sStr,'*','%');
   Result := sFieldName + ' like ''' + l_sStr + '''';
 end
 else if Pos('%',l_sStr) > 0 then begin
   Result := sFieldName + ' like ''' + l_sStr + '''';
 end
 else begin
   Result := sFieldName + '=''' + l_sStr + '''';
 end;
end;

function  CorrectMailAddresses(const sMailAddresses: String): String;
var
 n, lng: Integer;
 l_sStr: String;
begin
 l_sStr := Trim(sMailAddresses);
 if Length(l_sStr) > 0 then begin
   n := Pos(',',l_sStr);
   if n > 0 then
     uStrUtil.replace(l_sStr, ',', ';');
   l_sStr := Trim(l_sStr);
   lng := Length(l_sStr);
   if (lng > 0) and (l_sStr[lng] = ';') then begin
     l_sStr[lng] := ' ';
     l_sStr := Trim(l_sStr);
   end;
 end;
 Result := l_sStr;
end;

function GetFileVersionsString(const p_sFilepath : string) : string;
const // Das Ergebnis soll ja nett formatiert sein :)
  sFixVerFormat = 'Fileversion: %d.%d.%d.%d / Productversion: %d.%d.%d.%d';
var
 dwVersionSize : DWord; // Buffer für die Grösse der Versionsinfo der abgefragten Datei
 dwDummy : DWord; // Dummy, Wert wird nicht benötigt
 pVerBuf : Pointer; // Buffer für die Versionsdaten
 pFixBuf : PVSFixedFileInfo; // Buffer für die Versionsinfo fester Länge
 sReqdInfo : string; // Hier kommt rein, welcher Teil der Versionsinfo abgefragt werden soll
begin
 // Annahme: Die Datei hat keine Versionsinfo
 Result := Format(sFixVerFormat,[0,0,0,0,0,0,0,0]);
 dwDummy := 0; // Dummy initialisieren
 sReqdInfo := '\'; // Es soll die Versionsinfo fester Länge abgefragt werden
 // Mal sehen, wieviel Platz die Versionsinfo der Datei braucht
 try
   dwVersionSize := GetFileVersionInfoSize(PChar(p_sFilepath),dwDummy);
 except
   dwVersionSize := 0;
 end;
 if dwVersionSize > 0 then begin // Wenn > 0, dann Versionsinfo vorhanden
   try
     pVerBuf := AllocMem(dwVersionSize); // Buffer initialisieren
     // Gesamte Versionsinformationen auslesen
     if GetFileVersionInfo(PChar(p_sFilepath),0,dwVersionSize,pVerBuf) then begin
       // Werte für Versionsinfo fester Länge extrahieren
       if VerQueryValue(pVerBuf,PChar(sReqdInfo),Pointer(pFixBuf),dwDummy) then begin
         // und als Ergebnis ausgeben
         Result := Format(sFixVerFormat,[
            (pFixBuf^.dwFileVersionMS and $FFFF0000) shr 16, // 1. Stelle HiWord, deshalb nach unten schieben
            pFixBuf^.dwFileVersionMS and $0000FFFF,
            (pFixBuf^.dwFileVersionLS and $FFFF0000) shr 16,
            pFixBuf^.dwFileVersionLS and $0000FFFF,

            (pFixBuf^.dwProductVersionMS and $FFFF0000) shr 16,
            pFixBuf^.dwProductVersionMS and $0000FFFF,
            (pFixBuf^.dwProductVersionLS and $FFFF0000) shr 16,
            pFixBuf^.dwProductVersionLS and $0000FFFF
           ]);
       end;
     end;
   finally // Resourcen wieder freigeben
     FreeMem(pVerBuf,dwVersionSize);
   end;
 end;
end;

end.

