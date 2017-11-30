unit FMX.STEP.Lexer;

interface

{$SCOPEDENUMS ON}

uses
  System.SysUtils, System.TypInfo;

type
  TSTEPToken = (Unknown, EndOfFile, KeyWord, IdentNo, Equal, &String, Integer, Float, OpenBracket, CloseBracket, Comma, SemiComma);

  TKeyWord = (
    kw_UNKNOWN,
    kw_CONVERSION_BASED_UNIT,
    kw_NAMED_UNIT,
    kw_VERTEX_POINT,
    kw_CARTESIAN_POINT,
    kw_DIRECTION,
    kw_VECTOR,
    kw_LINE,
    kw_CIRCLE,
    kw_EDGE_CURVE,
    kw_EDGE_LOOP,
    kw_B_SPLINE_SURFACE_WITH_KNOTS,
    kw_ORIENTED_EDGE,
    kw_CURVE_STYLE,
    kw_ADVANCED_FACE,
    kw_FACE_OUTER_BOUND,
    kw_PRESENTATION_STYLE_ASSIGNMENT,
    kw_SURFACE_SIDE_STYLE,
    kw_SURFACE_OF_REVOLUTION,
    kw_SURFACE_STYLE_USAGE,
    kw_SURFACE_STYLE_FILL_AREA,
    kw_FILL_AREA_STYLE,
    kw_FILL_AREA_STYLE_COLOUR,
    kw_LENGTH_MEASURE_WITH_UNIT,
    kw_DIMENSIONAL_EXPONENTS,
    kw_DRAUGHTING_PRE_DEFINED_CURVE_FONT,
    kw_COLOUR_RGB,
    kw_AXIS1_PLACEMENT,
    kw_AXIS2_PLACEMENT_3D
  );

  TKeyWordRec = record
    Str: string;
    Hash: Integer;
  end;

const
{$J+}
  // hash table for fast text processing
  // completed in implementation section
  KEY_WORDS: array[TKeyWord] of TKeyWordRec = (
    (Str: ''                         ),
    (Str: 'CONVERSION_BASED_UNIT'    ),
    (Str: 'NAMED_UNIT'               ),
    (Str: 'VERTEX_POINT'             ),
    (Str: 'CARTESIAN_POINT'          ),
    (Str: 'DIRECTION'                ),
    (Str: 'VECTOR'                   ),
    (Str: 'LINE'                     ),
    (Str: 'CIRCLE'                   ),
    (Str: 'EDGE_CURVE'               ),
    (Str: 'EDGE_LOOP'                ),
    (Str: 'B_SPLINE_SURFACE_WITH_KNOTS'),
    (Str: 'ORIENTED_EDGE'            ),
    (Str: 'CURVE_STYLE'              ),
    (Str: 'ADVANCED_FACE'            ),
    (Str: 'FACE_OUTER_BOUND'         ),
    (Str: 'PRESENTATION_STYLE_ASSIGNMENT' ),
    (Str: 'SURFACE_SIDE_STYLE'       ),
    (Str: 'SURFACE_OF_REVOLUTION'    ),
    (Str: 'SURFACE_STYLE_USAGE'      ),
    (Str: 'SURFACE_STYLE_FILL_AREA'  ),
    (Str: 'FILL_AREA_STYLE'          ),
    (Str: 'FILL_AREA_STYLE_COLOUR'   ),
    (Str: 'LENGTH_MEASURE_WITH_UNIT' ),
    (Str: 'DIMENSIONAL_EXPONENTS'    ),
    (Str: 'DRAUGHTING_PRE_DEFINED_CURVE_FONT'),
    (Str: 'COLOUR_RGB'               ),
    (Str: 'AXIS1_PLACEMENT'          ),
    (Str: 'AXIS2_PLACEMENT_3D'       )
  );
{$J-}

type
  TSTEPLexer = class
  strict private
    procedure StringToKeyWord;
    procedure SkipBlanks; inline;
  private
    m_lAhead: Boolean;
    m_oSTEP: TextFile;
    m_enToken: TSTEPToken;
    m_lUseCommaToken: Boolean;
    m_iIdxCurChar: Integer;
    m_iIdentNo: Integer;
    m_sCurrentLine:  String;
    m_iLineId: Integer;
    m_sString: string;
    m_enKeyWord: TKeyWord;
    FFormatSettings: TFormatSettings;

    function GetTokenFloat: Single;
    function GetTokenInteger: Integer;
    function GetChar: Char;
  public
    constructor Create(const i_sFileName: string);
    destructor Destroy; override;

    function NextToken: TSTEPToken;
    procedure NextTokenExpected(i_enToken: TSTEPToken); inline;
    function IsExpectedToken(i_enToken: TSTEPToken): boolean;
    property Token: TSTEPToken read m_enToken;

    procedure SkipKeyWordBlock;

    function IsDigit: Boolean;
    function IsChar: Boolean;
    function IsCharUpper: Boolean;

    property TokenKeyWord: TKeyWord read m_enKeyWord;
    property TokenString: string read m_sString;
    property TokenIdentNo: Integer read m_iIdentNo;
    property TokenFloat: Single read GetTokenFloat;
    property TokenInteger: Integer read GetTokenInteger;
    property CurrentLine: String read m_sCurrentLine;
    property CurrentChar: Char read GetChar;
    property IdxCurChar: Integer read m_iIdxCurChar;

    property Ahead: boolean read m_lAhead write m_lAhead;
    property UseCommaToken: boolean read m_lUseCommaToken write m_lUseCommaToken;
  end;

 ESTEPLexerError = class(Exception);

implementation

uses
  System.Hash, FMX.Consts;

{ TSTEPParser }

constructor TSTEPLexer.Create(const i_sFileName: string);
begin
  AssignFile(m_oSTEP, i_sFileName);
  Reset(m_oSTEP);
  FFormatSettings := TFormatSettings.Create;
end;

destructor TSTEPLexer.Destroy;
begin
  CloseFile(m_oSTEP);
  inherited;
end;

function TSTEPLexer.GetChar: Char;
begin
 if (0 <= m_iIdxCurChar) and (m_iIdxCurChar < m_sCurrentLine.Length) then
   Result := m_sCurrentLine.Chars[m_iIdxCurChar]
 else
   Result := #0;
end;

function TSTEPLexer.GetTokenFloat: Single;
begin
 if not TryStrToFloat(m_sString, Result) then
   Result := 0;
end;

function TSTEPLexer.GetTokenInteger: Integer;
begin
 Result := StrToInt(m_sString);
end;

function TSTEPLexer.NextToken: TSTEPToken;
begin
 if m_lAhead then begin
   m_lAhead := False;
   Exit(m_enToken);
 end;
if m_enToken = TSTEPToken.EndOfFile then
  Exit(TSTEPToken.EndOfFile);

 while True do begin
   if CurrentChar = #0 then begin
     if EOF(m_oSTEP) then begin
       m_enToken := TSTEPToken.EndOfFile;
       Exit(TSTEPToken.EndOfFile);
     end;
     ReadLn(m_oSTEP, m_sCurrentLine);
     Inc(m_iLineId);
     if Length(m_sCurrentLine) = 0 then
       Continue;
     m_iIdxCurChar := 0;
   end;
   SkipBlanks;
   if CurrentChar <> #0 then
     Break;
 end;

 m_enToken := TSTEPToken.Unknown;
 case CurrentChar of
   '#': begin
     m_sString := '';
     Inc(m_iIdxCurChar);
     while IsDigit do begin
       m_sString := m_sString + CurrentChar;
       Inc(m_iIdxCurChar);
     end;
     if TryStrToInt(m_sString,m_iIdentNo) then
       m_enToken := TSTEPToken.IdentNo;
   end;
   '=': begin
     m_enToken := TSTEPToken.Equal;
     Inc(m_iIdxCurChar);
   end;
   'a'..'z', 'A'..'Z', '_': begin
     m_sString := CurrentChar;
     Inc(m_iIdxCurChar);
     while IsDigit or IsChar or (CurrentChar = '_') do begin
       m_sString := m_sString + CurrentChar;
       Inc(m_iIdxCurChar);
     end;
     m_enToken := TSTEPToken.KeyWord;
     StringToKeyWord;
     if TokenKeyWord = TKeyWord.kw_UNKNOWN then
       m_enToken := TSTEPToken.Unknown;
   end;
   '''': begin
     m_sString := '';
     Inc(m_iIdxCurChar);
     // $D -> CL
     // $A -> CR
     while not (Byte(CurrentChar) in [Byte(#10), Byte(#13), Byte(#0), Byte('''')]) do begin
       m_sString := m_sString + CurrentChar;
       Inc(m_iIdxCurChar);
     end;
     if CurrentChar <> '''' then
       raise ESTEPLexerError.CreateResFmt(@SAseLexerCharError, [m_iLineId, '''', CurrentChar])
     else
       Inc(m_iIdxCurChar);
     m_enToken := TSTEPToken.String;
   end;
   '(': begin
     m_enToken := TSTEPToken.OpenBracket;
     Inc(m_iIdxCurChar);
   end;
   ')': begin
     m_enToken := TSTEPToken.CloseBracket;
     Inc(m_iIdxCurChar);
   end;
   ';': begin
     m_enToken := TSTEPToken.SemiComma;
     Inc(m_iIdxCurChar);
   end;
   '-', '0'..'9': begin
     m_sString := CurrentChar;
     Inc(m_iIdxCurChar);
     while IsDigit do begin
       m_sString := m_sString + CurrentChar;
       Inc(m_iIdxCurChar);
     end;
     if (not m_lUseCommaToken and (Byte(CurrentChar) in [Byte('.'), Byte(',')])) or
        (CurrentChar = '.') then begin
       FFormatSettings.DecimalSeparator := CurrentChar;
       m_sString := m_sString + CurrentChar;
       Inc(m_iIdxCurChar);
       while IsDigit do begin
         m_sString := m_sString + CurrentChar;
         Inc(m_iIdxCurChar);
       end;
       m_enToken := TSTEPToken.Float;
     end
     else
       m_enToken := TSTEPToken.Integer;
   end;
   ',': begin
     if m_lUseCommaToken then begin
       m_enToken := TSTEPToken.Comma;
     end;
     Inc(m_iIdxCurChar);
   end;
   else
     Inc(m_iIdxCurChar);
 end;
 Result := m_enToken;
end;

procedure TSTEPLexer.NextTokenExpected(i_enToken: TSTEPToken);
var
 l_lExpectedTokenOk: Boolean;
begin
 NextToken;
 l_lExpectedTokenOk:= IsExpectedToken(i_enToken);
 if not l_lExpectedTokenOk then
   raise ESTEPLexerError.CreateResFmt(@SAseLexerTokenError, [m_iLineId,
           GetEnumName(TypeInfo(TSTEPToken), Integer(i_enToken)),
           GetEnumName(TypeInfo(TSTEPToken), Integer(m_enToken))]);
end;

procedure TSTEPLexer.SkipBlanks;
begin
 while Byte(CurrentChar) in [$1..$20] do
   Inc(m_iIdxCurChar);
end;

procedure TSTEPLexer.SkipKeyWordBlock;
var
 l_iBracketCount: Integer;
begin
 Assert(Token = TSTEPToken.KeyWord);
 if NextToken = TSTEPToken.OpenBracket then begin
   l_iBracketCount := 1;
   repeat
     case NextToken of
       TSTEPToken.OpenBracket: Inc(l_iBracketCount);
       TSTEPToken.CloseBracket: Dec(l_iBracketCount);
       TSTEPToken.EndOfFile: Exit;
   end;
   until (Token = TSTEPToken.EndOfFile) or (l_iBracketCount = 0);
   // NextToken;
 end
 else begin
   while not (NextToken in [TSTEPToken.KeyWord, TSTEPToken.EndOfFile, TSTEPToken.CloseBracket]) do
   {none};
   m_lAhead := True;
 end;
end;

procedure TSTEPLexer.StringToKeyWord;
var
 l_enKw: TKeyWord;
 l_iHash: Integer;
begin
 l_iHash := THashBobJenkins.GetHashValue(PChar(m_sString)^, m_sString.Length * SizeOf(Char), 2004);
 m_enKeyWord := TKeyWord.kw_UNKNOWN;
 for l_enKw := Succ(Low(TKeyWord)) to High(TKeyWord) do begin
   if KEY_WORDS[l_enKw].Hash = l_iHash then begin
     if SameStr(m_sString, KEY_WORDS[l_enKw].Str) then begin
       m_enKeyWord := l_enKw;
       Exit;
     end;
   end;
 end;
end;

function TSTEPLexer.IsChar: Boolean;
begin
 Result := Byte(CurrentChar) in [Byte('A')..Byte('Z'), Byte('a')..Byte('z')];
end;

function TSTEPLexer.IsCharUpper: Boolean;
begin
 Result := Byte(CurrentChar) in [Byte('A')..Byte('Z')];
end;

function TSTEPLexer.IsDigit: Boolean;
begin
 Result := Byte(CurrentChar) in [Byte('0')..Byte('9')];
end;

function TSTEPLexer.IsExpectedToken(i_enToken: TSTEPToken): boolean;
begin
 Result:= True;
 if m_enToken <> i_enToken then begin
   Result:= False;
//     raise ESTEPLexerError.CreateResFmt(@ESTEPLexerError, [FLineId,
//        GetEnumName(TypeInfo(TSTEPToken), Integer(i_enToken)),
//        GetEnumName(TypeInfo(TSTEPToken), Integer(m_enToken))]);
 end;
end;

procedure InitializeKeyWordsHash;
var
 l_enKw: TKeyWord;
begin
 for l_enKw := Succ(Low(TKeyWord)) to High(TKeyWord) do
   KEY_WORDS[l_enKw].Hash := THashBobJenkins.GetHashValue(PChar(KEY_WORDS[l_enKw].Str)^,
      KEY_WORDS[l_enKw].Str.Length * SizeOf(Char), 2004);
end;

initialization
 InitializeKeyWordsHash;

//-- finalization
//--   UnregisterAliases;
end.
