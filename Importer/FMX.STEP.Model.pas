unit FMX.STEP.Model;

interface

{$SCOPEDENUMS ON}

uses
 System.SysUtils, System.Types,
{$ifdef CODESITE}
  CodeSiteLogging,
{$endif}
 FMX.STEP.Lexer, FMX.Import;

type
  TSTEPMaterial = class;
  TSTEPMaterialDynArray = array of TSTEPMaterial;

  TSTEPMaterial = class(TGEMaterial)
  private
    FSubMaterials: TGEMaterials;
  public
    property SubMaterials: TGEMaterials read FSubMaterials;
    constructor Create;
    destructor Destroy; override;
  end;

  TSTEPMesh = record
    FVertexSource: TGEVertexSource;
    FTriangleMesh: TGETriangleMeshID;
    FFaceMaterials : TIntegerDynArray;
    FSubMeshes: array of TGEMesh;
  end;
  TSTEPMeshDynArray = array of TSTEPMesh;

  TSTEPModel = class(TCustomModel)
  private
     FMaterials: TGEMaterials;
     FMeshes: TSTEPMeshDynArray;

     procedure ParseVertexList(
        const i_oLexer : TSTEPLexer;
        const ANumVertex: Integer;
        const AVertexSource: TGEVertexSource);
     procedure ParseNormalList(
        const i_oLexer : TSTEPLexer;
        const ANumFaceNormal: Integer;
        const ANumVertexNormal: Integer;
          var AMesh: TSTEPMesh);
     procedure ParseTexCoordList(
        const i_oLexer : TSTEPLexer;
        const ANumTexCoord: Integer;
        const AVertexSource: TGEVertexSource);
     procedure ParseFaceList(
        const i_oLexer : TSTEPLexer;
        const ANumFaces: Integer;
        var AMesh: TSTEPMesh);
      procedure ParseTFaceList(
        const i_oLexer : TSTEPLexer;
        const ANumFaces: Integer;
        var AMesh: TSTEPMesh);
     procedure ParseMesh(const i_oLexer : TSTEPLexer; var AMesh : TSTEPMesh);
     procedure ParseMap(const i_oLexer : TSTEPLexer; var AFileName: String);
     procedure ParseMaterialList(const i_oLexer : TSTEPLexer);
     procedure ParseMaterial(const i_oLexer : TSTEPLexer; AMaterial: TSTEPMaterial);
     procedure ParseGeometry(const i_oLexer : TSTEPLexer);
     procedure ParseModel(const i_oLexer : TSTEPLexer);
     function  AddSubMesh(const AVertexSource: TGEVertexSource): TGEMesh;

     procedure SaveSTEPCommand(i_iLeftTokenIdentNo: Integer; l_enKeyWord: TKeyWord; s_sRestLine: String);
     procedure ParseSTEPCommand(i_iLeftTokenIdentNo: Integer);
     procedure ParseAllUnsolvedSTEPCommands();
  public
     property Meshes: TSTEPMeshDynArray read FMeshes;
     property Materials: TGeMaterials read FMaterials;

     procedure LoadFromFile(const i_sFileName: String); override;
     constructor Create;
     destructor Destroy; override;
  end;

  EAseParserError = class(Exception);

implementation

uses
  FMX.Types3D, FMX.Consts, System.TypInfo;

type
  TSTEPLexerHack = class(TSTEPLexer);

constructor TSTEPModel.Create;
begin
  FMaterials := TGEMaterials.Create(True);
end;

destructor TSTEPModel.Destroy;
var
  i , j: Integer;
begin
  for i := 0 to High(FMeshes) do
    for j := 0 to High(FMeshes[i].FSubMeshes) do
      FMeshes[i].FSubMeshes[j].Free;
  FMaterials.Free;
end;

procedure TSTEPModel.LoadFromFile(const i_sFileName: String);
var
 l_oLexer : TSTEPLexer;
begin
{$ifdef CODESITE}
 CodeSite.EnterMethod('LoadFromFile');
{$endif}
 l_oLexer := TSTEPLexer.Create(i_sFileName);
 ParseModel(l_oLexer);
 l_oLexer.Free;
{$ifdef CODESITE}
 CodeSite.ExitMethod('LoadFromFile');
{$endif}
end;

procedure TSTEPModel.ParseModel(const i_oLexer : TSTEPLexer);
var
 l_lHasUnsolvedIdents: Boolean;
 l_iLastTokenIdentNo, l_iLeftTokenIdentNo: Integer;
 l_enLastKeyWord: TKeyWord;
 l_sRestLine: String;
begin
 l_sRestLine := '';
 l_lHasUnsolvedIdents:= False;
 l_iLastTokenIdentNo := 0;
 l_iLeftTokenIdentNo := 0;
 l_enLastKeyWord     := TKeyWord.kw_UNKNOWN;
 while (i_oLexer.NextToken <> TSTEPToken.EndOfFile) do begin
   case i_oLexer.Token of
     TSTEPToken.SemiComma: begin
       if (l_iLeftTokenIdentNo > 0) and (l_enLastKeyWord <> TKeyWord.kw_UNKNOWN) then begin
         self.SaveSTEPCommand(l_iLeftTokenIdentNo,l_enLastKeyWord,l_sRestLine);
         // Evtl. Abschlussarbeiten
         if not l_lHasUnsolvedIdents then begin
           // Da alle Daten bekannt sind, erfolgt die Behandlung sofort.
           self.ParseSTEPCommand(l_iLeftTokenIdentNo);
         end;
       end;
       l_sRestLine := '';
       l_lHasUnsolvedIdents:= False;
       l_iLastTokenIdentNo := 0;
       l_iLeftTokenIdentNo := 0;
       l_enLastKeyWord     := TKeyWord.kw_UNKNOWN;
     end;
     TSTEPToken.IdentNo: begin
       l_iLastTokenIdentNo := i_oLexer.TokenIdentNo;
       if (l_iLeftTokenIdentNo > 0) and (l_enLastKeyWord <> TKeyWord.kw_UNKNOWN) then begin
         // Es gibt eine Ident-Nr. in einer zu behandelnden STEP-Anweisung
         l_lHasUnsolvedIdents := True;
{$ifdef CODESITE}
         CodeSite.Send('   IdentNo: ' + IntToStr(l_iLastTokenIdentNo));
{$endif}
       end;
     end;
     TSTEPToken.Equal: begin
       if (l_iLastTokenIdentNo > 0) and (l_iLeftTokenIdentNo = 0) then begin
         l_iLeftTokenIdentNo := l_iLastTokenIdentNo;
         l_sRestLine := Copy(i_oLexer.CurrentLine,i_oLexer.IdxCurChar+1,9999);
{$ifdef CODESITE}
         CodeSite.Send('LeftTokenIdentNo: ' + IntToStr(l_iLeftTokenIdentNo));
         CodeSite.Send('   RestLine: ' + l_sRestLine);
{$endif}
       end;
     end;
     TSTEPToken.KeyWord: begin
       case i_oLexer.TokenKeyWord of
         TKeyWord.kw_CONVERSION_BASED_UNIT,
         TKeyWord.kw_NAMED_UNIT,
         TKeyWord.kw_VERTEX_POINT,
         TKeyWord.kw_CARTESIAN_POINT,
         TKeyWord.kw_DIRECTION,
         TKeyWord.kw_VECTOR,
         TKeyWord.kw_LINE,
         TKeyWord.kw_CIRCLE,
         TKeyWord.kw_EDGE_CURVE,
         TKeyWord.kw_EDGE_LOOP,
         TKeyWord.kw_B_SPLINE_SURFACE_WITH_KNOTS,
         TKeyWord.kw_ORIENTED_EDGE,
         TKeyWord.kw_CURVE_STYLE,
         TKeyWord.kw_ADVANCED_FACE,
         TKeyWord.kw_FACE_OUTER_BOUND,
         TKeyWord.kw_PRESENTATION_STYLE_ASSIGNMENT,
         TKeyWord.kw_SURFACE_SIDE_STYLE,
         TKeyWord.kw_SURFACE_OF_REVOLUTION,
         TKeyWord.kw_SURFACE_STYLE_USAGE,
         TKeyWord.kw_SURFACE_STYLE_FILL_AREA,
         TKeyWord.kw_FILL_AREA_STYLE,
         TKeyWord.kw_FILL_AREA_STYLE_COLOUR,
         TKeyWord.kw_LENGTH_MEASURE_WITH_UNIT,
         TKeyWord.kw_DIMENSIONAL_EXPONENTS,
         TKeyWord.kw_DRAUGHTING_PRE_DEFINED_CURVE_FONT,
         TKeyWord.kw_COLOUR_RGB,
         TKeyWord.kw_AXIS1_PLACEMENT,
         TKeyWord.kw_AXIS2_PLACEMENT_3D: begin
           l_enLastKeyWord := i_oLexer.TokenKeyWord;
{$ifdef CODESITE}
           CodeSite.Send('   KeyWord: ' + KEY_WORDS[l_enLastKeyWord].Str);
{$endif}
         end
       else
         i_oLexer.SkipKeyWordBlock;
       end;
     end;
   else
     i_oLexer.IsExpectedToken(TSTEPToken.KeyWord);
   end;
 end;
 self.ParseAllUnsolvedSTEPCommands();
end;

procedure TSTEPModel.SaveSTEPCommand(i_iLeftTokenIdentNo: Integer; l_enKeyWord: TKeyWord; s_sRestLine: String);
begin

end;

procedure TSTEPModel.ParseSTEPCommand(i_iLeftTokenIdentNo: Integer);
begin

end;

procedure TSTEPModel.ParseAllUnsolvedSTEPCommands();
begin

end;

procedure TSTEPModel.ParseMap(const i_oLexer : TSTEPLexer; var AFileName: String);
var
    ExpectedTokenOk: boolean;
begin
  i_oLexer.NextTokenExpected(TSTEPToken.OpenBracket);
  while not (i_oLexer.NextToken in [TSTEPToken.EndOfFile, TSTEPToken.CloseBracket]) do
  begin
    if (i_oLexer.IsExpectedToken(TSTEPToken.KeyWord)) then
    begin
     {
      case i_oLexer.TokenKeyWord of
        TKeyWord.kw_BITMAP:
        begin
          i_oLexer.NextTokenExpected(TSTEPToken.String);
          if i_oLexer.IsExpectedToken(TSTEPToken.String) then
            AFileName := i_oLexer.TokenString;
        end;
      else
        i_oLexer.SkipKeyWordBlock;
      end;
    }
    end;
  end;
  ExpectedTokenOk := i_oLexer.IsExpectedToken(TSTEPToken.CloseBracket);
  if not ExpectedTokenOk then
    raise ESTEPLexerError.CreateRes(@SAseLexerFileCorruption);
end;


procedure TSTEPModel.ParseMaterial(const i_oLexer : TSTEPLexer; AMaterial: TSTEPMaterial);
var
  LNumSubmaterials, LSubmaterial: Integer;
  ExpectedTokenOk: Boolean;
begin
//  i_oLexer.NextTokenExpected(TSTEPToken.Integer);
//  if (LIdx < 0) or (LIdx >= ANumTexCoord) then
//    i_oLexer.Error('');
  i_oLexer.NextTokenExpected(TSTEPToken.OpenBracket);
  {
  while not (i_oLexer.NextToken in [TSTEPToken.EndOfFile, TSTEPToken.CloseBracket]) do
  begin
    if (i_oLexer.IsExpectedToken(TSTEPToken.KeyWord)) then
    begin
      case i_oLexer.TokenKeyWord of
        TKeyWord.kw_MATERIAL_AMBIENT:
        begin
            i_oLexer.NextTokenExpected(TSTEPToken.Float);
            AMaterial.FAmbient.R := i_oLexer.TokenFloat;
            i_oLexer.NextTokenExpected(TSTEPToken.Float);
            AMaterial.FAmbient.G := i_oLexer.TokenFloat;
            i_oLexer.NextTokenExpected(TSTEPToken.Float);
            AMaterial.FAmbient.B := i_oLexer.TokenFloat;
        end;
        TKeyWord.kw_MATERIAL_DIFFUSE:
        begin
            i_oLexer.NextTokenExpected(TSTEPToken.Float);
            AMaterial.FDiffuse.R := i_oLexer.TokenFloat;
            i_oLexer.NextTokenExpected(TSTEPToken.Float);
            AMaterial.FDiffuse.G := i_oLexer.TokenFloat;
            i_oLexer.NextTokenExpected(TSTEPToken.Float);
            AMaterial.FDiffuse.B := i_oLexer.TokenFloat;
        end;
        TKeyWord.kw_MATERIAL_SPECULAR:
        begin
            i_oLexer.NextTokenExpected(TSTEPToken.Float);
            AMaterial.FSpecular.R := i_oLexer.TokenFloat;
            i_oLexer.NextTokenExpected(TSTEPToken.Float);
            AMaterial.FSpecular.G := i_oLexer.TokenFloat;
            i_oLexer.NextTokenExpected(TSTEPToken.Float);
            AMaterial.FSpecular.B := i_oLexer.TokenFloat;
        end;
        TKeyWord.kw_MAP_DIFFUSE:
        begin
           ParseMap(i_oLexer, AMaterial.FDiffuseMap);
        end;
        TKeyWord.kw_NUMSUBMTLS:
        begin
            i_oLexer.NextTokenExpected(TSTEPToken.Integer);
            LNumSubmaterials := i_oLexer.TokenInteger;
            AMaterial.FSubMaterials.Count := LNumSubmaterials;
        end;
        TKeyWord.kw_SUBMATERIAL:
        begin
            i_oLexer.NextTokenExpected(TSTEPToken.Integer);
            LSubmaterial := i_oLexer.TokenInteger;
            AMaterial.FSubMaterials[LSubmaterial] := TSTEPMaterial.Create;
            AMaterial.FSubMaterials[LSubmaterial].FName := AMaterial.FName + 'sub'+ IntToStr(LSubmaterial);
            ParseMaterial(i_oLexer, TSTEPMaterial(AMaterial.FSubMaterials[LSubmaterial]));

        end;
      else
        i_oLexer.SkipKeyWordBlock;
      end;
    end;
  end;
  }
  ExpectedTokenOk:= i_oLexer.IsExpectedToken(TSTEPToken.CloseBracket);
  if not ExpectedTokenOk then
    raise ESTEPLexerError.CreateRes(@SAseLexerFileCorruption);
end;

procedure TSTEPModel.ParseMaterialList(const i_oLexer : TSTEPLexer);
var
  LNumMaterials: Integer;
  LIdx: Integer;
  ExpectedTokenOk: Boolean;
begin
{
  LNumMaterials := 0;
  i_oLexer.NextTokenExpected(TSTEPToken.OpenBracket);
  while not (i_oLexer.NextToken in [TSTEPToken.EndOfFile, TSTEPToken.CloseBracket]) do
  begin

    if i_oLexer.IsExpectedToken(TSTEPToken.KeyWord) then
    begin
      case i_oLexer.TokenKeyWord of
        TKeyWord.kw_MATERIAL_COUNT:
        begin
          i_oLexer.NextTokenExpected(TSTEPToken.Integer);
          LNumMaterials := i_oLexer.TokenInteger;
          FMaterials.Count := LNumMaterials;
        end;
        TKeyWord.kw_MATERIAL:
          begin
            i_oLexer.NextTokenExpected(TSTEPToken.Integer);
            LIdx := i_oLexer.TokenInteger;

            if (LIdx < 0) or (LIdx >= LNumMaterials) then
              raise EAseParserError.CreateRes(@SAseParserWrongMaterialsNumError);
            FMaterials[LIdx] := TSTEPMaterial.Create;
            FMaterials[LIdx].FName := IntToStr(LIdx);
            ParseMaterial(i_oLexer, TSTEPMaterial(FMaterials.Items[LIdx]));
          end;
      else
        i_oLexer.SkipKeyWordBlock;
      end;
    end;
  end;
  ExpectedTokenOk:= i_oLexer.IsExpectedToken(TSTEPToken.CloseBracket);
  if not ExpectedTokenOk then
    raise ESTEPLexerError.CreateRes(@SAseLexerFileCorruption);
    }
end;


procedure TSTEPModel.ParseVertexList(
  const i_oLexer : TSTEPLexer;
  const ANumVertex: Integer;
  const AVertexSource: TGEVertexSource);
var
  LNumVertex: Integer;
  LIdx: Integer;
  LPositions: TPoint3DDynArray;
  ExpectedTokenOk: Boolean;
begin
{
  i_oLexer.NextTokenExpected(TSTEPToken.OpenBracket);
  LNumVertex := 0;
  SetLength(LPositions, ANumVertex);

  while not (i_oLexer.NextToken in [TSTEPToken.EndOfFile, TSTEPToken.CloseBracket]) do
  begin
    if i_oLexer.IsExpectedToken(TSTEPToken.KeyWord) then
    begin
      case i_oLexer.TokenKeyWord of
        TKeyWord.kw_MESH_VERTEX:
          begin
            Inc(LNumVertex);
            if (LNumVertex > ANumVertex) then
              raise EAseParserError.CreateRes(@SAseParserWrongVertexNumError);
            i_oLexer.NextTokenExpected(TSTEPToken.Integer);

            LIdx := i_oLexer.TokenInteger;
            if (LIdx < 0) or (LIdx >= ANumVertex) then
              raise EAseParserError.CreateRes(@SAseParserWrongVertexIdxError);

            i_oLexer.NextTokenExpected(TSTEPToken.Float);
            LPositions[LIdx].X := i_oLexer.TokenFloat;
            i_oLexer.NextTokenExpected(TSTEPToken.Float);
            LPositions[LIdx].Y := i_oLexer.TokenFloat;
            i_oLexer.NextTokenExpected(TSTEPToken.Float);
            LPositions[LIdx].Z := i_oLexer.TokenFloat;
          end;
      else
        i_oLexer.SkipKeyWordBlock;
      end;
    end;
  end;

  ExpectedTokenOk:= i_oLexer.IsExpectedToken(TSTEPToken.CloseBracket);
  if not ExpectedTokenOk then
    raise ESTEPLexerError.CreateRes(@SAseLexerFileCorruption);
  if (ANumVertex <> LNumVertex) then
    raise EAseParserError.CreateRes(@SAseParserWrongVertexNumError);

  AVertexSource.PositionSource := LPositions;
}
end;

procedure TSTEPModel.ParseNormalList(
    const i_oLexer : TSTEPLexer;
    const ANumFaceNormal: Integer;
    const ANumVertexNormal: Integer;
    var AMesh: TSTEPMesh);
var
  LNumVertexNormal: Integer;
  LVertexNormals: TPoint3DDynArray;
  LNumFaceNormal: Integer;
  LFaceNormals: TPoint3DDynArray;
  ExpectedTokenOk: Boolean;
begin
{
  i_oLexer.NextTokenExpected(TSTEPToken.OpenBracket);
  LNumVertexNormal := 0;
  SetLength(LVertexNormals, ANumVertexNormal);
  LNumFaceNormal:= 0;
  SetLength(LFaceNormals, ANumFaceNormal);

  while not (i_oLexer.NextToken in [TSTEPToken.EndOfFile, TSTEPToken.CloseBracket]) do
  begin
    if i_oLexer.IsExpectedToken(TSTEPToken.KeyWord) then
    begin
      case i_oLexer.TokenKeyWord of
        TKeyWord.kw_MESH_FACENORMAL:
        begin
           if (LNumFaceNormal > ANumFaceNormal) then
              raise EAseParserError.CreateRes(@SAseParserWrongNormalNumError);
            i_oLexer.NextTokenExpected(TSTEPToken.Integer);
            LNumFaceNormal:= i_oLexer.TokenInteger;

            i_oLexer.NextTokenExpected(TSTEPToken.Float);
            LFaceNormals[LNumFaceNormal].X := i_oLexer.TokenFloat;
            i_oLexer.NextTokenExpected(TSTEPToken.Float);
            LFaceNormals[LNumFaceNormal].Y := i_oLexer.TokenFloat;
            i_oLexer.NextTokenExpected(TSTEPToken.Float);
            LFaceNormals[LNumFaceNormal].Z := i_oLexer.TokenFloat;

                                                                                                                            
                                                                                                                                 

//            AMesh.FTriangleMesh[LNumNormal div 3][LNumNormal mod 3].Normal := LNumNormal;
             -- This comment out because that are tools that they don't export these parameters in order
//            Inc(LNumFaceNormal);
        end;
        TKeyWord.kw_MESH_VERTEXNORMAL:
          begin
            if (LNumVertexNormal > ANumVertexNormal) then
              raise EAseParserError.CreateRes(@SAseParserWrongNormalNumError);
            i_oLexer.NextTokenExpected(TSTEPToken.Integer);
            LNumVertexNormal:= i_oLexer.TokenInteger;

            i_oLexer.NextTokenExpected(TSTEPToken.Float);
            LVertexNormals[LNumVertexNormal].X := i_oLexer.TokenFloat;
            i_oLexer.NextTokenExpected(TSTEPToken.Float);
            LVertexNormals[LNumVertexNormal].Y := i_oLexer.TokenFloat;
            i_oLexer.NextTokenExpected(TSTEPToken.Float);
            LVertexNormals[LNumVertexNormal].Z := i_oLexer.TokenFloat;

            AMesh.FTriangleMesh[LNumVertexNormal div 3][LNumVertexNormal mod 3].Normal := LNumVertexNormal;
            -- This comment out because that are tools that they don't export these parameters in order
//            Inc(LNumVertexNormal);
         end;
      else
        i_oLexer.SkipKeyWordBlock;
      end;
    end;
  end;

  ExpectedTokenOk:= i_oLexer.IsExpectedToken(TSTEPToken.CloseBracket);
  if not ExpectedTokenOk then
    raise ESTEPLexerError.CreateRes(@SAseLexerFileCorruption);
//  if (ANumFaceNormal <> LNumFaceNormal) or (ANumVertexNormal <> LNumVertexNormal) then
//    raise EAseParserError.CreateRes(@SAseParserWrongNormalNumError);

  AMesh.FVertexSource.NormalSource := LVertexNormals;
  }
end;

procedure TSTEPModel.ParseTexCoordList(
    const i_oLexer : TSTEPLexer;
    const ANumTexCoord: Integer;
    const AVertexSource: TGEVertexSource);
var
  LNumTexCoord: Integer;
  LIdx: Integer;
  LTexCoord: TPointFDynArray;
  ExpectedTokenOk: Boolean;
begin
{
  i_oLexer.NextTokenExpected(TSTEPToken.OpenBracket);
  LNumTexCoord := 0;
  SetLength(LTexCoord, ANumTexCoord);

  while not (i_oLexer.NextToken in [TSTEPToken.EndOfFile, TSTEPToken.CloseBracket]) do
  begin
    if i_oLexer.IsExpectedToken(TSTEPToken.KeyWord) then
    begin
      case i_oLexer.TokenKeyWord of
        TKeyWord.kw_MESH_TVERT:
          begin
            Inc(LNumTexCoord);
            if (LNumTexCoord > ANumTexCoord) then
              raise EAseParserError.CreateRes(@SAseParserWrongTexCoordNumError);
            i_oLexer.NextTokenExpected(TSTEPToken.Integer);
            LIdx := i_oLexer.TokenInteger;
            if (LIdx < 0) or (LIdx >= ANumTexCoord) then
              raise EAseParserError.CreateRes(@SAseParserWrongTexCoordIdxError);

            i_oLexer.NextTokenExpected(TSTEPToken.Float);
            LTexCoord[LIdx].X := i_oLexer.TokenFloat;
            i_oLexer.NextTokenExpected(TSTEPToken.Float);
            LTexCoord[LIdx].Y := i_oLexer.TokenFloat;
            i_oLexer.NextTokenExpected(TSTEPToken.Float);
            i_oLexer.TokenFloat;
          end;
      else
        i_oLexer.SkipKeyWordBlock;
      end;
    end;
  end;

  ExpectedTokenOk:= i_oLexer.IsExpectedToken(TSTEPToken.CloseBracket);
  if not ExpectedTokenOk then
    raise ESTEPLexerError.CreateRes(@SAseLexerFileCorruption);
  if (ANumTexCoord <> LNumTexCoord) then
    raise EAseParserError.CreateRes(@SAseParserWrongTexCoordNumError);

  AVertexSource.Texture0Source := LTexCoord;
  }
end;

procedure TSTEPModel.ParseFaceList(
  const i_oLexer : TSTEPLexer;
  const ANumFaces: Integer;
  var AMesh: TSTEPMesh);
var
  LTriangle: ^TGETriangleID;
  LNumFaces: Integer;
  LIdx, LSmoothGroup: Integer;
  LC: char;
  ExpectedTokenOk: Boolean;
begin
{
  i_oLexer.NextTokenExpected(TSTEPToken.OpenBracket);
  LNumFaces := 0;
  LIdx := -1;
  LTriangle := nil;
  while not (i_oLexer.NextToken in [TSTEPToken.EndOfFile, TSTEPToken.CloseBracket]) do
  begin
    case i_oLexer.Token of
      TSTEPToken.KeyWord:
        case i_oLexer.TokenKeyWord of
          TKeyWord.kw_MESH_MTLID:
            begin
              i_oLexer.NextTokenExpected(TSTEPToken.Integer);
              AMesh.FFaceMaterials[LIdx] := i_oLexer.TokenInteger;
            end;

          TKeyWord.kw_MESH_SMOOTHING:
            begin
              i_oLexer.UseCommaToken := True;
              -- Some tools use to export all the parameters even they are disabled or they don't have any relevance.
                Here is the case of the smooth group parameter.
//              i_oLexer.NextTokenExpected(TSTEPToken.Integer);
              i_oLexer.NextToken;
              LSmoothGroup:= 0;
              if i_oLexer.Token = TSTEPToken.Integer then
              begin
                LSmoothGroup := i_oLexer.TokenInteger;
                while i_oLexer.NextToken = TSTEPToken.Comma do
                  i_oLexer.NextTokenExpected(TSTEPToken.Integer);
              end
              else
                if (i_oLexer.Token = TSTEPToken.KeyWord) and (i_oLexer.TokenKeyWord = TKeyWord.kw_MESH_MTLID) then
                  LSmoothGroup :=  0;

              i_oLexer.Ahead := true;
              i_oLexer.UseCommaToken := False;

              if Assigned(LTriangle) then
              begin
                LTriangle[0].SmoothGroup := LSmoothGroup;
                LTriangle[1].SmoothGroup := LSmoothGroup;
                LTriangle[2].SmoothGroup := LSmoothGroup;
              end;
            end;

          TKeyWord.kw_MESH_FACE:
            begin
              if (LNumFaces > ANumFaces) then
                raise EAseParserError.CreateRes(@SAseParserWrongFacesNumError);

              i_oLexer.NextTokenExpected(TSTEPToken.Integer);
              LIdx := i_oLexer.TokenInteger;
              if (LIdx < 0) or (LIdx >= ANumFaces) then
                raise EAseParserError.CreateRes(@SAseParserWrongFacesIdxError);

              LTriangle := @AMesh.FTriangleMesh[LIdx];
              LTriangle[0] := NullVertexID;
              LTriangle[1] := NullVertexID;
              LTriangle[2] := NullVertexID;

              Inc(LNumFaces);
            end;
        else
          i_oLexer.SkipKeyWordBlock;
        end;
      TSTEPToken.IdentNo:
        begin
          if LIdx = -1 then
            raise EAseParserError.CreateRes(@SAseParserWrongFacesIdxError);

          case Length(i_oLexer.TokenIdent) of
            1:
              begin
                LC := i_oLexer.TokenIdent[1];
                //-- i_oLexer.NextTokenExpected(TSTEPToken.Colon);
                i_oLexer.NextTokenExpected(TSTEPToken.Integer);
                case LC of
                  'A': LTriangle[0].Position := i_oLexer.TokenInteger;
                  'B': LTriangle[1].Position := i_oLexer.TokenInteger;
                  'C': LTriangle[2].Position := i_oLexer.TokenInteger;
                end;
              end;
          //  2:
          else
            //-- i_oLexer.NextTokenExpected(TSTEPToken.Colon);
            i_oLexer.NextTokenExpected(TSTEPToken.Integer);
            Continue;
          end;
        end;
      TSTEPToken.SemiComma:
      begin

      end
    else
      raise EAseParserError.CreateRes(@SAseParserUnexpectedKyWordError);
    end;
  end;

  ExpectedTokenOk:= i_oLexer.IsExpectedToken(TSTEPToken.CloseBracket);
  if not ExpectedTokenOk then
    raise ESTEPLexerError.CreateRes(@SAseLexerFileCorruption);
  if (ANumFaces <> LNumFaces) then
    raise EAseParserError.CreateRes(@SAseParserWrongFacesNumError);
}
end;

procedure TSTEPModel.ParseTFaceList(
  const i_oLexer : TSTEPLexer;
  const ANumFaces: Integer;
  var AMesh: TSTEPMesh);
var
  LNumFace: Integer;
  LIdx: Integer;
  ExpectedTokenOk: Boolean;
begin
 {
  i_oLexer.NextTokenExpected(TSTEPToken.OpenBracket);
  LNumFace := 0;

  while not (i_oLexer.NextToken in [TSTEPToken.EndOfFile, TSTEPToken.CloseBracket]) do
  begin
    if i_oLexer.IsExpectedToken(TSTEPToken.KeyWord) then
    begin
      case i_oLexer.TokenKeyWord of
        TKeyWord.kw_MESH_TFACE:
          begin
            Inc(LNumFace);
            if (LNumFace > Length(AMesh.FTriangleMesh)) then
              raise EAseParserError.CreateRes(@SAseParserWrongTriangleMeshNumError);
            i_oLexer.NextTokenExpected(TSTEPToken.Integer);
            LIdx := i_oLexer.TokenInteger;
            if (LIdx < 0) or (LIdx > High(AMesh.FTriangleMesh)) then
              raise EAseParserError.CreateRes(@SAseParserWrongTriangleMeshIdxError);

            i_oLexer.NextTokenExpected(TSTEPToken.Integer);
            AMesh.FTriangleMesh[LIdx][0].Texture0 := i_oLexer.TokenInteger;
            i_oLexer.NextTokenExpected(TSTEPToken.Integer);
            AMesh.FTriangleMesh[LIdx][1].Texture0 := i_oLexer.TokenInteger;
            i_oLexer.NextTokenExpected(TSTEPToken.Integer);
            AMesh.FTriangleMesh[LIdx][2].Texture0 := i_oLexer.TokenInteger;
          end;
      else
        i_oLexer.SkipKeyWordBlock;
      end;
    end;
  end;

  ExpectedTokenOk:= i_oLexer.IsExpectedToken(TSTEPToken.CloseBracket);
  if not ExpectedTokenOk then
    raise ESTEPLexerError.CreateRes(@SAseLexerFileCorruption);
  if (Length(AMesh.FTriangleMesh) <> LNumFace) then
    raise EAseParserError.CreateRes(@SAseParserWrongTriangleMeshNumError);
 }
end;

function TSTEPModel.AddSubMesh(const AVertexSource: TGEVertexSource): TGEMesh;
var
  LMesh: ^TSTEPMesh;
begin
  result := TGEMesh.Create(AVertexSource);
  LMesh := @FMeshes[High(FMeshes)];
  SetLength(LMesh.FSubMeshes, Length(LMesh.FSubMeshes) + 1);
  LMesh.FSubMeshes[High(LMesh.FSubMeshes)] := result;
end;

procedure TSTEPModel.ParseMesh(const i_oLexer : TSTEPLexer; var AMesh : TSTEPMesh);
var
  LNumVertex, LNumFaces, LNumTVVertex: Integer;
  ExpectedTokenOk: Boolean;
begin
 {
  i_oLexer.NextTokenExpected(TSTEPToken.OpenBracket);
  LNumFaces := 0;
  LNumVertex := 0;
  LNumTVVertex := 0;

  while not (i_oLexer.NextToken in [TSTEPToken.EndOfFile, TSTEPToken.CloseBracket]) do
  begin
    if i_oLexer.IsExpectedToken(TSTEPToken.KeyWord) then
    begin
      case i_oLexer.TokenKeyWord of
        TKeyWord.kw_MESH_NUMVERTEX:
          begin
            i_oLexer.NextTokenExpected(TSTEPToken.Integer);
            LNumVertex := i_oLexer.TokenInteger;
          end;
        TKeyWord.kw_MESH_NUMFACES:
          begin
            i_oLexer.NextTokenExpected(TSTEPToken.Integer);
            LNumFaces := i_oLexer.TokenInteger;
            SetLength(AMesh.FTriangleMesh, LNumFaces);
            SetLength(AMesh.FFaceMaterials, LNumFaces);
          end;
        TKeyWord.kw_MESH_NUMTVERTEX:
          begin
            i_oLexer.NextTokenExpected(TSTEPToken.Integer);
            LNumTVVertex := i_oLexer.TokenInteger;
          end;
        TKeyWord.kw_MESH_VERTEX_LIST:
            ParseVertexList(i_oLexer, LNumVertex, AMesh.FVertexSource);
        TKeyWord.kw_MESH_TVERTLIST:
           ParseTexCoordList(i_oLexer, LNumTVVertex, AMesh.FVertexSource);
        TKeyWord.kw_MESH_NORMALS:
           ParseNormalList(i_oLexer, LNumFaces, LNumVertex, AMesh);
        TKeyWord.kw_MESH_FACE_LIST:
           ParseFaceList(i_oLexer, LNumFaces, AMesh);
        TKeyWord.kw_MESH_TFACELIST:
           ParseTFaceList(i_oLexer, LNumFaces, AMesh);
      else
        i_oLexer.SkipKeyWordBlock;
      end;
    end;
  end;
  ExpectedTokenOk:= i_oLexer.IsExpectedToken(TSTEPToken.CloseBracket);
  if not ExpectedTokenOk then
    raise ESTEPLexerError.CreateRes(@SAseLexerFileCorruption);
 }
end;

procedure TSTEPModel.ParseGeometry(const i_oLexer : TSTEPLexer);
var
  LMaterialRef,  LSubMaterial, i: Integer;
  LMeshData: TSTEPMesh;
  LMesh: TGEMesh;
  ExpectedTokenOk: Boolean;
begin
 {
  LMeshData.FVertexSource := TGEVertexSource.Create;

  i_oLexer.NextTokenExpected(TSTEPToken.OpenBracket);
  LMaterialRef := -1;

  while not (i_oLexer.NextToken in [TSTEPToken.EndOfFile, TSTEPToken.CloseBracket]) do
  begin
    if i_oLexer.IsExpectedToken(TSTEPToken.KeyWord) then
    begin
      case i_oLexer.TokenKeyWord of
        TKeyWord.kw_MESH:
           ParseMesh(i_oLexer, LMeshData);
        TKeyWord.kw_MATERIAL_REF:
         begin
           i_oLexer.NextTokenExpected(TSTEPToken.Integer);
           LMaterialRef := i_oLexer.TokenInteger;

         end;
      else
        i_oLexer.SkipKeyWordBlock;
      end;
    end;
  end;

  ExpectedTokenOk:= i_oLexer.IsExpectedToken(TSTEPToken.CloseBracket);
  if not ExpectedTokenOk then
    raise ESTEPLexerError.CreateRes(@SAseLexerFileCorruption);

  SetLength(FMeshes, Length(FMeshes) + 1);
  FMeshes[high(FMeshes)] := LMeshData;

  LSubMaterial := -1;
  LMesh := nil;
  if (LMaterialRef <> -1) and
     (TSTEPMaterial(FMaterials[LMaterialRef]).FSubMaterials.Count > 0) then
  begin
    for i := 0 to High(LMeshData.FFaceMaterials) do
    begin
      if (LSubMaterial <> LMeshData.FFaceMaterials[i]) then
      begin
         LSubMaterial := LMeshData.FFaceMaterials[i];
         LMesh := AddSubMesh(LMeshData.FVertexSource);
         LMesh.MaterialName := IntToStr(LMaterialRef)+ 'sub' + inttostr(LSubMaterial);
      end;
      LMesh.AddTriangle(LMeshData.FTriangleMesh[i]);
    end;
  end else
  begin
    LMesh := AddSubMesh(LMeshData.FVertexSource);
    LMesh.AddTriangleMesh(LMeshData.FTriangleMesh);
    LMesh.MaterialName := inttostr(LMaterialRef);
  end;

  LMeshData.FVertexSource.Free;
  }
end;

{ TSTEPMaterial }

constructor TSTEPMaterial.Create;
begin
  FSubMaterials := TGEMaterials.Create(True);
end;

destructor TSTEPMaterial.Destroy;
begin
  FSubMaterials.Free;

  inherited;
end;

end.
