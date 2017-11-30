unit uGenShaftMesh;

{ usage
  Ein Wellenabschnitt erfordert den Radius, Länge und Winkel
  Model       Flag=w, Wellenabschnittradius, Wellenabschnittlänge,  Winkel in x,y-Ebene zur x-Achse in Grad
  Example     '-W-, 0.2, 0.1, 90.0'

  Ein Kegelabschnitt erfordert den Radius zu Beginn und Ende des Kegelabschnitts, Länge und Winkel
  Example     '-K-, 0.2, 0.3, 0.1, 90.0'
}

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  Generics.Collections, System.RTLConsts, uMeshUtils,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Types3D,
  System.Math.Vectors, FMX.Objects3D, FMX.MaterialSources, FMX.Controls3D,
  FMX.Viewport3D, FMX.StdCtrls, FMX.Controls.Presentation, FMX.Layouts;

const
  QuadPoints = 8;

type

  TShaftSectionBase = Class(TObject)
  Private
    FPrev, FNext: TShaftSectionBase;
    FStartLocal, FEndLocal: TPoint3D;
    Procedure SetRadius(Value: Single);
  Protected
    FLength, FAngleX: Single;
    FRadiusBeg, FRadiusEnd: Single;
    FRadiusArrayBeg: TPathArray;
    FRadiusArrayEnd: TPathArray;
    VBuffer: TVertexBuffer;
    IBuffer: TIndexBuffer;
    Procedure SetFltValueFrmArrayString(Var AFloat: Single;
      Var AString: String);
    Procedure AddSq(ASq: TSqArray; Var AVOffset: Integer;
      Var AIOffset: Integer);
    Procedure AddCylinder(AR1, AX1, AY1, AZ1, AR2, AX2, AY2, AZ2: Real;
                          var AVOffset: Integer; Var AIOffset: Integer);
    Procedure BuildCylinder(RadiusArray1, RadiusArray2: TPathArray;
      Var AVOffset: Integer; Var AIOffset: Integer);
    Procedure BuildCylindersEnd(RadiusArray: TPathArray; AReverse: Boolean;
      Var AVOffset: Integer; Var AIOffset: Integer);
  Public
    Constructor Create;
    Destructor Destroy; override;
    Class Function CreateFrmTxt(AText: String): TShaftSectionBase;
    Function DeltaX: Real; virtual; abstract;
    Function DeltaY: Real; virtual; abstract;
    Function NoOfVertexes: Integer;
    Function NoOfIndexes: Integer;
    Procedure AddSection(ANewSection: TShaftSectionBase);
    Procedure AddData(Var NxtData, NxtIndex: Integer); virtual;
    Property Radius:    Single read FRadiusEnd write SetRadius;
    Property RadiusBeg: Single read FRadiusBeg write FRadiusBeg;
    Property RadiusEnd: Single read FRadiusEnd write FRadiusEnd;
    Property SecLength: Single read FLength    write FLength;
    Property RadiusArrayBeg: TPathArray read FRadiusArrayBeg write FRadiusArrayBeg;
    Property RadiusArrayEnd: TPathArray read FRadiusArrayEnd write FRadiusArrayEnd;
  End;

  TShaftStraight = Class(TShaftSectionBase)
  Private
  Public
    Constructor Create;
    Constructor CreateFrmTxt(AText: String);
    Function DeltaX: Real; override;
    Function DeltaY: Real; override;
    Procedure AddData(Var NxtData, NxtIndex: Integer); override;
    property Radius;
    property SecLength;
    Property RadiusArrayBeg;
    Property RadiusArrayEnd;
  End;

  TConeStraight = Class(TShaftSectionBase)
  Private
  Public
    Constructor Create;
    Constructor CreateFrmTxt(AText: String);
    Function DeltaX: Real; override;
    Function DeltaY: Real; override;
    Procedure AddData(Var NxtData, NxtIndex: Integer); override;
    property RadiusBeg;
    property RadiusEnd;
    property SecLength;
    Property RadiusArrayBeg;
    Property RadiusArrayEnd;
  End;

  TShaft3D = Class(TOwnMesh)
  Private
    FSections: TShaftSectionBase;
    m_lstConstructionCode: TStrings;
    procedure SetConstructionCode(const Value: TStrings);
  protected
    procedure ReadState(Reader: TReader); override;
  public
    constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;
    procedure ResetSections;
    Procedure AddSection(ANewSection: TShaftSectionBase);
    Procedure AddSectionFrmText(AText: String);
    Procedure RebuildMesh;
  published
    property MaterialSource;
    property Position;
    property Scale;
    property RotationAngle;
    property Locked default False;
    property Width;
    property Height;
    property Depth;
    property Opacity nodefault;
    property Projection;
    property HitTest default True;
    property VisibleContextMenu default True;
    property TwoSide default False;
    property Visible default True;
    property ZWrite default True;
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnKeyDown;
    property OnKeyUp;
    property OnRender;
    Property ConstructionCode: TStrings Read m_lstConstructionCode write SetConstructionCode;
  End;

function GetShaftSectionMesh(): TOwnMesh;
function GetConeSectionMesh():  TOwnMesh;
function GetComplexShaftMesh(): TOwnMesh;

implementation

function GetShaftSectionMesh(): TOwnMesh;
var
 l_oMeshShaft: TShaft3D;
begin
 l_oMeshShaft := TShaft3D.Create(Nil);
 l_oMeshShaft.ConstructionCode.Add('-W-,8.0,10.0,0.0');   // Flag=w, Wellenabschnittradius, Wellenabschnittlänge,  Winkel in x,y-Ebene zur x-Achse in Grad
 l_oMeshShaft.RebuildMesh;
 Result := l_oMeshShaft;
end;

function GetConeSectionMesh(): TOwnMesh;
var
 l_oMeshShaft: TShaft3D;
begin
 l_oMeshShaft := TShaft3D.Create(Nil);
 l_oMeshShaft.ConstructionCode.Add('-K-,8.0,10.0,10.0,0.0');   // Flag=c, Wellenabschnittradius zum Beginn, Wellenabschnittradius zum Ende, Wellenabschnittlänge,  Winkel in x,y-Ebene zur x-Achse in Grad
 l_oMeshShaft.RebuildMesh;
 Result := l_oMeshShaft;
end;

function GetComplexShaftMesh(): TOwnMesh;
var
 l_oMeshShaft: TShaft3D;
begin
 l_oMeshShaft := TShaft3D.Create(Nil);
 l_oMeshShaft.ConstructionCode.Add('-W-,  8.0,  5.0, 0.0');    // Flag=w, Wellenabschnittradius, Wellenabschnittlänge,  Winkel in x,y-Ebene zur x-Achse in Grad
 l_oMeshShaft.ConstructionCode.Add('-W-, 12.0, 12.0, 0.0');
 l_oMeshShaft.ConstructionCode.Add('-W-, 10.0, 8.0, 0.0');
 l_oMeshShaft.ConstructionCode.Add('-W-,  8.0, 20.0, 0.0');
 l_oMeshShaft.ConstructionCode.Add('-W-,  4.0, 10.0, 0.0');
 l_oMeshShaft.RebuildMesh;
 Result := l_oMeshShaft;
end;

function GetToken(var S: String; Separators: String; Stop: String = ''): String;
var
  I, len: Integer;
  CopyS: String;
begin
  Result := '';
  CopyS := S;
  len := Length(CopyS);
  for I := 1 to len do
  begin
    if Pos(CopyS[I], Stop) > 0 then
      Break;
    Delete(S, 1, 1);
    if Pos(CopyS[I], Separators) > 0 then
    begin
      Result := Result;
      Break;
    end;
    Result := Result + CopyS[I];
  end;
  Result := Trim(Result);
  S := Trim(S);
end;

//------------------------------------------------------------------------------

{ --------------------------------- TShaft3D -----------------------------------}

constructor TShaft3D.Create(AOwner: TComponent);
begin
 inherited;
 m_lstConstructionCode := TStringList.Create;
end;

destructor TShaft3D.Destroy;
begin
  FSections.Free;
  m_lstConstructionCode.Free;
  inherited;
end;

procedure TShaft3D.AddSection(ANewSection: TShaftSectionBase);
begin
 if ANewSection = nil then
   exit;
 ANewSection.VBuffer := Data.VertexBuffer;
 ANewSection.IBuffer := Data.IndexBuffer;
 if FSections = nil then
   FSections := ANewSection
 else
   FSections.AddSection(ANewSection);
end;

procedure TShaft3D.AddSectionFrmText(AText: String);
Var
  NewSection: TShaftSectionBase;
begin
  if AText = '' then
    exit;
  NewSection := TShaftSectionBase.CreateFrmTxt(AText);
  AddSection(NewSection);
end;

procedure TShaft3D.ReadState(Reader: TReader);
begin
  inherited;
  FreeAndNil(FSections);
  RebuildMesh;
end;

procedure TShaft3D.RebuildMesh;
var
 NxtData, NxtIndex: Integer;
 I: Integer;
begin
 if FSections = nil then Begin
   if m_lstConstructionCode.Count > 0 then
     for I := 0 to m_lstConstructionCode.Count - 1 do
       AddSectionFrmText(m_lstConstructionCode[I]);
   if FSections = nil then
     exit;
 End;
  { MeshRender uses D3DPT_TRIANGLELIST
    From http://www.directxtutorial.com/tutorial9/b-direct3dbasics/dx9B4.aspx#still
    [Table 4.3 - D3DPRIMITIVETYPE Values]
    Value 	Description
    D3DPT_POINTLIST 	  Shows a series of points.
    D3DPT_LINELIST      Shows a series of separated lines.
    D3DPT_LINESTRIP 	  Shows a series of connected lines.
    D3DPT_TRIANGLELIST 	Shows a series of separated triangles.
    D3DPT_TRIANGLESTRIP Shows a series of connected triangles.
    D3DPT_TRIANGLEFAN 	Shows a series of triangles with one shared corner.
  }
 NxtData := 0;
 NxtIndex := 0;
 Data.VertexBuffer.Length := FSections.NoOfVertexes;
 Data.IndexBuffer.Length  := FSections.NoOfIndexes;
 FSections.AddData(NxtData, NxtIndex);
 //XE2 Data.CalcNormals;
 Data.CalcFaceNormals;
 //-- Data.CalcSmoothNormals;
 //Data.CalcTangentBinormals;
end;

procedure TShaft3D.ResetSections;
begin
  FreeAndNil(FSections);
end;

procedure TShaft3D.SetConstructionCode(const Value: TStrings);
begin
 m_lstConstructionCode.Clear;
 m_lstConstructionCode.Assign(Value);
 if m_lstConstructionCode.Count > 0 then begin
   FreeAndNil(FSections);
   RebuildMesh;
 end;
end;

//------------------------------------------------------------------------------

{ TShaftStraight }

constructor TShaftStraight.Create;
begin
 inherited;
end;

procedure TShaftStraight.AddData(var NxtData, NxtIndex: Integer);
begin
 FEndLocal   := MovePoint(FStartLocal, DeltaX, DeltaY, 0.0);
 AddCylinder(Radius, FStartLocal.X, FStartLocal.Y, FStartLocal.Z,
             Radius, FEndLocal.X,   FEndLocal.Y,   FEndLocal.Z,   NxtData, NxtIndex);
 inherited;
end;

constructor TShaftStraight.CreateFrmTxt(AText: String);
Var
 Flag: String;
 AngleXAsDegrees: Single;
begin
 inherited Create;
 Flag := uGenShaftMesh.GetToken(AText, ',()');
 if uppercase(Flag[2]) <> 'W' then
   raise Exception.Create('TShaftStraight.CreateFrmTxt::' + AText);
 // Model       Flag w, Wellenabschnittradius, Wellenabschnittlänge,  Winkel in x,y-Ebene zur x-Achse in Grad
 // Example     '-W-,0.2,0.1,0.0'
 AngleXAsDegrees := 0.0;
 SetFltValueFrmArrayString(FRadiusBeg, AText);
 FRadiusEnd := FRadiusBeg;
 SetFltValueFrmArrayString(FLength, AText);
 SetFltValueFrmArrayString(AngleXAsDegrees, AText);
 if AngleXAsDegrees <> 0.0 then
   FAngleX := AngleXAsDegrees / 180 * System.Pi;
end;

function TShaftStraight.DeltaX: Real;
begin
 Result := FLength * Cos(FAngleX);
end;

function TShaftStraight.DeltaY: Real;
begin
 Result := FLength * Sin(FAngleX);
end;

//------------------------------------------------------------------------------

{ TConeStraight }

constructor TConeStraight.Create;
begin
 inherited;
end;

procedure TConeStraight.AddData(Var NxtData, NxtIndex: Integer);
begin
 FEndLocal   := MovePoint(FStartLocal, DeltaX, DeltaY, 0.0);
 AddCylinder(FRadiusBeg, FStartLocal.X, FStartLocal.Y, FStartLocal.Z,
             FRadiusEnd, FEndLocal.X,   FEndLocal.Y,   FEndLocal.Z,  NxtData, NxtIndex);
 inherited;
end;

constructor TConeStraight.CreateFrmTxt(AText: String);
Var
 Flag: String;
 AngleXAsDegrees: Single;
begin
 inherited Create;
 Flag := uGenShaftMesh.GetToken(AText, ',()');
 if uppercase(Flag[2]) <> 'K' then
   raise Exception.Create('TConeStraight.CreateFrmTxt::' + AText);
 // Model       Flag w, Wellenabschnittradius zum Beginn, Wellenabschnittradius zum Ende, Wellenabschnittlänge,  Winkel in x,y-Ebene zur x-Achse in Grad
 // Example     '-K-,0.2,0.3,0.1,0.0'
 AngleXAsDegrees := 0.0;
 SetFltValueFrmArrayString(FRadiusBeg, AText);
 SetFltValueFrmArrayString(FRadiusEnd, AText);
 SetFltValueFrmArrayString(FLength, AText);
 SetFltValueFrmArrayString(AngleXAsDegrees, AText);
 if AngleXAsDegrees <> 0.0 then
   FAngleX := AngleXAsDegrees / 180 * System.Pi;
end;

function TConeStraight.DeltaX: Real;
begin
 Result := FLength * Cos(FAngleX);
end;

function TConeStraight.DeltaY: Real;
begin
 Result := FLength * Sin(FAngleX);
end;

//------------------------------------------------------------------------------

{ TShaftSectionBase }

constructor TShaftSectionBase.Create;
begin
 inherited;
 SetLength(FRadiusArrayBeg,0);
 SetLength(FRadiusArrayEnd,0);
 FLength := 1;
 FRadiusBeg := 1;
 FRadiusEnd := 1;
end;

destructor TShaftSectionBase.Destroy;
begin
 SetLength(FRadiusArrayBeg,0);
 SetLength(FRadiusArrayEnd,0);
 FNext.Free;
 inherited;
end;

Procedure TShaftSectionBase.SetRadius(Value: Single);
begin
 FRadiusBeg := Value;
 FRadiusEnd := Value;
end;

procedure TShaftSectionBase.AddCylinder(AR1, AX1, AY1, AZ1, AR2, AX2, AY2, AZ2: Real;
                                        var AVOffset, AIOffset: Integer);
var
 l_lstPathArray: TPathArray;
begin
 if Length(FRadiusArrayBeg) = 0 then
   uMeshUtils.RadiusArray(AR1, AX1, AY1, AZ1, 0.0, QuadPoints, FRadiusArrayBeg);
 if Length(FRadiusArrayEnd) = 0 then
   uMeshUtils.RadiusArray(AR2, AX2, AY2, AZ2, 0.0, QuadPoints, FRadiusArrayEnd);
 if FPrev = nil then begin
   self.BuildCylindersEnd(FRadiusArrayBeg, false, AVOffset, AIOffset);
 end
 else begin
   if Abs(self.RadiusBeg - FPrev.RadiusEnd) > TEpsilon.Vector then begin
     if self.RadiusBeg > FPrev.RadiusEnd then begin
       self.BuildCylinder(FPrev.RadiusArrayEnd, FRadiusArrayBeg, AVOffset, AIOffset);
     end;
   end;
 end;
 self.BuildCylinder(FRadiusArrayBeg, FRadiusArrayEnd, AVOffset, AIOffset);
 if FNext = nil then begin
   self.BuildCylindersEnd(FRadiusArrayEnd, True, AVOffset, AIOffset);
 end
 else begin
   if Abs(self.RadiusEnd - FNext.RadiusBeg) > TEpsilon.Vector then begin
     if self.RadiusEnd > FNext.RadiusBeg then begin
       if Length(FNext.RadiusArrayBeg) = 0 then begin
         uMeshUtils.RadiusArray(FNext.RadiusBeg, AX2, AY2, AZ2, 0.0, QuadPoints, l_lstPathArray);
         self.BuildCylinder(FRadiusArrayEnd, l_lstPathArray, AVOffset, AIOffset);
       end
       else
         self.BuildCylinder(FRadiusArrayEnd, FNext.RadiusArrayBeg, AVOffset, AIOffset);
     end;
   end;
 end;
end;

procedure TShaftSectionBase.AddData(var NxtData, NxtIndex: Integer);
begin


 if FNext <> nil then begin
   FNext.FStartLocal := FEndLocal;
   FNext.AddData(NxtData, NxtIndex);
 end;
end;

procedure TShaftSectionBase.AddSection(ANewSection: TShaftSectionBase);
begin
 if FNext <> nil then
   FNext.AddSection(ANewSection)
 else begin
   FNext := ANewSection;
   ANewSection.FPrev := self; // no need for pop???
 end;
end;

procedure TShaftSectionBase.AddSq(ASq: TSqArray; var AVOffset, AIOffset: Integer);
begin
 VBuffer.Vertices[AVOffset] := ASq[0];
 VBuffer.Vertices[AVOffset + 1] := ASq[1];;
 VBuffer.Vertices[AVOffset + 2] := ASq[2];;
 VBuffer.Vertices[AVOffset + 3] := ASq[3];;
 IBuffer.Indices[AIOffset] := AVOffset + 3;
 IBuffer.Indices[AIOffset + 1] := AVOffset + 2;
 IBuffer.Indices[AIOffset + 2] := AVOffset + 1;
 IBuffer.Indices[AIOffset + 3] := AVOffset + 1;
 IBuffer.Indices[AIOffset + 4] := AVOffset + 0;
 IBuffer.Indices[AIOffset + 5] := AVOffset + 3;
 Inc(AVOffset, 4);
 Inc(AIOffset, 6);
end;

procedure TShaftSectionBase.BuildCylinder(RadiusArray1, RadiusArray2: TPathArray;
  var AVOffset, AIOffset: Integer);
Var
 I, h1, h2: Integer;
begin
 h1 := high(RadiusArray1);
 h2 := high(RadiusArray2);
 if h1 <> h2 then
   raise Exception.Create('TShaftSectionBase.BuildCylinder');
 I := 0;
 while (I < h1) and (I < h2) do begin
   AddSq(SqArray(RadiusArray1[I + 1], RadiusArray1[I], RadiusArray2[I],
                 RadiusArray2[I + 1]), AVOffset, AIOffset);
   Inc(I);
 end;
 AddSq(SqArray(RadiusArray1[0], RadiusArray1[h1], RadiusArray2[h2],
               RadiusArray2[0]), AVOffset, AIOffset);
end;

procedure TShaftSectionBase.BuildCylindersEnd(RadiusArray: TPathArray;
  AReverse: Boolean; var AVOffset, AIOffset: Integer);
Var
 I, sz, CtrOffset: Integer;
 Cntr: TPoint3D;
begin
 sz := high(RadiusArray);
 if sz < 4 then
   raise Exception.Create('TShaftSectionBase.BuildCylindersEnd');
 Cntr := Average3D(RadiusArray[0], RadiusArray[sz div 2]);
 VBuffer.Vertices[AVOffset] := Cntr;
 CtrOffset := AVOffset;
 // VBuffer.Diffuse[CtrOffset] := claYellow;
 Inc(AVOffset);
 I := 0;
 while I <= sz do begin
   VBuffer.Vertices[AVOffset] := RadiusArray[I];
   if I = sz then
     VBuffer.Vertices[AVOffset + 1] := RadiusArray[0]
   else
     VBuffer.Vertices[AVOffset + 1] := RadiusArray[I + 1];
   //VBuffer.Diffuse[AVOffset] := claBlue;
   //VBuffer.Diffuse[AVOffset + 1] := claRed;
   if AReverse then begin
     IBuffer.Indices[AIOffset] := AVOffset + 1;
     IBuffer.Indices[AIOffset + 1] := CtrOffset;
     IBuffer.Indices[AIOffset + 2] := AVOffset + 0;
   end
   else begin
     IBuffer.Indices[AIOffset] := AVOffset + 0;
     IBuffer.Indices[AIOffset + 1] := CtrOffset;
     IBuffer.Indices[AIOffset + 2] := AVOffset + 1;
   end;
   Inc(AVOffset, 2);
   Inc(AIOffset, 3);
   Inc(I);
 end;
end;

class function TShaftSectionBase.CreateFrmTxt(AText: String): TShaftSectionBase;
begin
 Result := nil;
 if Length(AText) < 5 then
   exit;
 case AText[2] of
   'w', 'W': begin
     Result := TShaftStraight.CreateFrmTxt(AText);
   end;
   'k', 'K': begin
     Result := TConeStraight.CreateFrmTxt(AText);
   end;
 end;
end;

function TShaftSectionBase.NoOfIndexes: Integer;
var
 l_iRadiusSections: Integer;
begin
 l_iRadiusSections := QuadPoints * 4;
 if FNext = nil then
   Result := 0
 Else
   Result := FNext.NoOfIndexes;
 // 6 Lines Indexs per Square
 // X Squares per Length
 Result := Result + l_iRadiusSections * 6;
 if FPrev = nil then begin
   // Abschluss vorne
   Result := Result + l_iRadiusSections * 3;
 end
 else begin
   if Abs(self.RadiusBeg - FPrev.RadiusEnd) > TEpsilon.Vector then begin
     if self.RadiusBeg > FPrev.RadiusEnd then begin
       Result := Result + l_iRadiusSections * 6;
     end;
   end;
 end;
 if FNext = nil then begin
   // Abschluss hinten
   Result := Result + l_iRadiusSections * 3;
 end
 else begin
   if Abs(self.RadiusEnd - FNext.RadiusBeg) > TEpsilon.Vector then begin
     if self.RadiusEnd > FNext.RadiusBeg then begin
       Result := Result + l_iRadiusSections * 6;
     end;
   end;
 end;
end;

function TShaftSectionBase.NoOfVertexes: Integer;
var
 l_iRadiusSections: Integer;
begin
 l_iRadiusSections := QuadPoints * 4;
 if FNext = nil then
   Result := 0
 Else
   Result := FNext.NoOfVertexes;
 // 4 Vertexes per Square
 // X Squares per Length
 Result := Result + l_iRadiusSections * 4;
 if FPrev = nil then begin
   // Abschluss vorne
   Result := Result + 1 + l_iRadiusSections * 2;
 end
 else begin
   if Abs(self.RadiusBeg - FPrev.RadiusEnd) > TEpsilon.Vector then begin
     if self.RadiusBeg > FPrev.RadiusEnd then begin
       Result := Result + l_iRadiusSections * 4;
     end;
   end;
 end;
 if FNext = nil then begin
   // Abschluss hinten
   Result := Result + 1 + l_iRadiusSections * 2;
 end
 else begin
   if Abs(self.RadiusEnd - FNext.RadiusBeg) > TEpsilon.Vector then begin
     if self.RadiusEnd > FNext.RadiusBeg then begin
       Result := Result + l_iRadiusSections * 4;
     end;
   end;
 end;
end;

procedure TShaftSectionBase.SetFltValueFrmArrayString(var AFloat: Single;
  var AString: String);
Var
 Val: Single;
begin
 try
   Val := StrToFloat(uGenShaftMesh.GetToken(AString, ',()'));
 except
   Val := 0.0;
 end;
 if Val <> 0.0 then
   AFloat := Val;
end;

end.
