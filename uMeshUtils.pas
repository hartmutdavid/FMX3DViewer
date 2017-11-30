unit uMeshUtils;

interface
uses System.SysUtils, System.Types, System.Classes, Generics.Collections,
     FMX.Types, FMX.Graphics,
     FMX.Types3D, FMX.Objects3D, System.Math, System.Math.Vectors, FMX.Utils;

type
 TSqArray = array [0..3] of TPoint3D;
 TPathArray = array of TPoint3D;

//-------------------------------- TOwnMesh ------------------------------------
TOwnMesh = class(TCustomMesh)
public
  constructor Create(AOwner: TComponent); override;
  destructor Destroy; override;
published
  property Data;
end;

//-------------------------------- TOwnModel3D ---------------------------------
TOwnModel3D = class(TModel3D)
public
  constructor Create(AOwner: TComponent); override;
  destructor Destroy; override;
  procedure ReadModel(Stream: TStream);
  procedure WriteModel(Stream: TStream);
  procedure UpdateMeshCollection;
  procedure Resize3D;
published
  property MeshCollection;
  property WrapMode;
end;

//------------------------------------------------------------------------------

Function  SqArray(ca,cb,cc,cd: TPoint3D): TSqArray;
procedure RadiusArray(aRadius,aX0,aY0,aZ0,aXAlpha: Single; AQuadPoints: Integer; var aPathArray: TPathArray);
Function  Average3D(AP1,AP2: TPoint3D): TPoint3D;
Function  MovePoint(v:TPoint3D; Ax0,Ay0,Az0:Single):TPoint3D;
Procedure MovePath(APath:TPathArray; Ax0,Ay0,Az0:Single);
function Polygon2Mesh(const AOwner: TComponent;
                      const Center, Size: TPoint3D; const Rect: TRectF;
                      const Points: TPolygon;
                      Front: Boolean = True; Back: Boolean = True; Left: Boolean = True): TMesh;
function GetListOfMeshDatas(const i_oModel: TModel3D; i_lstMeshDatas: TList<TMeshData>): Integer;

procedure GetStringsOfMeshDataPoint3DsForOBJ(i_oMeshData: TMeshData; i_arLines: TStringList);
procedure GetStringsOfMeshDataNormalsForOBJ(i_oMeshData: TMeshData; i_arLines: TStringList);
procedure GetStringsOfMeshDataTexCoordinatesForOBJ(i_oMeshData: TMeshData; i_arLines: TStringList);
procedure GetStringsOfMeshDataTriangleIndicesForOBJ(i_oMeshData: TMeshData; i_arLines: TStringList);

//------------------------------------------------------------------------------

implementation

//-------------------------------- TOwnMesh ------------------------------------

constructor TOwnMesh.Create(AOwner: TComponent);
begin
 inherited;
end;

destructor TOwnMesh.Destroy;
begin
 inherited;
end;

//-------------------------------- TOwnModel3D ---------------------------------

constructor TOwnModel3D.Create(AOwner: TComponent);
begin
 inherited;
end;

destructor TOwnModel3D.Destroy;
begin
 inherited;
end;

procedure TOwnModel3D.ReadModel(Stream: TStream);
begin
 inherited;
end;

procedure TOwnModel3D.WriteModel(Stream: TStream);
begin
 inherited;
end;

procedure TOwnModel3D.UpdateMeshCollection;
begin
 inherited;
end;

procedure TOwnModel3D.Resize3D;
begin
 inherited;
end;

//------------------------------------------------------------------------------

Function SqArray(ca,cb,cc,cd: TPoint3D):TSqArray;
begin
 Result[0]:= ca;
 Result[1]:= cb;
 Result[2]:= cc;
 Result[3]:= cd;
end;

Function QuadArray(aRadius,aXAlpha:Single; aQuadPoints: Integer): TPathArray;
Var
 DeltaX, DeltaY, RadiusSq, XsqYsq: Single;
 I: Integer;
begin
 if AQuadPoints < 1 then
   raise Exception.Create('QuadArray');
 SetLength(Result,AQuadPoints+1);
 //Result[0].X:= ARadius*Sin(AxAlpha+pi/2);
 //Result[0].Y:= ARadius*Cos(AxAlpha+pi/2);
 Result[0].X :=  ARadius * Sin(AxAlpha);
 Result[0].Y := -ARadius * Cos(AxAlpha);
 Result[0].Z := 0;
 Result[AQuadPoints].X := 0;
 Result[AQuadPoints].Y := 0;
 Result[AQuadPoints].Z := ARadius;
 if AQuadPoints < 2 then
   Exit;
 DeltaX:= Result[0].X / AQuadPoints;
 DeltaY:= Result[0].Y / AQuadPoints;
 RadiusSq := ARadius * ARadius;
 for I := 1 to AQuadPoints-1 do begin
   Result[AQuadPoints-i].X := {Sin(DeltaAlpha*i) ; //} DeltaX * I;
   Result[AQuadPoints-i].Y := {Cos(DeltaAlpha*i) ; //} DeltaY * I;
   XsqYsq := Result[AQuadPoints-i].Y * Result[AQuadPoints-i].Y+
             Result[AQuadPoints-i].X * Result[AQuadPoints-i].X;
   if XsqYsq < RadiusSq then
     Result[AQuadPoints-i].Z := Sqrt(RadiusSq-XsqYsq)
   else
     Result[AQuadPoints-i].Z :=0;
 end;
end;

Function NegateAll(v:TPoint3D):TPoint3D;
begin
 Result   := v;
 Result.X := -v.X;
 Result.Y := -v.Y;
 Result.Z := -v.Z;
end;

Function NegateXY(v:TPoint3D):TPoint3D;
begin
 Result   := v;
 Result.X := -v.X;
 Result.Y := -v.Y;
end;

Function NegateX(v:TPoint3D):TPoint3D;
begin
 Result   :=v;
 Result.X :=-v.X;
end;

Function NegateY(v:TPoint3D):TPoint3D;
begin
 Result   := v;
 Result.Y := -v.Y;
end;

Function NegateZ(v:TPoint3D):TPoint3D;
begin
 Result   := v;
 Result.Z := -v.Z;
end;

Function MovePoint(v:TPoint3D;Ax0,Ay0,Az0:Single): TPoint3D;
begin
 Result.Z := v.Z + Az0;
 Result.X := v.X + Ax0;
 Result.Y := v.Y + Ay0;
end;

Procedure  MovePath(APath:TPathArray; Ax0,Ay0,Az0: Single);
Var
 i: Integer;
begin
 for i := Low(APath) to High(APath) do
   APath[i] := MovePoint(APath[i],Ax0,Ay0,Az0);
end;


procedure RadiusArray(aRadius,aX0,aY0,aZ0,aXAlpha: Single; AQuadPoints: Integer; var aPathArray: TPathArray);
var
 QArray: TPathArray;
 i: Integer;
begin
 QArray := uMeshUtils.QuadArray(ARadius,AxAlpha,AQuadPoints);
 SetLength(aPathArray,AQuadPoints*4);
 aPathArray[0] := QArray[0];
 aPathArray[AQuadPoints*2] := uMeshUtils.NegateXY(QArray[0]);
 aPathArray[AQuadPoints]   := QArray[AQuadPoints];
 aPathArray[AQuadPoints*3] := uMeshUtils.NegateZ(QArray[AQuadPoints]);
 for i := 0 to AQuadPoints-2 do begin
   aPathArray[i+1]:= QArray[i+1];
   aPathArray[AQuadPoints*2-i-1] := uMeshUtils.NegateXY(QArray[i+1]);
   aPathArray[AQuadPoints*2+i+1] := uMeshUtils.NegateAll(QArray[i+1]);
   aPathArray[AQuadPoints*4-i-1] := uMeshUtils.NegateZ(QArray[i+1]);
 end;
 uMeshUtils.MovePath(aPathArray,Ax0,Ay0,Az0);
end;

Function Average3D(AP1,AP2:TPoint3D):TPoint3D;
begin
 Result.X := (AP1.X + AP2.X) / 2.0;
 Result.Y := (AP1.Y + AP2.Y) / 2.0;
 Result.Z := (AP1.Z + AP2.Z) / 2.0;
end;

// Analog zu TContextHelper.FillPolygon in der Unit FMX.Types3D
function Polygon2Mesh(const AOwner: TComponent;
                      const Center, Size: TPoint3D; const Rect: TRectF;
                      const Points: TPolygon;
                      Front: Boolean = True; Back: Boolean = True; Left: Boolean = True): TMesh;
const
 // to be used as descriptive indices
 X = 0;
 Y = 1;
 Z = 2;
 W = 3;
var
 VertexBuffer: TVertexBuffer;
 IndexBuffer: TIndexBuffer;
 MaxLimit, MinLimit: TPoint3D;
 CurrentPoint: TPointF;
 RelativePoint: TPoint3D;
 i, n: Integer;
 StartIndex: Integer;
 LeftLen, CurPos: Single;
 Index, VertexIndex1, VertexIndex2, VertexIndex3: Integer;
 PrevVertexIndex1, PrevVertexIndex2, PrevVertexIndex3: Integer;
 l_iIdxIndexBuffer, l_iIdxVertexBuffer, l_iBegIdxIndexBuffer, l_iBegLngVertexBuffer: Integer;
 FaceVector1, FaceVector2: TPoint3D;
 PrevFaceVector1, PrevFaceVector2: TPoint3D;
 Normal, PreviousNormal, CurrentNormal: TPoint3D;
 Vertex1, Vertex2, Vertex3: TPoint3D;
begin
 Result := Nil;
 if (Length(Points) = 0) or SameValue(Size.X, 0, TEpsilon.Vector) or SameValue(Size.Y, 0, TEpsilon.Vector) then
   Exit;

 MaxLimit := TPoint3D.Create($FFFF, $FFFF, 0);
 MinLimit := TPoint3D.Create(-$FFFF, -$FFFF, 0);
 LeftLen := 0;

 for I := 0 to High(Points) do begin
   if (Points[I].X >= $FFFF) and (Points[I].Y >= $FFFF) then
     Continue;

   CurrentPoint := Points[I];
   MaxLimit.X := Min(MaxLimit.X, CurrentPoint.X);
   MaxLimit.Y := Min(MaxLimit.Y, CurrentPoint.Y);
   MinLimit.X := Max(MinLimit.X, CurrentPoint.X);
   MinLimit.Y := Max(MinLimit.Y, CurrentPoint.Y);

   if Left and (I > 0) then
     if Points[I - 1].X >= $FFFF then begin
       if (I > 1) then
         LeftLen := LeftLen + TPointF.Create(X - Points[I - 2].X, Y - Points[I - 2].Y).Length;
     end
     else
       LeftLen := LeftLen + TPointF.Create(X - Points[I - 1].X, Y - Points[I - 1].Y).Length;
 end;

 if not IsRectEmpty(Rect) then begin
   MaxLimit.X := Min(MaxLimit.X, Rect.Left);
   MaxLimit.Y := Min(MaxLimit.Y, Rect.Top);
   MinLimit.X := Max(MinLimit.X, Rect.Right);
   MinLimit.Y := Max(MinLimit.Y, Rect.Bottom);
 end;

 if SameValue(MaxLimit.X, MinLimit.X, TEpsilon.Vector) then
   Exit;
 if SameValue(MaxLimit.Y, MinLimit.Y, TEpsilon.Vector) then
    Exit;

 Result := TMesh.Create(AOwner);     // <- TMesh anlegen!
 VertexBuffer := Result.Data.VertexBuffer;
 IndexBuffer  := Result.Data.IndexBuffer;
 l_iIdxVertexBuffer:= VertexBuffer.Length - 1;
 l_iIdxIndexBuffer := IndexBuffer.Length  - 1;

 // Front face
 if Front then begin
   VertexBuffer.Length := VertexBuffer.Length + Length(Points);
   // set vertices
   for I := 0 to High(Points) do begin
     Inc(l_iIdxVertexBuffer);
     if (Points[I].X >= $FFFF) and (Points[I].Y >= $FFFF) then begin
       VertexBuffer.Vertices[l_iIdxVertexBuffer] := TPoint3D.Zero;
       VertexBuffer.Normals[l_iIdxVertexBuffer]  := TPoint3D.Zero;
       VertexBuffer.TexCoord0[l_iIdxVertexBuffer]:= TPointF.Zero;
       Continue;
     end;
     RelativePoint := TPoint3D.Create((Points[I].X - MaxLimit.X) / Abs(MinLimit.X - MaxLimit.X),
        (Points[I].Y - MaxLimit.Y) / Abs(MinLimit.Y - MaxLimit.Y), 1);
     VertexBuffer.Vertices[l_iIdxVertexBuffer] := TPoint3D.Create(Center.X - (Size.X / 2) + (RelativePoint.X * Size.X),
        Center.Y - (Size.Y / 2) + (RelativePoint.Y * Size.Y), Center.Z - (Size.Z / 2) + (RelativePoint.Z * Size.Z));
     VertexBuffer.Normals[l_iIdxVertexBuffer] := TPoint3D.Create(0, 0, 1);
     VertexBuffer.TexCoord0[l_iIdxVertexBuffer] := TPointF.Create(0.02 + (X * 0.96), 0.02 + (Y * 0.96));
   end;
   // Set indices
   IndexBuffer.Length := IndexBuffer.Length + High(Points) * 3;
   StartIndex := 0;
   for I := 0 to High(Points) - 1 do begin
     if (Points[I].X >= $FFFF) and (Points[I].Y >= $FFFF) then begin
       StartIndex := I + 1;
       Continue;
     end;
     Inc(l_iIdxIndexBuffer);
     IndexBuffer[l_iIdxIndexBuffer + 0] := StartIndex;
     IndexBuffer[l_iIdxIndexBuffer + 1] := I + 1;
     IndexBuffer[l_iIdxIndexBuffer + 2] := I;
     Inc(l_iIdxIndexBuffer, 2);
   end;
   IndexBuffer.Length := l_iIdxIndexBuffer + 1;
 end;

 // Back Face
 if Back then begin
   VertexBuffer.Length := VertexBuffer.Length + Length(Points);
   // set vertices
   for I := 0 to High(Points) do begin
     Inc(l_iIdxVertexBuffer);
     if (Points[I].X >= $FFFF) and (Points[I].Y >= $FFFF) then begin
       VertexBuffer.Vertices[l_iIdxVertexBuffer] := TPoint3D.Zero;
       VertexBuffer.Normals[l_iIdxVertexBuffer] := TPoint3D.Zero;
       VertexBuffer.TexCoord0[l_iIdxVertexBuffer] := TPointF.Zero;
       Continue;
     end;
     RelativePoint := TPoint3D.Create((Points[I].X - MaxLimit.X) / Abs(MinLimit.X - MaxLimit.X),
        (Points[I].Y - MaxLimit.Y) / Abs(MinLimit.Y - MaxLimit.Y), 0);
     VertexBuffer.Vertices[l_iIdxVertexBuffer] := TPoint3D.Create(Center.X - (Size.X / 2) + (RelativePoint.X * Size.X),
        Center.Y - (Size.Y / 2) + (RelativePoint.Y * Size.Y), Center.Z - (Size.Z / 2) + (RelativePoint.Z * Size.Z));
     VertexBuffer.Normals[l_iIdxVertexBuffer] := TPoint3D.Create(0, 0, -1);
     VertexBuffer.TexCoord0[l_iIdxVertexBuffer] := TPointF.Create(0.02 + (X * 0.96), 0.02 + (Y * 0.96));
   end;

   // Set indices
   IndexBuffer.Length := IndexBuffer.Length + High(Points) * 3;
   StartIndex := 0;
   for I := 0 to High(Points) - 1 do begin
     if (Points[I].X >= $FFFF) and (Points[I].Y >= $FFFF) then begin
       StartIndex := I + 1;
       Continue;
     end;
     Inc(l_iIdxIndexBuffer);
     IndexBuffer[l_iIdxIndexBuffer + 0] := StartIndex;
     IndexBuffer[l_iIdxIndexBuffer + 1] := I + 1;
     IndexBuffer[l_iIdxIndexBuffer + 2] := I;
     Inc(l_iIdxIndexBuffer, 2);
   end;
   IndexBuffer.Length := l_iIdxIndexBuffer + 1;
 end;

 // sides
 if Left and (LeftLen > 0) then begin
   l_iBegLngVertexBuffer := l_iIdxVertexBuffer + 1;
   VertexBuffer.Length   := VertexBuffer.Length + Length(Points) * 2;
   // set vertices
   CurPos := 0;
   for I := 0 to High(Points) do begin
     Inc(l_iIdxVertexBuffer);
     if (Points[I].X >= $FFFF) and (Points[I].Y >= $FFFF) then begin
       VertexBuffer.Vertices[l_iIdxVertexBuffer] := TPoint3D.Zero;
       VertexBuffer.Normals[l_iIdxVertexBuffer] := TPoint3D.Zero;
       VertexBuffer.TexCoord0[l_iIdxVertexBuffer] := TPointF.Zero;
       VertexBuffer.Vertices[Length(Points) + l_iIdxVertexBuffer] := TPoint3D.Zero;
       VertexBuffer.Normals[Length(Points)  + l_iIdxVertexBuffer] := TPoint3D.Zero;
       VertexBuffer.TexCoord0[Length(Points)+ l_iIdxVertexBuffer] := TPointF.Zero;
       Continue;
     end;
     if (I > 0) then begin
       if Points[I - 1].X >= $FFFF then begin
         if (I > 1) then
           CurPos := CurPos + (Points[I] - Points[I - 2]).Length;
       end
       else
         CurPos := CurPos + (Points[I] - Points[I - 1]).Length;
     end;
     RelativePoint := TPoint3D.Create((Points[I].X - MaxLimit.X) / Abs(MinLimit.X - MaxLimit.X),
        ((Points[I].Y - MaxLimit.Y) / Abs(MinLimit.Y - MaxLimit.Y)), 0);
     VertexBuffer.Vertices[l_iIdxVertexBuffer] := TPoint3D.Create(Center.X - (Size.X / 2) + (RelativePoint.X * Size.X),
        Center.Y - (Size.Y / 2) + (RelativePoint.Y * Size.Y), Center.Z - (Size.Z / 2) + 1 * Size.Z);
     VertexBuffer.TexCoord0[l_iIdxVertexBuffer] := TPointF.Create(0, CurPos / LeftLen);
     VertexBuffer.Vertices[Length(Points) + l_iIdxVertexBuffer] := TPoint3D.Create(Center.X - (Size.X / 2) + (RelativePoint.X * Size.X),
        Center.Y - (Size.Y / 2) + (RelativePoint.Y * Size.Y), Center.Z - (Size.Z / 2) + 0 * Size.Z);
     VertexBuffer.TexCoord0[Length(Points) + l_iIdxVertexBuffer] := TPointF.Create(1, CurPos / LeftLen);
   end;
   // set indices
   n := 0;
   l_iBegIdxIndexBuffer := l_iIdxIndexBuffer + 1;
   IndexBuffer.Length := IndexBuffer.Length + High(Points) * 6;
   for I := 0 to High(Points) - 1 do begin
     if (Points[I].X >= $FFFF) and (Points[I].Y >= $FFFF) then
       Continue;
     if (Points[I + 1].X >= $FFFF) and (Points[I + 1].X >= $FFFF) then
       Continue;
     Inc(l_iIdxIndexBuffer);
     IndexBuffer[l_iIdxIndexBuffer + 0] := I;
     IndexBuffer[l_iIdxIndexBuffer + 2] := Length(Points) + I;
     IndexBuffer[l_iIdxIndexBuffer + 1] := Length(Points) + I + 1;
     IndexBuffer[l_iIdxIndexBuffer + 3] := Length(Points) + I + 1;
     IndexBuffer[l_iIdxIndexBuffer + 5] := I + 1;
     IndexBuffer[l_iIdxIndexBuffer + 4] := I;
     Inc(l_iIdxIndexBuffer,5);
     Inc(n);
   end;
   IndexBuffer.Length := l_iIdxIndexBuffer + 1;
   // Calculate face normals
   for I := 0 to (n div 6) - 1 do begin
      Index := I * 6;

      VertexIndex1 := IndexBuffer[l_iBegIdxIndexBuffer + Index];
      VertexIndex2 := IndexBuffer[l_iBegIdxIndexBuffer + Index + 2];
      VertexIndex3 := IndexBuffer[l_iBegIdxIndexBuffer + Index + 1];

      PrevVertexIndex1 := IndexBuffer[l_iBegIdxIndexBuffer + (Index + n - 6) mod n];
      PrevVertexIndex2 := IndexBuffer[l_iBegIdxIndexBuffer + (Index + 2 + n - 6) mod n];
      PrevVertexIndex3 := IndexBuffer[l_iBegIdxIndexBuffer + (Index + 1 + n - 6) mod n];

      Vertex1 := VertexBuffer.Vertices[VertexIndex1];
      Vertex2 := VertexBuffer.Vertices[VertexIndex2];
      Vertex3 := VertexBuffer.Vertices[VertexIndex3];
      FaceVector1 := Vertex3 - Vertex1;
      FaceVector2 := Vertex3 - Vertex2;

      Vertex1 := VertexBuffer.Vertices[PrevVertexIndex1];
      Vertex2 := VertexBuffer.Vertices[PrevVertexIndex2];
      Vertex3 := VertexBuffer.Vertices[PrevVertexIndex3];
      PrevFaceVector1 := Vertex3 - Vertex1;
      PrevFaceVector2 := Vertex3 - Vertex2;

      PreviousNormal := PrevFaceVector1.CrossProduct(PrevFaceVector2).Normalize;
      CurrentNormal := FaceVector1.CrossProduct(FaceVector2).Normalize;

      Normal := (PreviousNormal + CurrentNormal).Normalize;

      VertexBuffer.Normals[VertexIndex1] := Normal;
      VertexBuffer.Normals[VertexIndex2] := Normal;
   end;
   n := VertexBuffer.Length - l_iBegLngVertexBuffer + 1;
   VertexBuffer.Normals[l_iBegLngVertexBuffer + (n div 2) - 1] := VertexBuffer.Normals[0];
   VertexBuffer.Normals[l_iBegLngVertexBuffer + n - 1] := VertexBuffer.Normals[l_iBegLngVertexBuffer + (n div 2)];
 end;
end;

function GetListOfMeshDatas(const i_oModel: TModel3D; i_lstMeshDatas: TList<TMeshData>): Integer;
var
 i, n: Integer;
begin
 n := 0;
 for i := 0 to i_oModel.ChildrenCount - 1 do begin
   if i_oModel.Children[i] is TMesh then begin
     i_lstMeshDatas.Add(TMesh(i_oModel.Children[i]).Data);
     Inc(n);
   end
   else if i_oModel.Children[i] is TOwnMesh then begin
     i_lstMeshDatas.Add(TOwnMesh(i_oModel.Children[i]).Data);
     Inc(n);
   end
 end;
 if Assigned(i_oModel.MeshCollection) then begin
   for i := 0 to High(i_oModel.MeshCollection) do begin
     if Assigned(i_oModel.MeshCollection[i]) then begin
       i_lstMeshDatas.Add(i_oModel.MeshCollection[i].Data);
       Inc(n);
     end;
   end;
 end;
 Result := n;
end;

procedure GetStringsOfMeshDataPoint3DsForOBJ(i_oMeshData: TMeshData; i_arLines: TStringList);
var
 i: Integer;
 l_oVertexBuffer: TVertexBuffer;
 l_sLine: String;
begin
 l_oVertexBuffer := i_oMeshData.VertexBuffer;
 i_arLines.Add('# - Count Vertices: ' + IntToStr(l_oVertexBuffer.Length));
 for i := 0 to (l_oVertexBuffer.Length - 1) do begin
   l_sLine := 'v ' + FloatToStr(l_oVertexBuffer.Vertices[i].x, USFormatSettings) +
              ' '  + FloatToStr(l_oVertexBuffer.Vertices[i].y, USFormatSettings) +
              ' '  + FloatToStr(l_oVertexBuffer.Vertices[i].z, USFormatSettings);
   i_arLines.Add(l_sLine);
 end;
end;

procedure GetStringsOfMeshDataNormalsForOBJ(i_oMeshData: TMeshData; i_arLines: TStringList);
var
 i: Integer;
 l_oVertexBuffer: TVertexBuffer;
 l_sLine: String;
begin
 l_oVertexBuffer := i_oMeshData.VertexBuffer;
 i_arLines.Add('# - Count Normals: ' + IntToStr(l_oVertexBuffer.Length));
 for i := 0 to (l_oVertexBuffer.Length - 1) do begin
   l_sLine := 'vn ' + FloatToStr(l_oVertexBuffer.Normals[i].x, USFormatSettings) +
              ' '   + FloatToStr(l_oVertexBuffer.Normals[i].y, USFormatSettings) +
              ' '   + FloatToStr(l_oVertexBuffer.Normals[i].z, USFormatSettings);
   i_arLines.Add(l_sLine);
 end;
end;

procedure GetStringsOfMeshDataTexCoordinatesForOBJ(i_oMeshData: TMeshData; i_arLines: TStringList);
var
 i: Integer;
 l_oVertexBuffer: TVertexBuffer;
 l_sLine: String;
begin
 l_oVertexBuffer := i_oMeshData.VertexBuffer;
 i_arLines.Add('# - Count TexCoord0: ' + IntToStr(l_oVertexBuffer.Length));
 for i := 0 to (l_oVertexBuffer.Length - 1) do begin
   l_sLine := 'vt ' + FloatToStr(l_oVertexBuffer.TexCoord0[i].x, USFormatSettings) +
              ' '   + FloatToStr(l_oVertexBuffer.TexCoord0[i].y, USFormatSettings);
   i_arLines.Add(l_sLine);
 end;
end;

procedure GetStringsOfMeshDataTriangleIndicesForOBJ(i_oMeshData: TMeshData; i_arLines: TStringList);
var
 i: Integer;
 l_oIndexBuffer:  TIndexBuffer;
 l_sStr, l_sLine: String;
begin
 l_oIndexBuffer := i_oMeshData.IndexBuffer;
 i_arLines.Add('# - Count IndexBuffer: ' + IntToStr(l_oIndexBuffer.Length));
 l_sLine := 'f ';
 for i := 0 to (l_oIndexBuffer.Length - 1) do begin
   l_sStr  := IntToStr(l_oIndexBuffer[i]+1);
   l_sLine := l_sLine + l_sStr + '/' + l_sStr + '/' + l_sStr;
   if (i + 1) mod 3 = 0 then begin
     i_arLines.Add(l_sLine);
     l_sLine := 'f ';
   end
   else
     l_sLine := l_sLine + ' ';
 end;
 if Length(l_sLine) > 2 then
   i_arLines.Add(l_sLine);
end;

end.
