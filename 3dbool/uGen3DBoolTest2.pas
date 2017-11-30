unit uGen3DBoolTest2;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.Math, System.Generics.Collections,
  FMX.Types, FMX.Graphics, FMX.Types3D,
  System.Math.Vectors, FMX.Objects3D, FMX.Controls3D
{$ifdef CODESITE}
  , CodeSiteLogging
{$endif}
  ;

function ExecuteGen3DBoolTest2(io_oModel3DForMesh: TModel3D): String;

implementation

uses u3DBoolMisc, u3DBoolVertex, u3DBoolSolid, u3DBoolModeller, uDefaultCoordinates;

function getColorArray(cnt: Integer; color: TColor): TList<TColor>;
var
 i: Integer;
begin
 Result := TList<TColor>.Create;
 for i:= 0 to cnt-1 do
   Result[i] := color;
end;

function ExecuteGen3DBoolTest2(io_oModel3DForMesh: TModel3D): String;
var
 i: Integer;
 l_oMesh: TMesh;
 box, sphere, cylinder1: T3DBoolSolid;
 tmp1, tmp2: T3DBoolSolid;
 modeller1, modeller2, modeller3, modeller4: T3DBoolModeller;
 DEFAULT_BOX_VERTICES:    TList<T3DBoolPoint3d>;
 DEFAULT_BOX_COORDINATES: TList<Cardinal>;
 l_o3DBoolVertices: TList<T3DBoolPoint3d>;
 l_o3DBoolIndexes:  TList<Cardinal>;
 l_oVertexBuffer: TVertexBuffer;
 l_oIndexBuffer:  TIndexBuffer;
{$ifdef CODESITE}
 n, idx1, idx2, idx3: Integer;
 l_sStr: String;
 l_o3DBoolPoint3d: T3DBoolPoint3d;
{$endif}
begin
 io_oModel3DForMesh.Clear;
 l_oMesh := TMesh.Create(io_oModel3DForMesh);
 //
 box    := T3DBoolSolid.Create(g_lstDefaultBoxVertices,g_lstDefaultBoxCoordinates,TColors.Red);
 sphere := T3DBoolSolid.Create(g_lstDefaultSphereVertices, g_lstDefaultSphereCoordinates, TColors.Blue);
 sphere.scale(0.68, 0.68, 0.68);

 cylinder1 := T3DBoolSolid.Create(g_lstDefaultCylinderVertices, g_lstDefaultCylinderCoordinates, TColors.Green);
 cylinder1.scale(0.38, 1.0, 0.38);
 //-- cylinder1.rotate(PI/2.0, 0.0);    // <- Steht senkrecht
 //-- cylinder1.rotate(0.0,PI/2.0);     // <- Loch von vorn
 //-- cylinder1.rotate(0.0,0.0,PI/2.0);      <- Loch von links n. rechts

 modeller1 := T3DBoolModeller.Create(box, sphere);
 tmp1 := modeller1.getIntersection();

 modeller2 := T3DBoolModeller.Create(tmp1, cylinder1);
 tmp2 := modeller2.getDifference();

 l_o3DBoolVertices := tmp2.getVertices;
 l_o3DBoolIndexes  := tmp2.getIndices();
 l_oVertexBuffer   := l_oMesh.Data.VertexBuffer;
 l_oVertexBuffer.Length := l_o3DBoolVertices.Count;
 l_oIndexBuffer    := l_oMesh.Data.IndexBuffer;
 l_oIndexBuffer.Length  := l_o3DBoolIndexes.Count;
 for i := 0 to l_o3DBoolVertices.Count-1 do begin
   l_oVertexBuffer.Vertices[i] := Point3D(l_o3DBoolVertices[i].x,l_o3DBoolVertices[i].y,l_o3DBoolVertices[i].z);
 end;
 for i := 0 to l_o3DBoolIndexes.Count-1 do begin
   l_oIndexBuffer[i] := l_o3DBoolIndexes[i];
 end;
 l_oMesh.Data.CalcFaceNormals;
 io_oModel3DForMesh.AddObject(l_oMesh);
 io_oModel3DForMesh.Scale.X := 6;
 io_oModel3DForMesh.Scale.Y := 6;
 io_oModel3DForMesh.Scale.Z := 6;
 io_oModel3DForMesh.Repaint;
 Result := '3DBoolTest2';
end;

end.
