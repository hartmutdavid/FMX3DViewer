unit uGen3DBoolTest1;

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

function ExecuteGen3DBoolTest1(io_oModel3DForMesh: TModel3D): String;

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

function ExecuteGen3DBoolTest1(io_oModel3DForMesh: TModel3D): String;
var
 i: Integer;
 l_oMesh: TMesh;
 box, sphere, cylinder1, cylinder2, cylinder3: T3DBoolSolid;
 tmp1, tmp2, tmp3, tmp4: T3DBoolSolid;
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
 cylinder1.scale(0.38, 1, 0.38);

 cylinder2 := T3DBoolSolid.Create(g_lstDefaultCylinderVertices, g_lstDefaultCylinderCoordinates, TColors.Green);
 cylinder2.scale(0.38, 1, 0.38);
 cylinder2.rotate(PI / 2, 0);

 cylinder3 := T3DBoolSolid.Create(g_lstDefaultCylinderVertices, g_lstDefaultCylinderCoordinates, TColors.Green);
 cylinder3.scale(0.38, 1, 0.38);
 cylinder3.rotate(PI / 2, 0);
 cylinder3.rotate(0, PI / 2);

 modeller1 := T3DBoolModeller.Create(box, sphere);
 tmp1 := modeller1.getIntersection();

{$ifdef CODESITE}
 l_o3DBoolVertices := tmp1.getVertices;
 l_o3DBoolIndexes  := tmp1.getIndices();
 CodeSite.Send('tmp1 := modeller1.getIntersection() - Vertices');
 CodeSite.Send(' Vertices.Count=' + IntToStr(l_o3DBoolVertices.Count));
 for i := 0 to l_o3DBoolVertices.Count-1 do begin
   l_o3DBoolPoint3d := l_o3DBoolVertices[i];
   l_sStr := Format(' %4d: (%8.2f,%8.2f,%8.2f)',
                     [i,l_o3DBoolPoint3d.X,l_o3DBoolPoint3d.Y,l_o3DBoolPoint3d.Z]);
   CodeSite.Send(l_sStr);
   if i > 40 then
     break;
 end;  }
 CodeSite.Send('tmp1 := modeller1.getIntersection() - Indices');
 CodeSite.Send(' Indexes.Count=' + IntToStr(l_o3DBoolIndexes.Count));
 for i := 0 to l_o3DBoolIndexes.Count-1 do begin
   idx1 := l_o3DBoolIndexes[i];
   l_sStr := Format(' %4d: %4d',
                     [i,idx1]);
   CodeSite.Send(l_sStr);
   if i > 40 then
     break;
 end;
{$endif}

 modeller2 := T3DBoolModeller.Create(tmp1, cylinder1);
 tmp2 := modeller2.getDifference();

{$ifdef CODESITE}
 l_o3DBoolVertices := tmp2.getVertices;
 l_o3DBoolIndexes  := tmp2.getIndices();
 CodeSite.Send('tmp2 := modeller2.getDifference() - Vertices');
 CodeSite.Send(' Vertices.Count=' + IntToStr(l_o3DBoolVertices.Count));
 for i := 0 to l_o3DBoolVertices.Count-1 do begin
   l_o3DBoolPoint3d := l_o3DBoolVertices[i];
   l_sStr := Format(' %4d: (%8.2f,%8.2f,%8.2f)',
                     [i,l_o3DBoolPoint3d.X,l_o3DBoolPoint3d.Y,l_o3DBoolPoint3d.Z]);
   CodeSite.Send(l_sStr);
   if i > 40 then
     break;
 end;
 CodeSite.Send('tmp2 := modeller2.getDifference() - Indices');
 CodeSite.Send(' Indexes.Count=' + IntToStr(l_o3DBoolIndexes.Count));
 for i := 0 to l_o3DBoolIndexes.Count-1 do begin
   idx1 := l_o3DBoolIndexes[i];
   l_sStr := Format(' %4d: %4d',
                     [i,idx1]);
   CodeSite.Send(l_sStr);
   if i > 40 then
     break;
 end;
{$endif}

 modeller3 := T3DBoolModeller.Create(tmp2, cylinder2);
 tmp3 := modeller3.getDifference();

{$ifdef CODESITE}
 l_o3DBoolVertices := tmp3.getVertices;
 l_o3DBoolIndexes  := tmp3.getIndices();
 CodeSite.Send('tmp3 := modeller3.getDifference() - Vertices');
 CodeSite.Send(' Vertices.Count=' + IntToStr(l_o3DBoolVertices.Count));
 for i := 0 to l_o3DBoolVertices.Count-1 do begin
   l_o3DBoolPoint3d := l_o3DBoolVertices[i];
   l_sStr := Format(' %4d: (%8.2f,%8.2f,%8.2f)',
                     [i,l_o3DBoolPoint3d.X,l_o3DBoolPoint3d.Y,l_o3DBoolPoint3d.Z]);
   CodeSite.Send(l_sStr);
   if i > 40 then
     break;
 end;
 CodeSite.Send('tmp3 := modeller3.getDifference() - Indices');
 CodeSite.Send(' Indexes.Count=' + IntToStr(l_o3DBoolIndexes.Count));
 for i := 0 to l_o3DBoolIndexes.Count-1 do begin
   idx1 := l_o3DBoolIndexes[i];
   l_sStr := Format(' %4d: %4d',
                     [i,idx1]);
   CodeSite.Send(l_sStr);
   if i > 40 then
     break;
 end;
{$endif}

 modeller4 := T3DBoolModeller.Create(tmp3, cylinder2);
 tmp4 := modeller4.getDifference();

 {$ifdef CODESITE}
 l_o3DBoolVertices := tmp4.getVertices;
 l_o3DBoolIndexes  := tmp4.getIndices();
 CodeSite.Send('tmp4 := modeller4.getDifference() - Vertices');
 CodeSite.Send(' Vertices.Count=' + IntToStr(l_o3DBoolVertices.Count));
 for i := 0 to l_o3DBoolVertices.Count-1 do begin
   l_o3DBoolPoint3d := l_o3DBoolVertices[i];
   l_sStr := Format(' %4d: (%8.2f,%8.2f,%8.2f)',
                     [i,l_o3DBoolPoint3d.X,l_o3DBoolPoint3d.Y,l_o3DBoolPoint3d.Z]);
   CodeSite.Send(l_sStr);
   if i > 40 then
     break;
 end;
 CodeSite.Send('tmp4 := modeller4.getDifference() - Indices');
 CodeSite.Send(' Indexes.Count=' + IntToStr(l_o3DBoolIndexes.Count));
 for i := 0 to l_o3DBoolIndexes.Count-1 do begin
   idx1 := l_o3DBoolIndexes[i];
   l_sStr := Format(' %4d: %4d',
                     [i,idx1]);
   CodeSite.Send(l_sStr);
   if i > 40 then
     break;
 end;
{$endif}
 l_o3DBoolVertices := tmp4.getVertices;
 l_o3DBoolIndexes  := tmp4.getIndices();
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
 Result := '3DBoolTest1';
end;

end.
