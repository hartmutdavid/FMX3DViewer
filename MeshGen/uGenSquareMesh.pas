unit uGenSquareMesh;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.Math, System.Generics.Collections,
  FMX.Types, FMX.Graphics, FMX.Types3D,
  System.Math.Vectors, FMX.Objects3D, FMX.Controls3D, uMeshUtils
{$ifdef CODESITE}
  , CodeSiteLogging
{$endif}
  ;

function GetSquareMesh(): TOwnMesh;

implementation

function GetSquareMesh(): TOwnMesh;
const
 c_iSquareCnt = 6;
 c_fZDist     = 0.4;
var
 i, idxVertexBuffer, idxIndexBuffer: Integer;
 l_fZDiff: Single;
 l_oMesh: TOwnMesh;
begin
 l_oMesh := TOwnMesh.Create(Nil);
 //
 l_oMesh.Data.VertexBuffer.Length := 4 * c_iSquareCnt;
 l_oMesh.Data.IndexBuffer.Length  := 6 * c_iSquareCnt;
 //
 // Rechteck
 //   0----------1
 //    |        |
 //    |        |
 //   3----------2
 l_fZDiff := 0.0;
 idxVertexBuffer := 0;
 idxIndexBuffer  := 0;
 for i := 0 to c_iSquareCnt-1 do begin
   l_oMesh.Data.VertexBuffer.Vertices[idxVertexBuffer + 0] := Point3D(-1.0, 1.0, l_fZDiff);
   l_oMesh.Data.VertexBuffer.Vertices[idxVertexBuffer + 1] := Point3D( 1.0, 1.0, l_fZDiff);
   l_oMesh.Data.VertexBuffer.Vertices[idxVertexBuffer + 2] := Point3D( 1.0,-1.0, l_fZDiff);
   l_oMesh.Data.VertexBuffer.Vertices[idxVertexBuffer + 3] := Point3D(-1.0,-1.0, l_fZDiff);
   //
   l_oMesh.Data.IndexBuffer[idxIndexBuffer + 0] := idxVertexBuffer + 0;
   l_oMesh.Data.IndexBuffer[idxIndexBuffer + 1] := idxVertexBuffer + 1;
   l_oMesh.Data.IndexBuffer[idxIndexBuffer + 2] := idxVertexBuffer + 2;
   l_oMesh.Data.IndexBuffer[idxIndexBuffer + 3] := idxVertexBuffer + 2;
   l_oMesh.Data.IndexBuffer[idxIndexBuffer + 4] := idxVertexBuffer + 3;
   l_oMesh.Data.IndexBuffer[idxIndexBuffer + 5] := idxVertexBuffer + 0;
   //
   Inc(idxVertexBuffer,4);
   Inc(idxIndexBuffer,6);
   l_fZDiff := l_fZDiff + c_fZDist;
 end;
 //
 l_oMesh.Data.CalcFaceNormals;
 Result := l_oMesh;
end;

end.
