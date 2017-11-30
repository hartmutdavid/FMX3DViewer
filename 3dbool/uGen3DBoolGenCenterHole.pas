unit uGen3DBoolGenCenterHole;

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

function GetActMeshModelWithCenterHole(i_lstMeshData: TList<TMeshData>): TOwnMesh;

implementation

uses u3DBoolMisc, u3DBoolVertex, u3DBoolSolid, u3DBoolModeller, uDefaultCoordinates,
     u3DBoolFMXSolid, u3DBoolBound;

function getColorArray(cnt: Integer; color: TColor): TList<TColor>;
var
 i: Integer;
begin
 Result := TList<TColor>.Create;
 for i:= 0 to cnt-1 do
   Result[i] := color;
end;

function GetActMeshModelWithCenterHole(i_lstMeshData: TList<TMeshData>): TOwnMesh;
var
 i: Integer;
 l_oMesh: TOwnMesh;
 cylinder: T3DBoolSolid;
 l_oCenterModel3D, l_oCenterCylinder: TPoint3D;
 tmp1: T3DBoolSolid;
 modeller: T3DBoolModeller;
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
 l_oActModel: T3DBoolFMXSolid;
 l_oActModelBound, l_oHoleBound: T3DBoolBound;
begin
 l_oMesh := TOwnMesh.Create(Nil);
 //
 l_oActModel      := T3DBoolFMXSolid.Create(i_lstMeshData,TColors.Red);
 l_oActModelBound := T3DBoolBound.Create(l_oActModel.getVertices());
 l_oCenterModel3D.X := -(l_oActModelBound.xMin + l_oActModelBound.xMax) * 0.5;
 l_oCenterModel3D.Y := -(l_oActModelBound.yMin + l_oActModelBound.yMax) * 0.5;
 l_oCenterModel3D.Z := -(l_oActModelBound.zMin + l_oActModelBound.zMax) * 0.5;
 l_oActModel.translate(l_oCenterModel3D.X, l_oCenterModel3D.Y, l_oCenterModel3D.Z);    // <- mittig auf 0,0,0

 cylinder := T3DBoolSolid.Create(g_lstDefaultCylinderVertices, g_lstDefaultCylinderCoordinates, TColors.Green);
 cylinder.scale(10.0, 100.0, 10.0);
 //-- cylinder.translate(l_oActModelBound.xMin, l_oActModelBound.yMin, l_oActModelBound.zMin);
 cylinder.rotate(0.0,PI/2.0);     // <- Loch von vorn
 l_oHoleBound := T3DBoolBound.Create(cylinder.getVertices());
 l_oCenterCylinder.X := -(l_oHoleBound.xMin + l_oHoleBound.xMax) * 0.5;
 l_oCenterCylinder.Y := -(l_oHoleBound.yMin + l_oHoleBound.yMax) * 0.5;
 l_oCenterCylinder.Z := -(l_oHoleBound.zMin + l_oHoleBound.zMax) * 0.5;
 l_oActModel.translate(l_oCenterCylinder.x, l_oCenterCylinder.y, l_oCenterCylinder.z);

{ Für die Blechschale ...
l_oActModelBound
	xMax	77,4700012207031
	xMin	-77,4700012207031
	yMax	0
	yMin	-50,7999992370605
	zMax	71,1199951171875
	zMin	-71,1199951171875
l_oCenterModel3D
	X	0
	Y	25,3999996185303
	Z	0
l_oHoleBound
	xMax	9,84799957275391
	xMin	-9,84799957275391
	yMax	20
	yMin	-20
	zMax	10
	zMin	-10
l_oCenterCylinder
	V	(0, 0, 0)
	X	0
	Y	0
	Z	0
} 
 
 modeller := T3DBoolModeller.Create(l_oActModel, cylinder);
 tmp1 := modeller.getUnion();

 l_o3DBoolVertices := tmp1.getVertices;
 l_o3DBoolIndexes  := tmp1.getIndices();
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
 Result := l_oMesh;
end;

end.
