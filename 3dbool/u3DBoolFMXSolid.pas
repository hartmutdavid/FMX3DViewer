unit u3DBoolFMXSolid;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.Math, System.Generics.Collections,
  FMX.Types, FMX.Graphics, FMX.Types3D,
  System.Math.Vectors, FMX.Objects3D, FMX.Controls3D,
  uMeshUtils, u3DBoolSolid, u3DBoolMisc;

type
  T3DBoolFMXSolid = class(T3DBoolSolid)
  public
    constructor Create(i_oMeshObj: TOwnMesh;
                       i_oColor :  TColor); overload;
    constructor Create(i_lstMeshDatas: TList<TMeshData>;
                       i_oColor :  TColor); overload;
    destructor  Destroy; override;
  end;

implementation

constructor T3DBoolFMXSolid.Create(i_oMeshObj: TOwnMesh;
                   i_oColor :  TColor);
var
 i, j, l_iVertexCnt, l_iIndicesCnt: Integer;
 l_oPoint3D: TPoint3D;
 l_o3DBoolPoint3d: T3DBoolPoint3d;
 l_arVertices: TList<T3DBoolPoint3d>;
 l_arIndices:  TList<Cardinal>;
 l_oMeshData: TMeshData;
begin
 inherited Create;
 self.setInitialFeatures();
 //
 l_oMeshData := i_oMeshObj.Data;
 with l_oMeshData do begin
   l_iVertexCnt := VertexBuffer.Length;
   for j := 0 to l_iVertexCnt - 1 do begin
     l_oPoint3D := VertexBuffer.Vertices[j];
     l_o3DBoolPoint3d := T3DBoolPoint3d.Create;
     l_o3DBoolPoint3d.x := l_oPoint3D.x;
     l_o3DBoolPoint3d.y := l_oPoint3D.y;
     l_o3DBoolPoint3d.z := l_oPoint3D.z;
     self.vertices.Add(l_o3DBoolPoint3d);
     self.colors.Add(i_oColor);
   end;
   l_iIndicesCnt := IndexBuffer.Length;
   for j := 0 to l_iIndicesCnt - 1 do begin
     self.indices.Add(IndexBuffer[j]);
   end;
 end;
 self.defineGeometry();
end;

constructor T3DBoolFMXSolid.Create(i_lstMeshDatas: TList<TMeshData>;
                                   i_oColor :  TColor);
var
 i, j, l_iVertexCnt, l_iIndicesCnt, l_iOffset: Integer;
 l_oPoint3D: TPoint3D;
 l_o3DBoolPoint3d: T3DBoolPoint3d;
 l_arVertices: TList<T3DBoolPoint3d>;
 l_arIndices:  TList<Cardinal>;
 l_oMeshData: TMeshData;
begin
 inherited Create;
 self.setInitialFeatures();
 //
 l_iOffset := 0;
 for i := 0 to i_lstMeshDatas.Count-1 do begin
   l_oMeshData := i_lstMeshDatas[i];
   with l_oMeshData do begin
     l_iVertexCnt := VertexBuffer.Length;
     for j := 0 to l_iVertexCnt - 1 do begin
       l_oPoint3D := VertexBuffer.Vertices[j];
       l_o3DBoolPoint3d := T3DBoolPoint3d.Create;
       l_o3DBoolPoint3d.x := l_oPoint3D.x;
       l_o3DBoolPoint3d.y := l_oPoint3D.y;
       l_o3DBoolPoint3d.z := l_oPoint3D.z;
       self.vertices.Add(l_o3DBoolPoint3d);
       self.colors.Add(i_oColor);
     end;
     l_iIndicesCnt := IndexBuffer.Length;
     for j := 0 to l_iIndicesCnt - 1 do begin
       self.indices.Add(IndexBuffer[j]+l_iOffset);
     end;
   end;
   l_iOffset := l_iOffset + l_iVertexCnt;
 end;
 self.defineGeometry();
end;

destructor  T3DBoolFMXSolid.Destroy;
begin
  inherited Destroy;
end;

end.
