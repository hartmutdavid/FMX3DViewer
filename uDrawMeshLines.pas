unit uDrawMeshLines;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections, System.Types, System.UITypes, System.UIConsts,
  System.Messaging, System.Math.Vectors, FMX.Types, FMX.Graphics, FMX.Types3D;

type
  TContextHelperOwn = class helper for TContext3D
  public
    { helper }
    procedure DrawMeshLines(const aMeshData: TMeshData;
                  const Center, Size: TPoint3D;
                  const Opacity: Single;
                  const Color: TAlphaColor);
  end;

implementation

uses FMX.Materials;

procedure TContextHelperOwn.DrawMeshLines(const aMeshData: TMeshData;
                  const Center, Size: TPoint3D; const Opacity: Single;
                  const Color: TAlphaColor);
var
 Mat: TColorMaterial;
begin
 Mat := TColorMaterial.Create;
 Mat.Color := Color;
 DrawLines(aMeshData.VertexBuffer, aMeshData.IndexBuffer, Mat, Opacity);
end;

end.
