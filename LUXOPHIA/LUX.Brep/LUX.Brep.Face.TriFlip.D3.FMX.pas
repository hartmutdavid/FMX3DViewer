unit LUX.Brep.Face.TriFlip.D3.FMX;

interface

uses System.Types, System.Classes, System.Math.Vectors,
     FMX.Types3D, FMX.Controls3D, FMX.MaterialSources,
     LUX, LUX.D3, LUX.Brep.Face.TriFlip.D3, LUX.Data.Tree,
     LUX.Brep.Poin, LUX.Brep.Face.TriFlip;

type

TTriFaceShape3D = class( TControl3D )
 private
   procedure MakeGeometry;
 protected
   _Geometry : TMeshData;
   _Material : TMaterialSource;
   _Model    : TTriFaceModel3D;
   procedure SetModel( const Model_: TTriFaceModel3D );
   procedure Render; override;
 public
   constructor Create( Owner_:TComponent ); override;
   destructor Destroy; override;
   //
   property Material : TMaterialSource read _Material write _Material;
   property Model    : TTriFaceModel3D read _Model    write SetModel   ;
   property MeshData : TMeshData read _Geometry       write _Geometry;
 end;

procedure TriFaceMakeGeometry(aTriFlipModel: TTriFaceModel3D; var aMeshData: TMeshData);

implementation

uses System.SysUtils, System.RTLConsts;

//------------------------------------------------------------------------------
//--------------------------- TTriFaceShape3D ----------------------------------
//------------------------------------------------------------------------------

procedure TriFaceMakeGeometry(aTriFlipModel: TTriFaceModel3D; var aMeshData: TMeshData);
var
 i, n :Integer;
 l_oTriFace: TTriFace3D;
 l_oTriPoint: TTriPoin3D;
begin
 Assert(Assigned(aTriFlipModel));
 Assert(Assigned(aMeshData));
 with aMeshData do begin
   with VertexBuffer do begin
     Length := aTriFlipModel.PoinModel.ChildsN;
     for i := 0 to aTriFlipModel.PoinModel.ChildsN-1 do begin
       l_oTriPoint := aTriFlipModel.PoinModel.Childs[i];
       Vertices [i] := l_oTriPoint.Pos;
       Normals  [i] := l_oTriPoint.Nor;
       TexCoord0[i] := TPointF.Create( l_oTriPoint.Tex.X, l_oTriPoint.Tex.Y );
     end;
   end;
   with IndexBuffer do begin
     Length := 3 {Poin} * aTriFlipModel.ChildsN;
     i := 0;
     for n := 0 to aTriFlipModel.ChildsN-1 do begin
       l_oTriFace := aTriFlipModel.Childs[n];
       Indices[i] := l_oTriFace.Poin[3].Order;
       Inc(i);
       Indices[i] := l_oTriFace.Poin[2].Order;
       Inc(i);
       Indices[i] := l_oTriFace.Poin[1].Order;
       Inc(i);
     end;
   end;
 end;
end;

procedure TTriFaceShape3D.MakeGeometry;
begin
 TriFaceMakeGeometry(_Model, _Geometry);
end;

procedure TTriFaceShape3D.SetModel( const Model_:TTriFaceModel3D );
begin
 _Model := Model_;  MakeGeometry;
end;

procedure TTriFaceShape3D.Render;
begin
 Context.SetMatrix( AbsoluteMatrix );
 _Geometry.Render( Context, TMaterialSource.ValidMaterial(_Material), AbsoluteOpacity );
end;

constructor TTriFaceShape3D.Create( Owner_:TComponent );
begin
 inherited;
 _Geometry := TMeshData.Create;
end;

destructor TTriFaceShape3D.Destroy;
begin
 if Assigned(_Geometry) then
   _Geometry.Free;
 inherited;
end;

end.
