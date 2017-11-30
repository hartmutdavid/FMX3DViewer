unit u3DBoolSolid;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.Math, System.Generics.Collections,
  FMX.Types, FMX.Graphics, FMX.Types3D,
  System.Math.Vectors, FMX.Objects3D, FMX.Controls3D,
  u3DBoolMisc;

type
  T3DBoolSolid = class(T3DBoolShape3D)
  protected
    indices  : TList<Cardinal>;
    vertices : TList<T3DBoolPoint3d>;
    colors   : TList<TColor>;
    //
    procedure setInitialFeatures();
    procedure defineGeometry();
    function  getMean(): T3DBoolPoint3d;
  public
    constructor Create(); overload;
    constructor Create(i_lstVertices: TList<T3DBoolPoint3d>;
                       i_lstIndices:  TList<Cardinal>;
                       i_lstColors:   TList<TColor>); overload;
    constructor Create(i_lstVertices: TList<T3DBoolPoint3d>;
                       i_lstIndices:  TList<Cardinal>;
                       i_oColor :     TColor); overload;
    destructor  Destroy; override;
    //
    function  getVertices(): TList<T3DBoolPoint3d>;
    function  getIndices() : TList<Cardinal>;
    function  getColors()  : TList<TColor>;
    function  isEmpty(): boolean;
    procedure setData(i_lstVertices : TList<T3DBoolPoint3d>; i_lstIndices : TList<Cardinal>; i_lstColors : TList<TColor>); overload;
    procedure setData(i_lstVertices : TList<T3DBoolPoint3d>; i_lstIndices : TList<Cardinal>; i_oColor : TColor);  overload;
    procedure translate(dx, dy: Real3D; dz: Real3D=0.0);
    procedure rotate(dx, dy: Real3D; dz: Real3D=0.0);
    procedure zoom(dz : Real3D);
    procedure scale(dx, dy, dz : Real3D);
  end;

implementation


{ T3DBoolSolid }


// Constructs an empty solid
constructor T3DBoolSolid.Create();
begin
 inherited Create;
 setInitialFeatures();
end;

// Construct a solid based on data arrays. An exception may occur in the case of
// abnormal arrays (indices making references to inexistent vertices, there are less
// colors than vertices...)
//   @param vertices array of points defining the solid vertices
//   @param indices array of indices for a array of vertices
//   @param colors array of colors defining the vertices colors
constructor T3DBoolSolid.Create(i_lstVertices: TList<T3DBoolPoint3d>;
                                i_lstIndices:  TList<Cardinal>;
                                i_lstColors:   TList<TColor>);
begin
 inherited Create;
 self.setInitialFeatures();
 self.setData(i_lstVertices, i_lstIndices, i_lstColors);
end;

constructor T3DBoolSolid.Create(i_lstVertices: TList<T3DBoolPoint3d>;
                       i_lstIndices:  TList<Cardinal>;
                       i_oColor :   TColor);
begin
 inherited Create;
 self.setInitialFeatures();
 self.setData(i_lstVertices, i_lstIndices, i_oColor);
end;

destructor T3DBoolSolid.Destroy();
begin
  inherited Destroy;
end;

// Sets the initial features common to all constructors
procedure T3DBoolSolid.setInitialFeatures();
begin
 vertices := TList<T3DBoolPoint3d>.Create;
 indices  := TList<Cardinal>.Create;
 colors   := TList<TColor>.Create;
 //setCapability(Shape3D.ALLOW_GEOMETRY_WRITE);
 //setCapability(Shape3D.ALLOW_APPEARANCE_WRITE);
 //setCapability(Shape3D.ALLOW_APPEARANCE_READ);
end;

// Gets the solid vertices
//   @return solid vertices
function T3DBoolSolid.getVertices(): TList<T3DBoolPoint3d>;
var
 i : integer;
 newVertices: TList<T3DBoolPoint3d>;
begin
 newVertices := TList<T3DBoolPoint3d>.Create;
 for i := 0 to vertices.Count-1 do
   newVertices.Add(vertices[i]);
 Result := newVertices;
end;

// Gets the solid indices for its vertices
//   @return solid indices for its vertices
function T3DBoolSolid.getIndices(): TList<Cardinal>;
var
 i : integer;
 newIndices : TList<Cardinal>;
begin
 newIndices := TList<Cardinal>.Create;
 for i := 0 to indices.Count-1 do
   newIndices.Add(indices[i]);
 Result := newIndices;
end;

// Gets the vertices colors
//   @return vertices colors
function T3DBoolSolid.getColors(): TList<TColor>;
var
 i : Integer;
 newColors : TList<TColor>;
begin
 newColors := TList<TColor>.Create;
 for i := 0 to colors.Count-1 do
   newColors.Add(colors[i]);
 Result := newColors;
end;

// Gets if the solid is empty (without any vertex)
//   @return true if the solid is empty, false otherwise
function T3DBoolSolid.isEmpty(): Boolean;
begin
 if (indices.Count = 0) then
   Result := true
 else
   Result := false;
end;

// Sets the solid data. Each vertex may have a different color. An exception may
// occur in the case of abnormal arrays (e.g., indices making references to
// inexistent vertices, there are less colors than vertices...)
//   @param vertices array of points defining the solid vertices
//   @param indices array of indices for a array of vertices
//   @param colors array of colors defining the vertices colors
procedure T3DBoolSolid.setData(i_lstVertices : TList<T3DBoolPoint3d>;
                               i_lstIndices  : TList<Cardinal>;
                               i_lstColors   : TList<TColor>);
var
 i : integer;
begin
 if (i_lstVertices.Count > 0) and (i_lstIndices.Count > 0) then begin
   for i := 0 to i_lstVertices.Count-1 do begin
     self.vertices.Add(i_lstVertices[i].Clone());
     self.colors.Add(i_lstColors[i]);
   end;
   for i := 0 to i_lstIndices.Count-1 do
     self.indices.Add(i_lstIndices[i]);
   self.defineGeometry();
 end;
end;

// Sets the solid data. Defines the same color to all the vertices. An exception may
// may occur in the case of abnormal arrays (e.g., indices making references to
// inexistent vertices...)
//   @param vertices array of points defining the solid vertices
//   @param indices array of indices for a array of vertices
//   @param color the color of the vertices (the solid color)
procedure T3DBoolSolid.setData(i_lstVertices : TList<T3DBoolPoint3d>;
                               i_lstIndices  : TList<Cardinal>;
                               i_oColor      : TColor);
var
 i: Integer;
 l_lstColors : TList<TColor>;
begin
 l_lstColors := TList<TColor>.Create;
 for i := 0 to i_lstVertices.Count-1 do begin
   l_lstColors.Add(i_oColor);
 end;
 self.setData(i_lstVertices,i_lstIndices,l_lstColors);
end;

// Applies a translation into a solid
//   @param dx translation on the x axis
//   @param dy translation on the y axis
//   @param dz translation on the z axis
procedure T3DBoolSolid.translate(dx, dy: Real3D; dz: Real3D=0.0);
var
 i : integer;
begin
 if (dx <> 0.0) or (dy <> 0.0) or (dz <> 0.0) then begin
   for i := 0 to vertices.Count-1 do begin
     vertices[i].x := vertices[i].x + dx;
     vertices[i].y := vertices[i].y + dy;
     vertices[i].z := vertices[i].z + dz;
   end;
   defineGeometry();
 end;
end;

// Applies a rotation into a solid
//   @param dx rotation on the x axis
//   @param dy rotation on the y axis
//   @param dz rotation on the z axis
procedure T3DBoolSolid.rotate(dx, dy : Real3D; dz: Real3D=0.0);
var
 i : integer;
 cosX, cosY, cosZ, sinX, sinY, sinZ, newX, newY, newZ: Real3D;
 mean: T3DBoolPoint3d;
begin
 cosX := cos(dx);
 cosY := cos(dy);
 cosZ := cos(dz);
 sinX := sin(dx);
 sinY := sin(dy);
 sinZ := sin(dz);
 if (dx <> 0.0) or (dy <> 0.0) or (dz <> 0.0) then begin
   mean := self.getMean();
   for i := 0 to vertices.Count-1 do begin
     vertices[i].x := vertices[i].x - mean.x;
     vertices[i].y := vertices[i].y - mean.y;
     vertices[i].z := vertices[i].z - mean.z;
     if (dx <> 0.0) then begin
       newY := vertices[i].y * cosX - vertices[i].z * sinX;
       newZ := vertices[i].y * sinX + vertices[i].z * cosX;
       vertices[i].y := newY;
       vertices[i].z := newZ;
     end;
     if (dy <> 0.0) then begin
       newX := vertices[i].x * cosY + vertices[i].z * sinY;
       newZ :=-vertices[i].x * sinY + vertices[i].z * cosY;
       vertices[i].x := newX;
       vertices[i].z := newZ;
     end;
     if (dz <> 0.0) then begin      //TODO: Test mit dz !!!
       newX := vertices[i].x * cosZ + vertices[i].y * sinZ;
       newY :=-vertices[i].x * sinZ + vertices[i].y * cosZ;
       vertices[i].x := newX;
       vertices[i].y := newY;
     end;
     vertices[i].x := vertices[i].x + mean.x;
     vertices[i].y := vertices[i].y + mean.y;
     vertices[i].z := vertices[i].z + mean.z;
    end;
  end;
  defineGeometry();
end;

// Applies a zoom into a solid
//   @param dz translation on the z axis
procedure T3DBoolSolid.zoom(dz : Real3D);
var
 i : integer;
begin
 if (dz <> 0.0) then begin
   for i := 0 to vertices.Count-1 do begin
     vertices[i].z := vertices[i].z + dz;
   end;
   defineGeometry();
 end;
end;

// Applies a scale changing into the solid
//   @param dx scale changing for the x axis
//   @param dy scale changing for the y axis
//   @param dz scale changing for the z axis
procedure T3DBoolSolid.scale(dx, dy, dz : Real3D);
var
 i : integer;
begin
 for i := 0 to vertices.Count-1 do begin
   vertices[i].x := vertices[i].x * dx;
   vertices[i].y := vertices[i].y * dy;
   vertices[i].z := vertices[i].z * dz;
 end;
 defineGeometry();
end;

// Creates a geometry based on the indexes and vertices set for the solid
procedure T3DBoolSolid.defineGeometry();
// var
// ng : NormalGenerator;
// gi : GeometryInfo;
begin
//  GeometryInfo gi = new GeometryInfo(GeometryInfo.TRIANGLE_ARRAY);
//  gi.setCoordinateIndices(indices);
//  gi.setCoordinates(vertices);
//  NormalGenerator ng = new NormalGenerator();
//  ng.generateNormals(gi);
//
//  gi.setColors(colors);
//  gi.setColorIndices(indices);
//  gi.recomputeIndices();
//
//  setGeometry(gi.getIndexedGeometryArray());
end;

// Gets the solid mean
//   @return point representing the mean
function T3DBoolSolid.getMean(): T3DBoolPoint3d;
var
 i : integer;
 mean : T3DBoolPoint3d;
begin
 mean := T3DBoolPoint3d.Create;
 for i := 0 to vertices.Count-1 do begin
   mean.x := mean.x + vertices[i].x;
   mean.y := mean.y + vertices[i].y;
   mean.z := mean.z + vertices[i].z;
 end;
 mean.x := mean.x / vertices.Count;
 mean.y := mean.y / vertices.Count;
 mean.z := mean.z / vertices.Count;
 Result := mean;
end;


end.
