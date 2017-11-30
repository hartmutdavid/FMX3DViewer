unit u3DBoolVertex;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.Math, System.Generics.Collections,
  FMX.Types, FMX.Graphics, FMX.Types3D,
  System.Math.Vectors, FMX.Objects3D, FMX.Controls3D,
  u3DBoolMisc;

type
 T3DBoolVertex = class
  private
    // vertex color
    Color : TColor;
    // vertex status relative to other object
    Status : enVertexFaceType;
    // references to vertices conected to it by an edge
    adjacentVertices : TList<T3DBoolVertex>;
  public
    // vertex coordinate in X
    x : Real3D;
    // vertex coordinate in Y
    y : Real3D;
    // vertex coordinate in Z
    z : Real3D;
    // Constructs a vertex with unknown status
    constructor Create; overload;
    // Constructs a vertex with unknown status
    constructor Create(position: T3DBoolPoint3d; i_oColor: TColor); overload;
    // Constructs a vertex with unknown status
    constructor Create(xk, yk, zk: Real3D; i_oColor: TColor); overload;
    // Constructs a vertex with definite status
    constructor Create(position: T3DBoolPoint3d; i_oColor: TColor; i_enStatus: enVertexFaceType); overload;
    // Constructs a vertex with a definite status
    constructor Create(xk, yk, zk: Real3D; i_oColor: TColor; i_enStatus: enVertexFaceType); overload;
    //
    function  clone() : T3DBoolVertex;
    function  toString(): String;
    function  equals(anVertex: T3DBoolVertex): boolean;
    procedure setStatus(i_enStatus: enVertexFaceType);
    function  getPosition(): T3DBoolPoint3d;
    function  getAdjacentVertices(): TList<T3DBoolVertex>;
    function  getStatus(): enVertexFaceType;
    function  getColor(): TColor;
    procedure addAdjacentVertex(adjacentVertex : T3DBoolVertex);
    procedure mark(i_enStatus : enVertexFaceType);
    //
    destructor Destroy; override;
  end;


implementation

{ TVertex }

constructor T3DBoolVertex.Create();
begin
 inherited Create;
 self.x := 0.0;
 self.y := 0.0;
 self.z := 0.0;
 self.status := enVertexFaceType.UNKNOWN;
end;

// Constructs a vertex with unknown status
//   @param position vertex position
//   @param color vertex color
constructor T3DBoolVertex.Create(position: T3DBoolPoint3d; i_oColor: TColor);
begin
 inherited Create;
 self.color := i_oColor;
 self.x := position.x;
 self.y := position.y;
 self.z := position.z;
 self.adjacentVertices := TList<T3DBoolVertex>.Create;
 self.status := enVertexFaceType.UNKNOWN;
end;

// Constructs a vertex with unknown status
//   @param x coordinate on the x axis
//   @param y coordinate on the y axis
//   @param z coordinate on the z axis
//   @param color vertex color
constructor T3DBoolVertex.Create(xk, yk, zk: Real3D; i_oColor: TColor);
begin
 inherited Create;
 self.color := i_oColor;
 self.x := xk;
 self.y := yk;
 self.z := zk;
 self.adjacentVertices := TList<T3DBoolVertex>.Create;
 self.status := enVertexFaceType.UNKNOWN;
end;

// Constructs a vertex with definite status
//   @param position vertex position
//   @param color vertex color
//   @param status vertex status - UNKNOWN, BOUNDARY, INSIDE or OUTSIDE
constructor T3DBoolVertex.Create(position: T3DBoolPoint3d; i_oColor: TColor; i_enStatus: enVertexFaceType);
begin
 inherited Create;
 self.color := i_oColor;
 self.x := position.x;
 self.y := position.y;
 self.z := position.z;
 self.adjacentVertices := TList<T3DBoolVertex>.Create;
 self.status := i_enStatus;
end;

// Constructs a vertex with a definite status
//   @param x coordinate on the x axis
//   @param y coordinate on the y axis
//   @param z coordinate on the z axis
//   @param color vertex color
//   @param status vertex status - UNKNOWN, BOUNDARY, INSIDE or OUTSIDE
constructor T3DBoolVertex.Create(xk, yk, zk: Real3D; i_oColor: TColor; i_enStatus: enVertexFaceType);
begin
 inherited Create;
 self.color := i_oColor;
 self.x := xk;
 self.y := yk;
 self.z := zk;
 self.adjacentVertices := TList<T3DBoolVertex>.Create;
 self.status := status;
end;

destructor T3DBoolVertex.Destroy();
begin
 inherited Destroy;
end;

// Clones the vertex object
//   @return cloned vertex object
function T3DBoolVertex.clone(): T3DBoolVertex;
var
  i : integer;
  clone: T3DBoolVertex;
begin
 clone   := T3DBoolVertex.Create;
 clone.x := self.x;
 clone.y := self.y;
 clone.z := self.z;
 clone.color  := color;
 clone.status := self.status;
 clone.adjacentVertices := TList<T3DBoolVertex>.Create;
 for i := 0 to adjacentVertices.Count-1 do
   clone.adjacentVertices.Add(adjacentVertices[i].Clone());
 Result := clone;
end;

// Makes a string definition for the Vertex object
//   @return the string definition
function T3DBoolVertex.toString(): String;
begin
 Result := '(' + FloatToStr(self.x) + ', ' + FloatToStr(self.y) + ', ' + FloatToStr(self.z) +')';
end;

// Checks if an vertex is equal to another. To be equal, they have to have the same
// coordinates(with some tolerance) and color
//   @param anVertex the other vertex to be tested
//   @return true if they are equal, false otherwise.
function T3DBoolVertex.equals(anVertex: T3DBoolVertex): boolean;
begin
 if not Assigned(anVertex) then begin
    result := false;
 end
 else begin
   result := (Abs(x-anVertex.x) < TOL) and (Abs(y-anVertex.y) < TOL) and
             (Abs(z-anVertex.z) < TOL) and (self.color = anVertex.color);
 end;
end;

// Sets the vertex status
//   @param status vertex status - UNKNOWN, BOUNDARY/SAME, INSIDE or OUTSIDE
procedure T3DBoolVertex.setStatus(i_enStatus: enVertexFaceType);
begin
 if (i_enStatus >= enVertexFaceType.UNKNOWN) and (i_enStatus <= enVertexFaceType.SAME) then begin
   self.status := i_enStatus;
 end;
end;

// Gets the vertex position
//   @return vertex position
function T3DBoolVertex.getPosition(): T3DBoolPoint3d;
begin
 Result := T3DBoolPoint3d.Create(x,y,z);
end;

// Gets an array with the adjacent vertices
//   @return array of the adjacent vertices
function T3DBoolVertex.getAdjacentVertices(): TList<T3DBoolVertex>;
var
 i : integer;
 arrVertex: TList<T3DBoolVertex>;
begin
 arrVertex := TList<T3DBoolVertex>.Create;
 for i:= 0 to self.adjacentVertices.Count-1 do
   arrVertex.Add(self.adjacentVertices[i]);
 Result := arrVertex;
end;

// Gets the vertex status
//   @return vertex status - UNKNOWN, BOUNDARY, INSIDE or OUTSIDE
function T3DBoolVertex.getStatus(): enVertexFaceType;
begin
 Result := self.status;
end;

// Gets the vertex color
//   @return vertex color
function T3DBoolVertex.getColor(): TColor;
begin
 Result := self.color;
end;

// Sets a vertex as being adjacent to it
//   @param adjacentVertex an adjacent vertex
procedure T3DBoolVertex.addAdjacentVertex(adjacentVertex : T3DBoolVertex);
begin
 if not adjacentVertices.contains(adjacentVertex) then begin
   adjacentVertices.add(adjacentVertex);
 end;
end;

// Sets the vertex status, setting equally the adjacent ones
//   @param status new status to be set
procedure T3DBoolVertex.mark(i_enStatus : enVertexFaceType);
var
 i : integer;
begin
 self.status := i_enStatus;
 for i := 0 to self.adjacentVertices.Count-1 do begin
   if adjacentVertices[i].getStatus() = enVertexFaceType.UNKNOWN then
     adjacentVertices[i].mark(i_enStatus);
 end;
end;

end.
