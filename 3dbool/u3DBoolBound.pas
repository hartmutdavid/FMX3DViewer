unit u3DBoolBound;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.Math, System.Generics.Collections, System.Math.Vectors,
  FMX.Types, FMX.Graphics, FMX.Types3D, FMX.Objects3D, FMX.Controls3D,
  u3DBoolMisc;

type
  T3DBoolBound = class
  private
    procedure checkVertex(i_oVertex : T3DBoolPoint3d);
  public
    xMax : Real3D;
    xMin : Real3D;
    yMax : Real3D;
    yMin : Real3D;
    zMax : Real3D;
    zMin : Real3D;
    constructor Create; overload;
    constructor Create(p1, p2, p3: T3DBoolPoint3d); overload;
    constructor Create(i_lstVertices: TList<T3DBoolPoint3d>); overload;
    destructor  Destroy; override;
    //
    function toString(): String;
    function overlap(i_oBound : T3DBoolBound): boolean;
  end;


implementation


{ T3DBoolBound }

constructor T3DBoolBound.Create();
begin
 inherited Create;
 xMax := 0.0;
 xMin := 0.0;
 yMax := 0.0;
 yMin := 0.0;
 zMax := 0.0;
 zMin := 0.0;
end;

// Bound constructor for a face
//  @param p1 point relative to the first vertex
//  @param p2 point relative to the second vertex
//  @param p3 point relative to the third vertex
constructor T3DBoolBound.Create(p1, p2, p3: T3DBoolPoint3d);
var
 i : integer;
begin
 xMax := p1.x;
 xMin := p1.x;
 yMax := p1.y;
 yMin := p1.y;
 zMax := p1.z;
 zMin := p1.z;
 self.checkVertex(p2);
 self.checkVertex(p3);
end;

// Bound constructor for a object 3d
//   @param vertices the object vertices
constructor T3DBoolBound.Create(i_lstVertices: TList<T3DBoolPoint3d>);
var
 i: Integer;
begin
 xMax := i_lstVertices[0].x;
 xMin := i_lstVertices[0].x;
 yMax := i_lstVertices[0].y;
 yMin := i_lstVertices[0].y;
 zMax := i_lstVertices[0].z;
 zMin := i_lstVertices[0].z;
 for i := 1 to i_lstVertices.Count-1 do
   checkVertex(i_lstVertices[i]);
end;

destructor T3DBoolBound.Destroy();
begin
 inherited Destroy;
end;

// Makes a string definition for the bound object
//   @return the string definition
function T3DBoolBound.toString(): String;
begin
  Result := '(xMin:' + FloatToStr(xMin) + ', xMax:' + FloatToStr(xMax) +
            ' yMin:' + FloatToStr(yMin) + ', yMax:' + FloatToStr(yMax) +
            ' zMin:' + FloatToStr(zMin) + ', zMax:' + FloatToStr(zMax) + ')';
end;

// Checks if a bound overlaps other one
//   @param bound other bound to make the comparison
//   @return true if they insersect, false otherwise
function T3DBoolBound.overlap(i_oBound : T3DBoolBound): boolean;
begin
 if (xMin > (i_oBound.xMax + TOL)) or (xMax < (i_oBound.xMin - TOL)) or
    (yMin > (i_oBound.yMax + TOL)) or (yMax < (i_oBound.yMin - TOL)) or
    (zMin > (i_oBound.zMax + TOL)) or (zMax < (i_oBound.zMin - TOL)) then begin
   Result := False;
 end
 else begin
   Result := True;
 end;
end;

// Checks if one of the coordinates of a vertex exceed the ones found before
//   @param vertex vertex to be tested
procedure T3DBoolBound.checkVertex(i_oVertex : T3DBoolPoint3d);
begin
 if (i_oVertex.x > xMax) then begin
   xMax := i_oVertex.x;
 end
 else if (i_oVertex.x < xMin) then begin
   xMin := i_oVertex.x;
 end;
 if (i_oVertex.y > yMax) then begin
   yMax := i_oVertex.y;
 end
 else if (i_oVertex.y < yMin) then
 begin
  yMin := i_oVertex.y;
 end;
 if (i_oVertex.z > zMax) then begin
   zMax := i_oVertex.z;
 end
 else if (i_oVertex.z < zMin) then begin
   zMin := i_oVertex.z;
 end;
end;

end.
