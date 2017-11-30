unit u3DBoolLine;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.Math, System.Generics.Collections, System.Math.Vectors,
  FMX.Types, FMX.Graphics, FMX.Types3D, FMX.Objects3D, FMX.Controls3D,
  u3DBoolMisc, u3DBoolFace;

type
  T3DBoolLine = class
  private
    point :     T3DBoolPoint3d;
    direction : T3DBoolVector3d;
  public
    constructor Create; overload;
    constructor Create(face1, face2: T3DBoolFace); overload;
    constructor Create(direction: T3DBoolVector3d; point: T3DBoolPoint3d); overload;
    destructor  Destroy; override;
    //
    function clone(): T3DBoolLine;
    function toString(): String;
    function getPoint(): T3DBoolPoint3d;
    function getDirection(): T3DBoolVector3d;
    procedure setPoint(point : T3DBoolPoint3d);
    procedure setDirection(direction : T3DBoolVector3d);
    function computePointToPointDistance(otherPoint : T3DBoolPoint3d): Real3D;
    function computeLineIntersection(otherLine : T3DBoolLine): T3DBoolPoint3d;
    function computePlaneIntersection(normal : T3DBoolVector3d; planePoint : T3DBoolPoint3d): T3DBoolPoint3d;
    procedure perturbDirection();
  end;

implementation


{ T3DBoolLine }

constructor T3DBoolLine.Create();
begin
 inherited Create;
end;

// Constructor for a line. The line created is the intersection between two planes
//   @param face1 face representing one of the planes
//   @param face2 face representing one of the planes
constructor T3DBoolLine.Create(face1, face2: T3DBoolFace);
var
 normalFace2 : T3DBoolVector3d;
 normalFace1 : T3DBoolVector3d;
 d2 : Real3D;
 d1 : Real3D;
begin
 inherited Create;
 normalFace1 := face1.getNormal();
 normalFace2 := face2.getNormal();
 direction   := T3DBoolVector3d.Create();
 direction.cross(normalFace1,normalFace2);
 // if direction lenght is not zero (the planes aren't parallel )...
 if not (direction.length() < TOL) then begin
   // getting a line point, zero is set to a coordinate whose direction
   // component isn't zero (line intersecting its origin plan)
   point := T3DBoolPoint3d.Create();
   d1 := -(normalFace1.x * face1.v1.x + normalFace1.y * face1.v1.y + normalFace1.z * face1.v1.z);
   d2 := -(normalFace2.x * face2.v1.x + normalFace2.y * face2.v1.y + normalFace2.z * face2.v1.z);
   if (Abs(direction.x) > TOL) then begin
     point.x := 0;
     point.y := (d2 * normalFace1.z - d1 * normalFace2.z) / direction.x;
     point.z := (d1 * normalFace2.y - d2 * normalFace1.y) / direction.x;
   end
   else if (Abs(direction.y) > TOL) then begin
      point.x := (d1 * normalFace2.z - d2* normalFace1.z) / direction.y;
      point.y := 0;
      point.z := (d2 * normalFace1.x - d1* normalFace2.x) / direction.y;
    end
    else begin
      point.x := (d2 * normalFace1.y - d1 * normalFace2.y) / direction.z;
      point.y := (d1 * normalFace2.x - d2 * normalFace1.x) / direction.z;
      point.z := 0;
    end;
  end;
  direction.normalize();
end;

// Constructor for a ray
//   @param direction direction ray
//   @param point beginning of the ray
constructor T3DBoolLine.Create(direction: T3DBoolVector3d; point: T3DBoolPoint3d);
begin
 self.direction := direction.Clone();
 self.point     := point.Clone();
 direction.normalize();
end;

destructor T3DBoolLine.Destroy();
begin
  inherited Destroy;
end;

// Clones the Line object
//   @return cloned Line object
function T3DBoolLine.clone(): T3DBoolLine;
var
 clone : T3DBoolLine;
begin
 clone := T3DBoolLine.Create();
 clone.direction := direction.Clone();
 clone.point     := point.Clone();
 Result := clone;
end;

// Makes a string definition for the Line object
//   @return the string definition
function T3DBoolLine.toString(): String;
begin
 Result := 'Direction: ' + direction.toString() + ', Point: ' + point.toString();
end;

// Gets the point used to represent the line
//   @return point used to represent the line
function T3DBoolLine.getPoint(): T3DBoolPoint3d;
begin
 Result := self.point.Clone();
end;

// Gets the line direction
//   @return line direction
function T3DBoolLine.getDirection(): T3DBoolVector3d;
begin
 Result := direction.Clone();
end;

// Sets a new point
//   @param point new point
procedure T3DBoolLine.setPoint(point : T3DBoolPoint3d);
begin
 self.point := point.Clone();
end;

// Sets a new direction
//   @param direction new direction
procedure T3DBoolLine.setDirection(direction : T3DBoolVector3d);
begin
 self.direction := direction.Clone();
end;

// Computes the distance from the line point to another point
//   @param otherPoint the point to compute the distance from the line point. The point
//          is supposed to be on the same line.
//   @return points distance. If the point submitted is behind the direction, the
//          distance is negative
function T3DBoolLine.computePointToPointDistance(otherPoint : T3DBoolPoint3d): Real3D;
var
 vec : T3DBoolVector3d;
 distance : Real3D;
begin
 distance := otherPoint.distance(point);
 vec      := T3DBoolVector3d.Create(otherPoint.x - self.point.x,
                   otherPoint.y - self.point.y,
                   otherPoint.z - self.point.z );
 vec.normalize();
 if (vec.dot(direction) < 0) then begin
   Result := -distance;
 end
 else begin
   Result := distance;
 end;
end;

// Computes the point resulting from the intersection with another line
//   @param otherLine the other line to apply the intersection. The lines are supposed
//          to intersect
//   @return point resulting from the intersection. If the point coundn't be obtained, return null
function T3DBoolLine.computeLineIntersection(otherLine : T3DBoolLine): T3DBoolPoint3d;
var
 x : Real3D;
 y : Real3D;
 z : Real3D;
 t : Real3D;
 linePoint : T3DBoolPoint3d;
 lineDirection : T3DBoolVector3d;
begin
 // x = x1 + a1*t = x2 + b1*s
 // y = y1 + a2*t = y2 + b2*s
 // z = z1 + a3*t = z2 + b3*s
 linePoint := otherLine.getPoint();
 lineDirection := otherLine.getDirection();

 if Abs(direction.y*lineDirection.x-direction.x*lineDirection.y) > TOL then begin
   t := (-point.y * lineDirection.x + linePoint.y * lineDirection.x + lineDirection.y * point.x - lineDirection.y * linePoint.x) /
        (direction.y * lineDirection.x - direction.x * lineDirection.y);
  end
  else if (Abs(-direction.x * lineDirection.z + direction.z * lineDirection.x) > TOL) then begin
    t := -(-lineDirection.z * point.x + lineDirection.z * linePoint.x + lineDirection.x * point.z - lineDirection.x * linePoint.z) /
          (-direction.x*lineDirection.z+direction.z*lineDirection.x);
  end
  else if (Abs(-direction.z * lineDirection.y + direction.y * lineDirection.z) >TOL) then begin
    t := (point.z * lineDirection.y - linePoint.z * lineDirection.y - lineDirection.z * point.y + lineDirection.z * linePoint.y) /
         (-direction.z * lineDirection.y + direction.y * lineDirection.z);
  end
  else begin
    result := nil;
    exit;
  end;
  x := point.x + direction.x * t;
  y := point.y + direction.y * t;
  z := point.z + direction.z * t;
  Result := T3DBoolPoint3d.Create(x, y, z);
end;

// Compute the point resulting from the intersection with a plane
//   @param normal the plane normal
//   @param planePoint a plane point.
//   @return intersection point. If they don't intersect, return null
function T3DBoolLine.computePlaneIntersection(normal : T3DBoolVector3d;
                   planePoint : T3DBoolPoint3d): T3DBoolPoint3d;
var
 A : Real3D;
 B : Real3D;
 C : Real3D;
 D : Real3D;
 t : Real3D;
 denominator : Real3D;
 numerator   : Real3D;
 resultPoint : T3DBoolPoint3d;
begin
 A := normal.x;
 B := normal.y;
 C := normal.z;
 D := -(normal.x * planePoint.x + normal.y * planePoint.y + normal.z * planePoint.z);
 numerator  := A * point.x + B * point.y + C * point.z + D;
 denominator:= A * direction.x + B * direction.y + C * direction.z;
 if Abs(denominator) < TOL then begin
   if Abs(numerator) < TOL then begin
     Result := point.Clone();
   end
   else begin
     Result := Nil;
   end;
  end
  else begin
    t := -numerator / denominator;
    ResultPoint := T3DBoolPoint3d.Create();
    ResultPoint.x := point.x + t * direction.x;
    ResultPoint.y := point.y + t * direction.y;
    ResultPoint.z := point.z + t * direction.z;
    result := resultPoint;
  end;
end;

// Changes slightly the line direction
procedure T3DBoolLine.perturbDirection();
begin
 direction.x := direction.x + u3DBoolMisc.TOL * T3DBoolHelper.Random();
 direction.y := direction.y + u3DBoolMisc.TOL * T3DBoolHelper.Random();
 direction.z := direction.z + u3DBoolMisc.TOL * T3DBoolHelper.Random();
end;

end.
