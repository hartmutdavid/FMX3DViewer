unit u3DBoolFace;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.Math, System.Generics.Collections, System.Math.Vectors,
  FMX.Types, FMX.Graphics, FMX.Types3D, FMX.Objects3D, FMX.Controls3D,
  u3DBoolMisc, u3DBoolVertex, u3DBoolBound;

type
  T3DBoolFace = class
  private
    status : enVertexFaceType;
    function hasPoint(point : T3DBoolPoint3d): boolean;
    function linePositionInX(point : T3DBoolPoint3d; pointLine1 : T3DBoolPoint3d; pointLine2 : T3DBoolPoint3d): enVertexFaceType;
    function linePositionInY(point : T3DBoolPoint3d; pointLine1 : T3DBoolPoint3d; pointLine2 : T3DBoolPoint3d): enVertexFaceType;
    function linePositionInZ(point : T3DBoolPoint3d; pointLine1 : T3DBoolPoint3d; pointLine2 : T3DBoolPoint3d): enVertexFaceType;
  public
    // first vertex
    v1 : T3DBoolVertex;
    // second vertex
    v2 : T3DBoolVertex;
    // third vertex
    v3 : T3DBoolVertex;
    //
    constructor Create(i_oVertex1, i_oVertex2, i_oVertex3: T3DBoolVertex);
    destructor Destroy; override;
    //
    function clone(): T3DBoolFace;
    function toString(): String;
    function equals(anObject : T3DBoolFace): boolean;
    function getBound(): T3DBoolBound;
    function getNormal(): T3DBoolVector3d;
    function getStatus(): enVertexFaceType;
    function getArea(): Real3D;
    procedure invert();
    function simpleClassify(): boolean;
    procedure rayTraceClassify(anObject : TObject);
  end;


implementation

uses u3DBoolLine, u3DBoolObject3D;

{ TFace }

// Constructs a face with unknown status.
//   @param v1 a face vertex
//   @param v2 a face vertex
//   @param v3 a face vertex
constructor T3DBoolFace.Create(i_oVertex1, i_oVertex2, i_oVertex3: T3DBoolVertex);
begin
 inherited Create;
 self.v1 := i_oVertex1;
 self.v2 := i_oVertex2;
 self.v3 := i_oVertex3;
 status  := enVertexFaceType.UNKNOWN;
end;

destructor T3DBoolFace.Destroy();
begin
 inherited Destroy;
end;

// Clones the face object
//   @return cloned face object
function T3DBoolFace.clone(): T3DBoolFace;
var
 l_oClone : T3DBoolFace;
begin
 l_oClone := T3DBoolFace.Create(self.v1,self.v2,self.v3);
 l_oClone.status := self.status;
 result := l_oClone;
end;

// Makes a string definition for the Face object
//   @return the string definition
function T3DBoolFace.toString(): String;
begin
 Result := v1.toString() + #10 + v2.toString() + #10 + v3.toString();
end;

// Checks if a face is equal to another. To be equal, they have to have equal
// vertices in the same order
//   @param anObject the other face to be tested
//   @return true if they are equal, false otherwise.
function T3DBoolFace.equals(anObject : T3DBoolFace): boolean;
var
 cond3 : boolean;
 cond2 : boolean;
 cond1 : boolean;
begin
 if not Assigned(anObject) then begin
   Result := false;
 end
 else begin
   cond1 := v1.equals(anObject.v1) and v2.equals(anObject.v2) and v3.equals(anObject.v3);
   cond2 := v1.equals(anObject.v2) and v2.equals(anObject.v3) and v3.equals(anObject.v1);
   cond3 := v1.equals(anObject.v3) and v2.equals(anObject.v1) and v3.equals(anObject.v2);
   Result:= cond1 or cond2 or  cond3;
 end;
end;

// Gets the face bound
//   @return face bound
function T3DBoolFace.getBound(): T3DBoolBound;
begin
 Result := T3DBoolBound.Create(v1.getPosition(), v2.getPosition(), v3.getPosition());
end;

// Gets the face bound
//   @return face bound
function T3DBoolFace.getNormal(): T3DBoolVector3d;
var
 p1 : T3DBoolPoint3d;
 p2 : T3DBoolPoint3d;
 p3 : T3DBoolPoint3d;
 xy, xz, normal: T3DBoolVector3d;
begin
 p1 := v1.getPosition();
 p2 := v2.getPosition();
 p3 := v3.getPosition();
 xy := T3DBoolVector3d.Create(p2.x - p1.x, p2.y - p1.y, p2.z - p1.z);
 xz := T3DBoolVector3d.Create(p3.x - p1.x, p3.y - p1.y, p3.z - p1.z);
 normal := T3DBoolVector3d.Create;
 normal.cross(xy,xz);
 normal.normalize();
 Result := normal;
end;

// Gets the face status
//   @return face status - UNKNOWN, INSIDE, OUTSIDE, SAME OR OPPOSITE
function T3DBoolFace.getStatus(): enVertexFaceType;
begin
 Result := status;
end;

// Gets the face area
//   @return face area
function T3DBoolFace.getArea(): Real3D;
var
 a, B, c: Real3D;
 xy : T3DBoolVector3d;
 xz : T3DBoolVector3d;
 p1 : T3DBoolPoint3d;
 p2 : T3DBoolPoint3d;
 p3 : T3DBoolPoint3d;
begin
 p1 := v1.getPosition();
 p2 := v2.getPosition();
 p3 := v3.getPosition();
 xy := T3DBoolVector3d.Create(p2.x - p1.x, p2.y - p1.y, p2.z - p1.z);
 xz := T3DBoolVector3d.Create(p3.x - p1.x, p3.y - p1.y, p3.z - p1.z);
 a  := p1.distance(p2);
 c  := p1.distance(p3);
 B  := xy.angle(xz);
 Result := (a * c * Sin(B)) / 2.0;
end;

// Invert face direction (normal direction)
procedure T3DBoolFace.invert();
var
 vertexTemp : T3DBoolVertex;
begin
 vertexTemp := v2;
 v2 := v1;
 v1 := vertexTemp;
end;

// Classifies the face if one of its vertices are classified as INSIDE or OUTSIDE
//   @return true if the face could be classified, false otherwise
function T3DBoolFace.simpleClassify(): boolean;
var
 status1, status2, status3 : enVertexFaceType;
begin
 status1 := v1.getStatus();
 status2 := v2.getStatus();
 status3 := v3.getStatus();
 if (status1 = enVertexFaceType.INSIDE) or (status1 = enVertexFaceType.OUTSIDE) then begin
    self.status := status1;
    Result := true;
 end
 else if (status2 = enVertexFaceType.INSIDE) or (status2 = enVertexFaceType.OUTSIDE) then begin
   self.status := status2;
   Result := true;
 end
 else if (status3 = enVertexFaceType.INSIDE) or (status3 = enVertexFaceType.OUTSIDE) then begin
   self.status := status3;
   Result := true;
 end
 else begin
   Result := false;
 end;
end;

// Classifies the face based on the ray trace technique
//   @param object object3d used to compute the face status
procedure T3DBoolFace.rayTraceClassify(anObject : TObject);
var
 i, l_iNumFaces : integer;
 success  : boolean;
 distance : Real3D;
 closestDistance : Real3D;
 dotProduct : Real3D;
 p0   : T3DBoolPoint3d;
 face : T3DBoolFace;
 ray  : T3DBoolLine;
 closestFace : T3DBoolFace;
 intersectionPoint : T3DBoolPoint3d;
 l_oObj: T3DBoolObject3D;
begin
 if not (anObject is T3DBoolObject3D) then
   exit;
 l_oObj := T3DBoolObject3D(anObject);
 p0   := T3DBoolPoint3d.Create();
 p0.x := (v1.x + v2.x + v3.x) / 3.0;
 p0.y := (v1.y + v2.y + v3.y) / 3.0;
 p0.z := (v1.z + v2.z + v3.z) / 3.0;
 ray := T3DBoolLine.Create(getNormal(), p0);
 closestFace := Nil;
 repeat begin
    success := true;
    closestDistance := MaxSingle;
    // for each face from the other solid...
    l_iNumFaces := l_oObj.getNumFaces();
    for i := 0 to l_iNumFaces-1 do begin
      face := l_oObj.getFace(i);
      dotProduct := face.getNormal().dot(ray.getDirection());
      intersectionPoint := ray.computePlaneIntersection(face.getNormal(),face.v1.getPosition());
      // if ray intersects the plane...
      if Assigned(intersectionPoint) then begin
        distance := ray.computePointToPointDistance(intersectionPoint);
        if (Abs(distance) < u3DBoolMisc.TOL) and (Abs(dotProduct) < u3DBoolMisc.TOL) then begin
          // disturb the ray in order to not lie into another plane
          ray.perturbDirection();
          success := false;
          break;
        end;
        // if ray lies in plane...
        if (Abs(distance) < u3DBoolMisc.TOL) and (Abs(dotProduct) > u3DBoolMisc.TOL) then begin
          // if ray intersects the face...
          if (face.hasPoint(intersectionPoint)) then begin
            // faces coincide
            closestFace := face;
            closestDistance := 0;
            break;
          end;
        end
        // if ray intersects plane...
        else if (Abs(dotProduct) > u3DBoolMisc.TOL)  and (distance > u3DBoolMisc.TOL) then begin
          if (distance < closestDistance) then begin
            // if ray intersects the face
            if (face.hasPoint(intersectionPoint)) then begin
              // this face is the closest face untill now
              closestDistance := distance;
              closestFace := face;
            end;
          end;
        end;
      end;
    end;
 end
 until (success);
 //
 if not Assigned(closestFace) then begin
   status := u3DBoolMisc.OUTSIDE;
 end
 else begin
   dotProduct:=closestFace.getNormal().dot(ray.getDirection());
   if abs(closestDistance) < u3DBoolMisc.TOL then begin
     if dotProduct > u3DBoolMisc.TOL then begin
       status := u3DBoolMisc.SAME;
     end
     else if (dotProduct < -u3DBoolMisc.TOL) then begin
       status := u3DBoolMisc.OPPOSITE;
     end;
   end
   else if (dotProduct > u3DBoolMisc.TOL) then begin
     status := u3DBoolMisc.INSIDE;
   end
   else if (dotProduct < -u3DBoolMisc.TOL) then begin
     status := u3DBoolMisc.OUTSIDE;
   end;
  end;
end;

// Checks if the the face contains a point
//   @param point to be tested
//   @param true if the face contains the point, false otherwise
function T3DBoolFace.hasPoint(point : T3DBoolPoint3d): boolean;
var
 hasUp, hasOn, hasDown: boolean;
 result1, result2, result3: enVertexFaceType;
 normal : T3DBoolVector3d;
begin
 normal:=getNormal();
 if Abs(normal.x) > u3DBoolMisc.TOL then begin
   result1 := linePositionInX(point,v1.getPosition(),v2.getPosition());
   result2 := linePositionInX(point,v2.getPosition(),v3.getPosition());
   result3 := linePositionInX(point,v3.getPosition(),v1.getPosition());
 end
 else if Abs(normal.y) > u3DBoolMisc.TOL then begin
   result1 := linePositionInY(point,v1.getPosition(),v2.getPosition());
   result2 := linePositionInY(point,v2.getPosition(),v3.getPosition());
   result3 := linePositionInY(point,v3.getPosition(),v1.getPosition());
 end
 else begin
   result1 := linePositionInZ(point,v1.getPosition(),v2.getPosition());
   result2 := linePositionInZ(point,v2.getPosition(),v3.getPosition());
   result3 := linePositionInZ(point,v3.getPosition(),v1.getPosition());
 end;
 if (((result1 = enVertexFaceType.UP)   or (result2 = enVertexFaceType.UP)   or (result3 = enVertexFaceType.UP)) and
     ((result1 = enVertexFaceType.DOWN) or (result2 = enVertexFaceType.DOWN) or (result3 = enVertexFaceType.DOWN))) then begin
    Result := true;
 end
 else if (result1 = enVertexFaceType.ON) or (result2 = enVertexFaceType.ON) or (result3 = enVertexFaceType.ON) then begin
   Result := true;
 end
 else begin
   Result := false;
 end;
end;


// Gets the position of a point relative to a line in the x plane
//   @param point point to be tested
//   @param pointLine1 one of the line ends
//   @param pointLine2 one of the line ends
//   @return position of the point relative to the line - UP, DOWN, ON, NONE
function T3DBoolFace.linePositionInX(point, pointLine1, pointLine2 : T3DBoolPoint3d): enVertexFaceType;
var
 a, b, z: Real3D;
begin
 if ((Abs(pointLine1.y-pointLine2.y) > u3DBoolMisc.TOL) and
    (((point.y >= pointLine1.y) and (point.y <= pointLine2.y)) or
    ((point.y <= pointLine1.y) and (point.y >= pointLine2.y)))) then begin
   a := (pointLine2.z - pointLine1.z) / (pointLine2.y - pointLine1.y);
   b := pointLine1.z - a * pointLine1.y;
   z := a * point.y + b;
   if (z>point.z + u3DBoolMisc.TOL) then begin
     Result := enVertexFaceType.UP;
   end
   else if (z < point.z - u3DBoolMisc.TOL) then begin
     Result := enVertexFaceType.DOWN;
   end
   else begin
     Result := enVertexFaceType.ON;
   end;
 end else begin
   Result := enVertexFaceType.NONE;
 end;
end;


// Gets the position of a point relative to a line in the y plane
//   @param point point to be tested
//   @param pointLine1 one of the line ends
//   @param pointLine2 one of the line ends
//   @return position of the point relative to the line - UP, DOWN, ON, NONE
function T3DBoolFace.linePositionInY(point, pointLine1, pointLine2 : T3DBoolPoint3d): enVertexFaceType;
var
  a, b, z : Real3D;
begin
 if ((Abs(pointLine1.x - pointLine2.x) > u3DBoolMisc.TOL) and (((point.x >= pointLine1.x) and
     (point.x <= pointLine2.x)) or ((point.x <= pointLine1.x) and
     (point.x >= pointLine2.x)))) then begin
   a := (pointLine2.z - pointLine1.z) / (pointLine2.x - pointLine1.x);
   b := pointLine1.z - a * pointLine1.x;
   z := a * point.x + b;
   if (z > point.z + u3DBoolMisc.TOL) then begin
     Result := enVertexFaceType.UP;
   end
   else if (z < point.z - TOL) then begin
     Result := enVertexFaceType.DOWN;
   end
   else begin
     Result := enVertexFaceType.ON;
   end;
 end
 else begin
   Result := enVertexFaceType.NONE;
 end;
end;

function T3DBoolFace.linePositionInZ(point, pointLine1, pointLine2 : T3DBoolPoint3d): enVertexFaceType;
var
 a, b, y : Real3D;
begin
 if ((Abs(pointLine1.x - pointLine2.x) > u3DBoolMisc.TOL) and
    (((point.x >= pointLine1.x) and
     (point.x <= pointLine2.x)) or ((point.x <= pointLine1.x) and
     (point.x >= pointLine2.x)))) then begin
   a := (pointLine2.y - pointLine1.y) / (pointLine2.x - pointLine1.x);
   b := pointLine1.y - a * pointLine1.x;
   y := a * point.x + b;
   if (y > point.y + u3DBoolMisc.TOL) then begin
     Result := enVertexFaceType.UP;
   end
   else if (y < point.y - u3DBoolMisc.TOL) then begin
     Result := enVertexFaceType.DOWN;
   end else begin
     Result := enVertexFaceType.ON;
   end;
  end else begin
    Result := enVertexFaceType.NONE;
  end;
end;

end.
