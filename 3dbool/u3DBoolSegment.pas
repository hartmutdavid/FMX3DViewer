unit u3DBoolSegment;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.Math, System.Generics.Collections,
  FMX.Types, FMX.Graphics, FMX.Types3D,
  System.Math.Vectors, FMX.Objects3D, FMX.Controls3D,
  u3DBoolMisc, u3DBoolVertex, u3DBoolLine, u3DBoolFace;


type
 enSegmentType = (UNKNOWN,VERTEX,FACE,EDGE);

  T3DBoolSegment = class
  private
    // line resulting from the two planes intersection
    line : T3DBoolLine;
    // shows how many ends were already defined
    index : Integer;
    // distance from the segment starting point to the point defining the plane
    startDist : Real3D;
    // distance from the segment ending point to the point defining the plane
    endDist   : Real3D;
    // starting point status relative to the face
    startType : enSegmentType;
    // intermediate status relative to the face
    middleType: enSegmentType;
    // ending point status relative to the face
    endType   : enSegmentType;
    // nearest vertex from the starting point
    startVertex : T3DBoolVertex;
    // nearest vertex from the ending point
    endVertex : T3DBoolVertex;
    // start of the intersection point
    startPos : T3DBoolPoint3d;
    //  end of the intersection point
    endPos   : T3DBoolPoint3d;
    //
    function setVertex(vertex : T3DBoolVertex): boolean;
    function setEdge(vertex1, vertex2 : T3DBoolVertex): boolean;
    procedure swapEnds();
  public
    constructor Create(); overload;
    constructor Create(i_oLine: T3DBoolLine; i_oFace: T3DBoolFace; sign1, sign2, sign3: Integer); overload;
    destructor Destroy; override;
    //
    function clone(): T3DBoolSegment;
    function getStartVertex(): T3DBoolVertex;
    function getEndVertex(): T3DBoolVertex;
    function getStartDistance(): Real3D;
    function getEndDistance(): Real3D;
    function getStartType(): enSegmentType;
    function getIntermediateType(): enSegmentType;
    function getEndType(): enSegmentType;
    function getNumEndsSet(): Integer;
    function getStartPosition(): T3DBoolPoint3d;
    function getEndPosition(): T3DBoolPoint3d;
    function intersect(segment : T3DBoolSegment): boolean;
  end;

implementation

{ TSegment }

constructor T3DBoolSegment.Create();
begin
 inherited Create;
end;

// Constructs a Segment based on elements obtained from the two planes relations
//   @param line resulting from the two planes intersection
//   @param face face that intersects with the plane
//   @param sign1 position of the face vertex1 relative to the plane (-1 behind, 1 front, 0 on)
//   @param sign2 position of the face vertex1 relative to the plane (-1 behind, 1 front, 0 on)
//   @param sign3 position of the face vertex1 relative to the plane (-1 behind, 1 front, 0 on)
constructor T3DBoolSegment.Create(i_oLine: T3DBoolLine; i_oFace: T3DBoolFace; sign1, sign2, sign3: Integer);
begin
 inherited Create;
 self.line  := i_oLine;
 self.index := 0;
 // VERTEX is an end
 if (sign1 = 0) then begin
   self.setVertex(i_oFace.v1);
   // other vertices on the same side - VERTEX-VERTEX VERTEX
   if (sign2 = sign3) then begin
     self.setVertex(i_oFace.v1);
   end;
 end;
 // VERTEX is an end
 if (sign2 = 0) then begin
   self.setVertex(i_oFace.v2);
   // other vertices on the same side - VERTEX-VERTEX VERTEX
   if (sign1 = sign3) then begin
     self.setVertex(i_oFace.v2);
   end;
 end;
 // VERTEX is an end
 if (sign3 = 0) then begin
   self.setVertex(i_oFace.v3);
   // other vertices on the same side - VERTEX-VERTEX VERTEX
   if (sign1 = sign2) then begin
     self.setVertex(i_oFace.v3);
   end;
 end;
 // There are undefined ends - one or more edges cut the planes intersection line
 if (self.getNumEndsSet() <> 2) then begin
   // EDGE is an end
   if ((sign1 = 1) and (sign2 = -1)) or
      ((sign1 =-1) and (sign2 = 1)) then begin
     self.setEdge(i_oFace.v1,i_oFace.v2);
   end;
   // EDGE is an end
   if ((sign2 = 1) and (sign3 = -1)) or
      ((sign2 =-1) and (sign3 = 1)) then begin
     self.setEdge(i_oFace.v2,i_oFace.v3);
   end;
   // EDGE is an end
   if ((sign3 = 1) and (sign1 =-1)) or
      ((sign3 =-1) and (sign1 = 1)) then begin
     self.setEdge(i_oFace.v3,i_oFace.v1);
   end;
 end;
end;

destructor T3DBoolSegment.Destroy();
begin
  inherited Destroy;
end;

// Clones the Segment object
//   @return cloned Segment object
function T3DBoolSegment.clone(): T3DBoolSegment;
var
 clone : T3DBoolSegment;
begin
 clone := T3DBoolSegment.Create();
 clone.line        := line.Clone();
 clone.index       := index;
 clone.startDist   := startDist;
 clone.endDist     := endDist;
 clone.startType   := startType;
 clone.middleType  := middleType;
 clone.endType     := endType;
 clone.startVertex := startVertex.Clone();
 clone.endVertex   := endVertex.Clone();
 clone.startPos    := startPos.Clone();
 clone.endPos      := endPos.Clone();
 Result := clone;
end;

// Gets the start vertex
//   @return start vertex
function T3DBoolSegment.getStartVertex(): T3DBoolVertex;
begin
 Result := startVertex;
end;

// Gets the end vertex
//   @return end vertex
function T3DBoolSegment.getEndVertex(): T3DBoolVertex;
begin
 Result := endVertex;
end;

// Gets the distance from the origin until the starting point
//   @return distance from the origin until the starting point
function T3DBoolSegment.getStartDistance(): Real3D;
begin
 Result := startDist;
end;

// Gets the distance from the origin until ending point
//   @return distance from the origin until the ending point
function T3DBoolSegment.getEndDistance(): Real3D;
begin
 Result := endDist;
end;

// Gets the type of the starting point
//   @return type of the starting point
function T3DBoolSegment.getStartType(): enSegmentType;
begin
 Result := startType;
end;

// Gets the type of the segment between the starting and ending points
//   @return type of the segment between the starting and ending points
function T3DBoolSegment.getIntermediateType(): enSegmentType;
begin
 Result := middleType;
end;

// Gets the type of the ending point
//   @return type of the ending point
function T3DBoolSegment.getEndType(): enSegmentType;
begin
 Result := endType;
end;

// Gets the number of ends already set
//   @return number of ends already set
function T3DBoolSegment.getNumEndsSet(): Integer;
begin
 Result := index;
end;

// Gets the starting position
//   @return start position
function T3DBoolSegment.getStartPosition(): T3DBoolPoint3d;
begin
 Result := startPos;
end;

// Gets the ending position
//   @return ending position
function T3DBoolSegment.getEndPosition(): T3DBoolPoint3d;
begin
 Result := endPos;
end;

// Checks if two segments intersect
//   @param segment the other segment to check the intesection
//   @return true if the segments intersect, false otherwise
function T3DBoolSegment.intersect(segment : T3DBoolSegment): boolean;
begin
 if (endDist < (segment.startDist + u3DBoolMisc.TOL)) or (segment.endDist < (startDist + u3DBoolMisc.TOL)) then begin
   Result := false;
 end
 else begin
   Result := true;
 end;
end;

// Sets an end as vertex (starting point if none end were defined, ending point otherwise)
//   @param vertex the vertex that is an segment end
//   @return false if all the ends were already defined, true otherwise
function T3DBoolSegment.setVertex(vertex : T3DBoolVertex): boolean;
begin
 // none end were defined - define starting point as VERTEX
 if (index = 0) then begin
   startVertex := vertex;
   startType   := enSegmentType.VERTEX;
   startDist   := line.computePointToPointDistance(vertex.getPosition());
   startPos    := startVertex.getPosition();
   Inc(index);
   Result := True;
 end
 else begin
   // starting point were defined - define ending point as VERTEX
   if (index = 1) then begin
     endVertex := vertex;
     endType   := enSegmentType.VERTEX;
     endDist   := line.computePointToPointDistance(vertex.getPosition());
     endPos    := endVertex.getPosition();
     Inc(index);
     // defining middle based on the starting point VERTEX-VERTEX-VERTEX
     if (startVertex = endVertex) then begin
        middleType := enSegmentType.VERTEX;
     end
     else if (startType = enSegmentType.VERTEX) then  // VERTEX-EDGE-VERTEX
     begin
       middleType := enSegmentType.EDGE;
     end;
     // the ending point distance should be smaller than starting point distance
     if (startDist > endDist) then begin
       swapEnds();
     end;
     Result := True;
   end
   else begin
     Result := False;
   end;
 end;
end;

// Sets an end as edge (starting point if none end were defined, ending point otherwise)
//   @param vertex1 one of the vertices of the intercepted edge
//   @param vertex2 one of the vertices of the intercepted edge
//   @return false if all ends were already defined, true otherwise
function T3DBoolSegment.setEdge(vertex1, vertex2: T3DBoolVertex): boolean;
var
 point2 : T3DBoolPoint3d;
 point1 : T3DBoolPoint3d;
 edgeDirection : T3DBoolVector3d;
 edgeLine : T3DBoolLine;
begin
 point1 := vertex1.getPosition();
 point2 := vertex2.getPosition();
 edgeDirection := T3DBoolVector3d.Create(point2.x - point1.x, point2.y - point1.y, point2.z - point1.z);
 edgeLine      := T3DBoolLine.Create(edgeDirection, point1);
 if (index = 0) then begin
   startVertex := vertex1;
   startType   := enSegmentType.EDGE;
   startPos    := line.computeLineIntersection(edgeLine);
   startDist   := line.computePointToPointDistance(startPos);
   middleType  := enSegmentType.FACE;
   Inc(index);
   Result := True;
 end
 else if (index = 1) then begin
   endVertex  := vertex1;
   endType    := enSegmentType.EDGE;
   endPos     := line.computeLineIntersection(edgeLine);
   endDist    := line.computePointToPointDistance(endPos);
   middleType := enSegmentType.FACE;
   Inc(index);
   // the ending point distance should be smaller than  starting point distance
   if (startDist > endDist) then begin
     swapEnds();
   end;
   result := true;
 end
 else begin
   result := false;
 end;
end;

procedure T3DBoolSegment.swapEnds();
var
 typeTemp : enSegmentType;
 distTemp : Real3D;
 posTemp  : T3DBoolPoint3d;
 vertexTemp : T3DBoolVertex;
begin
 distTemp  := startDist;
 startDist := endDist;
 endDist   := distTemp;
 typeTemp  := startType;
 startType := endType;
 endType   := typeTemp;
 vertexTemp  := startVertex;
 startVertex := endVertex;
 endVertex := vertexTemp;
 posTemp   := startPos;
 startPos  := endPos;
 endPos    := posTemp;
end;

end.
