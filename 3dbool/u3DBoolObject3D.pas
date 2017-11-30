unit u3DBoolObject3D;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.Math, System.Generics.Collections, System.Math.Vectors,
  FMX.Types, FMX.Graphics, FMX.Types3D, FMX.Objects3D, FMX.Controls3D,
  u3DBoolMisc, u3DBoolBound, u3DBoolVertex, u3DBoolFace,
  u3DBoolSegment, u3DBoolSolid
{$ifdef CODESITE}
  , CodeSiteLogging
{$endif}
  ;

type
  T3DBoolObject3D = class
  private
    m_lstVertices: TList<T3DBoolVertex>;
    m_lstFaces   : TList<T3DBoolFace>;
    m_oBound:      T3DBoolBound;
    function  addFace(v1, v2, v3: T3DBoolVertex): T3DBoolFace;
    function  addVertex(pos : T3DBoolPoint3d; color : TColor; i_enStatus : enVertexFaceType): T3DBoolVertex;
    function  computeDistance(vertex: T3DBoolVertex; face: T3DBoolFace): Real3D;
    procedure splitFace(facePos: Integer; segment1, segment2 : T3DBoolSegment);
    procedure breakFaceInTwo(facePos : Integer; newPos : T3DBoolPoint3d; splitEdge : Integer); overload;
    procedure breakFaceInTwo(facePos : Integer; newPos : T3DBoolPoint3d; endVertex : T3DBoolVertex); overload;
    procedure breakFaceInThree(facePos: Integer; newPos1, newPos2 : T3DBoolPoint3d; splitEdge : Integer); overload;
    procedure breakFaceInThree(facePos: Integer; newPos : T3DBoolPoint3d; endVertex : T3DBoolVertex); overload;
    procedure breakFaceInThree(facePos: Integer; newPos1, newPos2 : T3DBoolPoint3d; startVertex, endVertex : T3DBoolVertex); overload;
    procedure breakFaceInThree(facePos: Integer; newPos : T3DBoolPoint3d); overload;
    procedure breakFaceInFour(facePos: Integer; newPos1, newPos2 : T3DBoolPoint3d; endVertex : T3DBoolVertex);
    procedure breakFaceInFive(facePos: Integer; newPos1, newPos2 : T3DBoolPoint3d; linedVertex : Integer);
  public
    constructor Create(); overload;
    constructor Create(solid: T3DBoolSolid); overload;
    destructor Destroy; override;
    //
    function  clone(): T3DBoolObject3D;
    function  getNumFaces(): Integer;
    function  getFace(i_iIndex : integer): T3DBoolFace;
    function  getBound(): T3DBoolBound;
    procedure splitFaces(anObject : T3DBoolObject3D);
    procedure classifyFaces(anObject: T3DBoolObject3D);
    procedure invertInsideFaces();
 published
    property Vertices: TList<T3DBoolVertex> read m_lstVertices;
    property Faces   : TList<T3DBoolFace>   read m_lstFaces;
    property Bound   : T3DBoolBound read m_oBound write m_oBound;
 end;

implementation

uses u3DBoolLine;

{ T3DBoolObject3D }

constructor T3DBoolObject3D.Create();
begin
 inherited Create;
end;

// Constructs a Object3d object based on a solid file.
//   @param solid solid used to construct the Object3d object
constructor T3DBoolObject3D.Create(solid: T3DBoolSolid);
var
 i : integer;
 l_oVertex, v1, v2, v3: T3DBoolVertex;
 colors : TList<TColor>;
 verticesTemp : TList<T3DBoolVertex>;
 verticesPoints : TList<T3DBoolPoint3d>;
 l_lstIndices: TList<Cardinal>;
begin
 verticesPoints := solid.getVertices();
 l_lstIndices := solid.getIndices();
 colors  := solid.getColors();
 verticesTemp := TList<T3DBoolVertex>.Create;
 // create vertices
 m_lstVertices := TList<T3DBoolVertex>.Create;
 for i := 0 to verticesPoints.Count-1 do begin
   l_oVertex := self.addVertex(verticesPoints[i],colors[i],enVertexFaceType.UNKNOWN);
   verticesTemp.add(l_oVertex);
 end;
 // create faces
 m_lstFaces := TList<T3DBoolFace>.Create;
 i := 0;
 while i < l_lstIndices.Count do begin
   v1 := verticesTemp[l_lstIndices[i]];
   v2 := verticesTemp[l_lstIndices[i + 1]];
   v3 := verticesTemp[l_lstIndices[i + 2]];
   self.addFace(v1,v2,v3);
   i := i + 3;
 end;
 // create bound
 m_oBound := T3DBoolBound.Create(verticesPoints);
end;

destructor T3DBoolObject3D.Destroy();
begin
 inherited Destroy;
end;

// Clones the Object3D object
//   @return cloned Object3D object
function T3DBoolObject3D.clone(): T3DBoolObject3D;
var
 i : integer;
 clone : T3DBoolObject3D;
begin
 clone := T3DBoolObject3D.Create;
 clone.m_lstVertices := TList<T3DBoolVertex>.Create;
 for i := 0 to vertices.Count-1 do begin
   clone.Vertices.Add(vertices[i].Clone());
 end;
 clone.m_lstFaces := TList<T3DBoolFace>.Create;
 for i := 0 to m_lstVertices.Count-1 do begin
   clone.m_lstFaces.add(m_lstFaces[i].Clone());
 end;
 clone.bound := self.bound;
 Result := clone;
end;

// Gets the number of faces
//   @return number of faces
function T3DBoolObject3D.getNumFaces(): Integer;
begin
 Result := m_lstFaces.Count;
end;

// Gets a face reference for a given position
//   @param index required face position
//   @return face reference , null if the position is invalid
function T3DBoolObject3D.getFace(i_iIndex : Integer): T3DBoolFace;
begin
 if (i_iIndex < 0) or (i_iIndex >= m_lstFaces.Count) then begin
   result := nil;
 end
 else begin
   result := self.m_lstFaces[i_iIndex];
 end;
end;

// Gets the solid bound
//   @return solid bound
function T3DBoolObject3D.getBound(): T3DBoolBound;
begin
 Result := self.m_oBound;
end;

// Method used to add a face properly for internal methods
//   @param v1 a face vertex
//   @param v2 a face vertex
//   @param v3 a face vertex
function T3DBoolObject3D.addFace(v1, v2, v3: T3DBoolVertex): T3DBoolFace;
var
 l_oFace : T3DBoolFace;
begin
 if not (v1.equals(v2) or v1.equals(v3) or v2.equals(v3)) then begin
   l_oFace := T3DBoolFace.Create(v1, v2, v3);
   if l_oFace.getArea() > u3DBoolMisc.TOL then begin
     m_lstFaces.add(l_oFace);
     Result := l_oFace;
   end
   else begin
     l_oFace.Free;
     Result := nil;
   end;
 end
 else begin
   Result := nil;
 end;
end;

// Method used to add a vertex properly for internal methods
//   @param pos vertex position
//   @param color vertex color
//   @param status vertex status
//   @return the vertex inserted (if a similar vertex already exists, this is returned)
function T3DBoolObject3D.addVertex(pos: T3DBoolPoint3d; color: TColor; i_enStatus : enVertexFaceType): T3DBoolVertex;
var
 i : integer;
 l_oVertex : T3DBoolVertex;
begin
 l_oVertex := T3DBoolVertex.Create(pos, color, i_enStatus);
 i := 0;
 while i < m_lstVertices.Count do begin
   if l_oVertex.equals(m_lstVertices[i]) then
     break;
   Inc(i);
 end;
 if i = m_lstVertices.Count then begin
   vertices.add(l_oVertex);
   result := l_oVertex;
 end
 else begin
   l_oVertex := m_lstVertices[i];
   l_oVertex.setStatus(i_enStatus);
   result := l_oVertex;
 end;
end;

// Split faces so that none face is intercepted by a face of other object
//   @param object the other object 3d used to make the split
procedure T3DBoolObject3D.splitFaces(anObject : T3DBoolObject3D);
var
 i, j, numFacesStart, numFacesBefore, lastNumFaces, facesIgnored: integer;
 LoopCnt0, LoopCnt1, LoopCnt2, LoopCnt3, LoopCnt4, LoopCnt5, LoopCnt6, LoopCnt7: Integer;
 line : T3DBoolLine;
 face1, face2 : T3DBoolFace;
 segment1, segment2 : T3DBoolSegment;
 distFace1Vert1, distFace1Vert2, distFace1Vert3, distFace2Vert1, distFace2Vert2, distFace2Vert3: Real3D;
 signFace1Vert1, signFace1Vert2, signFace1Vert3, signFace2Vert1, signFace2Vert2, signFace2Vert3: Integer;
 segments: TList<T3DBoolSegment>;
begin
{$ifdef CODESITE}
 CodeSite.EnterMethod('T3DBoolObject3D.splitFaces');
{$endif}
  numFacesBefore := getNumFaces();
  numFacesStart  := getNumFaces();
  facesIgnored   := 0;
  //
  LoopCnt0 := 0;
  LoopCnt1 := 0;
  LoopCnt2 := 0;
  LoopCnt3 := 0;
  LoopCnt4 := 0;
  LoopCnt5 := 0;
  LoopCnt6 := 0;
  LoopCnt7 := 0;
  //
  // if the objects bounds overlap...
  if getBound().overlap(anObject.getBound()) then begin
    // for each object1 face...
    i := 0;
    while i < self.getNumFaces() do begin
      //
      Inc(LoopCnt0);
      //
      // if object1 face bound and object2 bound overlap ...
      face1 := self.getFace(i);
      if (face1.getBound().overlap(anObject.getBound())) then begin
        // for each object2 face...
        for j := 0 to anObject.getNumFaces()-1 do begin
          //
          Inc(LoopCnt1);
          // if (i = 291) and (j = 646) then LoopCnt0 := 0;
          //
          // if object1 face bound and object2 face bound overlap...
          face2 := anObject.getFace(j);
          if (face1.getBound().overlap(face2.getBound())) then begin
            //
            Inc(LoopCnt2);
            //
            // PART I - DO TWO POLIGONS INTERSECT?
            // POSSIBLE RESULTS: INTERSECT, NOT_INTERSECT, COPLANAR
            distFace1Vert1 := computeDistance(face1.v1,face2);
            distFace1Vert2 := computeDistance(face1.v2,face2);
            distFace1Vert3 := computeDistance(face1.v3,face2);
            // distances signs from the face1 vertices to the face2 plane
            if Abs(distFace1Vert1) <= u3DBoolMisc.TOL then
              signFace1Vert1 := 0
            else begin
              if distFace1Vert1 > u3DBoolMisc.TOL then
                signFace1Vert1 := 1
              else
                signFace1Vert1 := -1;
            end;
            if Abs(distFace1Vert2) <= u3DBoolMisc.TOL then
              signFace1Vert2 := 0
            else begin
              if distFace1Vert2 > u3DBoolMisc.TOL then
                signFace1Vert2 := 1
              else
                signFace1Vert2 := -1;
            end;
            if Abs(distFace1Vert3) <= u3DBoolMisc.TOL then
              signFace1Vert3 := 0
            else begin
              if distFace1Vert3 > u3DBoolMisc.TOL then
                signFace1Vert3 := 1
              else
                signFace1Vert3 := -1;
            end;
            // if all the signs are zero, the planes are coplanar
            // if all the signs are positive or negative, the planes do not intersect
            // if the signs are not equal...
            if not ((signFace1Vert1 = signFace1Vert2) and (signFace1Vert2 = signFace1Vert3)) then begin
              //
              Inc(LoopCnt3);
              //
              // distance from the face2 vertices to the face1 plane
              distFace2Vert1 := computeDistance(face2.v1,face1);
              distFace2Vert2 := computeDistance(face2.v2,face1);
              distFace2Vert3 := computeDistance(face2.v3,face1);
              // distances signs from the face2 vertices to the face1 plane
              if Abs(distFace2Vert1) <= u3DBoolMisc.TOL then
                signFace2Vert1 := 0
              else begin
                if distFace2Vert1 > u3DBoolMisc.TOL then
                  signFace2Vert1 := 1
                else
                  signFace2Vert1 := -1;
              end;
              if Abs(distFace2Vert2) <= u3DBoolMisc.TOL then
                signFace2Vert2 := 0
              else begin
                if distFace2Vert2 > u3DBoolMisc.TOL then
                  signFace2Vert2 := 1
                else
                  signFace2Vert2 := -1;
              end;
              if Abs(distFace2Vert3) <= u3DBoolMisc.TOL then
                signFace2Vert3 := 0
              else begin
                if distFace2Vert3 > u3DBoolMisc.TOL then
                  signFace2Vert3 := 1
                else
                  signFace2Vert3 := -1;
              end;
              // if the signs are not equal...
              if not ((signFace2Vert1 = signFace2Vert2) and (signFace2Vert2=signFace2Vert3)) then begin
                //
                Inc(LoopCnt4);
                //
                line     := T3DBoolLine.Create(face1, face2);
                // intersection of the face1 and the plane of face2
                segment1 := T3DBoolSegment.Create(line, face1, signFace1Vert1, signFace1Vert2, signFace1Vert3);
                // intersection of the face2 and the plane of face1
                segment2 := T3DBoolSegment.Create(line, face2, signFace2Vert1, signFace2Vert2, signFace2Vert3);
                // if the two segments intersect...
                if (segment1.intersect(segment2)) then begin
                  //
                  Inc(LoopCnt5);
                  //
                  // PART II - SUBDIVIDING NON-COPLANAR POLYGONS
                  lastNumFaces := getNumFaces();
                  self.splitFace(i,segment1,segment2);
                  //
                  // prevent from infinite loop (with a loss of faces...)
                  // if (numFacesStart*20<getNumFaces()) {
                  //   System.out.println("possible infinite loop situation: terminating faces split");
                  //   return;
                  // }
                  //
                  // if the face in the position isn't the same, there was a break
                  if face1 <> self.getFace(i) then begin
                    //
                    Inc(LoopCnt6);
{$ifdef CODESITE}
                    CodeSite.Send('LoopCnt6=' + IntToStr(LoopCnt6) + ', i=' + IntToStr(i) + ', j=' + IntToStr(j));
{$endif}
                    //
                    //if the generated solid is equal the origin...
                    if face1.equals(getFace(getNumFaces() - 1)) then begin
                      //
                      Inc(LoopCnt7);
                      //
                      // return it to its position and jump it
                      if i <> self.getNumFaces()-1 then begin
                        m_lstFaces.Delete(getNumFaces()-1);
                        m_lstFaces.insert(i,face1);
                      end
                      else begin
                        continue;
                      end;
                    end
                    else begin
                      Dec(i);
                      break;
                    end;
                  end;
                end;
              end;
            end;
          end;
        end;
      end;
      inc(i);
    end;
  end;
{$ifdef CODESITE}
 CodeSite.Send('anObject.m_lstVertices.Count=' + IntToStr(anObject.m_lstVertices.Count));
 CodeSite.Send('anObject.m_lstFaces.Count   =' + IntToStr(anObject.m_lstFaces.Count));
 CodeSite.Send('LoopCnt0=' + IntToStr(LoopCnt0));
 CodeSite.Send('LoopCnt1=' + IntToStr(LoopCnt1));
 CodeSite.Send('LoopCnt2=' + IntToStr(LoopCnt2));
 CodeSite.Send('LoopCnt3=' + IntToStr(LoopCnt3));
 CodeSite.Send('LoopCnt4=' + IntToStr(LoopCnt4));
 CodeSite.Send('LoopCnt5=' + IntToStr(LoopCnt5));
 CodeSite.Send('LoopCnt6=' + IntToStr(LoopCnt6));
 CodeSite.Send('LoopCnt7=' + IntToStr(LoopCnt7));
 CodeSite.ExitMethod('T3DBoolObject3D.splitFaces');
{$endif}
end;

// Computes closest distance from a vertex to a plane
//   @param vertex vertex used to compute the distance
//   @param face face representing the plane where it is contained
//   @return the closest distance from the vertex to the plane
function T3DBoolObject3D.computeDistance(vertex : T3DBoolVertex; face : T3DBoolFace): Real3D;
var
 a, b, c, d : Real3D;
 normal : T3DBoolVector3d;
begin
 normal := face.getNormal();
 a := normal.x;
 b := normal.y;
 c := normal.z;
 d := -(a * face.v1.x + b * face.v1.y + c * face.v1.z);
 Result := a * vertex.x + b * vertex.y + c * vertex.z + d;
end;

// Split an individual face
//   @param facePos face position on the array of faces
//   @param segment1 segment representing the intersection of the face with the plane
//          of another face
//   @return segment2 segment representing the intersection of other face with the
//           plane of the current face plane
procedure T3DBoolObject3D.splitFace(facePos : integer; segment1, segment2 : T3DBoolSegment);
var
 startPosVertex, endPosVertex: T3DBoolVertex;
 startPos, endPos : T3DBoolPoint3d;
 startType, endType, middleType: enSegmentType;
 startDist, endDist : Real3D;
 dot1, dot2, dot3 : Real3D;
 splitEdge, linedVertex : Integer;
 startVertex, endVertex : T3DBoolVertex;
 linedVertexPos : T3DBoolPoint3d;
 vertexVector : T3DBoolVector3d;
 l_oFace : T3DBoolFace;
 segmentVector : T3DBoolVector3d;
begin
 l_oFace := self.getFace(facePos);
 startVertex := segment1.getStartVertex();
 endVertex   := segment1.getEndVertex();
 // starting point: deeper starting point
 if segment2.getStartDistance() > (segment1.getStartDistance() + u3DBoolMisc.TOL) then begin
   startDist := segment2.getStartDistance();
   startType := segment1.getIntermediateType();
   startPos  := segment2.getStartPosition();
 end
 else begin
   startDist := segment1.getStartDistance();
   startType := segment1.getStartType();
   startPos  := segment1.getStartPosition();
 end;
 // ending point: deepest ending point
 if segment2.getEndDistance() < (segment1.getEndDistance() - u3DBoolMisc.TOL) then begin
   endDist := segment2.getEndDistance();
   endType := segment1.getIntermediateType();
   endPos  := segment2.getEndPosition();
 end
 else begin
   endDist := segment1.getEndDistance();
   endType := segment1.getEndType();
   endPos  := segment1.getEndPosition();
 end;
 middleType := segment1.getIntermediateType();
 // set vertex to BOUNDARY if it is start type
 if startType = enSegmentType.VERTEX then begin
   startVertex.setStatus(enVertexFaceType.SAME);
 end;
 // set vertex to BOUNDARY if it is end type
 if (endType = enSegmentType.VERTEX) then begin
   endVertex.setStatus(enVertexFaceType.SAME);      // = BOUNDARY
 end;
 // VERTEX-_______-VERTEX
 if (startType = enSegmentType.VERTEX) and (endType = enSegmentType.VERTEX) then begin
   exit;
 end
 // ______-EDGE-______
 else if (middleType=enSegmentType.EDGE) then begin
   if ((startVertex = l_oFace.v1) and (endVertex = l_oFace.v2)) or
      ((startVertex = l_oFace.v2) and (endVertex = l_oFace.v1)) then begin
     splitEdge:=1;
   end
   else if ((startVertex = l_oFace.v2) and (endVertex = l_oFace.v3)) or
           ((startVertex = l_oFace.v3) and (endVertex = l_oFace.v2)) then begin
     splitEdge:=2;
   end
   else begin
     splitEdge:=3;
   end;
   // VERTEX-EDGE-EDGE
   if (startType = enSegmentType.VERTEX) then begin
     breakFaceInTwo(facePos,endPos,splitEdge);
     exit;
   end
   // EDGE-EDGE-VERTEX
   else if (endType = enSegmentType.VERTEX) then begin
     breakFaceInTwo(facePos,startPos,splitEdge);
     exit;
   end
   // EDGE-EDGE-EDGE
   else if (startDist = endDist) then begin
     breakFaceInTwo(facePos,endPos,splitEdge);
   end
   else begin
     if ((startVertex = l_oFace.v1) and (endVertex = l_oFace.v2)) or
        ((startVertex = l_oFace.v2) and (endVertex = l_oFace.v3)) or
        ((startVertex = l_oFace.v3) and (endVertex = l_oFace.v1)) then begin
        breakFaceInThree(facePos,startPos,endPos,splitEdge);
      end else begin
        breakFaceInThree(facePos,endPos,startPos,splitEdge);
      end;
    end;
    exit;
  end
  // ______-FACE-______
  //
  // VERTEX-FACE-EDGE
  else if (startType = enSegmentType.VERTEX) and (endType = enSegmentType.EDGE) then begin
    breakFaceInTwo(facePos,endPos,endVertex);
  end
  // EDGE-FACE-VERTEX
  else if (startType = enSegmentType.EDGE) and (endType = enSegmentType.VERTEX) then begin
    breakFaceInTwo(facePos,startPos,startVertex);
  end
  // VERTEX-FACE-FACE
  else if (startType = enSegmentType.VERTEX) and (endType = enSegmentType.FACE) then begin
    breakFaceInThree(facePos,endPos,startVertex);
  end
  // FACE-FACE-VERTEX
  else if (startType = enSegmentType.FACE) and (endType = enSegmentType.VERTEX) then begin
    breakFaceInThree(facePos,startPos,endVertex);
  end
  // EDGE-FACE-EDGE
  else if (startType = enSegmentType.EDGE) and (endType = enSegmentType.EDGE) then begin
    breakFaceInThree(facePos,startPos,endPos,startVertex,endVertex);
  end
  // EDGE-FACE-FACE
  else if (startType = enSegmentType.EDGE) and (endType=enSegmentType.FACE) then begin
    breakFaceInFour(facePos,startPos,endPos,startVertex);
  end
  // FACE-FACE-EDGE
  else if (startType = enSegmentType.FACE) and (endType = enSegmentType.EDGE) then begin
    breakFaceInFour(facePos,endPos,startPos,endVertex);
  end
  // FACE-FACE-FACE
  else if (startType = enSegmentType.FACE) and (endType = enSegmentType.FACE) then begin
    segmentVector := T3DBoolVector3d.Create(startPos.x - endPos.x, startPos.y - endPos.y, startPos.z - endPos.z);
    // if the intersection segment is a point only...
    if (Abs(segmentVector.x) < u3DBoolMisc.TOL) and
       (Abs(segmentVector.y) < u3DBoolMisc.TOL) and
       (Abs(segmentVector.z) < u3DBoolMisc.TOL) then begin
      breakFaceInThree(facePos,startPos);
      exit;
    end;
    // gets the vertex more lined with the intersection segment
    vertexVector := T3DBoolVector3d.Create(endPos.x - l_oFace.v1.x, endPos.y - l_oFace.v1.y, endPos.z - l_oFace.v1.z);
    vertexVector.normalize();
    dot1 := Abs(segmentVector.dot(vertexVector));
    vertexVector := T3DBoolVector3d.Create(endPos.x - l_oFace.v2.x, endPos.y - l_oFace.v2.y, endPos.z - l_oFace.v2.z);
    vertexVector.normalize();
    dot2 := Abs(segmentVector.dot(vertexVector));
    vertexVector := T3DBoolVector3d.Create(endPos.x - l_oFace.v3.x, endPos.y - l_oFace.v3.y, endPos.z - l_oFace.v3.z);
    vertexVector.normalize();
    dot3 := Abs(segmentVector.dot(vertexVector));
    if (dot1 > dot2) and (dot1 > dot3) then begin
      linedVertex := 1;
      linedVertexPos := l_oFace.v1.getPosition();
    end
    else if (dot2 > dot3) and (dot2 > dot1) then begin
      linedVertex := 2;
      linedVertexPos := l_oFace.v2.getPosition();
    end else begin
      linedVertex := 3;
      linedVertexPos := l_oFace.v3.getPosition();
    end;
    // Now find which of the intersection endpoints is nearest to that vertex.
    if (linedVertexPos.distance(startPos) > linedVertexPos.distance(endPos)) then begin
      breakFaceInFive(facePos,startPos,endPos,linedVertex);
    end
    else begin
      breakFaceInFive(facePos,endPos,startPos,linedVertex);
    end;
  end;
end;

// Face breaker for VERTEX-EDGE-EDGE / EDGE-EDGE-VERTEX
//   @param facePos face position on the faces array
//   @param newPos new vertex position
//   @param edge that will be split
procedure T3DBoolObject3D.breakFaceInTwo(facePos: integer; newPos: T3DBoolPoint3d; splitEdge: integer);
var
 l_oFace : T3DBoolFace;
 l_oVertex : T3DBoolVertex;
begin
 l_oFace := m_lstFaces[facePos];
 m_lstFaces.delete(facePos);
 l_oVertex := addVertex(newPos,l_oFace.v1.getColor(),enVertexFaceType.SAME);   // BOUNDARY
 if (splitEdge = 1) then begin
   addFace(l_oFace.v1,l_oVertex,l_oFace.v3);
   addFace(l_oVertex,l_oFace.v2,l_oFace.v3);
 end
 else if (splitEdge = 2) then
 begin
   addFace(l_oFace.v2,l_oVertex,l_oFace.v1);
   addFace(l_oVertex,l_oFace.v3,l_oFace.v1);
 end
 else begin
   addFace(l_oFace.v3,l_oVertex,l_oFace.v2);
   addFace(l_oVertex,l_oFace.v1,l_oFace.v2);
 end;
end;

// Face breaker for VERTEX-FACE-EDGE / EDGE-FACE-VERTEX
//   @param facePos face position on the faces array
//   @param newPos new vertex position
//   @param endVertex vertex used for splitting
procedure T3DBoolObject3D.breakFaceInTwo(facePos: Integer; newPos: T3DBoolPoint3d; endVertex: T3DBoolVertex);
var
 face: T3DBoolFace;
 vertex: T3DBoolVertex;
begin
 face := m_lstFaces[facePos];
 m_lstFaces.delete(facePos);
 vertex := addVertex(newPos,face.v1.getColor(),enVertexFaceType.SAME);      // BOUNDARY
 if endVertex.equals(face.v1) then begin
   addFace(face.v1,vertex,face.v3);
   addFace(vertex,face.v2,face.v3);
 end
 else if endVertex.equals(face.v2) then begin
   addFace(face.v2,vertex,face.v1);
   addFace(vertex,face.v3,face.v1);
 end
 else begin
   addFace(face.v3,vertex,face.v2);
   addFace(vertex,face.v1,face.v2);
 end;
end;

// Face breaker for EDGE-EDGE-EDGE
//  @param facePos face position on the faces array
//  @param newPos1 new vertex position
//  @param newPos2 new vertex position
//  @param splitEdge edge that will be split
procedure T3DBoolObject3D.breakFaceInThree(facePos: Integer; newPos1, newPos2 : T3DBoolPoint3d; splitEdge: Integer);
var
 face: T3DBoolFace;
 vertex1, vertex2: T3DBoolVertex;
begin
 face := m_lstFaces[facePos];
 m_lstFaces.delete(facePos);
 vertex1 := addVertex(newPos1,face.v1.getColor(),enVertexFaceType.SAME);     // = BOUNDARY
 vertex2 := addVertex(newPos2,face.v1.getColor(),enVertexFaceType.SAME);
 if (splitEdge = 1) then begin
   addFace(face.v1,vertex1,face.v3);
   addFace(vertex1,vertex2,face.v3);
   addFace(vertex2,face.v2,face.v3);
 end
 else if (splitEdge = 2) then begin
   addFace(face.v2,vertex1,face.v1);
   addFace(vertex1,vertex2,face.v1);
   addFace(vertex2,face.v3,face.v1);
 end
 else begin
   addFace(face.v3,vertex1,face.v2);
   addFace(vertex1,vertex2,face.v2);
   addFace(vertex2,face.v1,face.v2);
 end;
end;

// Face breaker for VERTEX-FACE-FACE / FACE-FACE-VERTEX
//   @param facePos face position on the faces array
//   @param newPos new vertex position
//   @param endVertex vertex used for the split
procedure T3DBoolObject3D.breakFaceInThree(facePos: Integer; newPos: T3DBoolPoint3d; endVertex: T3DBoolVertex);
var
 face: T3DBoolFace;
 vertex: T3DBoolVertex;
begin
 face := m_lstFaces[facePos];
 m_lstFaces.delete(facePos);
 vertex := addVertex(newPos,face.v1.getColor(),enVertexFaceType.SAME);     // BOUNDARY
 if endVertex.equals(face.v1) then begin
   addFace(face.v1,face.v2,vertex);
   addFace(face.v2,face.v3,vertex);
   addFace(face.v3,face.v1,vertex);
 end
 else if endVertex.equals(face.v2) then begin
   addFace(face.v2,face.v3,vertex);
   addFace(face.v3,face.v1,vertex);
   addFace(face.v1,face.v2,vertex);
 end
 else begin
   addFace(face.v3,face.v1,vertex);
   addFace(face.v1,face.v2,vertex);
   addFace(face.v2,face.v3,vertex);
 end;
end;

// Face breaker for EDGE-FACE-EDGE
//   @param facePos face position on the faces array
//   @param newPos1 new vertex position
//   @param newPos2 new vertex position
//   @param startVertex vertex used the new faces creation
//   @param endVertex vertex used for the new faces creation
procedure T3DBoolObject3D.breakFaceInThree(facePos: Integer; newPos1, newPos2: T3DBoolPoint3d; startVertex, endVertex : T3DBoolVertex);
var
 face : T3DBoolFace;
 vertex1, vertex2 : T3DBoolVertex;
begin
 face := m_lstFaces[facePos];
 m_lstFaces.delete(facePos);
 vertex1 := addVertex(newPos1,face.v1.getColor(),enVertexFaceType.SAME);     // = BOUNDARY
 vertex2 := addVertex(newPos2,face.v1.getColor(),enVertexFaceType.SAME);
 if startVertex.equals(face.v1) and endVertex.equals(face.v2) then begin
    addFace(face.v1,vertex1,vertex2);
    addFace(face.v1,vertex2,face.v3);
    addFace(vertex1,face.v2,vertex2);
  end
  else if startVertex.equals(face.v2) and endVertex.equals(face.v1) then begin
    addFace(face.v1,vertex2,vertex1);
    addFace(face.v1,vertex1,face.v3);
    addFace(vertex2,face.v2,vertex1);
  end
  else if startVertex.equals(face.v2) and endVertex.equals(face.v3) then  begin
    addFace(face.v2,vertex1,vertex2);
    addFace(face.v2,vertex2,face.v1);
    addFace(vertex1,face.v3,vertex2);
  end
  else if startVertex.equals(face.v3) and endVertex.equals(face.v2) then begin
    addFace(face.v2,vertex2,vertex1);
    addFace(face.v2,vertex1,face.v1);
    addFace(vertex2,face.v3,vertex1);
  end
  else if startVertex.equals(face.v3) and endVertex.equals(face.v1) then begin
    addFace(face.v3,vertex1,vertex2);
    addFace(face.v3,vertex2,face.v2);
    addFace(vertex1,face.v1,vertex2);
  end
  else begin
    addFace(face.v3,vertex2,vertex1);
    addFace(face.v3,vertex1,face.v2);
    addFace(vertex2,face.v1,vertex1);
  end;
end;

// Face breaker for FACE-FACE-FACE (a point only)
//   @param facePos face position on the faces array
//   @param newPos new vertex position
procedure T3DBoolObject3D.breakFaceInThree(facePos: integer; newPos: T3DBoolPoint3d);
var
 face: T3DBoolFace;
 vertex: T3DBoolVertex;
begin
 face := m_lstFaces[facePos];
 m_lstFaces.delete(facePos);
 vertex := addVertex(newPos,face.v1.getColor(),enVertexFaceType.SAME);   // = BOUNDARY
 addFace(face.v1,face.v2,vertex);
 addFace(face.v2,face.v3,vertex);
 addFace(face.v3,face.v1,vertex);
end;

// Face breaker for EDGE-FACE-FACE / FACE-FACE-EDGE
//   @param facePos face position on the faces array
//   @param newPos1 new vertex position
//   @param newPos2 new vertex position
//   @param endVertex vertex used for the split
procedure T3DBoolObject3D.breakFaceInFour(facePos: integer; newPos1, newPos2: T3DBoolPoint3d; endVertex: T3DBoolVertex);
var
 face: T3DBoolFace;
 vertex1, vertex2: T3DBoolVertex;
begin
 face := m_lstFaces[facePos];
 m_lstFaces.delete(facePos);
 vertex1 := addVertex(newPos1,face.v1.getColor(),enVertexFaceType.SAME);    // = BOUNDARY
 vertex2 := addVertex(newPos2,face.v1.getColor(),enVertexFaceType.SAME);    // = BOUNDARY
 if endVertex.equals(face.v1) then begin
   addFace(face.v1,vertex1,vertex2);
   addFace(vertex1,face.v2,vertex2);
   addFace(face.v2,face.v3,vertex2);
   addFace(face.v3,face.v1,vertex2);
 end
 else if endVertex.equals(face.v2) then begin
   addFace(face.v2,vertex1,vertex2);
   addFace(vertex1,face.v3,vertex2);
   addFace(face.v3,face.v1,vertex2);
   addFace(face.v1,face.v2,vertex2);
 end
 else begin
   addFace(face.v3,vertex1,vertex2);
   addFace(vertex1,face.v1,vertex2);
   addFace(face.v1,face.v2,vertex2);
   addFace(face.v2,face.v3,vertex2);
 end;
end;

// Face breaker for FACE-FACE-FACE
//   @param facePos face position on the faces array
//   @param newPos1 new vertex position
//   @param newPos2 new vertex position
//   @param linedVertex what vertex is more lined with the interersection found
procedure T3DBoolObject3D.breakFaceInFive(facePos : integer; newPos1, newPos2: T3DBoolPoint3d; linedVertex : integer);
var
 cont : Real3D;
 face : T3DBoolFace;
 vertex2 : T3DBoolVertex;
 vertex1 : T3DBoolVertex;
begin
 face := m_lstFaces[facePos];
 m_lstFaces.delete(facePos);
 vertex1 := addVertex(newPos1,face.v1.getColor(),enVertexFaceType.SAME);     // = BOUNDARY
 vertex2 := addVertex(newPos2,face.v1.getColor(),enVertexFaceType.SAME);     // = BOUNDARY
 cont := 0.0;
 if linedVertex = 1 then begin
   addFace(face.v2,face.v3,vertex1);
   addFace(face.v2,vertex1,vertex2);
   addFace(face.v3,vertex2,vertex1);
   addFace(face.v2,vertex2,face.v1);
   addFace(face.v3,face.v1,vertex2);
 end
 else if linedVertex = 2 then begin
   addFace(face.v3,face.v1,vertex1);
   addFace(face.v3,vertex1,vertex2);
   addFace(face.v1,vertex2,vertex1);
   addFace(face.v3,vertex2,face.v2);
   addFace(face.v1,face.v2,vertex2);
 end
 else begin
   addFace(face.v1,face.v2,vertex1);
   addFace(face.v1,vertex1,vertex2);
   addFace(face.v2,vertex2,vertex1);
   addFace(face.v1,vertex2,face.v3);
   addFace(face.v2,face.v3,vertex2);
 end;
end;

// Classify faces as being inside, outside or on boundary of other object
//   @param object object 3d used for the comparison
procedure T3DBoolObject3D.classifyFaces(anObject : T3DBoolObject3D);
var
 face : T3DBoolFace;
 i, l_iNumFaces : Integer;
begin
 l_iNumFaces := self.getNumFaces();
 // calculate adjacency (Nachbarschaft) information
 for i := 0 to l_iNumFaces-1 do begin
   face := self.getFace(i);
   face.v1.addAdjacentVertex(face.v2);
   face.v1.addAdjacentVertex(face.v3);
   face.v2.addAdjacentVertex(face.v1);
   face.v2.addAdjacentVertex(face.v3);
   face.v3.addAdjacentVertex(face.v1);
   face.v3.addAdjacentVertex(face.v2);
 end;
 // for each face
 for i := 0 to l_iNumFaces-1 do begin
   face := getFace(i);
   if not face.simpleClassify() then begin
     // makes the ray trace classification
     face.rayTraceClassify(anObject);
     // mark the vertices
     if (face.v1.getStatus() = enVertexFaceType.UNKNOWN) then begin
       face.v1.mark(face.getStatus());
     end;
     if (face.v2.getStatus() = enVertexFaceType.UNKNOWN) then begin
       face.v2.mark(face.getStatus());
     end;
     if (face.v3.getStatus() = enVertexFaceType.UNKNOWN) then begin
       face.v3.mark(face.getStatus());
     end;
   end;
 end;
end;

// Inverts faces classified as INSIDE, making its normals point outside. Usually
// used into the second solid when the difference is applied. */
procedure T3DBoolObject3D.invertInsideFaces();
var
 face : T3DBoolFace;
 i : integer;
begin
 for i := 0 to getNumFaces()-1 do begin
   face := self.getFace(i);
   if (face.getStatus() = enVertexFaceType.INSIDE) then begin
     face.invert();
   end;
 end;
end;

end.
