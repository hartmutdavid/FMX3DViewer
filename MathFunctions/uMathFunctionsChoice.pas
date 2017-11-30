unit uMathFunctionsChoice;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.UIConsts,
  System.Math, System.Math.Vectors,
  FMX.Graphics, FMX.Objects3D, FMX.Types3D, FMX.MaterialSources, uMeshUtils;


function GetMathFunctionMesh(const i_iIdxMathFunction: Integer;
                             var o_fMinX, o_fMaxX,
                                 o_fMinY, o_fMaxY,
                                 o_fMinZ, o_fMaxZ: Double): TOwnMesh;
function ExecuteMathFunction(const i_iIdxMathFunction: Integer;
                             i_oModel3D: TOwnModel3D;
                             var o_fMinX, o_fMaxX,
                                 o_fMinY, o_fMaxY,
                                 o_fMinZ, o_fMaxZ: Double): String;

implementation

function GetMathFunctionDescriptionName(i_iIdxMathFunction: Integer): String;
begin
 case i_iIdxMathFunction of
   1 : Result := 'MathFunction: -2000*Sin((x^2+y^2)/180*Pi)/(x^2+y^2)';
   2 : Result := 'MathFunction: -5*Exp(Sin(x/10)-Cos(y/10))';
   3 : Result := 'MathFunction: -5*Exp(Sin(x/10)+Cos(y/10))';
   4 : Result := 'MathFunction: -5*Exp(Sin(Sqrt(Abs(x)))+Cos(Sqrt(Abs(y))))';
   5 : Result := 'MathFunction: -7*Exp(ArcTan2(x^2,y^2))';
   6 : Result := 'MathFunction: sin(x^2 + y^2)*5.0';
   7 : Result := 'MathFunction: sin(x)*cos(y)*5.0';
   8 : Result := 'MathFunction: 0.75/Exp(x^2*y^2)*5';
   9 : Result := 'MathFunction: x*y^3-y*x^3';
  10 : Result := 'MathFunction: (x^2+3*y^2)*e^(-x^2-y^2)';
  11 : Result := 'MathFunction: -x*y*e^(-x^2-y^2)';
  12 : Result := 'MathFunction: x^2+y^2';
  13 : Result := 'MathFunction: cos(abs(x)+abs(y))';
  14 : Result := 'MathFunction: cos(abs(x)+abs(y))*(abs(x)+abs(y))';
  else Result := 'MathFunction: ???';
 end;
end;

function MathFunction(const i_iIdxMathFunction: Integer; x, y: Double): Double;
var
 l_fTmp : Double;
begin
 l_fTmp := x*x + y*y;
 if l_fTmp < Epsilon then
   l_fTmp := Epsilon;
 case i_iIdxMathFunction of
   1 : Result := -2000*Sin(l_fTmp/180*Pi)/l_fTmp;
   2 : Result := -5*Exp(Sin(x/10)-Cos(y/10));
   3 : Result := -5*Exp(Sin(x/10)+Cos(y/10));
   4 : Result := -5*Exp(Sin(Sqrt(Abs(x)))+Cos(Sqrt(Abs(y))));
   5 : Result := -7*Exp(ArcTan2(x*x,y*y));
   6 : Result := sin(x*x + y*y)*5.0;
   7 : Result := sin(x)*cos(y)*5.0;
   8 : Result := 0.75/Exp(Power(x,2.0)*Power(y,2.0))*5.0;  // 0.75/Exp(x^2*y^2)*5
   9 : Result := x*Power(y,3.0)-y*Power(x,3.0);      // x*y^3-y*x^3
  10 : Result := (x*x+3*y*y)*exp(-x*x-y*y);          // (x^2+3*y^2)*e^(-x^2-y^2)
  11 : Result := -x*y*exp(-x*x-y*y);                 // -x*y*e^(-x^2-y^2)
  12 : Result := x*x+y*y;                            // x^2+y^2
  13 : Result := cos(abs(x)+abs(y));                 // cos(abs(x)+abs(y))
  14 : Result := cos(abs(x)+abs(y))*(abs(x)+abs(y)); // cos(abs(x)+abs(y))*(abs(x)+abs(y))
  else Result := 0.0;
 end;
end;

function GetMathFunctionMesh(const i_iIdxMathFunction: Integer;
                             var o_fMinX, o_fMaxX,
                                 o_fMinY, o_fMaxY,
                                 o_fMinZ, o_fMaxZ: Double): TOwnMesh;
const
 MathFunction_MaxX = 30;
 MathFunction_MaxY = 30;
 MathFunction_MinZ =  2000.0;
 MathFunction_MaxZ = -2000.0;
var
 k, j, n:  Integer;
 u, v : Double;
 P : array [0..3] of TPoint3D;
 d : Double;
 NP, NI : Integer;
 Bmp:     TBitmap;
 BmpData: TBitmapData;
 ColTexture: TTextureMaterialSource;
 l_oMesh: TOwnMesh;
begin
 o_fMinX := -MathFunction_MaxX;
 o_fMaxX := MathFunction_MaxX;
 o_fMinY := -MathFunction_MaxY;
 o_fMaxY := MathFunction_MaxY;
 o_fMinZ := MathFunction_MinZ;
 o_fMaxZ := MathFunction_MaxZ;

 l_oMesh := TOwnMesh.Create(Nil);

 l_oMesh.Data.Clear;
 l_oMesh.WrapMode := TMeshWrapMode.Original;

 d := 1.0;

 NP := 0;
 NI := 0;

 // We have to set these up front. The buffers are cleared every time Length is set.
 l_oMesh.Data.VertexBuffer.Length := Round(2*MathFunction_MaxX*2*MathFunction_MaxY/d/d)*4;
 l_oMesh.Data.IndexBuffer.Length  := Round(2*MathFunction_MaxX*2*MathFunction_MaxY/d/d)*6;

 Bmp := TBitmap.Create(20,360);
 Try
   if Bmp.Map(TMapAccess.ReadWrite,BmpData)then
     for k := 0 to 359 do
       for j:=0 to 19 do
         BmpData.SetPixel(j,k, CorrectColor(HSLtoRGB(k/360,0.75,0.5)));

   Bmp.Unmap(BmpData);
   u := -MathFunction_MaxX;
   while u < MathFunction_MaxX do begin
     v := -MathFunction_MaxY;
     while v < MathFunction_MaxY do begin
       // Set up the points in the XY plane
       P[0].x := u;
       P[0].y := v;
       P[1].x := u+d;
       P[1].y := v;
       P[2].x := u+d;
       P[2].y := v+d;
       P[3].x := u;
       P[3].y := v+d;

       // Calculate the corresponding function values for Y = f(X,Y)
       P[0].z := MathFunction(i_iIdxMathFunction, P[0].x, P[0].y);
       P[1].z := MathFunction(i_iIdxMathFunction, P[1].x, P[1].y);
       P[2].z := MathFunction(i_iIdxMathFunction, P[2].x, P[2].y);
       P[3].z := MathFunction(i_iIdxMathFunction, P[3].x, P[3].y);
       if P[0].z < o_fMinZ then o_fMinZ := P[0].z;
       if P[0].z > o_fMaxZ then o_fMaxZ := P[0].z;
       if P[1].z < o_fMinZ then o_fMinZ := P[1].z;
       if P[1].z > o_fMaxZ then o_fMaxZ := P[1].z;
       if P[2].z < o_fMinZ then o_fMinZ := P[2].z;
       if P[2].z > o_fMaxZ then o_fMaxZ := P[2].z;
       if P[3].z < o_fMinZ then o_fMinZ := P[3].z;
       if P[3].z > o_fMaxZ then o_fMaxZ := P[3].z;

       with l_oMesh.Data do begin
         // Set the points
         with VertexBuffer do begin
           Vertices[NP+0] := P[0];
           Vertices[NP+1] := P[1];
           Vertices[NP+2] := P[2];
           Vertices[NP+3] := P[3];
         end;

         // Map the colors
         with VertexBuffer do begin
           TexCoord0[NP+0] := PointF(0,(P[0].z+35)/45);
           TexCoord0[NP+1] := PointF(0,(P[1].z+35)/45);
           TexCoord0[NP+2] := PointF(0,(P[2].z+35)/45);
           TexCoord0[NP+3] := PointF(0,(P[3].z+35)/45);
         end;

         // Map the triangles
         IndexBuffer[NI+0] := NP+1;
         IndexBuffer[NI+1] := NP+2;
         IndexBuffer[NI+2] := NP+3;
         IndexBuffer[NI+3] := NP+3;
         IndexBuffer[NI+4] := NP+0;
         IndexBuffer[NI+5] := NP+1;
       end;

       NP := NP + 4;
       NI := NI + 6;

       v := v + d;
     end;
     u := u + d;
   end;
   ColTexture:= TTextureMaterialSource.Create(l_oMesh);
   ColTexture.Parent := l_oMesh;
   ColTexture.Texture.Assign(Bmp);
 Finally
   Bmp.Free;
 End;
 l_oMesh.MaterialSource := ColTexture;
 l_oMesh.Data.CalcFaceNormals;
 Result := l_oMesh;
end;

function ExecuteMathFunction(const i_iIdxMathFunction: Integer;
                             i_oModel3D: TOwnModel3D;
                             var o_fMinX, o_fMaxX,
                                 o_fMinY, o_fMaxY,
                                 o_fMinZ, o_fMaxZ: Double): String;
var
 l_oMesh: TOwnMesh;
begin
 Result := '';
 l_oMesh := GetMathFunctionMesh(i_iIdxMathFunction,
                                o_fMinX, o_fMaxX,
                                o_fMinY, o_fMaxY,
                                o_fMinZ, o_fMaxZ);
 if Assigned(l_oMesh) then begin
   i_oModel3D.AddObject(l_oMesh);
   l_oMesh.Visible := True;
   Result  := GetMathFunctionDescriptionName(i_iIdxMathFunction);
 end;
end;

end.
