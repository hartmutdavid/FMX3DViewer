unit uGenMeshChoice;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, FMX.Objects3D,
  uMeshUtils;

function ExecuteGenerator(const i_iIdxGenerator: Integer; io_oModel3DForMesh: TOwnModel3D): String;

implementation

uses uGenSquareMesh, uGenPipeMesh, uGenShaftMesh;

function ExecuteGenerator(const i_iIdxGenerator: Integer; io_oModel3DForMesh: TOwnModel3D): String;
var
 l_oMesh: TOwnMesh;
begin
 Result := '';
 l_oMesh := Nil;
 io_oModel3DForMesh.Clear;
 case i_iIdxGenerator of
   1: begin
     l_oMesh := uGenSquareMesh.GetSquareMesh();
     Result := 'SquareMesh';
   end;
   2: begin
     l_oMesh := uGenPipeMesh.GetPipeSectionMesh();
     Result := 'PipeSectionMesh';
   end;
   3: begin
     l_oMesh := uGenPipeMesh.GetPipeBendMesh();
     Result := 'PipeBendMesh';
   end;
   4: begin
     l_oMesh := uGenPipeMesh.GetComplexPipeMesh();
     Result := 'ComplexPipeMesh';
   end;
   5: begin
     l_oMesh := uGenShaftMesh.GetShaftSectionMesh();
     Result := 'ShaftSectionMesh';
   end;
   6: begin
     l_oMesh := uGenShaftMesh.GetConeSectionMesh();
     Result := 'ConeSectionMesh';
   end;
   7: begin
     l_oMesh := uGenShaftMesh.GetComplexShaftMesh();
     Result := 'ComplexShaftMesh';
   end;
 end;
 if Assigned(l_oMesh) then begin
   if (i_iIdxGenerator >= 1) and (i_iIdxGenerator <= 4) then begin
     io_oModel3DForMesh.AddObject(l_oMesh);
     io_oModel3DForMesh.Scale.X := 6;
     io_oModel3DForMesh.Scale.Y := 6;
     io_oModel3DForMesh.Scale.Z := 6;
     io_oModel3DForMesh.Repaint;
   end
   else if (i_iIdxGenerator >= 5) and (i_iIdxGenerator <= 7) then begin
     io_oModel3DForMesh.AddObject(l_oMesh);
     io_oModel3DForMesh.Scale.X := 3;
     io_oModel3DForMesh.Scale.Y := 3;
     io_oModel3DForMesh.Scale.Z := 3;
     io_oModel3DForMesh.Repaint;
   end;
 end;
end;

end.
