unit FMX.STEP.Importer;

interface

{$SCOPEDENUMS ON}

uses
 System.Classes, FMX.Import, FMX.Objects3D;

type
  TSTEPModelImporter = class(TModelImporter)
  public
    function GetDescription: string; override;
    function GetExt: string; override;

    function LoadFromFile(const AFileName: string;
      out AMesh: TMeshDynArray; const AOwner: TComponent): boolean; override;
  end;

implementation

uses
  System.SysUtils, System.Math.Vectors, FMX.STEP.Model;

{ TSTEPModelImporter }

function TSTEPModelImporter.GetDescription: string;
begin
  Result := 'STEP Importer';
end;

function TSTEPModelImporter.GetExt: string;
begin
  Result := 'STP';
end;

function TSTEPModelImporter.LoadFromFile(const AFileName: string;
  out AMesh: TMeshDynArray; const AOwner: TComponent): Boolean;
var
 i, j, idx :  Integer;
 //-- LSTEPMesh :  TSTEPMesh;
 l_oSTEPModel : TSTEPModel;
begin
 l_oSTEPModel := TSTEPModel.Create();
 l_oSTEPModel.LoadFromFile(AFileName);

 AMesh := nil;
 idx := 0;

 {
  for i := 0 to High(LASEModel.Meshes) do
  begin
    LASEMesh := LASEModel.Meshes[i];
    SetLength(AMesh, Length(AMesh) + Length(LASEMesh.FSubMeshes));
    for j := 0 to High(LASEMesh.FSubMeshes) do
    begin
      AMesh[idx] := LASEMesh.FSubMeshes[j].CreateMesh(AOwner, TMatrix3D.Identity,
        LMaterials.Materials);
      Inc(idx);
    end;
  end;
 }
 
 l_oSTEPModel.Free;
 Result := True;
end;

var
  STEPImporterId: Integer;

initialization
  STEPImporterId := TModelImportServices.RegisterImporter(TSTEPModelImporter.Create);

finalization
  TModelImportServices.UnregisterImporter(STEPImporterId);

end.
