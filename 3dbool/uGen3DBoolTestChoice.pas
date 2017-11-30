unit uGen3DBoolTestChoice;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, FMX.Objects3D,
  uMeshUtils;

function Execute3DBoolTest(const i_iIdxChoice: Integer; io_oModel3DForMesh: TOwnModel3D): String;

implementation

uses uGen3DBoolTest1, uGen3DBoolTest2;

function Execute3DBoolTest(const i_iIdxChoice: Integer; io_oModel3DForMesh: TOwnModel3D): String;
begin
 Result := '';
 case i_iIdxChoice of
   1: begin
     Result := uGen3DBoolTest1.ExecuteGen3DBoolTest1(io_oModel3DForMesh);
   end;
   2: begin
     Result := uGen3DBoolTest2.ExecuteGen3DBoolTest2(io_oModel3DForMesh);
   end;
 end;
end;

end.
