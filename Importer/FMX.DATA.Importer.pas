unit FMX.DATA.Importer;

interface

{$SCOPEDENUMS ON}

uses
  System.Classes, System.SysUtils, FMX.Types3D, FMX.Import, FMX.Objects3D, System.Math.Vectors,
{$ifdef CODESITE}
  CodeSiteLogging,
{$endif}
 System.Types;

type
  TDATAModelImporter = class(TModelImporter)
  public
    function GetDescription: string; override;
    function GetExt: string; override;

    function LoadFromFile(const AFileName: string;
      out AMesh: TMeshDynArray; const AOwner: TComponent): boolean; override;
  end;

implementation

uses
  FMX.DATA.Model;

{ TDATAModelImporter }

function TDATAModelImporter.GetDescription: string;
begin
  Result := 'Native Mesh Data';
end;

function TDATAModelImporter.GetExt: string;
begin
  Result := 'DATA';
end;

function TDATAModelImporter.LoadFromFile(const AFileName: string;
  out AMesh: TMeshDynArray; const AOwner: TComponent): Boolean;
var
 l_lOk: Boolean;
 i, l_iMaxIdx, idxIndexBuffer: Integer;
 l_oDATAModel: TDATAModel;
 x, y, z: Single;
begin
 l_oDATAModel := TDATAModel.Create();
 l_oDATAModel.LoadFromFile(AFileName);
 SetLength(AMesh, 1);
 AMesh[0] := TMesh.Create(AOwner);
 l_iMaxIdx := 0;
 for i := 0 to High(l_oDATAModel.DataPrimitives) do begin
   l_lOk := True;
   if i = 0 then begin
     if (l_oDATAModel.DataPrimitives[i].Node1 = l_oDATAModel.DataPrimitives[i].Node2) and
        (l_oDATAModel.DataPrimitives[i].Node2 = l_oDATAModel.DataPrimitives[i].Node3) then
       l_lOk := False;
   end;
   if l_lOk then begin
     if l_oDATAModel.DataPrimitives[i].Node4 = $FFFF then
       l_iMaxIdx := l_iMaxIdx + 3
     else
       l_iMaxIdx := l_iMaxIdx + 12;
   end;
 end;
 AMesh[0].Data.IndexBuffer.Length  := l_iMaxIdx;
 AMesh[0].Data.VertexBuffer.Length := Length(l_oDATAModel.DataNodes);
 for i := 0 to High(l_oDATAModel.DataNodes) do begin
   x := l_oDATAModel.DataNodes[i].X;
   y := l_oDATAModel.DataNodes[i].Y;
   z := l_oDATAModel.DataNodes[i].Z;
   AMesh[0].Data.VertexBuffer.Vertices[i] := Point3D(x,y,z);
   //-- AMesh[0].Data.VertexBuffer.TexCoord0[i]:= PointF(l_oDATAModel.DataNodes[i].Intensity*0.001,0);
 end;
 idxIndexBuffer := 0;
 for i := 0 to High(l_oDATAModel.DataPrimitives) do begin
   l_lOk := True;
   if i = 0 then begin
     if (l_oDATAModel.DataPrimitives[i].Node1 = l_oDATAModel.DataPrimitives[i].Node2) and
        (l_oDATAModel.DataPrimitives[i].Node2 = l_oDATAModel.DataPrimitives[i].Node3) then
       l_lOk := False;
   end;
   if l_lOk then begin
     if l_oDATAModel.DataPrimitives[i].Node4 = $FFFF then begin
       // triangle
       AMesh[0].Data.IndexBuffer[idxIndexBuffer + 0] := l_oDATAModel.DataPrimitives[i].Node1;
       AMesh[0].Data.IndexBuffer[idxIndexBuffer + 1] := l_oDATAModel.DataPrimitives[i].Node2;
       AMesh[0].Data.IndexBuffer[idxIndexBuffer + 2] := l_oDATAModel.DataPrimitives[i].Node3;
       idxIndexBuffer := idxIndexBuffer + 3;
     end
     else begin
       // Rechteck
       //   4----------3
       //    |        |
       //    |        |
       //   1----------2
       AMesh[0].Data.IndexBuffer[idxIndexBuffer + 0] := l_oDATAModel.DataPrimitives[i].Node1;
       AMesh[0].Data.IndexBuffer[idxIndexBuffer + 1] := l_oDATAModel.DataPrimitives[i].Node2;
       AMesh[0].Data.IndexBuffer[idxIndexBuffer + 2] := l_oDATAModel.DataPrimitives[i].Node3;
       AMesh[0].Data.IndexBuffer[idxIndexBuffer + 3] := l_oDATAModel.DataPrimitives[i].Node1;
       AMesh[0].Data.IndexBuffer[idxIndexBuffer + 4] := l_oDATAModel.DataPrimitives[i].Node4;
       AMesh[0].Data.IndexBuffer[idxIndexBuffer + 5] := l_oDATAModel.DataPrimitives[i].Node3;

       AMesh[0].Data.IndexBuffer[idxIndexBuffer + 6] := l_oDATAModel.DataPrimitives[i].Node1;
       AMesh[0].Data.IndexBuffer[idxIndexBuffer + 7] := l_oDATAModel.DataPrimitives[i].Node3;
       AMesh[0].Data.IndexBuffer[idxIndexBuffer + 8] := l_oDATAModel.DataPrimitives[i].Node4;
       AMesh[0].Data.IndexBuffer[idxIndexBuffer + 9] := l_oDATAModel.DataPrimitives[i].Node1;
       AMesh[0].Data.IndexBuffer[idxIndexBuffer +10] := l_oDATAModel.DataPrimitives[i].Node4;
       AMesh[0].Data.IndexBuffer[idxIndexBuffer +11] := l_oDATAModel.DataPrimitives[i].Node2;

       idxIndexBuffer := idxIndexBuffer + 12;
       //-- AMesh[0].Data.IndexBuffer[idxIndexBuffer + 0] := l_oDATAModel.DataPrimitives[i].Node1;
       //-- AMesh[0].Data.IndexBuffer[idxIndexBuffer + 1] := l_oDATAModel.DataPrimitives[i].Node4;
       //-- AMesh[0].Data.IndexBuffer[idxIndexBuffer + 2] := l_oDATAModel.DataPrimitives[i].Node2;
       //-- idxIndexBuffer := idxIndexBuffer + 3;
     end;
   end;
 end;
 AMesh[0].Data.CalcFaceNormals(True);
 l_oDATAModel.Free;
 Result := True;
end;

var
 DATAImporterId: Integer;

initialization
  DATAImporterId := TModelImportServices.RegisterImporter(TDATAModelImporter.Create);

finalization
  TModelImportServices.UnregisterImporter(DATAImporterId);

end.
