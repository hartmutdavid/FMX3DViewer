unit FMX.DATA.Model;

interface

{$SCOPEDENUMS ON}

uses
  System.SysUtils, System.Classes, System.Types, FMX.Types3D, FMX.Types,
  FMX.Objects3D, System.Math, FMX.Import, System.UITypes, System.Math.Vectors;

type
 // Structures used in our binary file
 // The structure is quite simplified here, original data came from a FEM
 // package and was in (huge) text files, and parsing text files is not the
 // purpose of this demo, so data was simplified ;)
 TDataNode = record
   X, Y, Z : Single;
   Intensity : Single;
 end;
 TDataPrimitive = record
   Node1, Node2, Node3, Node4 : Word;  // if Node4 is $FFFF, codes a triangle
 end;
 TLstDataNodes = array of TDataNode;
 TLstDataPrimitives = array of TDataPrimitive;

 TDATAModel = class(TCustomModel)
 private
   m_lstDataNodes :      TLstDataNodes;
   m_lstDataPrimitives : TLstDataPrimitives;
 public
   procedure LoadFromFile(const i_sFileName: String);  override;
   constructor Create();
   destructor Destroy(); override;
   //
   property DataNodes:      TLstDataNodes      read m_lstDataNodes;
   property DataPrimitives: TLstDataPrimitives read m_lstDataPrimitives;
 end;


implementation

constructor TDATAModel.Create();
begin
 SetLength(m_lstDataNodes, 0);
 SetLength(m_lstDataPrimitives, 0);
end;

destructor TDATAModel.Destroy();
var
  i : Integer;
begin
 inherited Destroy;
 SetLength(m_lstDataNodes, 0);
 SetLength(m_lstDataPrimitives, 0);
end;

procedure TDATAModel.LoadFromFile(const i_sFileName: String);
var
 n : Integer;
 l_oStream : TFileStream;
begin
 SetLength(m_lstDataNodes, 0);
 SetLength(m_lstDataPrimitives, 0);
 l_oStream := TFileStream.Create(i_sFileName, fmOpenRead);
 l_oStream.Read(n, 4);
 SetLength(m_lstDataNodes, n);
 l_oStream.Read(n, 4);
 SetLength(m_lstDataPrimitives, n);
 l_oStream.Read(m_lstDataNodes[0], Length(m_lstDataNodes)*SizeOf(TDataNode));
 l_oStream.Read(m_lstDataPrimitives[0], Length(m_lstDataPrimitives)*SizeOf(TDataPrimitive));
 l_oStream.Free;
end;

end.
