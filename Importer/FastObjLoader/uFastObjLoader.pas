unit uFastObjLoader;

interface

//load and save meshes in WaveFront OBJ format

{
Zur Real-Konvertierung diese Funktion verwenden, aus System.SysUtil:

function TextToFloat(const S: string; var Value: Double;
  const AFormatSettings: TFormatSettings): Boolean; overload;

}

uses
    Classes, sysutils,  uDefineTypesCtm;

procedure Load3DObjModel(const FileName: string;
                  var faces: TFaces;
                  var vertices: TVertices;
                  var vertexRGBA : TVertexRGBA);
procedure Save3DObjModel(const FileName: string;
                  var faces: TFaces;
                  var vertices: TVertices;
                  var vertexRGBA : TVertexRGBA);


implementation

function FSize (lFName: String): longint;
var
 F : File Of byte;
begin
 result := 0;
 if not fileexists(lFName) then 
   exit;
 Assign (F, lFName);
 Reset (F);
 result := FileSize(F);
 Close (F);
end;

procedure Load3DObjModel(const FileName: string;
                   var faces: TFaces;
                   var vertices: TVertices;
                   var vertexRGBA : TVertexRGBA);
const
  kBlockSize = 8192;
var
 l_lOk: Boolean;
 f: TextFile;
 fsz : int64;
 d: Double;
 l_sLine, l_sStr: string;
 l_lstLineParts: TStringList;
 i, j, li, num_v, num_f, new_f: integer;
 l_oUSFormatSettings: TFormatSettings;
 l_lstLines: TStringList;
begin
 fsz := FSize (FileName);
 if fsz < 32 then
   exit;
 //init values
 l_oUSFormatSettings := TFormatSettings.Invariant;
 num_v := 0;
 num_f := 0;
 l_lstLineParts := TStringList.Create;
 setlength(vertices, (fsz div 70)+kBlockSize); //guess number of faces based on filesize to reduce reallocation frequencey
 setlength(faces, (fsz div 35)+kBlockSize); //guess number of vertices based on filesize to reduce reallocation frequencey
 setlength(vertexRGBA,0);
 //load faces and vertices
 l_lstLines := TStringList.Create;
 l_lstLines.LoadFromFile(FileName);
 for li := 0 to l_lstLines.Count-1 do begin
   l_sLine := l_lstLines[li];
   if length(l_sLine) < 7 then
     continue;
   if (l_sLine[1] <> 'v') and (l_sLine[1] <> 'f') then
     continue; //only read 'f'ace and 'v'ertex lines
   if (l_sLine[2] = 'p') or (l_sLine[2] = 'n') or (l_sLine[2] = 't') then
     continue; //ignore vp/vn/vt data: avoid delimiting text yields 20% faster loads
   l_lstLineParts.DelimitedText := l_sLine;
   if l_lstLineParts.count > 3 then begin
     if l_lstLineParts[0] = 'f' then begin
       //warning: need to handle "f v1/vt1/vn1 v2/vt2/vn2 v3/vt3/vn3"
       //warning: face could be triangle, quad, or more vertices!
       new_f := l_lstLineParts.count - 3;
       if ((num_f+new_f) >= length(faces)) then
         setlength(faces, length(faces)+new_f+kBlockSize);
       for i := 1 to (l_lstLineParts.count-1) do begin
         if (pos('/', l_lstLineParts[i]) > 1) then begin
           // f v1 v2 v3
           // f v1//vn1 v2//vn2 v3//vn3
           // f v1/vt1/vn1 v2/vt2/vn2 v3/vt3/vn3
           l_lstLineParts[i] := Copy(l_lstLineParts[i], 1, pos('/', l_lstLineParts[i])-1);
         end;
       end;
       j := 1;
       if new_f = 1 then begin
         // f v1 v2 v3
         faces[num_f].X := StrToUInt(l_lstLineParts[1])   - 1;
         faces[num_f].Y := StrToUInt(l_lstLineParts[2]) - 1;
         faces[num_f].Z := StrToUInt(l_lstLineParts[3]) - 1;
         inc(num_f);
       end
       else if new_f = 2 then begin
         // f v1 v2 v3 v4
         faces[num_f].X := StrToUInt(l_lstLineParts[2]) - 1;
         faces[num_f].Y := StrToUInt(l_lstLineParts[3]) - 1;
         faces[num_f].Z := StrToUInt(l_lstLineParts[4]) - 1;
         inc(num_f);
         faces[num_f].X := StrToUInt(l_lstLineParts[4]) - 1;
         faces[num_f].Y := StrToUInt(l_lstLineParts[1]) - 1;
         faces[num_f].Z := StrToUInt(l_lstLineParts[2]) - 1;
         inc(num_f);
       end
       else begin
         for j := 1 to (new_f) do begin
           faces[num_f].X := StrToUInt(l_lstLineParts[j])   - 1; //-1 since "A valid vertex index starts from 1"
           faces[num_f].Y := StrToUInt(l_lstLineParts[j+1]) - 1; //-1 since "A valid vertex index starts from 1"
           faces[num_f].Z := StrToUInt(l_lstLineParts[j+2]) - 1; //-1 since "A valid vertex index starts from 1"
           inc(num_f);
         end;
       end;
     end
     else if l_lstLineParts[0] = 'v' then begin
       if ((num_v+1) >= length(vertices)) then
         SetLength(vertices, length(vertices)+kBlockSize);
       l_lOk := TextToFloat(l_lstLineParts[1], vertices[num_v].X, l_oUSFormatSettings);
       l_lOk := TextToFloat(l_lstLineParts[2], vertices[num_v].Y, l_oUSFormatSettings);
       l_lOk := TextToFloat(l_lstLineParts[3], vertices[num_v].Z, l_oUSFormatSettings);
       if l_lstLineParts.count >= 7 then begin
         // v  x  y  z                4x  ohne Farben
         // v  x  y  z  R  G  B       7x  with RGB
         // v  x  y  z  R  G  B  A    8x  with RGBA
         if ((num_v+1) >= length(vertexRGBA)) then
           SetLength(vertexRGBA, length(vertices));
         l_sStr := Copy(l_lstLineParts[4],1,6);
         l_lOk := TextToFloat(l_sStr, d, l_oUSFormatSettings);
         vertexRGBA[num_v].R := Trunc(d * 255.0);
         l_sStr := Copy(l_lstLineParts[5],1,6);
         l_lOk := TextToFloat(l_sStr, d, l_oUSFormatSettings);
         vertexRGBA[num_v].G := Trunc(d * 255.0);
         l_sStr := Copy(l_lstLineParts[6],1,6);
         l_lOk := TextToFloat(l_sStr, d, l_oUSFormatSettings);
         vertexRGBA[num_v].B := Trunc(d * 255.0);
         if l_lstLineParts.count = 8 then begin
           l_sStr := Copy(l_lstLineParts[7],1,6);
           l_lOk := TextToFloat(l_sStr, d, l_oUSFormatSettings);
           vertexRGBA[num_v].A := Trunc(d * 255.0);
         end
         else begin
           vertexRGBA[num_v].A := 200;
         end;
       end;
       inc(num_v);
     end;
   end;
 end;
 l_lstLines.Clear;
 l_lstLines.Free;
 l_lstLineParts.free;
 setlength(faces, num_f);
 setlength(vertices, num_v);
 if Length(vertexRGBA) > 0 then
   SetLength(vertexRGBA, num_v);
end; // LoadObj()

procedure Save3DObjModel(const FileName: string;
                   var faces: TFaces;
                   var vertices: TVertices;
                   var vertexRGBA : TVertexRGBA);
//create WaveFront object file
// https://en.wikipedia.org/wiki/Wavefront_.obj_file
var
   f : TextFile;
   FileNameObj: string;
   i : integer;
begin
  if (length(faces) < 1) or (length(vertices) < 3) then begin
     writeln('You need to open a mesh before you can save it');
     exit;
  end;
  FileNameObj := changeFileExt(FileName, '.obj');
  AssignFile(f, FileNameObj);
  ReWrite(f);
  WriteLn(f, '# WaveFront Object format image created with Surf Ice');
  for i := 0 to (length(vertices)-1) do
      WriteLn(f, 'v ' + floattostr(vertices[i].X)+' '+floattostr(vertices[i].Y)+' '+ floattostr(vertices[i].Z));
  for i := 0 to (length(faces)-1) do
      WriteLn(f, 'f ' + inttostr(faces[i].X+1)+' '+inttostr(faces[i].Y+1)+' '+ inttostr(faces[i].Z+1)); //+1 since "A valid vertex index starts from 1 "
  CloseFile(f);
end;


end.

