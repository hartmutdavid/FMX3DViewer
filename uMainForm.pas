unit uMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  Generics.Collections, System.IOUtils, System.DateUtils,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Types3D,
  System.Math.Vectors, FMX.Objects3D, FMX.MaterialSources, FMX.Controls3D,
  FMX.Viewport3D, FMX.StdCtrls, FMX.Controls.Presentation, FMX.Layouts,
  FMX.ListBox, uMeshUtils, uDefineTypesCtm,
  LUX.D3, LUX.Brep.Face.TriFlip.D3, LUX.Brep.Face.TriFlip.D3.FMX, FMX.ScrollBox,
  FMX.Memo, FMX.Colors
{$ifdef CODESITE}
  , CodeSiteLogging
{$endif}
  ;

type
 TAchse3D = class;

 TActionType = (atUnknown,atLoadModel,atGenerateMesh,atMathFunction,at3DBool);

 TCursor3D = class(TControl3D)
 private
   FMoveMode: Boolean;
   FOnTracking: TNotifyEvent;
   FPlaneNorm:  TPoint3D;
   fX: TAchse3D;
   fY: TAchse3D;
   fZ: TAchse3D;
   function GetSlave: TControl3D;
 protected
   procedure Render; override;
   procedure MouseDown3D(Button: TMouseButton; Shift: TShiftState; X, Y: Single; RayPos, RayDir: TVector3D); override;
   procedure MouseMove3D(Shift: TShiftState; X, Y: Single; RayPos, RayDir: TVector3D); override;
   procedure MouseUp3D(Button: TMouseButton; Shift: TShiftState; X, Y: Single; RayPos, RayDir: TVector3D); override;
 public
   constructor Create(AOwner: TComponent); override;
   destructor Destroy; override;
   procedure RecalcPos;
   procedure SetNewClient(const value: TFmxObject);
   procedure SwapMove(Sender: TObject);
   procedure DoTracking; virtual;
 published
   property Slave: TControl3D read GetSlave;        // Eigentuemer holen, wenn vorhanden
   property Move: Boolean read FMoveMode;         // Umschalten Drehen / Bewegen
   property OnTracking: TNotifyEvent read FOnTracking write FOnTracking;
 end;

 TAchse3D = class(TControl3D)
 private
   fCursor3D: TCursor3D;
 protected
   procedure Render; override;
   procedure DoMouseEnter; override;
   procedure DoMouseLeave; override;
   procedure MouseDown3D(Button: TMouseButton; Shift: TShiftState; X, Y: Single; RayPos, RayDir: TVector3D); override;
   procedure MouseMove3D(Shift: TShiftState; X, Y: Single; RayPos, RayDir: TVector3D); override;
   procedure MouseUp3D(Button: TMouseButton; Shift: TShiftState; X, Y: Single; RayPos, RayDir: TVector3D); override;
 public
   constructor Create(AOwner: TComponent); override;
 published
   property Cursor3D: TCursor3D read fCursor3D;
 end;

  TMainForm = class(TForm)
    Layout1: TLayout;
    Layout2: TLayout;
    TrackBarForCamera: TTrackBar;
    ButtonCopy: TButton;
    ButtonDel: TButton;
    lbCoord: TLabel;
    Viewport3D: TViewport3D;
    Light: TLight;
    Grid3D: TGrid3D;
    RoundCube: TRoundCube;
    DummyCenter: TDummy;
    chbxMove: TCheckBox;
    btnLoadModel: TButton;
    PanelRight: TPanel;
    cobxMeshGeneratorChoice: TComboBox;
    lbMeshGen: TLabel;
    StatusBar: TStatusBar;
    lblStatus: TLabel;
    btnClear: TButton;
    cobxMathFunctions: TComboBox;
    lbMathFunction: TLabel;
    Text3D: TText3D;
    btnSaveAsObj: TButton;
    btnTransToMeshCollection: TButton;
    btnTransToTriFlipModel: TButton;
    chbxLight: TCheckBox;
    grbxMeshGen: TGroupBox;
    grbxCSG: TGroupBox;
    cobx3DBoolTestChoice: TComboBox;
    btnGenCenterHole: TButton;
    TrackBarForScale: TTrackBar;
    lbCamera: TLabel;
    lbScale: TLabel;
    lbCameraZValue: TLabel;
    lbScaleTrackbarValue: TLabel;
    lbModelName: TLabel;
    lbModelNameValue: TLabel;
    GridLayout1: TGridLayout;
    Label1: TLabel;
    lbXMinValue: TLabel;
    Label3: TLabel;
    lbYMinValue: TLabel;
    Label5: TLabel;
    lbZMinValue: TLabel;
    Label7: TLabel;
    lbXMaxValue: TLabel;
    Label9: TLabel;
    lbYMaxValue: TLabel;
    Label11: TLabel;
    lbZMaxValue: TLabel;
    StrokeCubeForMesh: TStrokeCube;
    btnTest: TButton;
    LightMaterialSource1: TLightMaterialSource;
    PanelMain: TPanel;
    PanelBottom: TPanel;
    Splitter1: TSplitter;
    txaProt: TMemo;
    lbScaleValue: TLabel;
    PanelScale: TPanel;
    grbxMeshLines: TGroupBox;
    chbxShowMeshLines: TCheckBox;
    cobxColorMeshLines: TColorComboBox;
    procedure DummyCenterClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ButtonCopyClick(Sender: TObject);
    procedure ButtonDelClick(Sender: TObject);
    procedure TrackBarForCameraTracking(Sender: TObject);
    procedure Viewport3DMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure Viewport3DMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Single);
    procedure Viewport3DMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; var Handled: Boolean);
    procedure chbxMoveChange(Sender: TObject);
    procedure btnLoadModelClick(Sender: TObject);
    procedure cobxMeshGeneratorChoiceChange(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure cobxMathFunctionsChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnSaveAsObjClick(Sender: TObject);
    procedure btnTransToMeshCollectionClick(Sender: TObject);
    procedure btnTransToTriFlipModelClick(Sender: TObject);
    procedure chbxLightChange(Sender: TObject);
    procedure cobx3DBoolTestChoiceChange(Sender: TObject);
    procedure btnGenCenterHoleClick(Sender: TObject);
    procedure TrackBarForScaleTracking(Sender: TObject);
    procedure btnTestClick(Sender: TObject);
    procedure chbxShowMeshLinesChange(Sender: TObject);
    procedure cobxColorMeshLinesChange(Sender: TObject);
  private
    { Private-Deklarationen }
    m_sModelName:  String;
    updatePos:     TPointF;
    m_oMinCoord, m_oMaxCoord: TPoint3D;   // Min-/Max-Koord.
    m_fModel3DMinMaxDistance: Single;     // Scale Bereich des 3D-Obj
    m_oCenterModel3D:         TPoint3D;   // Berechnete Mitte des 3D-Modells
    m_oMinMaxSizeModel3D:     TPoint3D;   // Groesse des 3D-Modells
    m_oModelBaseScale:        TPoint3D;   // Basismodell-Skalierung
    m_oCameraBasePos:         TPoint3D;   // Kamera-Basisposition
    m_oCameraAngleOfView:     Single;
    //
    m_fRotationCenterX1:     Single;
    m_fRotationCenterY1:     Single;
    m_fRotationAngleX1:      Single;
    m_fRotationAngleY1:      Single;
    m_fRotationCenterX2:     Single;
    m_fRotationCenterY2:     Single;
    m_fRotationAngleX2:      Single;
    m_fRotationAngleY2:      Single;
    //
    // CTM/OBJ-Loader-Daten
    m_lstFaces :      TFaces;
    m_lstVertices:    TVertices;
    m_lstVertexRGBA : TVertexRGBA;
    //
    procedure ClearModel3D;
    procedure Reset3DElements(i_enActionType: TActionType);
    procedure DeleteAllMeshes;
    procedure SetMaterial;
    function  HasMaterialSource(): Boolean;
    procedure RemoveMaterial;
    procedure SetNewModelPosSize;
    procedure SetNewScaleOfTrackBar(i_iModelScaleFactor: Single);
    function  Create3DModelByCtmObjData: Boolean;
    function  HandleMeshLines(): Boolean;
    procedure ProtCurrentMeshCounts;
  public
    { Public-Deklarationen }
    m_oModel3D: TOwnModel3D;
    //
    // TriFlip-Modell
    m_oFaceModel: TTriFaceModel3D;
    //
    MyCursor: TCursor3D;
    procedure Update(Sender: TObject);
  end;

var
  MainForm: TMainForm;

implementation

uses System.Math, FMX.ASE.Importer, FMX.DAE.Importer, FMX.OBJ.Importer,
     LUX.Data.Tree,
     uGenMeshChoice, uMathFunctionsChoice, uGen3DBoolTestChoice, uGlb,
     uGen3DBoolGenCenterHole, uImportDlg
//DAV: >>>
     , uLoaderCtm, uFastObjLoader, uSimplifyMeshCtm
//DAV: <<<
     ;

{$R *.fmx}

function CalcModel3D(const aModel: TModel3D; out aMin, aMax, aMinMaxSizeModel3D, aCenterModel3D: TPoint3D): Single;
var
 l_lFirst: Boolean;
 i, j, n, vCnt: Integer;
 P: TPoint3D;
 F, B: Single;
 l_oMeshData: TMeshData;
 l_lstMeshData: TList<TMeshData>;
begin
 Result := 1.0;
 l_lFirst := True;
 aMinMaxSizeModel3D := TPoint3D.Create(1,1,1);
 aCenterModel3D     := TPoint3D.Create(0,0,0);
 l_lstMeshData := TList<TMeshData>.Create;
 n := uMeshUtils.GetListOfMeshDatas(aModel,l_lstMeshData);
 i := 0;
 while i < n do begin
   l_oMeshData := l_lstMeshData[i];
   with l_oMeshData do begin   // Jedes Mesh aufrufen!
     vCnt := VertexBuffer.Length;
     for j := 0 to vCnt-1 do begin  // Alle Punkte des aktuellen Mesh erfassen
       P := VertexBuffer.Vertices[j];
       if l_lFirst then begin
         aMin := P;
         aMax := P;
         l_lFirst := False;
       end
       else begin    // den kleinsten und groessten Bereich ermitteln
         aMin.X := Min(aMin.X, P.X);
         aMin.Y := Min(aMin.Y, P.Y);
         aMin.Z := Min(aMin.Z, P.Z);
         aMax.X := Max(aMax.X, P.X);
         aMax.Y := Max(aMax.Y, P.Y);
         aMax.Z := Max(aMax.Z, P.Z);
       end;
     end;
   end;
   Inc(i);
 end;
 l_lstMeshData.Free;
 with aMin do
   F := Min(X, Min(Y,Z));
 with aMax do
   B := Max(X, Max(Y,Z));
 aMinMaxSizeModel3D := TPoint3D.Create(aMax.X-aMin.X, aMax.Y-aMin.Y, aMax.Z-aMin.Z);  // Größe
 with aMinMaxSizeModel3D do begin
   aCenterModel3D.X := -(aMin.X + X * 0.5);   // auf Mitte X
   aCenterModel3D.Y := -(aMin.Y + Y * 0.5);   // auf Mitte Y
   aCenterModel3D.Z := -(aMin.Z + Z * 0.5);   // auf Mitte Z
 end;
 Result := B - F;  // Min-/Max-Abstand ermitteln
end;

function ScaleModel3D(const aStrokeCubeSize: Single; aMinMaxSizeModel3D: TPoint3D): TPoint3D;
var
 X, Y, Z: Single;
begin
 X := 1;
 Y := 1;
 Z := 1;
 if aStrokeCubeSize <> 0 then begin
   X := aMinMaxSizeModel3D.X / aStrokeCubeSize * 0.5;
   Y := aMinMaxSizeModel3D.Y / aStrokeCubeSize * 0.5;
   Z := aMinMaxSizeModel3D.Z / aStrokeCubeSize * 0.5;
 end;
 Result := TPoint3D.Create(X,Y,Z);
end;

function ScaleCubeContainer(const aStrokeCubeSize: Single; aMinMaxSizeModel3D: TPoint3D): TPoint3D;
var
 X, Y, Z: Single;
begin
 X := 1;
 Y := 1;
 Z := 1;
 if aMinMaxSizeModel3D.X <> 0 then
   X := 10.0 * aStrokeCubeSize / aMinMaxSizeModel3D.X;
 if aMinMaxSizeModel3D.Y <> 0 then
   Y := 10.0 * aStrokeCubeSize / aMinMaxSizeModel3D.Y;
 if aMinMaxSizeModel3D.Z <> 0 then
   Z := 10.0 * aStrokeCubeSize / aMinMaxSizeModel3D.Z;
 Result := TPoint3D.Create(X,Y,Z);
end;

function DivVector3D(const aV: TVector3D; const aD: Single): TVector3D;
begin
 if (aV.X <> 0) and (aV.Y <> 0) and (aV.Z <> 0) then
   Result := TVector3D.Create(aD/aV.X, aD/aV.Y, aD/aV.Z)
 else
   Result := aV;
end;

function GetScaleFactorByTrackBar(aTrackBarValue: Single): Single;
begin
 if aTrackBarValue > 0 then begin
   // aTrackBarValue:=0.00001 ->  Result=1.00001
   // aTrackBarValue:=1.0     ->  Result=2.0
   // aTrackBarValue:=2.0     ->  Result=3.0
   // aTrackBarValue:=3.0     ->  Result=4.0
   Result := aTrackBarValue + 1.0;
 end
 else if aTrackBarValue < 0 then begin
   // aTrackBarValue:=-0.00001 ->  Result=1.0
   // aTrackBarValue:=-1.0     ->  Result=0.5
   // aTrackBarValue:=-2.0     ->  Result=0.33
   // aTrackBarValue:=-3.0     ->  Result=0.25
   Result := 1.0 / (Abs(aTrackBarValue) + 1.0);
 end
 else
   Result := 1.0;
end;

function GetTrackBarValueByScaleFactor(aScaleValue: Single): Single;
begin
 if aScaleValue > 1.0 then begin
   // aScaleValue:=1.00001 ->  Result=0.00001
   // aScaleValue:=2.0     ->  Result=1.0
   // aScaleValue:=3.0     ->  Result=2.0
   // aScaleValue:=4.0     ->  Result=3.0
   Result := aScaleValue - 1.0;
 end
 else if aScaleValue < 1.0 then begin
   // aScaleValue:=0.9999  ->  Result=-0.00001
   // aScaleValue:=0.5     ->  Result=-1.0
   // aScaleValue:=0.33    ->  Result=-2.0
   // aScaleValue:=0.25    ->  Result=-3.0
   Result := -((1.0-aScaleValue) / aScaleValue);
 end
 else
   Result := 1.0;
end;

{ --------------------------------- TCursor3D ----------------------------------------------}

constructor TCursor3D.Create(AOwner: TComponent);
var
 T,L: Single;
begin
 inherited;
 FPlaneNorm := TPoint3D.Create(0,0,-1); // Plane X/Y bevegen
 T := 0.3;  // Breite
 L := 0.8;  // Laenge
 Visible := False;
 Tag := 3;   // fuer X/Y Z=P1anNorm

 // X-Achse
 fX := TAchse3D.Create(Self);
 AddObject(fX);
 fX.Tag := 1;
 fX.SetSize(T,T,L);

 // Y-Achse
 fY := TAchse3D.Create(Self);
 AddObject(fY);
 fY.Tag := 2;
 fY.SetSize(L,T,T);

 // Z-Achse
 fZ := TAchse3D.Create(Self);
 AddObject(fZ);
 fZ.Tag:=3;
 fZ.SetSize(T,L,T);
end;

destructor TCursor3D.Destroy;
begin
 RemoveObject(fX);
 RemoveObject(fY);
 RemoveObject(fZ);
 if Assigned(fX) then
   FreeAndNil(fX);
 if Assigned(fY) then
   FreeAndNil(fY);
 if Assigned(fZ) then
   FreeAndNil(fZ);
 inherited;
end;

procedure TCursor3D.DoTracking;
begin
 if (not (csLoading in ComponentState)) and Assigned(FOnTracking) then
   FOnTracking(Slave);   // Uebergabe des Sklaven!
end;

function TCursor3D.GetSlave: TControl3D;
begin
 Result := Nil;
 if Assigned(Parent) and (Parent is TControl3D) then
   Result := TControl3D(Parent);
end;

procedure TCursor3D.MouseDown3D(Button: TMouseButton; Shift: TShiftState;
             X, Y: Single; RayPos, RayDir: TVector3D);
var
 I: TPoint3D;
begin
 if (ssLeft in Shift) and Assigned(Slave) then begin
   AutoCapture := True;
   I := NullPoint3D;
   RayCastPlaneIntersect(RayPos,RayDir,NullPoint3D,FPlaneNorm,I);  // Check die Oberflaeche!
   Slave.Position.DefaultValue := Slave.Position.Point - TPoint3D(LocalToAbsoluteVector(I));
   DoTracking;
 end;
 inherited;
end;

procedure TCursor3D.MouseMove3D(Shift: TShiftState;
             X, Y: Single; RayPos, RayDir: TVector3D);
var
 I: TPoint3D;
begin
 if (ssLeft in Shift) and Assigned(Slave) then begin
   I := NullPoint3D;
   RayCastPlaneIntersect(RayPos,RayDir,NullPoint3D,FPlaneNorm,I);  // Check die Oberflaeche!
   Slave.Position.Point := Slave.Position.DefaultValue + TPoint3D(LocalToAbsoluteVector(I));
   DoTracking;
 end;
 inherited;
end;

procedure TCursor3D.MouseUp3D(Button: TMouseButton; Shift: TShiftState;
             X, Y: Single; RayPos, RayDir: TVector3D);
begin
 inherited;
 AutoCapture := False;
end;

procedure TCursor3D.RecalcPos;
var
 S, D, Q: TPoint3D;
 R: Single;
begin
 if Assigned(Slave) then begin
   Visible := True;
   Self.SetSize(Slave.Width, Slave.Height, Slave.Depth);
   S := Slave.Scale.Point;
   D := TPoint3D.Create(Slave.Width * S.X, Slave.Height * S.Y, Slave.Depth * S.Z);
   Q := TPoint3D.Create(1/S.X, 1/S.Y, 1/S.Z);
   R := 1;   // Abstand
   fX.Position.Point := Point3D((D.X * 0.5) + R + fX.Width, 0, 0) * Q;
   fX.Scale.Point := Q;
   fY.Position.Point := Point3D(0, -((D.Y * 0.5) + R + fY.Height), 0) * Q;
   fY.Scale.Point := Q;
   fZ.Position.Point := Point3D(0, 0, -((D.Z * 0.5) + R + fZ.Depth)) * Q;
   fZ.Scale.Point := Q;
 end
 else
   Visible := False;
end;

procedure TCursor3D.Render;
var
 S: Single;
 C: TAlphaColor;
begin
 S := 1;
 C := TAlphaColors.Blue;
 if Move then begin // Wenn move, einfach Linie 1aenger machen
   S := 50;
   C := TAlphaColors.Ghostwhite;
 end;
 Context.DrawLine(NullPoint3D,fX.Position.Point * S, 1, C);   // X
 Context.DrawLine(NullPoint3D,fY.Position.Point * S, 1, C);   // Y
 Context.DrawLine(NullPoint3D,fZ.Position.Point * S, 1, C);   // Z
 Context.DrawCube(NullPoint3D,TPoint3D.Create(Width,Height,Depth),1,TAlphaColors.Blue);
end;

procedure TCursor3D.SetNewClient(const Value: TFmxObject);
begin
 Parent:= Value;  // Wechsel ausfuehren
 RecalcPos;       // Aktualisieren
end;

procedure TCursor3D.SwapMove(Sender: TObject);
begin
 FMoveMode := not FMoveMode;
 Repaint;
end;

{ --------------------------------- TAchsen3D ----------------------------------------------}

constructor TAchse3D.Create(AOwner: TComponent);
begin
 inherited;
 if not (AOwner is TCursor3D) then
   raise Exception.Create('Kein Cursor3D');
 fCursor3D := TCursor3D(AOwner);
 Cursor := crHandPoint;
 OnDblClick := fCursor3D.SwapMove;   // Umschalten Drehen/Bewegen
end;

procedure TAchse3D.Render;
var
 C,N: TAlphaColor;
 HierIstHinten: Boolean;  // False
begin
 if fCursor3D.Move then begin
   N := TAlphaColors.Black;
   if IsMouseOver then
     C := TAlphaColors.Yellow
   else
     C := TAlphaColors.Green;
 end
 else begin
   N := TAlphaColors.Blue;
   if IsMouseOver then
     C := TAlphaColors.Red
   else
     C := TAlphaColors.Deepskyblue;
 end;
 if fCursor3D.Tag = Tag then begin  // fuer PlanNorm
   if IsMouseOver then
     C := TAlphaColors.Chartreuse
   else
     C := TAlphaColors.Chocolate;
 end;
 Context.FillCube(NullPoint3D, TPoint3D.Create(Width,Height,Depth),1,C);
 Context.DrawCube(NullPoint3D, TPoint3D.Create(Width,Height,Depth),1,N);
end;

procedure TAchse3D.DoMouseEnter;
begin
 inherited;
 Repaint;
end;

procedure TAchse3D.DoMouseLeave;
begin
 inherited;
 Repaint;
end;

procedure TAchse3D.MouseDown3D(Button: TMouseButton;
                      Shift: TShiftState; X, Y: Single;
                      RayPos, RayDir: TVector3D);
var
 C: TPoint3D;
 Wc,Winkel:Single;
 W,P1,P2: TPoint3D;
begin
 if (ssleft in Shift) and Assigned(fCursor3D.Slave) then begin
   AutoCapture := True;
   case Tag of
     1: Cursor3D.FPlaneNorm := TPoint3D.Create(-1,0,0);
     2: Cursor3D.FPlaneNorm := TPoint3D.Create(0,-1,0);
     3: Cursor3D.FPlaneNorm := TPoint3D.Create(0,0,-1);   // Standard X/Y
   end;
   Cursor3D.Tag := Tag;    //Abgleich, was aktuell ist
   if Cursor3D.Move then begin
     P1 := RayPos + RayDir * RayPos.Length;
     P2 := AbsoluteToLocal3D(fCursor3D.Slave.Position.Point);
     case Tag of
       1: P2.X := P2.X - P1.X;   // X-Achse
       2: P2.Y := P2.Y - P1.Y;   // Y-Achse
       3: P2.Z := P2.Z - P1.Z;   // Z-Achse
     end;
     Position.DefaultValue := P2;
   end
   else begin
     RotationCenter.Point := fCursor3D.Slave.RotationAngle.Point;
     Position.DefaultValue:= Context.WorldToScreen(TProjection.Camera, fCursor3D.Slave.AbsolutePosition);
     Position.DefaultValue:= Point3D(Position.DefaultValue.X,Position.DefaultValue.Y,
                                     RadToDeg(ArcTan2(Position.DefaultValue.Y - Y,
                                                      X - Position.DefaultValue.X)));  // Winkel berechnen
   end;
   Repaint;
   fCursor3D.DoTracking;
 end;
 inherited;
end;

procedure TAchse3D.MouseMove3D(Shift: TShiftState; X, Y: Single;
 RayPos, RayDir: TVector3D);
var
 C: TPoint3D;
 Wc,Winkel:Single;
 W,P1,P2: TPoint3D;
begin
 if (ssleft in Shift) and Assigned(fCursor3D.Slave) and Assigned(Viewport) then begin
   if Cursor3D.Move then begin
     P1 := RayPos + RayDir * RayPos.Length;
       P2 := Position.DefaultValue;
       case Tag of
         1: P2.X := P2.X + P1.X;   // X-Achse
         2: P2.Y := P2.Y + P1.Y;   // Y-Achse
         3: P2.Z := P2.Z + P1.Z;   // Z-Achse
       end;
       fCursor3D.Slave.Position.Point := LocalToAbsolute3D(P2);
   end
   else begin
     C := Viewport.CurrentCamera.AbsolutePosition;
     C := AbsoluteToLocal3D(C).Normalize;
     Winkel := RadToDeg(ArcTan2(Position.DefaultValue.Y - Y, X - Position.DefaultValue.X)); // Winkel
     Wc := Winkel - Position.DefaultValue.Z;
     W := RotationCenter.Point;
     case tag of    // Auf welcher Achse soll gedreht werden?
       1: if C.Y > 0 then // X-Achse
            W.Y := W.Y + Wc
          else
            W.Y := W.Y - Wc;
       2: if C.Z > 0 then // Y-Achse
            W.Z := W.Z + Wc
          else
            W.Z := W.Z - Wc;
       3: if C.X > 0 then // Z-Achse
            W.X := W.X + Wc
          else
            W.X := W.X - Wc;
     end;
     fCursor3D.Slave.RotationAngle.Point := W;
   end;
   fCursor3D.DoTracking;
 end;
 inherited;
end;

procedure TAchse3D.MouseUp3D(Button: TMouseButton; Shift: TShiftState; X, Y: Single; RayPos, RayDir: TVector3D);
begin
 inherited;
 AutoCapture := False;
end;

{ --------------------------------- TForm1 ----------------------------------------------}

procedure TMainForm.FormCreate(Sender: TObject);
var
 F: TFmxObject;
begin
{$ifdef CODESITE}
 CodeSite.Clear;
{$endif}
 for F in Viewport3D.Children do
   F.SetDesign(True);
 MyCursor := TCursor3D.Create(Nil);
 MyCursor.Parent := Viewport3D;
 MyCursor.OnTracking := Update;    // Link
 m_oModel3D := TOwnModel3D.Create(self);
 m_oModel3D.Parent := StrokeCubeForMesh;
 m_oModel3D.OnDblClick := DummyCenterClick;
 m_oModel3D.HitTest := False;
 m_fModel3DMinMaxDistance := 0.0;
 chbxShowMeshLines.IsChecked := False;
 //
 m_oFaceModel := Nil;
end;

procedure TMainForm.FormShow(Sender: TObject);
var
 I: IViewport3D;
begin
 I := Viewport3D;
 with TControl3D(I.CurrentCamera) do begin
   m_oCameraBasePos.x := Position.x;
   m_oCameraBasePos.y := Position.y;
   m_oCameraBasePos.z := Position.z;
 end;
 m_oCameraAngleOfView := I.CurrentCamera.AngleOfView;
 with TDummy(I.CurrentCamera.Parent) do begin
   m_fRotationCenterX1 := RotationCenter.X;
   m_fRotationCenterY1 := RotationCenter.Y;
   m_fRotationAngleX1  := RotationAngle.X;
   m_fRotationAngleY1  := RotationAngle.Y;
   with TDummy(Parent) do begin
     m_fRotationCenterX2 := RotationCenter.X;
     m_fRotationCenterY2 := RotationCenter.Y;
     m_fRotationAngleX2  := RotationAngle.X;
     m_fRotationAngleY2  := RotationAngle.Y;
   end;
 end;
 self.Reset3DElements(atUnknown);
end;

procedure TMainForm.ButtonCopyClick(Sender: TObject);
var
 P, E: TFmxObject;
begin
 P := MyCursor.Parent;
 if Not Assigned(P) then
   Exit;
 MyCursor.SetNewClient(Nil);    // Verbindung kappen!
 E := P.Clone(Nil);             // Objekt kopieren
 E.Parent := Viewport3D;        // in den Vievport schieben
 With TControl3D(E) do begin
   OnClick := DummyCenterClick;     // Verbindung herstellen, demit es reagiert
   SetDesign(True);             // auch Licht anzeigen lassen
 end;
 MyCursor.SetNewClient(E);      // Kopiertes Objekt wird aktuell
end;

procedure TMainForm.ButtonDelClick(Sender: TObject);
var
 P: TFmxObject;
begin
 P := MyCursor.Parent;
 if not Assigned(P) then
   Exit;
 MyCursor.SetNewClient(Nil);   // Verbindung kappen!
 if P.Name <> '' then
   Exit;                       // Keine Orginale loeschen, die haben einen Namen
 Viewport3D.RemoveObject(P);
 FreeAndNil(P);
end;

procedure TMainForm.chbxLightChange(Sender: TObject);
begin
 Light.Enabled := chbxLight.IsChecked;
end;

procedure TMainForm.chbxMoveChange(Sender: TObject);
begin
 MyCursor.SwapMove(Sender);
end;

procedure TMainForm.DummyCenterClick(Sender: TObject);
begin
 MyCursor.SetNewClient(TFmxObject(Sender));   // Uebergebe das neue Object
end;

procedure TMainForm.TrackBarForCameraTracking(Sender: TObject);
var
 I: IViewport3D;   // Interface
begin
 I := Viewport3D;
 with TControl3D(I.CurrentCamera) do begin
   Position.Z := Single(-TrackBarForCamera.Value);
   lbCameraZValue.Text := Format('%3.2f',[Position.Z]);
 end;
end;

procedure TMainForm.TrackBarForScaleTracking(Sender: TObject);
var
 l_fScaleVal: Single;
begin
 if m_fModel3DMinMaxDistance > 0.0 then begin
   l_fScaleVal := GetScaleFactorByTrackBar(TrackBarForScale.Value);
   m_oModel3D.BeginUpdate;
   //-- m_oModel3D.Position.y := m_oCenterModel3D.y * l_fScaleVal;
   StrokeCubeForMesh.Scale.x  := l_fScaleVal;
   StrokeCubeForMesh.Scale.y  := l_fScaleVal;
   StrokeCubeForMesh.Scale.z  := l_fScaleVal;
   {DAV: >>>
   m_oModel3D.Scale.x  := l_fScaleVal;
   m_oModel3D.Scale.y  := l_fScaleVal;
   m_oModel3D.Scale.z  := l_fScaleVal;
   --}
   m_oModel3D.EndUpdate;
   m_oModel3D.Repaint;
 end
 else begin
   l_fScaleVal := 0.0;
   TrackBarForScale.Value := 0.0;
 end;
 lbScaleTrackbarValue.Text := Format('%4.3f',[TrackBarForScale.Value]);
 lbScaleValue.Text := Format('Scale: %4.3f',[l_fScaleVal]);
end;

procedure TMainForm.Update(Sender: TObject);
var
 P, W: TPoint3D;
 s: String;
begin
 with TControl3D(Sender) do begin
   P := Position.Point;
   W := RotationAngle.Point;
   s := Name;
 end;
 lbCoord.Text := 'Pos: ' + 'X: ' + FloatToStrF(P.X, ffFixed, 7, 2) + Space +
                           'Y: ' + FloatToStrF(P.Y, ffFixed, 7, 2) + Space +
                           'Z: ' + FloatToStrF(P.Z, ffFixed, 7, 2) + Space + Space +
                 'Ang: ' + 'X: ' + FloatToStrF(W.X, ffFixed, 7, 2) + Space +
                           'Y: ' + FloatToStrF(W.Y, ffFixed, 7, 2) + Space +
                           'Z: ' + FloatToStrF(W.Z, ffFixed, 7, 2);
end;

procedure TMainForm.Viewport3DMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
var
 I: IViewport3D;
begin
 if ssLeft in Shift then begin
   I := Viewport3D;
   with TDummy(I.CurrentCamera.Parent) do begin
     RotationCenter.X := RotationAngle.X + Y;
     with TDummy(Parent) do
       RotationCenter.Y := RotationAngle.Y - X;     // Horizontal
   end;
 end;
end;

procedure TMainForm.Viewport3DMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Single);
var
 I: IViewport3D;
begin
 if ssLeft in Shift then begin
   I := Viewport3D;
   with TDummy(I.CurrentCamera.Parent) do begin
     RotationAngle.X := RotationCenter.X - Y;
     with TDummy(Parent) do
       RotationAngle.Y := RotationCenter.Y + X;     // Horizontal
   end;
 end;
end;

procedure TMainForm.Viewport3DMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; var Handled: Boolean);
var
 I: IViewport3D;
begin
 TrackBarForCamera.OnTracking := Nil;
 I := Viewport3D;
 with TControl3D(I.CurrentCamera) do begin
   Position.Z := Position.Z + ((WheelDelta * I.GetViewportScale) / PointF(Viewport3D.Width,Viewport3D.Height).Length);
   TrackBarForCamera.Value := Position.Z;
 end;
 TrackBarForCamera.OnTracking := TrackBarForCameraTracking;
end;

procedure TMainForm.btnClearClick(Sender: TObject);
begin
 self.ClearModel3D;
 self.Reset3DElements(atUnknown);
end;

procedure TMainForm.ClearModel3D;
begin
 m_oModel3D.SetDesign(False);
 self.DeleteAllMeshes;
 m_oModel3D.BeginUpdate;
 m_oModel3D.Clear;
 m_oModel3D.Position.Point  := Point3D(0,0,0);
 m_oModel3D.Scale.Point     := Point3D(1,1,1);
 m_oModel3D.RotationAngle.X := 0;
 m_oModel3D.RotationAngle.Y := 0;
 m_oModel3D.RotationAngle.Z := 0;
 m_oModel3D.Height := 10;
 m_oModel3D.Depth  := 10;
 m_oModel3D.Width  := 10;
 m_oModel3D.EndUpdate;
 StrokeCubeForMesh.BeginUpdate;
 StrokeCubeForMesh.Height   := 10;
 StrokeCubeForMesh.Depth    := 10;
 StrokeCubeForMesh.Width    := 10;
 StrokeCubeForMesh.Position.Point := Point3D(0,-5,0);
 StrokeCubeForMesh.RotationAngle.X := 0;
 StrokeCubeForMesh.RotationAngle.Y := 0;
 StrokeCubeForMesh.RotationAngle.Z := 0;
 StrokeCubeForMesh.Scale.X := 1;
 StrokeCubeForMesh.Scale.Y := 1;
 StrokeCubeForMesh.Scale.Z := 1;
 StrokeCubeForMesh.EndUpdate;
 m_fModel3DMinMaxDistance := 0.0;
 m_oMinCoord := Point3D(0.0,0.0,0.0);
 m_oMaxCoord := Point3D(0.0,0.0,0.0);
 m_oModelBaseScale := Point3D(1.0,1.0,1.0);
 //
 if Assigned(m_oFaceModel) then begin
   m_oFaceModel.DeleteChilds;
   m_oFaceModel.Free;
   m_oFaceModel := Nil;
 end;
 self.Update(m_oModel3D);
end;

procedure TMainForm.Reset3DElements(i_enActionType: TActionType);
var
 I: IViewport3D;
begin
 // i_enActionType:  atUnknown,atLoadModel,atGenerateModel,atMathFunction
 if Text3D.Visible then
   Text3D.Visible := False;
 MyCursor.SetNewClient(Nil);    // Verbindung kappen!
 //
 Viewport3D.BeginUpdate;
 Viewport3D.Position.X := 0.0;
 Viewport3D.Position.Y := 121.0;
 Viewport3D.RotationAngle := 0.0;
 Viewport3D.RotationCenter.X := 0.5;
 Viewport3D.RotationCenter.Y := 0.5;
 Viewport3D.Scale.X := 1.0;
 Viewport3D.Scale.Y := 1.0;
 I := Viewport3D;
 with TControl3D(I.CurrentCamera) do begin
   Position.x := m_oCameraBasePos.x;
   Position.y := m_oCameraBasePos.y;
   Position.z := m_oCameraBasePos.z;
   lbCameraZValue.Text := Format('%3.2f',[Position.Z]);
 end;
 I.CurrentCamera.AngleOfView := m_oCameraAngleOfView;
 with TDummy(I.CurrentCamera.Parent) do begin
   RotationCenter.X := m_fRotationCenterX1;
   RotationCenter.Y := m_fRotationCenterY1;
   RotationAngle.X  := m_fRotationAngleX1;
   RotationAngle.Y  := m_fRotationAngleY1;
   with TDummy(Parent) do begin
     RotationCenter.X := m_fRotationCenterX2;
     RotationCenter.Y := m_fRotationCenterY2;
     RotationAngle.X  := m_fRotationAngleX2;
     RotationAngle.Y  := m_fRotationAngleY2;
   end;
 end;
 Viewport3D.EndUpdate;
 //
 DummyCenter.BeginUpdate;
 DummyCenter.Position.Point  := Point3D(0.0,0.0,0.0);
 DummyCenter.Scale.Point     := Point3D(1.0,1.0,1.0);
 DummyCenter.RotationAngle.Vector := Vector3D(0.0,0.0,0.0);
 DummyCenter.EndUpdate;
 m_oModel3D.BeginUpdate;
 m_oModel3D.Position.Point  := Point3D(0.0,0.0,0.0);
 m_oModel3D.Scale.Point     := Point3D(1.0,1.0,1.0);
 m_oModel3D.RotationAngle.Vector := Vector3D(0.0,0.0,0.0);
 m_oModel3D.EndUpdate;
 Light.BeginUpdate;
 Light.Position.Point  := Point3D(-16.0,-10.0,-10.0);
 Light.Scale.Point     := Point3D(1.0,1.0,1.0);
 Light.RotationAngle.Vector := Vector3D(340.0,50.0,0.0);
 Light.EndUpdate;
 //
 //-- StrokeCubeForMesh.Free;
 //-- self.BuildStrokeCubeForMesh;
 //-- m_oModel3D.Parent := StrokeCubeForMesh;
 StrokeCubeForMesh.BeginUpdate;
 StrokeCubeForMesh.Depth  := 10;
 StrokeCubeForMesh.Height := 10;
 StrokeCubeForMesh.Width  := 10;
 StrokeCubeForMesh.Position.Point  := Point3D(0.0,-StrokeCubeForMesh.Height/2.0,0.0);
 StrokeCubeForMesh.Scale.Point     := Point3D(1.0,1.0,1.0);
 StrokeCubeForMesh.RotationAngle.Vector := Vector3D(90.0,0.0,0.0);
 StrokeCubeForMesh.EndUpdate;
 //
 Grid3D.BeginUpdate;
 Grid3D.Position.Point  := Point3D(0.0,0.0,0.0);
 Grid3D.Scale.Point     := Point3D(1.0,1.0,1.0);
 Grid3D.RotationAngle.Vector := Vector3D(90.0,0.0,0.0);
 Grid3D.EndUpdate;
 if i_enActionType <> atGenerateMesh then
   cobxMeshGeneratorChoice.ItemIndex := -1;
 if i_enActionType <> atMathFunction then
   cobxMathFunctions.ItemIndex := -1;
 if i_enActionType <> at3DBool then
   cobx3DBoolTestChoice.ItemIndex := -1;
 TrackBarForCamera.Value := Abs(Trunc(m_oCameraBasePos.z));
 TrackBarForScale.Value  := 50.0;
 lbScaleTrackbarValue.Text := Format('%4.3f',[TrackBarForScale.Value]);
 lbScaleValue.Text := Format('Scale: %4.3f',[0.0]);
 lbModelNameValue.Text := '';
 lbXMinValue.Text := '0.0';
 lbYMinValue.Text := '0.0';
 lbZMinValue.Text := '0.0';
 lbXMaxValue.Text := '0.0';
 lbYMaxValue.Text := '0.0';
 lbZMaxValue.Text := '0.0';
end;

procedure TMainForm.DeleteAllMeshes;
var
 i: Integer;
 l_lstMeshes: TList<TMesh>;
 l_lstOwnMeshes: TList<TOwnMesh>;
 l_oMesh: TMesh;
 l_oOwnMesh: TOwnMesh;
begin
 l_lstMeshes    := TList<TMesh>.Create;
 l_lstOwnMeshes := TList<TOwnMesh>.Create;
 try
   for i := 0 to m_oModel3D.ChildrenCount - 1 do begin
     if m_oModel3D.Children[i] is TMesh then begin
       l_lstMeshes.Add(TMesh(m_oModel3D.Children[i]));
     end
     else if m_oModel3D.Children[i] is TOwnMesh then begin
       l_lstOwnMeshes.Add(TOwnMesh(m_oModel3D.Children[i]));
     end;
     // if i_oModel3D.Children[i].ChildrenCount > 0 then
     //   self.DeleteMesh(i_oModel3D, i_lstChildList[i].Children);     // <- sonst Absturz!
   end;
   if l_lstMeshes.Count > 0 then begin
     for l_oMesh in l_lstMeshes do begin
       m_oModel3D.RemoveObject(l_oMesh);
       l_oMesh.Free;
     end
   end;
   if l_lstOwnMeshes.Count > 0 then begin
     for l_oOwnMesh in l_lstOwnMeshes do begin
       m_oModel3D.RemoveObject(l_oOwnMesh);
       l_oOwnMesh.Free;
     end
   end;
 finally
   l_lstMeshes.Free;
   l_lstOwnMeshes.Free;
 end;
 for i := High(m_oModel3D.MeshCollection) downto 0 do begin
   m_oModel3D.MeshCollection[i].Free;
   m_oModel3D.MeshCollection[i] := Nil;
 end;
end;

function TMainForm.HasMaterialSource: Boolean;
var
 i: Integer;
begin
 Result := False;
 for i := 0 to m_oModel3D.ChildrenCount - 1 do begin
   if m_oModel3D.Children[i] is TMesh then begin
     Result := Assigned(TMesh(m_oModel3D.Children[i]).MaterialSource);
   end
   else if m_oModel3D.Children[i] is TOwnMesh then begin
     Result := Assigned(TOwnMesh(m_oModel3D.Children[i]).MaterialSource);
   end;
 end;
 for i := 0 to High(m_oModel3D.MeshCollection) do
   Result := Assigned(TMesh(m_oModel3D.MeshCollection[i]).MaterialSource);
end;

procedure TMainForm.SetMaterial;
var
 i: Integer;
begin
 for i := 0 to m_oModel3D.ChildrenCount - 1 do begin
   if m_oModel3D.Children[i] is TMesh then begin
     TMesh(m_oModel3D.Children[i]).MaterialSource := LightMaterialSource1;
   end
   else if m_oModel3D.Children[i] is TOwnMesh then begin
     TOwnMesh(m_oModel3D.Children[i]).MaterialSource := LightMaterialSource1;
   end;
 end;
 for i := 0 to High(m_oModel3D.MeshCollection) do
   TMesh(m_oModel3D.MeshCollection[i]).MaterialSource := LightMaterialSource1;
end;

procedure TMainForm.RemoveMaterial;
var
 i: Integer;
begin
 for i := 0 to m_oModel3D.ChildrenCount - 1 do begin
   if m_oModel3D.Children[i] is TMesh then begin
     TMesh(m_oModel3D.Children[i]).MaterialSource := Nil;
   end
   else if m_oModel3D.Children[i] is TOwnMesh then begin
     TOwnMesh(m_oModel3D.Children[i]).MaterialSource := Nil;
   end;
 end;
 for i := 0 to High(m_oModel3D.MeshCollection) do
   TMesh(m_oModel3D.MeshCollection[i]).MaterialSource := Nil;
end;

procedure TMainForm.SetNewModelPosSize;
var
 l_fMax: Double;
 l_oScalePoint: TPoint3D;
begin
 m_fModel3DMinMaxDistance := CalcModel3D(m_oModel3D, m_oMinCoord, m_oMaxCoord,
                                         m_oMinMaxSizeModel3D,
                                         m_oCenterModel3D );
 m_oModelBaseScale := ScaleModel3D(StrokeCubeForMesh.Width,m_oMinMaxSizeModel3D);
 m_oModel3D.BeginUpdate;
 m_oModel3D.Position.Point  := Point3D(0,0,0);
 //-- m_oModel3D.Position.Point := m_oCenterModel3D * m_oModelBaseScale.x;
 m_oModel3D.Scale.Point    := m_oModelBaseScale;
 m_oModel3D.EndUpdate;

 l_fMax := Max(Max(m_oModelBaseScale.X,m_oModelBaseScale.Y),m_oModelBaseScale.Y) * 1.2;
 StrokeCubeForMesh.BeginUpdate;
 StrokeCubeForMesh.Width  :=  l_fMax;
 StrokeCubeForMesh.Depth  :=  l_fMax;
 StrokeCubeForMesh.Height :=  l_fMax;
 StrokeCubeForMesh.Position.Point  := Point3D(0,0,0);
 //-- StrokeCubeForMesh.Position.Point := m_oCenterModel3D * m_oModelBaseScale.x;
 StrokeCubeForMesh.Scale.Point    := m_oModelBaseScale;
 StrokeCubeForMesh.EndUpdate;
 {
 l_oScalePoint     := ScaleCubeContainer(StrokeCubeForMesh.Width,m_oMinMaxSizeModel3D);
 StrokeCubeForMesh.BeginUpdate;
 StrokeCubeForMesh.Width  :=  l_oScalePoint.X ;
 StrokeCubeForMesh.Depth  :=  l_oScalePoint.Y;
 StrokeCubeForMesh.Height :=  l_oScalePoint.Z;
 StrokeCubeForMesh.Position.Point := m_oCenterModel3D * l_oScalePoint;
 StrokeCubeForMesh.Scale.Point    := l_oScalePoint;
 StrokeCubeForMesh.EndUpdate;
 }
 self.SetNewScaleOfTrackBar(m_oModelBaseScale.x);
 lbScaleTrackbarValue.Text := Format('%4.3f',[TrackBarForScale.Value]);
 lbScaleValue.Text := Format('Scale: %4.3f',[m_oModelBaseScale.x]);
 lbXMinValue.Text := Format('%4.3f',[m_oMinCoord.x]);
 lbYMinValue.Text := Format('%4.3f',[m_oMinCoord.y]);
 lbZMinValue.Text := Format('%4.3f',[m_oMinCoord.z]);
 lbXMaxValue.Text := Format('%4.3f',[m_oMaxCoord.x]);
 lbYMaxValue.Text := Format('%4.3f',[m_oMaxCoord.y]);
 lbZMaxValue.Text := Format('%4.3f',[m_oMaxCoord.z]);
 //
 {--
 txaProt.Lines.Add('- SetNewModelPosSize:');
 txaProt.Lines.Add('  - m_oMinCoord:');
 txaProt.Lines.Add('    x-Min: ' + Format('%4.3f',[m_oMinCoord.x]));
 txaProt.Lines.Add('    y-Min: ' + Format('%4.3f',[m_oMinCoord.y]));
 txaProt.Lines.Add('    z-Min: ' + Format('%4.3f',[m_oMinCoord.z]));
 txaProt.Lines.Add('  - m_oMaxCoord:');
 txaProt.Lines.Add('    x-Max: ' + Format('%4.3f',[m_oMaxCoord.x]));
 txaProt.Lines.Add('    y-Max: ' + Format('%4.3f',[m_oMaxCoord.y]));
 txaProt.Lines.Add('    z-Max: ' + Format('%4.3f',[m_oMaxCoord.z]));
 txaProt.Lines.Add('  - m_oMinMaxSizeModel3D:');
 txaProt.Lines.Add('    x: ' + Format('%4.3f',[m_oMinMaxSizeModel3D.x]));
 txaProt.Lines.Add('    y: ' + Format('%4.3f',[m_oMinMaxSizeModel3D.y]));
 txaProt.Lines.Add('    z: ' + Format('%4.3f',[m_oMinMaxSizeModel3D.z]));
 txaProt.Lines.Add('  - m_oModel3D.Position.Point:');
 txaProt.Lines.Add('    x: ' + Format('%4.3f',[m_oModel3D.Position.Point.x]));
 txaProt.Lines.Add('    y: ' + Format('%4.3f',[m_oModel3D.Position.Point.y]));
 txaProt.Lines.Add('    z: ' + Format('%4.3f',[m_oModel3D.Position.Point.z]));
 txaProt.Lines.Add('  - m_oModel3D.Scale.Point (m_oModelBaseScale):');
 txaProt.Lines.Add('    x: ' + Format('%4.3f',[m_oModel3D.Scale.Point.x]));
 txaProt.Lines.Add('    y: ' + Format('%4.3f',[m_oModel3D.Scale.Point.y]));
 txaProt.Lines.Add('    z: ' + Format('%4.3f',[m_oModel3D.Scale.Point.z]));
 txaProt.Lines.Add('  - NewScaleOfTrackBar:');
 txaProt.Lines.Add('    x: ' + Format('%4.3f',[m_oModel3D.Scale.Point.x]));
 txaProt.Lines.Add('    y: ' + Format('%4.3f',[m_oModel3D.Scale.Point.y]));
 txaProt.Lines.Add('    z: ' + Format('%4.3f',[m_oModel3D.Scale.Point.z]));
 txaProt.Lines.Add('- SetNewScaleOfTrackBar:');
 txaProt.Lines.Add('    i_iModelScaleFactor   : ' + Format('%4.3f',[m_oModelBaseScale.x]));
 txaProt.Lines.Add('    TrackBarForScale.Min  : ' + Format('%4.3f',[TrackBarForScale.Min]));
 txaProt.Lines.Add('    TrackBarForScale.Max  : ' + Format('%4.3f',[TrackBarForScale.Max]));
 txaProt.Lines.Add('    TrackBarForScale.Value: ' + Format('%4.3f',[TrackBarForScale.Value]));
 --}
end;

procedure TMainForm.SetNewScaleOfTrackBar(i_iModelScaleFactor: Single);
begin
 if i_iModelScaleFactor > 1.0 then begin
   if (i_iModelScaleFactor > 1.0) and (i_iModelScaleFactor <= 2.0) then begin
     TrackBarForScale.Min   := -5.0;
     TrackBarForScale.Max   := 5.0;
     TrackBarForScale.Value := -4.5;
   end
   else if (i_iModelScaleFactor >= 2.0) and (i_iModelScaleFactor <= 5.0) then begin
     TrackBarForScale.Min   := -20.0;
     TrackBarForScale.Max   := 20.0;
     TrackBarForScale.Value := -2.0;
   end
   else if (i_iModelScaleFactor >= 5.0) and (i_iModelScaleFactor <= 10.0) then begin
     TrackBarForScale.Min   := -40.0;
     TrackBarForScale.Max   := 40.0;
     TrackBarForScale.Value := -1.0;
   end
   else if (i_iModelScaleFactor >= 10.0) and (i_iModelScaleFactor <= 50.0) then begin
     TrackBarForScale.Min   := -50.0;
     TrackBarForScale.Max   := 50.0;
     TrackBarForScale.Value := 0.0;
   end
   else if (i_iModelScaleFactor >= 50.0) and (i_iModelScaleFactor <= 100.0) then begin
     TrackBarForScale.Min   := -100.0;
     TrackBarForScale.Max   := 100.0;
     TrackBarForScale.Value := 0.0;
   end
   else if (i_iModelScaleFactor >= 5.0) then begin
     TrackBarForScale.Min   := -100.0;
     TrackBarForScale.Max   := 100.0;
     TrackBarForScale.Value := 0.0;
   end;
 end
 else if i_iModelScaleFactor < 1.0 then begin
   if (i_iModelScaleFactor < 1.0) and (i_iModelScaleFactor >= 0.5) then begin
     TrackBarForScale.Min   := -5.0;
     TrackBarForScale.Max   := 30.0;
     TrackBarForScale.Value := 0.0;
   end
   else if (i_iModelScaleFactor <= 0.5) and (i_iModelScaleFactor >= 0.1) then begin
     TrackBarForScale.Min   := -10.0;
     TrackBarForScale.Max   := 60.0;
     TrackBarForScale.Value := 10.0;
   end
   else if (i_iModelScaleFactor <= 0.1) and (i_iModelScaleFactor >= 0.05) then begin
     TrackBarForScale.Min   := -10.0;
     TrackBarForScale.Max   := 70.0;
     TrackBarForScale.Value := 25.0;
   end
   else if (i_iModelScaleFactor <= 0.05) and (i_iModelScaleFactor >= 0.02) then begin
     TrackBarForScale.Min   := -50.0;
     TrackBarForScale.Max   := 90.0;
     TrackBarForScale.Value := 0.0;
   end
   else begin
     TrackBarForScale.Min   := -100.0;
     TrackBarForScale.Max   := 100.0;
     TrackBarForScale.Value := 0.0;
   end;
 end
 else begin
   TrackBarForScale.Min   := -5.0;
   TrackBarForScale.Max   := 5.0;
   TrackBarForScale.Value := -4.5;
 end;
 //-- TrackBarForScale.Value := GetTrackBarValueByScaleFactor(i_iModelScaleFactor);
end;

procedure TMainForm.cobxMeshGeneratorChoiceChange(Sender: TObject);
begin
 if cobxMeshGeneratorChoice.ItemIndex >= 0 then begin
   txaProt.Lines.Clear;
   lblStatus.Text := 'Generating... please wait';
   Application.ProcessMessages;
   self.ClearModel3D;
   self.Reset3DElements(atGenerateMesh);
   m_sModelName := uGenMeshChoice.ExecuteGenerator(cobxMeshGeneratorChoice.ItemIndex+1,m_oModel3D);
   txaProt.Lines.Add('Generated Mesh-Model: ' + m_sModelName);
   txaProt.Lines.Add('====================');
   self.SetNewModelPosSize;
   self.SetMaterial;
   self.HandleMeshLines();
   self.ProtCurrentMeshCounts;
   lbModelNameValue.Text := m_sModelName;
   lblStatus.Text := 'Ready';
 end;
end;

procedure TMainForm.cobxMathFunctionsChange(Sender: TObject);
var
 n: Integer;
 l_lOk: Boolean;
 l_fMinX, l_fMaxX, l_fMinY, l_fMaxY, l_fMinZ, l_fMaxZ: Double;
 l_fDistX, l_fDistY, l_fDistZ, l_fScalX, l_fScalY, l_fScalZ: Double;
begin
 n := cobxMathFunctions.ItemIndex+1;
 if n > 0 then begin
   txaProt.Lines.Clear;
   lblStatus.Text := 'Generating... please wait';
   Application.ProcessMessages;
   self.ClearModel3D;
   self.Reset3DElements(atMathFunction);
   Text3D.Text := cobxMathFunctions.Items[n-1];
   if not Text3D.Visible then
     Text3D.Visible := True;
   m_sModelName := uMathFunctionsChoice.ExecuteMathFunction(n,m_oModel3D,
                          l_fMinX, l_fMaxX, l_fMinY, l_fMaxY, l_fMinZ, l_fMaxZ);
   lbModelNameValue.Text := m_sModelName;
   txaProt.Lines.Add('Math-Function: ' + m_sModelName);
   txaProt.Lines.Add('==============');
   l_fDistX := l_fMaxX - l_fMinX;
   l_fDistY := l_fMaxY - l_fMinY;
   l_fDistZ := l_fMaxZ - l_fMinZ;
   l_fScalX := m_oModel3D.Width/l_fDistX;
   if l_fScalX > 1.0 then
     l_fScalX := 1.0;
   l_fScalY := m_oModel3D.Depth/l_fDistY;
   if l_fScalY > 1.0 then
     l_fScalY := 1.0;
   l_fScalZ := m_oModel3D.Height/l_fDistZ;
   if l_fScalZ > 1.0 then
     l_fScalZ := 1.0;
   if l_fScalZ < 0.00001 then
     l_fScalZ := 0.00001;
   m_oModel3D.Scale.Point := Point3D(l_fScalX,l_fScalY,l_fScalZ);
   lblStatus.Text := 'Ready';
   if not self.HandleMeshLines() then
     m_oModel3D.Repaint;
   self.ProtCurrentMeshCounts;
 end;
end;

procedure TMainForm.cobx3DBoolTestChoiceChange(Sender: TObject);
begin
 if cobx3DBoolTestChoice.ItemIndex >= 0 then begin
   txaProt.Lines.Clear;
   lblStatus.Text := 'Processing... please wait';
   Application.ProcessMessages;
   self.ClearModel3D;
   self.Reset3DElements(at3DBool);
   m_sModelName := uGen3DBoolTestChoice.Execute3DBoolTest(cobx3DBoolTestChoice.ItemIndex+1,m_oModel3D);
   txaProt.Lines.Add('3DBoolTest: ' + m_sModelName);
   txaProt.Lines.Add('==========');
   self.SetNewModelPosSize;
   self.SetMaterial;
   self.HandleMeshLines();
   self.ProtCurrentMeshCounts;
   lbModelNameValue.Text := m_sModelName;
   lblStatus.Text := 'Ready';
 end;
end;

procedure TMainForm.btnSaveAsObjClick(Sender: TObject);
var
 i, n: Integer;
 l_sFileName: String;
 l_arLines: TStringList;
 l_oMeshData: TMeshData;
 l_lstMeshData: TList<TMeshData>;
 W: TSaveDialog;
begin
 if Assigned(m_oModel3D) then begin
   l_oMeshData    := Nil;
   l_lstMeshData  := TList<TMeshData>.Create;
   n := uMeshUtils.GetListOfMeshDatas(m_oModel3D,l_lstMeshData);
   if n > 0 then begin
     l_arLines := Nil;
     W := TSaveDialog.Create(Self);
     try
       W.Options := [TOpenOption.ofOverwritePrompt];
       W.Filter :='3D file|*.obj';
       if W.Execute then begin
         l_sFileName := W.FileName;
         l_arLines := TStringList.Create;
         l_arLines.Add('# hadv3DViewer - OBJ File:');
         l_arLines.Add('o ' + m_sModelName);
         l_arLines.Add('usemtl None');
         l_arLines.Add('s off');
         if l_lstMeshData.Count > 0 then begin
           for i := 0 to n-1 do begin
             l_oMeshData := l_lstMeshData[i];
             uMeshUtils.GetStringsOfMeshDataPoint3DsForOBJ(l_oMeshData, l_arLines);
             uMeshUtils.GetStringsOfMeshDataNormalsForOBJ(l_oMeshData, l_arLines);
             uMeshUtils.GetStringsOfMeshDataTexCoordinatesForOBJ(l_oMeshData, l_arLines);
             uMeshUtils.GetStringsOfMeshDataTriangleIndicesForOBJ(l_oMeshData, l_arLines);
           end;
         end
         else if Assigned(l_oMeshData) then begin
           uMeshUtils.GetStringsOfMeshDataPoint3DsForOBJ(l_oMeshData, l_arLines);
           uMeshUtils.GetStringsOfMeshDataNormalsForOBJ(l_oMeshData, l_arLines);
           uMeshUtils.GetStringsOfMeshDataTexCoordinatesForOBJ(l_oMeshData, l_arLines);
           uMeshUtils.GetStringsOfMeshDataTriangleIndicesForOBJ(l_oMeshData, l_arLines);
         end;
         l_arLines.SaveToFile(l_sFileName);
       end;
     finally
       W.Free;
     end;
     if Assigned(l_arLines) then begin
       l_arLines.Clear;
       l_arLines.Free;
     end;
   end;
   l_lstMeshData.Free;
 end;
end;

procedure TMainForm.btnTransToMeshCollectionClick(Sender: TObject);
var
 i, n: Integer;
 LLength: Word;
 l_lstMeshes: TList<TMesh>;
 l_lstOwnMeshes: TList<TOwnMesh>;
 l_oMesh: TMesh;
 l_oOwnMesh: TOwnMesh;
 l_oBinStream: TMemoryStream;
begin
 lblStatus.Text := 'Transfer... please wait';
 Application.ProcessMessages;
 // Überführung der Mesh-Daten in "m_oModel3D.Children" nach "m_oModel3D.MeshCollection"
 self.Reset3DElements(atUnknown);
 l_lstMeshes    := TList<TMesh>.Create;
 l_lstOwnMeshes := TList<TOwnMesh>.Create;
 LLength   := 0;
 try
   for i := 0 to m_oModel3D.ChildrenCount - 1 do begin
     if m_oModel3D.Children[i] is TMesh then begin
       l_lstMeshes.Add(TMesh(m_oModel3D.Children[i]));
       Inc(LLength);
     end
     else if m_oModel3D.Children[i] is TOwnMesh then begin
       l_lstOwnMeshes.Add(TOwnMesh(m_oModel3D.Children[i]));
       Inc(LLength);
     end;
   end;
   if LLength > 0 then begin
     // Write all child object meshes to stream
     l_oBinStream := TMemoryStream.Create;
     l_oBinStream.WriteBuffer(LLength, 2);
     for i := 0 to l_lstMeshes.Count - 1 do begin
       l_oBinStream.WriteComponent(TCustomMesh(l_lstMeshes[i]));
     end;
     for i := 0 to l_lstOwnMeshes.Count - 1 do begin
       l_oBinStream.WriteComponent(TCustomMesh(l_lstOwnMeshes[i]));
     end;
     // Read meshes from stream for 3D-Model
     l_oBinStream.Seek(0, soFromBeginning);
     m_oModel3D.ReadModel(l_oBinStream);
     l_oBinStream.Free;
   end
   else begin
     MessageDlg('There are no Mesh Child Objects!', TMsgDlgType.mtInformation, [TMsgDlgBtn.mbOK], 0);
   end;
   if l_lstMeshes.Count > 0 then begin
     for l_oMesh in l_lstMeshes do begin
       m_oModel3D.RemoveObject(l_oMesh);
       l_oMesh.Free;
     end
   end;
   if l_lstOwnMeshes.Count > 0 then begin
     for l_oOwnMesh in l_lstOwnMeshes do begin
       m_oModel3D.RemoveObject(l_oOwnMesh);
       l_oOwnMesh.Free;
     end
   end;
   //
   if LLength > 0 then begin
     self.SetNewModelPosSize;
     self.SetMaterial;
     self.HandleMeshLines();
     self.ProtCurrentMeshCounts;
   end;
 finally
   l_lstMeshes.Free;
   l_lstOwnMeshes.Free;
 end;
 lblStatus.Text := 'Ready';
end;

procedure TMainForm.btnTransToTriFlipModelClick(Sender: TObject);
var
 i, j, k, l_iMeshesCnt, l_iVertexCnt, l_iOffset: Integer;
 F, B: Single;
 l_oPoint2D: TPointF;
 l_oPoint3D: TPoint3D;
 l_oMinMaxSizeModel3D, l_oCenterModel3D, l_oScalePoint: TPoint3D;
 l_oSingle3D: TSingle3D;
 P1, P2, P3:  TTriPoin3D;
 l_oArea3D:  TSingleArea3D;
 l_oOwnMesh:  TOwnMesh;
 l_oMeshData: TMeshData;
 l_lstMeshData: TList<TMeshData>;
 Ns, Ts: TArray<TSingle3D>;
begin
{
 Umgang mit Polygon-Modellen mit TriFlip-Datenstrukturen.
 Da es auch Verbindungsinformationen zwischen benachbarten dreieckigen Flächen enthält,
 sind Euler-Operationen wie das Schneiden und Verbinden von Polygonen möglich
 (Polygone schneiden oder kombinieren).
 Bei der Volumenmodellierung und dem computergestützten Entwurf ändern die
 Euler-Operatoren das Diagramm der Verbindungen, um Details eines Netzes
 hinzuzufügen oder zu entfernen, während die Topologie erhalten bleibt.
 Die Grenzdarstellung für ein festes Objekt, seine Oberfläche, ist ein
 Polygonnetz aus Eckpunkten, Kanten und Flächen. Die Topologie wird durch
 das Diagramm der Verbindungen zwischen Gesichtern erfasst. Ein bestimmtes
 Netz kann tatsächlich mehrere nicht verbundene Schalen (oder Körper) enthalten.
 Jeder Körper kann in mehrere verbundene Komponenten unterteilt werden,
 die jeweils durch ihre Randschleifengrenze definiert sind. Um ein hohles
 Objekt darzustellen, sind die Innen- und Außenflächen getrennte Schalen.
 In der Geometrie ändern Euler-Operatoren das Diagramm des Netzes, indem Flächen,
 Kanten und Scheitelpunkte nach einfachen Regeln erstellt oder entfernt werden,
 während die Gesamttopologie beibehalten wird, wodurch eine gültige Grenze
 beibehalten wird (d. H. Keine Löcher eingeführt werden).
 Die TriFlip-Modellimplementierung wird als Demo bereitgestellt, die Sie in
 FireMonkey unter Delphi 10 Berlin ausprobieren können.
}
 txaProt.Lines.Clear;
 txaProt.Lines.Add('TransToTriFlipModel:');
 txaProt.Lines.Add('====================');
 if Assigned(m_oFaceModel) then begin
   m_oFaceModel.DeleteChilds;
   m_oFaceModel.Free;
   m_oFaceModel := Nil;
 end;
 lblStatus.Text := 'Transfer... please wait';
 Application.ProcessMessages;
 l_lstMeshData := TList<TMeshData>.Create;
 l_iMeshesCnt := uMeshUtils.GetListOfMeshDatas(m_oModel3D, l_lstMeshData);
 if l_iMeshesCnt > 0 then begin
   m_oFaceModel := TTriFaceModel3D.Create;
   //
   l_iOffset := 0;
   for i := 0 to l_iMeshesCnt-1 do begin
     l_oMeshData := l_lstMeshData[i];
     with l_oMeshData do begin
       l_iVertexCnt := VertexBuffer.Length;
       for j := 0 to l_iVertexCnt - 1 do begin
         l_oPoint3D := VertexBuffer.Vertices[j];
         l_oSingle3D.x := l_oPoint3D.x;
         l_oSingle3D.y := l_oPoint3D.y;
         l_oSingle3D.z := l_oPoint3D.z;
         TTriPoin3D( TTriPoin3D.Create(m_oFaceModel.PoinModel)).Pos := l_oSingle3D;
       end;
       for j := 0 to l_iVertexCnt - 1 do begin
         l_oPoint3D := VertexBuffer.Normals[j];
         l_oSingle3D.x := l_oPoint3D.x;
         l_oSingle3D.y := l_oPoint3D.y;
         l_oSingle3D.z := l_oPoint3D.z;
         Ns := Ns + [l_oSingle3D];
       end;
       for j := 0 to l_iVertexCnt - 1 do begin
         l_oPoint2D := VertexBuffer.TexCoord0[j];
         l_oSingle3D.x := l_oPoint2D.x;
         l_oSingle3D.y := l_oPoint2D.y;
         l_oSingle3D.z := 0.0;
         Ts := Ts + [l_oSingle3D];
       end;
	     j := 0;
	     while j < IndexBuffer.Length do begin
         k  := IndexBuffer[j];
		     P1 := TTriPoin3D( m_oFaceModel.PoinModel.Childs[k+l_iOffset] );
		     P1.Tex := Ts[k];
		     P1.Nor := Ns[k];
		     Inc(j);
         k  := IndexBuffer[j];
		     P2 := TTriPoin3D( m_oFaceModel.PoinModel.Childs[k+l_iOffset] );
	    	 P2.Tex := Ts[k];
		     P2.Nor := Ns[k];
		     Inc(j);
         k  := IndexBuffer[j];
		     P3 := TTriPoin3D( m_oFaceModel.PoinModel.Childs[k+l_iOffset] );
		     P3.Tex := Ts[k];
		     P3.Nor := Ns[k];
		     m_oFaceModel.AddFace( P1, P2, P3 );
		     Inc(j);
       end;
     end;
     l_iOffset := l_iOffset + l_iVertexCnt;
     SetLength(Ns,0);
     SetLength(Ts,0);
   end;
   m_oFaceModel.JoinEdges;    // <- Erzeugt das reduzierte TriFlipModell!
   l_oArea3D := m_oFaceModel.GetBoundingBox;
  //
   self.DeleteAllMeshes;
   //
   l_oMeshData := TMeshData.Create;
   TriFaceMakeGeometry(m_oFaceModel, l_oMeshData);
   l_oMeshData.CalcFaceNormals;
   l_oMeshData.CalcTangentBinormals;
   //
   l_oOwnMesh := TOwnMesh.Create(m_oModel3D);
   l_oOwnMesh.Data := l_oMeshData;
   m_oModel3D.AddObject(l_oOwnMesh);
   //
   self.SetNewModelPosSize;
   if not self.HasMaterialSource() then
     self.SetMaterial;
   self.HandleMeshLines;
   self.ProtCurrentMeshCounts;
   lbModelNameValue.Text := m_sModelName;
 end
 else begin
   MessageDlg('There are no Meshes!', TMsgDlgType.mtInformation, [TMsgDlgBtn.mbOK], 0);
 end;
 l_lstMeshData.Free;
 lblStatus.Text := 'Ready';
end;

procedure TMainForm.btnGenCenterHoleClick(Sender: TObject);
var
 l_iMeshesCnt: Integer;
 l_oMesh: TOwnMesh;
 l_lstMeshData: TList<TMeshData>;
begin
 txaProt.Lines.Clear;
 lblStatus.Text := 'Generation hole... please wait';
 Application.ProcessMessages;
 l_lstMeshData := TList<TMeshData>.Create;
 l_iMeshesCnt := uMeshUtils.GetListOfMeshDatas(m_oModel3D, l_lstMeshData);
 if l_iMeshesCnt > 0 then begin
   l_oMesh := uGen3DBoolGenCenterHole.GetActMeshModelWithCenterHole(l_lstMeshData);
   self.ClearModel3D;
   self.Reset3DElements(atLoadModel);
   m_oModel3D.AddObject(l_oMesh);
   {
   m_oModel3D.Scale.X := 6;
   m_oModel3D.Scale.Y := 6;
   m_oModel3D.Scale.Z := 6;
   m_oModel3D.Repaint;
   }
   self.SetNewModelPosSize;
   self.SetMaterial;
   if not self.HandleMeshLines() then
     m_oModel3D.Repaint;
   self.ProtCurrentMeshCounts;
 end;
 lblStatus.Text := 'Ready';
end;

procedure TMainForm.btnLoadModelClick(Sender: TObject);
var
 l_lOk: Boolean;
 i, l_iAgress, l_iStartTri, l_iTargetTri: Integer;
 l_fRatio: Real;
 l_sExt, l_sStr, l_sFilePath: String;
 //-- l_oOpenDialog: TOpenDialog;
 l_rScalePoint: TPoint3D;
 l_dtStartTime: TDateTime;
 l_iSec: Int64;
begin
 l_lOk := True;
 txaProt.Lines.Clear;
 cobxMeshGeneratorChoice.ItemIndex := -1;
 if FormImportDlg.ShowModal = mrOk then begin
   l_sFilePath := FormImportDlg.FileName;
   l_sExt := UpperCase(TPath.GetExtension(l_sFilePath));
   m_sModelName := TPath.GetFileNameWithoutExtension(l_sFilePath);
   //
   txaProt.Lines.Add('Load Model: ' + m_sModelName);
   txaProt.Lines.Add('===========');
   lblStatus.Text := 'Loading... please wait';
   Application.ProcessMessages;
   self.ClearModel3D;
   self.Reset3DElements(atLoadModel);
   l_dtStartTime := NOW;
   if l_sExt = '.STP' then begin
     // Step noch nicht implementiert!
     MessageDlg('Kein Step-Loader implementiert!',
                TMsgDlgType.mtInformation, [TMsgDlgBtn.mbOK], 0);
     lblStatus.Text := 'Ready';
     l_lOk := False;
   end
   else if (l_sExt = '.CTM') or (l_sExt = '.OBJ') then begin
     if l_sExt = '.OBJ' then begin
       uFastObjLoader.Load3DObjModel(l_sFilePath, m_lstFaces,
                        m_lstVertices, m_lstVertexRGBA);
     end
     else begin
       FormImportDlg.CTMSimplifyMeshChecked;
       FormImportDlg.CTMAgress;
       FormImportDlg.CTMRatio;
       uLoaderCtm.ReadCTM(l_sFilePath, m_lstFaces,
                        m_lstVertices, m_lstVertexRGBA);
       if FormImportDlg.CTMSimplifyMeshChecked then begin
         l_iTargetTri := Round(length(m_lstFaces) * FormImportDlg.CTMRatio);
         uSimplifyMeshCtm.SimplifyMeshCtm(m_lstFaces, m_lstVertices, m_lstVertexRGBA,
                        l_iTargetTri, FormImportDlg.CTMAgress);
       end;
     end;
     if Length(m_lstFaces) > 0 then begin
       self.Create3DModelByCtmObjData;
     end;
     SetLength(m_lstFaces,0);
     SetLength(m_lstVertices,0);
     SetLength(m_lstVertexRGBA,0);
   end
   else begin
     m_sModelName := TPath.GetFileNameWithoutExtension(l_sFilePath);
     m_oModel3D.LoadFromFile(l_sFilePath);
   end;
   if l_lOk then begin
     l_iSec := SecondsBetween(Now, l_dtStartTime);
     lblStatus.Text := 'Ready - Sec: ' + IntToStr(l_iSec);
     txaProt.Lines.Add('- Load Time in sec: ' + IntToStr(l_iSec));
     self.SetNewModelPosSize;
     if not self.HasMaterialSource() then
       self.SetMaterial;
     self.HandleMeshLines;
     self.ProtCurrentMeshCounts;
     lbModelNameValue.Text := m_sModelName;
   end;
 end;
end;

function TMainForm.Create3DModelByCtmObjData: Boolean;
var
 l_lWithColor: Boolean;
 i, j:  Integer;
 l_oColorRec: TAlphaColorRec;     // in System.UITypes
 l_oColor:    TAlphaColor;        // in System.UITypes
 l_lstIndexBuffer:  TIndexBuffer;
 l_lstVertexBuffer: TVertexBuffer;
 l_oPoint3D: TPoint3D;
 l_oBmp: TBitmap;
 l_oBmpData: TBitmapData;
 l_oColTexture: TTextureMaterialSource;
 l_oOwnMesh:  TOwnMesh;
 l_oMeshData: TMeshData;
 l_iIdx: Cardinal;
 l_hshColors: TDictionary<TAlphaColor, Cardinal>;
 l_iColorsCap, l_iColorsCnt: Integer;
 l_arColors: array of TAlphaColor;
begin
 l_hshColors := TDictionary<TAlphaColor, Cardinal>.Create;
 l_iColorsCap := 1000;
 SetLength(l_arColors,l_iColorsCap);
 l_iIdx := 0;
 l_iColorsCnt := 1;
 l_arColors[l_iIdx] := TAlphaColors.White;
 l_hshColors.Add(TAlphaColors.Red,l_iIdx);
 //
 l_oMeshData := TMeshData.Create;
 //-- l_oMeshData.ChangeFormat([TVertexFormat.Vertex,TVertexFormat.Normal,TVertexFormat.TexCoord0,
 //--                          TVertexFormat.BiNormal,TVertexFormat.Tangent,TVertexFormat.Color0]);
 l_lstIndexBuffer  := l_oMeshData.IndexBuffer;
 l_lstIndexBuffer.Length := Length(m_lstFaces)*3;
 for i := 0 to High(m_lstFaces) do begin
   j := i * 3;
   l_lstIndexBuffer[j]   := m_lstFaces[i].X;
   l_lstIndexBuffer[j+1] := m_lstFaces[i].Y;
   l_lstIndexBuffer[j+2] := m_lstFaces[i].Z;
 end;
 l_lstVertexBuffer := l_oMeshData.VertexBuffer;
 l_lstVertexBuffer.Length := Length(m_lstVertices);
 l_lWithColor := Length(m_lstVertexRGBA) > 0;
 for i := 0 to High(m_lstVertices) do begin
   l_oPoint3D.X := m_lstVertices[i].X;
   l_oPoint3D.Y := m_lstVertices[i].Y;
   l_oPoint3D.Z := m_lstVertices[i].Z;
   l_lstVertexBuffer.Vertices[i] := l_oPoint3D;
   if l_lWithColor then begin
     l_oColorRec.R := m_lstVertexRGBA[i].R;
     l_oColorRec.G := m_lstVertexRGBA[i].G;
     l_oColorRec.B := m_lstVertexRGBA[i].B;
     l_oColorRec.A := m_lstVertexRGBA[i].A;
     l_oColor := l_oColorRec.Color;
     l_iIdx := 0;
     if not l_hshColors.TryGetValue(l_oColor,l_iIdx) then begin
       Inc(l_iColorsCnt);
       if l_iColorsCnt > l_iColorsCap then begin
         l_iColorsCap := l_iColorsCap + 1000;
         SetLength(l_arColors,l_iColorsCap);
       end;
       l_iIdx := l_iColorsCnt - 1;
       l_arColors[l_iIdx] := l_oColor;
       l_hshColors.Add(l_oColor,l_iIdx);
     end;
     l_lstVertexBuffer.TexCoord0[i] := PointF(0, l_iIdx)
   end;
 end;
 l_oMeshData.CalcFaceNormals;
 l_oMeshData.CalcTangentBinormals;
 //
 l_oOwnMesh := TOwnMesh.Create(m_oModel3D);
 l_oOwnMesh.Data := l_oMeshData;
 //
 if l_lWithColor then begin
   l_oBmp := TBitmap.Create(2,l_iColorsCnt);
   try
     if l_oBmp.Map(TMapAccess.ReadWrite,l_oBmpData)then begin
       for j := 0 to l_iColorsCnt-1 do begin
         l_oBmpData.SetPixel(0, j, l_arColors[j]);
         l_oBmpData.SetPixel(1, j, l_arColors[j]);
       end;
     end;
   finally
     l_oBmp.Unmap(l_oBmpData);
   end;
   {
   l_oColTexture:= TTextureMaterialSource.Create(l_oMesh);
   l_oColTexture.Parent := l_oMesh;
   l_oColTexture.Texture.Assign(l_oBmp);
   l_oMesh.MaterialSource := l_oColTexture;
   }
   l_oOwnMesh.MaterialSource := LightMaterialSource1;
   LightMaterialSource1.Texture.Assign(l_oBmp);
   l_oBmp.Free;
 end;
 l_hshColors.Clear;
 l_hshColors.Free;
 SetLength(l_arColors,0);
 //
 m_oModel3D.AddObject(l_oOwnMesh);
 Result := True;
end;

procedure TMainForm.chbxShowMeshLinesChange(Sender: TObject);
begin
 self.HandleMeshLines;
end;

procedure TMainForm.cobxColorMeshLinesChange(Sender: TObject);
begin
 self.HandleMeshLines;
end;

function TMainForm.HandleMeshLines(): Boolean;
var
 i: Integer;
 l_lDrawMeshLines, l_lWasChanged: Boolean;
 l_oColor: TAlphaColor;
 l_oOwnMesh: TOwnMesh;
 l_lstOwnMeshes: TList<TOwnMesh>;
begin
 l_lWasChanged := False;
 l_lDrawMeshLines := chbxShowMeshLines.IsChecked;
 l_oColor := cobxColorMeshLines.Color;
 l_lstOwnMeshes := TList<TOwnMesh>.Create;
 try
   for i := 0 to m_oModel3D.ChildrenCount - 1 do begin
     if m_oModel3D.Children[i] is TOwnMesh then begin
       l_lstOwnMeshes.Add(TOwnMesh(m_oModel3D.Children[i]));
     end;
   end;
   if l_lstOwnMeshes.Count > 0 then begin
     for l_oOwnMesh in l_lstOwnMeshes do begin
       if l_oOwnMesh.DrawMeshLines <> l_lDrawMeshLines then begin
         l_lWasChanged := True;
         l_oOwnMesh.DrawMeshLines := l_lDrawMeshLines;
       end;
       if l_oOwnMesh.MeshLineColor <> l_oColor then begin
         l_lWasChanged := True;
         l_oOwnMesh.MeshLineColor := l_oColor;
       end;
     end
   end;
 finally
   l_lstOwnMeshes.Free;
 end;
 if l_lWasChanged then
   m_oModel3D.Repaint;
 Result := l_lWasChanged;
end;

procedure TMainForm.ProtCurrentMeshCounts;
var
 i, l_iCntVertices, l_iCntIndices: Integer;
 l_lstMeshes: TList<TMesh>;
 l_lstOwnMeshes: TList<TOwnMesh>;
 l_oMeshData: TMeshData;
 l_oMesh:     TMesh;
 l_oOwnMesh:  TOwnMesh;
begin
 l_iCntVertices := 0;
 l_iCntIndices  := 0;
 l_lstMeshes    := TList<TMesh>.Create;
 l_lstOwnMeshes := TList<TOwnMesh>.Create;
 try
   for i := 0 to m_oModel3D.ChildrenCount - 1 do begin
     if m_oModel3D.Children[i] is TMesh then begin
       l_lstMeshes.Add(TMesh(m_oModel3D.Children[i]));
     end
     else if m_oModel3D.Children[i] is TOwnMesh then begin
       l_lstOwnMeshes.Add(TOwnMesh(m_oModel3D.Children[i]));
     end;
   end;
   if l_lstMeshes.Count > 0 then begin
     for l_oMesh in l_lstMeshes do begin
       l_oMeshData := l_oMesh.Data;
       l_iCntVertices := l_iCntVertices + l_oMeshData.VertexBuffer.Length;
       l_iCntIndices  := l_iCntIndices  + l_oMeshData.IndexBuffer.Length;
     end
   end;
   if l_lstOwnMeshes.Count > 0 then begin
     for l_oOwnMesh in l_lstOwnMeshes do begin
       l_oMeshData := l_oOwnMesh.Data;
       l_iCntVertices := l_iCntVertices + l_oMeshData.VertexBuffer.Length;
       l_iCntIndices  := l_iCntIndices  + l_oMeshData.IndexBuffer.Length;
     end
   end;
   if l_iCntVertices > 0 then begin
     txaProt.Lines.Add('- Count Vertices: ' + IntToStr(l_iCntVertices));
     txaProt.Lines.Add('- Count Indices : ' + IntToStr(l_iCntIndices));
   end;
 finally
   l_lstMeshes.Free;
   l_lstOwnMeshes.Free;
 end;
end;

procedure TMainForm.btnTestClick(Sender: TObject);
begin
 txaProt.Lines.Clear;
end;

end.
