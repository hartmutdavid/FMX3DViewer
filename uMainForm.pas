unit uMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  Generics.Collections, System.IOUtils,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Types3D,
  System.Math.Vectors, FMX.Objects3D, FMX.MaterialSources, FMX.Controls3D,
  FMX.Viewport3D, FMX.StdCtrls, FMX.Controls.Presentation, FMX.Layouts,
  FMX.ListBox, uMeshUtils
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
    LightMaterialSource1: TLightMaterialSource;
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
    chbxLight: TCheckBox;
    grbxMeshGen: TGroupBox;
    grbxCSG: TGroupBox;
    cobx3DBoolTestChoice: TComboBox;
    btnGenCenterHole: TButton;
    TrackBarForScale: TTrackBar;
    lbCamera: TLabel;
    lbScale: TLabel;
    lbCameraZValue: TLabel;
    lbScaleValue: TLabel;
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
    procedure chbxLightChange(Sender: TObject);
    procedure cobx3DBoolTestChoiceChange(Sender: TObject);
    procedure btnGenCenterHoleClick(Sender: TObject);
    procedure TrackBarForScaleTracking(Sender: TObject);
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
    procedure ClearModel3D;
    procedure Reset3DElements(i_enActionType: TActionType);
    procedure DeleteAllMeshes;
    procedure SetMaterial;
    procedure SetNewModelPosSize;
    procedure SetNewScaleOfTrackBar(i_iModelScaleFactor: Single);
  public
    { Public-Deklarationen }
    m_oModel3D: TOwnModel3D;
    //
    MyCursor: TCursor3D;
    procedure Update(Sender: TObject);
    procedure LoadModel;           // Datei öffnen
  end;

var
  MainForm: TMainForm;

implementation

uses System.Math, FMX.ASE.Importer, FMX.DAE.Importer, FMX.OBJ.Importer,
     uGenMeshChoice, uMathFunctionsChoice, uGen3DBoolTestChoice,
     uGen3DBoolGenCenterHole;

{$R *.fmx}

function CalcModel3D(const aModel: TModel3D; out aMin, aMax, aMinMaxSizeModel3D, aCenterModel3D: TPoint3D): Single;
var
 l_lFirst: Boolean;
 i, j, n: Integer;
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
 for i := 0 to n-1 do begin
   l_oMeshData := l_lstMeshData[i];
   with l_oMeshData do begin   // Jedes Mesh aufrufen!
     for j := 0 to VertexBuffer.Length -1 do begin  // Alle Punkte des aktuellen Mesh erfassen
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

function ScaleModel3D(const aStrokeCubeSize, aObjBereich: Single): TPoint3D;
var
 W: Single;
begin
 W := 0;
 if aObjBereich <> 0 then
   W := aStrokeCubeSize / aObjBereich;   // ohne Div NullVector3D-updateObjCenter
 Result := TPoint3D.Create(W,W,W);
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
   m_oModel3D.Scale.x  := l_fScaleVal;
   m_oModel3D.Scale.y  := l_fScaleVal;
   m_oModel3D.Scale.z  := l_fScaleVal;
   m_oModel3D.EndUpdate;
   m_oModel3D.Repaint;
 end
 else begin
   l_fScaleVal := 0.0;
   TrackBarForScale.Value := 0.0;
 end;
 lbScaleValue.Text := Format('%4.3f',[l_fScaleVal]);
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

procedure TMainForm.btnLoadModelClick(Sender: TObject);
begin
 self.LoadModel;
end;

procedure TMainForm.ClearModel3D;
var
 I: Integer;
 LMeshes: TList<TMesh>;
 LMesh: TMesh;
begin
 m_oModel3D.SetDesign(False);
 self.DeleteAllMeshes;
 m_oModel3D.Clear;
 m_oModel3D.Position.Point  := Point3D(0,0,0);
 m_oModel3D.Scale.Point     := Point3D(1,1,1);
 m_oModel3D.RotationAngle.X := 0;
 m_oModel3D.RotationAngle.Y := 0;
 m_oModel3D.RotationAngle.Z := 0;
 m_oModel3D.Height := StrokeCubeForMesh.Height;
 m_oModel3D.Depth  := StrokeCubeForMesh.Depth;
 m_oModel3D.Width  := StrokeCubeForMesh.Width;
 m_fModel3DMinMaxDistance := 0.0;
 m_oMinCoord := Point3D(0.0,0.0,0.0);
 m_oMaxCoord := Point3D(0.0,0.0,0.0);
 m_oModelBaseScale := Point3D(1.0,1.0,1.0);
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

 //-- StrokeCubeForMesh.Free;
 //-- self.BuildStrokeCubeForMesh;
 //-- m_oModel3D.Parent := StrokeCubeForMesh;

 StrokeCubeForMesh.BeginUpdate;
 StrokeCubeForMesh.Position.Point  := Point3D(0.0,-StrokeCubeForMesh.Height/2.0,0.0);
 StrokeCubeForMesh.Scale.Point     := Point3D(1.0,1.0,1.0);
 StrokeCubeForMesh.RotationAngle.Vector := Vector3D(90.0,0.0,0.0);
 StrokeCubeForMesh.EndUpdate;

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
 lbScaleValue.Text := Format('%4.3f',[0.0]);
 I := Viewport3D;
 with TControl3D(I.CurrentCamera) do begin
   Position.x := m_oCameraBasePos.x;
   Position.y := m_oCameraBasePos.y;
   Position.z := m_oCameraBasePos.z;
   lbCameraZValue.Text := Format('%3.2f',[Position.Z]);
 end;
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

procedure TMainForm.SetNewModelPosSize;
begin
 m_fModel3DMinMaxDistance := CalcModel3D(m_oModel3D, m_oMinCoord, m_oMaxCoord,
                                         m_oMinMaxSizeModel3D,
                                         m_oCenterModel3D );
 m_oModelBaseScale := ScaleModel3D(StrokeCubeForMesh.Width,m_fModel3DMinMaxDistance);
 m_oModel3D.BeginUpdate;
 m_oModel3D.Position.Point := m_oCenterModel3D * m_oModelBaseScale.x;
 m_oModel3D.Scale.Point    := m_oModelBaseScale;
 m_oModel3D.EndUpdate;
 self.SetNewScaleOfTrackBar(m_oModelBaseScale.x);
 lbScaleValue.Text := Format('%4.3f',[m_oModelBaseScale.x]);
 lbXMinValue.Text := Format('%4.3f',[m_oMinCoord.x]);
 lbYMinValue.Text := Format('%4.3f',[m_oMinCoord.y]);
 lbZMinValue.Text := Format('%4.3f',[m_oMinCoord.z]);
 lbXMaxValue.Text := Format('%4.3f',[m_oMaxCoord.x]);
 lbYMaxValue.Text := Format('%4.3f',[m_oMaxCoord.y]);
 lbZMaxValue.Text := Format('%4.3f',[m_oMaxCoord.z]);
end;

procedure TMainForm.SetNewScaleOfTrackBar(i_iModelScaleFactor: Single);
begin
 if i_iModelScaleFactor > 1.0 then begin
   if (i_iModelScaleFactor > 1.0) and (i_iModelScaleFactor < 2.0) then begin
     TrackBarForScale.Min   := -5.0;
     TrackBarForScale.Max   := 5.0;
   end
   else if (i_iModelScaleFactor >= 2.0) and (i_iModelScaleFactor < 5.0) then begin
     TrackBarForScale.Min   := -10.0;
     TrackBarForScale.Max   := 10.0;
   end
   else if (i_iModelScaleFactor >= 5.0) and (i_iModelScaleFactor < 10.0) then begin
     TrackBarForScale.Min   := -20.0;
     TrackBarForScale.Max   := 20.0;
   end
   else if (i_iModelScaleFactor >= 10.0) and (i_iModelScaleFactor < 50.0) then begin
     TrackBarForScale.Min   := -50.0;
     TrackBarForScale.Max   := 50.0;
   end
   else if (i_iModelScaleFactor >= 5.0) then begin
     TrackBarForScale.Min   := -100.0;
     TrackBarForScale.Max   := 100.0;
   end
 end
 else if i_iModelScaleFactor < 1.0 then begin
   if (i_iModelScaleFactor < 1.0) and (i_iModelScaleFactor > 0.5) then begin
     TrackBarForScale.Min   := -5.0;
     TrackBarForScale.Max   := 5.0;
   end
   else if (i_iModelScaleFactor <= 0.5) and (i_iModelScaleFactor > 0.1) then begin
     TrackBarForScale.Min   := -10.0;
     TrackBarForScale.Max   := 10.0;
   end
   else if (i_iModelScaleFactor <= 0.1) and (i_iModelScaleFactor > 0.05) then begin
     TrackBarForScale.Min   := -20.0;
     TrackBarForScale.Max   := 20.0;
   end
   else if (i_iModelScaleFactor <= 0.05) and (i_iModelScaleFactor > 0.02) then begin
     TrackBarForScale.Min   := -50.0;
     TrackBarForScale.Max   := 50.0;
   end
   else begin
     TrackBarForScale.Min   := -100.0;
     TrackBarForScale.Max   := 100.0;
   end;
 end
 else begin
   TrackBarForScale.Min   := -5.0;
   TrackBarForScale.Max   := 5.0;
 end;
 TrackBarForScale.Value := GetTrackBarValueByScaleFactor(i_iModelScaleFactor);
end;

procedure TMainForm.cobxMeshGeneratorChoiceChange(Sender: TObject);
begin
 if cobxMeshGeneratorChoice.ItemIndex >= 0 then begin
   lblStatus.Text := 'Generating... please wait';
   Application.ProcessMessages;
   self.ClearModel3D;
   self.Reset3DElements(atGenerateMesh);
   m_sModelName := uGenMeshChoice.ExecuteGenerator(cobxMeshGeneratorChoice.ItemIndex+1,m_oModel3D);
   self.SetNewModelPosSize;
   self.SetMaterial;
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
   m_oModel3D.Repaint;
 end;
end;

procedure TMainForm.cobx3DBoolTestChoiceChange(Sender: TObject);
begin
 if cobx3DBoolTestChoice.ItemIndex >= 0 then begin
   lblStatus.Text := 'Processing... please wait';
   Application.ProcessMessages;
   self.ClearModel3D;
   self.Reset3DElements(at3DBool);
   m_sModelName := uGen3DBoolTestChoice.Execute3DBoolTest(cobx3DBoolTestChoice.ItemIndex+1,m_oModel3D);
   self.SetNewModelPosSize;
   self.SetMaterial;
   lbModelNameValue.Text := m_sModelName;
   lblStatus.Text := 'Ready';
 end;
end;

procedure TMainForm.LoadModel;
var
 i: Integer;
 W: TOpenDialog;
 l_rScalePoint: TPoint3D;
begin
 cobxMeshGeneratorChoice.ItemIndex := -1;
 W := TOpenDialog.create(Self);
 try
   W.Filter :='3D files|*.obj;*.dae;*.ase;*.stp;*.data';
   if W.Execute then begin
     lblStatus.Text := 'Loading... please wait';
     Application.ProcessMessages;
     m_sModelName := TPath.GetFileNameWithoutExtension(W.FileName);
     self.ClearModel3D;
     self.Reset3DElements(atLoadModel);
     m_oModel3D.LoadFromFile(W.FileName);
     self.SetNewModelPosSize;
     self.SetMaterial;
     lbModelNameValue.Text := m_sModelName;
     lblStatus.Text := 'Ready';
   end;
 finally
   W.Free;
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
   l_lstMeshData := TList<TMeshData>.Create;
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
         for i := 0 to n-1 do begin
           l_oMeshData := l_lstMeshData[i];
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
   end;
 finally
   l_lstMeshes.Free;
   l_lstOwnMeshes.Free;
 end;
 lblStatus.Text := 'Ready';
end;


procedure TMainForm.btnGenCenterHoleClick(Sender: TObject);
var
 l_iMeshesCnt: Integer;
 l_oMesh: TOwnMesh;
 l_lstMeshData: TList<TMeshData>;
begin
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
   m_oModel3D.Repaint;
 end;
 lblStatus.Text := 'Ready';
end;

end.
