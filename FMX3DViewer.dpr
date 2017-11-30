program FMX3DViewer;

uses
  System.StartUpCopy,
  SysUtils,
  FMX.Forms,
  uMainForm in 'uMainForm.pas' {MainForm},
  uGenMeshChoice in 'MeshGen\uGenMeshChoice.pas',
  uGenPipeMesh in 'MeshGen\uGenPipeMesh.pas',
  uGenSquareMesh in 'MeshGen\uGenSquareMesh.pas',
  uGenShaftMesh in 'MeshGen\uGenShaftMesh.pas',
  u3DBoolMisc in '3dbool\u3DBoolMisc.pas',
  u3DBoolVertex in '3dbool\u3DBoolVertex.pas',
  u3DBoolBound in '3dbool\u3DBoolBound.pas',
  u3DBoolLine in '3dbool\u3DBoolLine.pas',
  u3DBoolFace in '3dbool\u3DBoolFace.pas',
  u3DBoolSegment in '3dbool\u3DBoolSegment.pas',
  u3DBoolSolid in '3dbool\u3DBoolSolid.pas',
  u3DBoolObject3D in '3dbool\u3DBoolObject3D.pas',
  u3DBoolModeller in '3dbool\u3DBoolModeller.pas',
  uMeshUtils in 'uMeshUtils.pas',
  FMX.STEP.Importer in 'Importer\FMX.STEP.Importer.pas',
  FMX.STEP.Lexer in 'Importer\FMX.STEP.Lexer.pas',
  FMX.STEP.Model in 'Importer\FMX.STEP.Model.pas',
  FMX.DATA.Importer in 'Importer\FMX.DATA.Importer.pas',
  FMX.DATA.Model in 'Importer\FMX.DATA.Model.pas',
  uMathFunctionsChoice in 'MathFunctions\uMathFunctionsChoice.pas',
  uGen3DBoolTestChoice in '3dbool\uGen3DBoolTestChoice.pas',
  uGen3DBoolTest1 in '3dbool\uGen3DBoolTest1.pas',
  uGen3DBoolTest2 in '3dbool\uGen3DBoolTest2.pas',
  uGen3DBoolGenCenterHole in '3dbool\uGen3DBoolGenCenterHole.pas',
  u3DBoolFMXSolid in '3dbool\u3DBoolFMXSolid.pas',
  uDefaultCoordinates in '3dbool\uDefaultCoordinates.pas';

{$R *.res}

begin
  FormatSettings.DecimalSeparator := '.';              { <= Fuer Gleitkommazahlen !!! }
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
