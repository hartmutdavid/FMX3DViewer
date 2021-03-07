unit uImportDlg;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.IOUtils,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Edit, FMX.EditBox, FMX.NumberBox;

type
  TFormImportDlg = class(TForm)
    txfFilePath: TEdit;
    btnChoiceFile: TButton;
    Label1: TLabel;
    OpenDialog: TOpenDialog;
    Panel1: TPanel;
    Label2: TLabel;
    Label3: TLabel;
    grbxCTM: TGroupBox;
    btnCancel: TButton;
    btnOk: TButton;
    chbxSimplifyMesh: TCheckBox;
    NumberBoxCTMAgress: TNumberBox;
    NumberBoxCTMRatio: TNumberBox;
    procedure btnChoiceFileClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnOkClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure txfFilePathExit(Sender: TObject);
  private
    { Private-Deklarationen }
    m_lOkClicked: Boolean;
    function GetFileName: String;
    function GetCTMSimplifyMeshChecked: Boolean;
    function GetCTMAgress: Single;
    function GetCTMRatio:  Single;
    procedure SetCTMOptions;
  public
    { Public-Deklarationen }
    Property OkClicked: Boolean read m_lOkClicked;
    Property FileName: String read GetFileName;
    Property CTMSimplifyMeshChecked: Boolean read GetCTMSimplifyMeshChecked;
    Property CTMAgress: Single read GetCTMAgress;
    Property CTMRatio:  Single read GetCTMRatio;
  end;

var
  FormImportDlg: TFormImportDlg;

implementation

{$R *.fmx}

uses uGlb;

procedure TFormImportDlg.FormShow(Sender: TObject);
begin
 Self.SetCTMOptions;
 m_lOkClicked := False;
end;

procedure TFormImportDlg.btnOkClick(Sender: TObject);
begin
 if FileExists(txfFilePath.Text) then begin
   m_lOkClicked := True;
   Close;
 end
 else
   MessageDlg('File not exists!', TMsgDlgType.mtInformation, [TMsgDlgBtn.mbOK], 0);
end;

procedure TFormImportDlg.btnCancelClick(Sender: TObject);
begin
 m_lOkClicked := False;
 Close;
end;

procedure TFormImportDlg.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 Action := TCloseAction.caHide;
end;

function TFormImportDlg.GetFileName: String;
begin
 Result := txfFilePath.Text;
end;

function TFormImportDlg.GetCTMSimplifyMeshChecked: Boolean;
begin
 Result := chbxSimplifyMesh.IsChecked;
end;

function TFormImportDlg.GetCTMAgress: Single;
begin
 Result := NumberBoxCTMAgress.Value;
end;

function TFormImportDlg.GetCTMRatio: Single;
begin
 Result := NumberBoxCTMRatio.Value;
end;

procedure TFormImportDlg.btnChoiceFileClick(Sender: TObject);
begin
 OpenDialog.InitialDir := uGlb.g_sModelPath;
 if OpenDialog.Execute then begin
   txfFilePath.Text := OpenDialog.FileName;
   Self.SetCTMOptions;
 end;
end;

procedure TFormImportDlg.txfFilePathExit(Sender: TObject);
begin
 if FileExists(txfFilePath.Text) then
   Self.SetCTMOptions
 else
   MessageDlg('File not exists!', TMsgDlgType.mtInformation, [TMsgDlgBtn.mbOK], 0);
end;

procedure TFormImportDlg.SetCTMOptions;
var
 l_sExt: String;
begin
 l_sExt := UpperCase(TPath.GetExtension(txfFilePath.Text));
 if l_sExt = '.CTM' then begin
   chbxSimplifyMesh.IsChecked := uGlb.g_lWidthSimplifyMesh;
   NumberBoxCTMAgress.Value := Single(uGlb.g_iAgress);
   NumberBoxCTMRatio.Value  := uGlb.g_fRatio;
   grbxCTM.Visible := True;
 end
 else
   grbxCTM.Visible := False;
end;

end.
