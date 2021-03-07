unit uImportDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Buttons, StdCtrls, ComCtrls, IniFiles;

type
  TFormImportDlg = class(TForm)
    LabelEPUBFile: TLabel;
    EditEPUBFile: TEdit;
    SpeedButtonEPUBFile: TSpeedButton;
    btnStart: TButton;
    OpenDialog: TOpenDialog;
    procedure SpeedButtonEPUBFileClick(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormImportDlg: TFormImportDlg;

implementation

{$R *.dfm}

uses uFormMain, uGlobal, uModelEPUBInfo;

procedure TFormImportDlg.FormClose(Sender: TObject; var Action: TCloseAction);
var
 l_lErrorSw: Boolean;
 l_oIniFile: TIniFile;
begin
 l_lErrorSw := False;      { <= Fehler Switch init. }
 l_oIniFile := Nil;
 try
   l_oIniFile := TIniFile.Create(g_sIniFilePath);
 except
   l_lErrorSw := True;
 end;
 if not l_lErrorSw then begin
   l_oIniFile.WriteString('System','LastDocFile',g_sLastDocFile);
 end;
 if Assigned(l_oIniFile) then
   l_oIniFile.Free;
 Action := TCloseAction.caFree;
end;

procedure TFormImportDlg.FormShow(Sender: TObject);
var
 i: Integer;
 l_sStr, l_sFileName: string;
begin
 l_sFileName := '';
 if (ParamCount > 0) then begin
   for i := 1 to ParamCount do begin
     l_sStr := UpperCase(ParamStr(i));
     if (l_sStr = '-FILENAME') then begin
       l_sFileName := Trim(ParamStr(i + 1));
     end
   end;
 end;
 if (Length(l_sFileName) > 0) and FileExists(l_sFileName) then begin
   EditEPUBFile.Text := l_sFileName;
   self.btnStartClick(Sender);
 end
 else if (Length(uGlobal.g_sLastDocFile) > 0) and FileExists(uGlobal.g_sLastDocFile) then begin
   EditEPUBFile.Text := uGlobal.g_sLastDocFile;
 end;
end;

procedure TFormImportDlg.SpeedButtonEPUBFileClick(Sender: TObject);
var
 l_sFileName: string;
begin
 l_sFileName := EditEPUBFile.Text;
 if OpenDialog.Execute then begin
   EditEPUBFile.Text := OpenDialog.FileName;
 end;
end;

procedure TFormImportDlg.btnStartClick(Sender: TObject);
var
 l_sFileName: string;
begin
 l_sFileName := EditEPUBFile.Text;
 if not Assigned(FormMain) then begin
   Application.CreateForm(TFormMain, FormMain);
   FormMain.InitDstBrowser;
 end;
 FormMain.OpenEPUBFile(l_sFileName);
 FormMain.Show;
 FormMain.BringToFront;
end;

end.
