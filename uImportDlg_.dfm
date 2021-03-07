object FormImportDlg: TFormImportDlg
  Left = 547
  Top = 249
  BorderStyle = bsDialog
  BorderWidth = 8
  Caption = 'PDF Viewer and Analyzer'
  ClientHeight = 78
  ClientWidth = 519
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnShow = FormShow
  DesignSize = (
    519
    78)
  PixelsPerInch = 96
  TextHeight = 13
  object LabelEPUBFile: TLabel
    Left = 0
    Top = 10
    Width = 44
    Height = 13
    Caption = '&ePub file:'
    FocusControl = EditEPUBFile
  end
  object SpeedButtonEPUBFile: TSpeedButton
    Left = 494
    Top = 7
    Width = 23
    Height = 23
    Anchors = [akTop, akRight]
    Caption = '...'
    OnClick = SpeedButtonEPUBFileClick
  end
  object EditEPUBFile: TEdit
    Left = 50
    Top = 8
    Width = 441
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
  end
  object btnStart: TButton
    Left = 195
    Top = 44
    Width = 164
    Height = 25
    Caption = '&Start'
    TabOrder = 1
    OnClick = btnStartClick
  end
  object OpenDialog: TOpenDialog
    DefaultExt = 'epub'
    Filter = 'EPUB Files (*.epub)|*.epub'
    Left = 16
    Top = 24
  end
end
