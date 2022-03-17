object FormSerialCommunication: TFormSerialCommunication
  Left = 0
  Top = 0
  Caption = 'Serial Communication'
  ClientHeight = 299
  ClientWidth = 520
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 520
    Height = 41
    Align = alTop
    TabOrder = 0
    object Label1: TLabel
      Left = 97
      Top = 5
      Width = 236
      Height = 13
      Caption = 'Double Click on the wanter communication port...'
    end
    object Button1: TButton
      Left = 8
      Top = 5
      Width = 83
      Height = 28
      Caption = 'Update List'
      TabOrder = 0
      OnClick = Button1Click
    end
  end
  object ListBoxSerial: TListBox
    Left = 0
    Top = 41
    Width = 520
    Height = 258
    Align = alClient
    Color = clWhite
    Columns = 2
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Tahoma'
    Font.Style = []
    ItemHeight = 18
    ParentFont = False
    ScrollWidth = 1
    TabOrder = 1
    OnDblClick = ListBoxSerialDblClick
  end
end
