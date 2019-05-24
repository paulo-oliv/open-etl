inherited FoEditQuery: TFoEditQuery
  Caption = 'Edit Query'
  ClientWidth = 642
  ExplicitWidth = 658
  ExplicitHeight = 338
  PixelsPerInch = 96
  TextHeight = 20
  object MM: TMemo [0]
    AlignWithMargins = True
    Left = 202
    Top = 3
    Width = 437
    Height = 298
    Align = alClient
    Color = clBtnFace
    ScrollBars = ssBoth
    TabOrder = 1
    ExplicitWidth = 430
  end
  object PnEsquerda: TPanel [1]
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 185
    Height = 298
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 0
    object ClConexoes: TCheckListBox
      AlignWithMargins = True
      Left = 3
      Top = 49
      Width = 179
      Height = 246
      Align = alClient
      ItemHeight = 20
      PopupMenu = PopupMenu1
      TabOrder = 0
    end
    object Button1: TButton
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 179
      Height = 40
      Action = AcReverseChecks
      Align = alTop
      Images = DmMain.IL32
      TabOrder = 1
    end
  end
  object pSplitter1: TpSplitter [2]
    Left = 191
    Top = 0
    Width = 8
    Height = 304
  end
  inherited ActionList: TActionList
    object AcReverseChecks: TAction
      Caption = 'Reverse Checks'
      ImageIndex = 13
      OnExecute = AcReverseChecksExecute
    end
    object AcEditConnection: TAction
      Caption = 'Edit Connection...'
      ImageIndex = 14
      OnExecute = AcEditConnectionExecute
    end
    object AcDeleteConnection: TAction
      Caption = 'Delete Connection'
      ImageIndex = 15
      OnExecute = AcDeleteConnectionExecute
    end
    object AcNewConnection: TAction
      Caption = 'New Connection...'
      ImageIndex = 11
      OnExecute = AcNewConnectionExecute
    end
    object AcCheckAll: TAction
      Caption = 'AcCheckAll'
    end
    object AcUncheckAll: TAction
      Caption = 'AcUncheckAll'
    end
  end
  object PopupMenu1: TPopupMenu
    Images = DmMain.IL32
    Left = 80
    Top = 144
    object New1: TMenuItem
      Action = AcNewConnection
    end
    object Edit1: TMenuItem
      Action = AcEditConnection
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object Delete1: TMenuItem
      Action = AcDeleteConnection
    end
  end
end
