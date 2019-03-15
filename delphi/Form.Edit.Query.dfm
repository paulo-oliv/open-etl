inherited FoEditQuery: TFoEditQuery
  Caption = 'Edit Query'
  PixelsPerInch = 96
  TextHeight = 20
  object pSplitter1: TpSplitter [0]
    Left = 191
    Top = 0
    Width = 8
    Height = 304
  end
  object MM: TMemo [1]
    AlignWithMargins = True
    Left = 202
    Top = 3
    Width = 430
    Height = 298
    Align = alClient
    Color = clBtnFace
    ScrollBars = ssBoth
    TabOrder = 1
  end
  object PnEsquerda: TPanel [2]
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 185
    Height = 298
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 2
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
      Action = AcInvert
      Align = alTop
      Images = DmImages.ILDev32
      TabOrder = 1
    end
  end
  inherited ActionList: TActionList
    object AcInvert: TAction
      Caption = 'Reverse Checks'
      ImageIndex = 13
      OnExecute = AcInvertExecute
    end
    object AcEdit: TAction
      Caption = 'Edit...'
      ImageIndex = 14
    end
    object AcDelete: TAction
      Caption = 'Delete'
      ImageIndex = 15
    end
    object AcNew: TAction
      Caption = 'New...'
      ImageIndex = 11
      OnExecute = AcNewExecute
    end
  end
  object PopupMenu1: TPopupMenu
    Images = DmImages.ILDev32
    Left = 80
    Top = 144
    object New1: TMenuItem
      Caption = 'New...'
      ImageIndex = 11
      OnClick = AcNewExecute
    end
    object Edit1: TMenuItem
      Caption = 'Edit...'
      ImageIndex = 14
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object Delete1: TMenuItem
      Caption = 'Delete'
      ImageIndex = 15
    end
  end
end
