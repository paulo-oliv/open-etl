inherited FoEditFiles: TFoEditFiles
  Caption = 'FoEditFiles'
  ClientHeight = 352
  ExplicitHeight = 391
  PixelsPerInch = 96
  TextHeight = 20
  object EdPath: TButtonedEdit [0]
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 629
    Height = 28
    Align = alTop
    Images = DmMain.IL16
    RightButton.ImageIndex = 0
    RightButton.Visible = True
    TabOrder = 0
    TextHint = 'Select the folder'
    OnRightButtonClick = EdPathRightButtonClick
  end
  object ListBoxFiles: TListBox [1]
    AlignWithMargins = True
    Left = 3
    Top = 72
    Width = 629
    Height = 277
    Align = alClient
    ItemHeight = 20
    TabOrder = 1
    OnDblClick = ListBoxFilesDblClick
  end
  object Panel1: TPanel [2]
    AlignWithMargins = True
    Left = 3
    Top = 37
    Width = 629
    Height = 29
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
    object EdFilter: TEdit
      Left = 0
      Top = 0
      Width = 453
      Height = 29
      Align = alClient
      AutoSize = False
      TabOrder = 0
      Text = '*.*'
      OnChange = EdFilterChange
    end
    object CbSubDir: TCheckBox
      AlignWithMargins = True
      Left = 456
      Top = 3
      Width = 170
      Height = 23
      Align = alRight
      Caption = 'Include subdirectories'
      TabOrder = 1
    end
  end
  inherited ActionList: TActionList
    object AcBrowseForFolder: TBrowseForFolder
      Category = 'File'
      Caption = 'AcBrowseForFolder'
      DialogCaption = 'AcBrowseForFolder'
      BrowseOptions = []
      BrowseOptionsEx = []
      ImageIndex = 0
    end
  end
end
