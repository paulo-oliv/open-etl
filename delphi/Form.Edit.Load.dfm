inherited FoEditLoad: TFoEditLoad
  ClientWidth = 651
  ExplicitWidth = 667
  ExplicitHeight = 338
  PixelsPerInch = 96
  TextHeight = 20
  object Label1: TLabel
    Left = 303
    Top = 174
    Width = 93
    Height = 20
    Caption = 'Schema name'
  end
  object Label2: TLabel
    Left = 8
    Top = 234
    Width = 121
    Height = 20
    Caption = 'Records in a block'
  end
  object Label3: TLabel
    Left = 279
    Top = 68
    Width = 64
    Height = 20
    Caption = 'File name'
  end
  object LbConnection: TLabel
    Left = 279
    Top = 8
    Width = 75
    Height = 20
    Caption = 'Connection'
    FocusControl = EdConnection
  end
  object EdSchema: TEdit
    Left = 303
    Top = 200
    Width = 241
    Height = 28
    TabOrder = 0
  end
  object CbCommit: TCheckBox
    Left = 135
    Top = 265
    Width = 323
    Height = 17
    Caption = 'Insert "COMMIT" statement after each block'
    TabOrder = 1
  end
  object CbDisableFK: TCheckBox
    Left = 279
    Top = 128
    Width = 217
    Height = 17
    Caption = 'Disable Foreign Key Checks'
    TabOrder = 2
  end
  object CbUse: TCheckBox
    Left = 279
    Top = 151
    Width = 209
    Height = 17
    Caption = 'Generate "USE" stantement'
    TabOrder = 3
  end
  object EdFileName: TEdit
    Left = 279
    Top = 94
    Width = 322
    Height = 28
    TabOrder = 4
  end
  object RgDestination: TRadioGroup
    Left = 8
    Top = 8
    Width = 265
    Height = 114
    Caption = 'Script destination'
    ItemIndex = 1
    Items.Strings = (
      'Run directly on a database'
      'Save to file'
      'Automatically load to Script Editor')
    TabOrder = 5
  end
  object RgCommand: TRadioGroup
    Left = 8
    Top = 128
    Width = 265
    Height = 100
    Caption = 'Command'
    ItemIndex = 2
    Items.Strings = (
      'Insert Ingore'
      'Replace'
      'Insert Update')
    TabOrder = 6
  end
  object EdConnection: TEdit
    Left = 279
    Top = 34
    Width = 322
    Height = 28
    TabOrder = 7
  end
  object EdBlock: TSpinEdit
    Left = 8
    Top = 260
    Width = 121
    Height = 30
    MaxValue = 0
    MinValue = 0
    TabOrder = 8
    Value = 500
  end
  object Button1: TButton
    Left = 607
    Top = 34
    Width = 33
    Height = 28
    Caption = '...'
    TabOrder = 9
  end
  object Button2: TButton
    Left = 607
    Top = 94
    Width = 33
    Height = 28
    Caption = '...'
    TabOrder = 10
  end
end
