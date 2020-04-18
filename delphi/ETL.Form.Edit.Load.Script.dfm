inherited FoEditLoadScript: TFoEditLoadScript
  ClientHeight = 278
  ClientWidth = 484
  ExplicitWidth = 500
  ExplicitHeight = 317
  PixelsPerInch = 96
  TextHeight = 20
  object PageControl1: TPageControl [0]
    Left = 0
    Top = 0
    Width = 484
    Height = 278
    ActivePage = TsSettings
    Align = alClient
    TabOrder = 0
    object TsSettings: TTabSheet
      Caption = 'Settings'
      object Label2: TLabel
        Left = 154
        Top = 34
        Width = 121
        Height = 20
        Caption = 'Records in a block'
      end
      object Label1: TLabel
        Left = 2
        Top = 183
        Width = 93
        Height = 20
        Caption = 'Schema name'
      end
      object CbCommit: TCheckBox
        Left = 154
        Top = 12
        Width = 323
        Height = 17
        Caption = 'Insert "COMMIT" statement after each block'
        TabOrder = 0
      end
      object CbDisableFK: TCheckBox
        Left = 3
        Top = 137
        Width = 217
        Height = 25
        Caption = 'Disable Foreign Key Checks'
        TabOrder = 1
      end
      object CbUse: TCheckBox
        Left = 3
        Top = 163
        Width = 209
        Height = 17
        Caption = 'Generate "USE" stantement'
        TabOrder = 2
      end
      object RgCommand: TRadioGroup
        Left = 3
        Top = 3
        Width = 145
        Height = 128
        Caption = 'Command'
        ItemIndex = 2
        Items.Strings = (
          'Insert Ingore'
          'Replace'
          'Insert Update'
          'Create Table')
        TabOrder = 3
      end
      object EdBlock: TSpinEdit
        Left = 154
        Top = 60
        Width = 57
        Height = 30
        MaxValue = 0
        MinValue = 0
        TabOrder = 4
        Value = 500
      end
      object EdSchema: TEdit
        Left = 3
        Top = 209
        Width = 241
        Height = 28
        TabOrder = 5
      end
    end
    object TsNames: TTabSheet
      Caption = 'Names'
      ImageIndex = 1
      object Gr: TcxVerticalGrid
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 470
        Height = 237
        Hint = ''
        BorderStyle = cxcbsNone
        Align = alClient
        TabOrder = 0
        Version = 1
        object GrEditorRow1: TcxEditorRow
          Properties.Caption = 'caption'
          Properties.EditPropertiesClassName = 'TcxTextEditProperties'
          Properties.DataBinding.ValueType = 'String'
          Properties.Value = 'value'
          ID = 0
          ParentID = -1
          Index = 0
          Version = 1
        end
      end
    end
  end
  inherited ActionList: TActionList
    Left = 312
    Top = 80
  end
end
