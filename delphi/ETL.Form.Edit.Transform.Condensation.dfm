inherited FoEditCondensation: TFoEditCondensation
  Caption = 'Edit Condensation'
  ClientHeight = 440
  ClientWidth = 385
  ExplicitWidth = 401
  ExplicitHeight = 479
  PixelsPerInch = 96
  TextHeight = 20
  object Gr: TcxVerticalGrid [0]
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 379
    Height = 434
    Hint = ''
    BorderStyle = cxcbsNone
    Align = alClient
    TabOrder = 0
    OnEditValueChanged = GrEditValueChanged
    Version = 1
    object GrEditorRow1: TcxEditorRow
      Properties.Caption = 'Teste'
      Properties.EditPropertiesClassName = 'TcxComboBoxProperties'
      Properties.EditProperties.DropDownListStyle = lsEditFixedList
      Properties.DataBinding.ValueType = 'String'
      Properties.Value = Null
      ID = 0
      ParentID = -1
      Index = 0
      Version = 1
    end
  end
end
