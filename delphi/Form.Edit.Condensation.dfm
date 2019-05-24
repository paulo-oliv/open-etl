inherited FoEditCondensation: TFoEditCondensation
  Caption = 'Edit Condensation'
  ClientHeight = 440
  ClientWidth = 413
  ExplicitWidth = 429
  ExplicitHeight = 474
  PixelsPerInch = 96
  TextHeight = 20
  object RgKind: TRadioGroup [0]
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 185
    Height = 434
    Align = alLeft
    Items.Strings = (
      'Sum'
      'Average'
      'Max'
      'Min'
      'Count'
      'Best')
    TabOrder = 0
    ExplicitTop = -2
  end
  object ClColumns: TCheckListBox [1]
    AlignWithMargins = True
    Left = 194
    Top = 3
    Width = 216
    Height = 434
    Align = alClient
    ItemHeight = 20
    TabOrder = 1
  end
end
