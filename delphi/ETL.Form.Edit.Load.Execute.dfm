inherited FoEditLoadExecute: TFoEditLoadExecute
  ClientHeight = 328
  ClientWidth = 651
  ExplicitWidth = 667
  ExplicitHeight = 367
  PixelsPerInch = 96
  TextHeight = 20
  object PageControl1: TPageControl [0]
    Left = 0
    Top = 0
    Width = 651
    Height = 328
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'TabSheet1'
      ExplicitLeft = -20
      ExplicitTop = 35
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
      object EdFileName: TEdit
        Left = 279
        Top = 94
        Width = 322
        Height = 28
        TabOrder = 0
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
        TabOrder = 1
      end
      object EdConnection: TEdit
        Left = 279
        Top = 34
        Width = 322
        Height = 28
        TabOrder = 2
      end
      object Button1: TButton
        Left = 607
        Top = 34
        Width = 33
        Height = 28
        Caption = '...'
        TabOrder = 3
      end
      object Button2: TButton
        Left = 607
        Top = 94
        Width = 33
        Height = 28
        Caption = '...'
        TabOrder = 4
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'TabSheet2'
      ImageIndex = 1
    end
  end
  inherited ActionList: TActionList
    Left = 200
    Top = 128
  end
end
