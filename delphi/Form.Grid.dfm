object FoGrid: TFoGrid
  Left = 0
  Top = 0
  BorderStyle = bsSizeToolWin
  Caption = 'Results'
  ClientHeight = 391
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Gr: TcxGrid
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 629
    Height = 385
    Align = alClient
    TabOrder = 0
    object tv: TcxGridBandedTableView
      Navigator.Buttons.CustomButtons = <>
      DataController.Summary.DefaultGroupSummaryItems = <>
      DataController.Summary.FooterSummaryItems = <>
      DataController.Summary.SummaryGroups = <>
      Bands = <
        item
        end>
    end
    object Lv: TcxGridLevel
      GridView = tv
    end
  end
end
