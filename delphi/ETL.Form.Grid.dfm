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
    Hint = ''
    Align = alClient
    TabOrder = 0
    object tv: TcxGridBandedTableView
      Navigator.Buttons.CustomButtons = <>
      Navigator.Buttons.Insert.Visible = False
      Navigator.Buttons.Delete.Visible = False
      Navigator.Buttons.Edit.Visible = False
      Navigator.Buttons.Post.Visible = False
      Navigator.Buttons.Cancel.Visible = False
      Navigator.Visible = True
      FilterBox.Visible = fvAlways
      DataController.Summary.DefaultGroupSummaryItems = <>
      DataController.Summary.FooterSummaryItems = <>
      DataController.Summary.SummaryGroups = <>
      FilterRow.Visible = True
      OptionsBehavior.NavigatorHints = True
      OptionsBehavior.FixedGroups = True
      OptionsCustomize.ColumnHiding = True
      OptionsCustomize.GroupBySorting = True
      OptionsCustomize.BandHiding = True
      OptionsData.Deleting = False
      OptionsData.Editing = False
      OptionsData.Inserting = False
      OptionsView.Footer = True
      OptionsView.GroupFooters = gfVisibleWhenExpanded
      OptionsView.GroupSummaryLayout = gslAlignWithColumns
      OptionsView.Indicator = True
      OptionsView.BandHeaders = False
      Bands = <
        item
        end>
    end
    object Lv: TcxGridLevel
      GridView = tv
    end
  end
end
