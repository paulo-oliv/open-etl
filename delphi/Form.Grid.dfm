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
    Height = 335
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
      OptionsCustomize.ColumnHiding = True
      OptionsCustomize.GroupBySorting = True
      OptionsCustomize.BandHiding = True
      OptionsData.Deleting = False
      OptionsData.Editing = False
      OptionsData.Inserting = False
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
  object Button1: TButton
    Left = 0
    Top = 341
    Width = 635
    Height = 25
    Align = alBottom
    Caption = 'Button1'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 0
    Top = 366
    Width = 635
    Height = 25
    Align = alBottom
    Caption = 'Button2'
    TabOrder = 2
    OnClick = Button2Click
  end
end
