object FoEdit: TFoEdit
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSizeToolWin
  ClientHeight = 304
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -15
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 20
  object ActionList: TActionList
    Images = DmMain.IL32
    Left = 192
    Top = 96
    object AcCloseWindow: TAction
      Caption = 'Close'
      ImageIndex = 2
      ShortCut = 27
      OnExecute = AcCloseWindowExecute
    end
  end
end
