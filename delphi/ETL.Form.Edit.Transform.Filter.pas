unit ETL.Form.Edit.Transform.Filter;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, ETL.Form.Edit.Transform, System.Actions, Vcl.ActnList,
  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxFilterControl;

type
  TFoEditFilter = class(TFoEditTransform)
    cxFilterControl1: TcxFilterControl;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FoEditFilter: TFoEditFilter;

implementation

{$R *.dfm}

end.
