unit Form.Edit;

interface

uses Vcl.Controls, Vcl.StdCtrls, Vcl.CheckLst, System.Classes, Vcl.Forms, cxGraphics, cxControls,
  cxLookAndFeels, cxLookAndFeelPainters, cxSplitter, upControls, System.Actions, Vcl.ActnList;

type
  TFoEdit = class(TForm)
    ActionList: TActionList;
    AcClose: TAction;
    procedure AcCloseExecute(Sender: TObject);
  private
  end;

implementation

{$R *.dfm}

procedure TFoEdit.AcCloseExecute(Sender: TObject);
begin
  Close;
end;

end.
