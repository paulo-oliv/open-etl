unit ETL.Form.Script;

interface

uses Vcl.Forms, cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxStyles,
  cxCustomData, cxFilter, cxData, cxDataStorage, cxEdit, cxNavigator, cxGridLevel, cxClasses,
  cxGridCustomView, cxGridCustomTableView, cxGridTableView, cxGridBandedTableView, System.Classes,
  Vcl.Controls, cxGrid, Vcl.StdCtrls;

type
  TFoScript = class(TForm)
    MemoScript: TMemo;
  strict private
  public
    class function New(const AOwner: TComponent): TFoScript;
  end;

implementation

{$R *.dfm}

{ TFoScript }

class function TFoScript.New(const AOwner: TComponent): TFoScript;
begin
  Result := TFoScript.Create(AOwner);
end;

end.
