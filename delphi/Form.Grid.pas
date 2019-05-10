unit Form.Grid;

interface

uses Vcl.Forms, cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxStyles,
  cxCustomData, cxFilter, cxData, cxDataStorage, cxEdit, cxNavigator, cxGridLevel, cxClasses,
  cxGridCustomView, cxGridCustomTableView, cxGridTableView, cxGridBandedTableView, System.Classes,
  Vcl.Controls, cxGrid;

type
  TFoGrid = class(TForm)
    Lv: TcxGridLevel;
    Gr: TcxGrid;
    tv: TcxGridBandedTableView;
  strict private
  public
    class function New(const AOwner: TComponent): TFoGrid;
  end;

implementation

{$R *.dfm}

{ TFoGrid }

class function TFoGrid.New(const AOwner: TComponent): TFoGrid;
begin
  Result := TFoGrid.Create(AOwner);
end;

end.
