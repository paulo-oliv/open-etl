unit ETL.Form.Edit.Extract.Files;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, ETL.Form.Edit.Extract, System.Actions, Vcl.ActnList;

type
  TFoEditFiles = class(TFoEditExtract)
  strict private
  end;

implementation

{$R *.dfm}
{ Adaptar a funcao abaixo para importar do Excel

  function XlsToStringGrid(AXLSFile: string): Boolean;
  const xlCellTypeLastCell = $0000000B;
  var
  XLApp, Sheet: OLEVariant;
  RangeMatrix: Variant;
  x, y, k, r: Integer;
  begin
  Result:=False;
  XLApp:=CreateOleObject('Excel.Application');
  try
  XLApp.Visible:=False; //Esconde Excel

  XLApp.Workbooks.Open(AXLSFile);
  Sheet:=XLApp.Workbooks[ExtractFileName(AXLSFile)].WorkSheets[1];
  Sheet.Cells.SpecialCells(xlCellTypeLastCell, EmptyParam).Activate;

  x:=XLApp.ActiveCell.Row; //Pegar o número da última linha

  y:=XLApp.ActiveCell.Column; //Pegar o número da última coluna
  //Seta Stringgrid linha e coluna
  AGrid.RowCount:=x;
  AGrid.ColCount:=y;
  //Associaca a variant WorkSheet com a variant do Delphi
  RangeMatrix:=XLApp.Range['A1', XLApp.Cells.Item[X, Y]].Value;
  //Cria o loop para listar os registros no TStringGrid
  k:=1;
  repeat
  for r:=1 to y do
  AGrid.Cells[(r - 1),(k - 1)]:=RangeMatrix[K, R];
  Inc(k,1);
  until k > x;
  RangeMatrix:=Unassigned;
  finally
  //Fecha o Excel
  if not VarIsEmpty(XLApp) then
  begin
  XLApp.Quit;
  XLAPP:=Unassigned;
  Sheet:=Unassigned;
  Result:=True;
  end;
  end;
  end; }

end.
