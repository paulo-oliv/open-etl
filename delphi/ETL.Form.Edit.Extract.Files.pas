unit ETL.Form.Edit.Extract.Files;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, ETL.Form.Edit.Extract, System.Actions, Vcl.ActnList,
  Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.StdActns;

type
  TFoEditFiles = class(TFoEditExtract)
    EdPath: TButtonedEdit;
    AcBrowseForFolder: TBrowseForFolder;
    ListBoxFiles: TListBox;
    Panel1: TPanel;
    EdFilter: TEdit;
    CbSubDir: TCheckBox;
    procedure EdPathRightButtonClick(Sender: TObject);
    procedure EdFilterChange(Sender: TObject);
    procedure ListBoxFilesDblClick(Sender: TObject);
  strict private
    procedure updateListFiles;
    procedure FixFiles;
  public
    class function New(const AOwner: TComponent): TFoEditFiles;
  end;

implementation

{$R *.dfm}
{ TFoEditFiles }

procedure TFoEditFiles.updateListFiles;
begin
  ListBoxFiles.Clear;
end;

procedure TFoEditFiles.EdFilterChange(Sender: TObject);
var
  S: TSearchRec;
  Pasta: string;
begin
  ListBoxFiles.Clear;
  if FindFirst(EdPath.Text + EdFilter.Text, faAnyFile, S) = 0 then
    repeat
      ListBoxFiles.Items.Add(S.Name);
    until FindNext(S) <> 0;
  FindClose(S);
end;

procedure TFoEditFiles.EdPathRightButtonClick(Sender: TObject);
begin
  if AcBrowseForFolder.Execute then
  begin
    EdPath.Text := AcBrowseForFolder.Folder;

  end;
end;

procedure TFoEditFiles.FixFiles;
var
  i: Integer;
  // LFile: TextFile;
  LStrs: TStringList;
  LNmDatabase: string;
begin
  LStrs := TStringList.Create;
  try
    for i := 0 to ListBoxFiles.Count - 1 do
    begin
      LStrs.LoadFromFile(EdPath.Text + ListBoxFiles.Items[i]);
      // AssignFile(LFile, EdPath.Text + ListBoxFiles.Items[i]);
      // try
      // Append(LFile);
      LNmDatabase := copy(ListBoxFiles.Items[i], 1, LastDelimiter('.', ListBoxFiles.Items[i]) - 1);
      LStrs.Insert(6, 'CREATE DATABASE ' + LNmDatabase + '; use ' + LNmDatabase + ';');
      // finally
      // CloseFile(LFile);
      // end;
      LStrs.SaveToFile(EdPath.Text + '_' + ListBoxFiles.Items[i]);
    end;
  finally
    LStrs.DisposeOf;
  end;
end;

procedure TFoEditFiles.ListBoxFilesDblClick(Sender: TObject);
begin
  FixFiles
end;

class function TFoEditFiles.New(const AOwner: TComponent): TFoEditFiles;
begin
  Result := TFoEditFiles.Create(AOwner);
end;

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
