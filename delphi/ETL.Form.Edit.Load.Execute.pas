unit ETL.Form.Edit.Load.Execute;

interface

uses ETL.Form.Edit, Vcl.StdCtrls, Vcl.Samples.Spin, Vcl.Controls, Vcl.ExtCtrls, System.Classes,
  System.Actions, Vcl.ActnList, Vcl.ComCtrls;

type
  TFoEditLoadExecute = class(TFoEdit)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    Label3: TLabel;
    LbConnection: TLabel;
    EdFileName: TEdit;
    RgDestination: TRadioGroup;
    EdConnection: TEdit;
    Button1: TButton;
    Button2: TButton;
    TabSheet2: TTabSheet;
  private
  public
    class function New(const AOwner: TComponent): TFoEditLoadExecute;
  end;


implementation

{$R *.dfm}

{ TFoEditLoadExecute }

class function TFoEditLoadExecute.New(const AOwner: TComponent): TFoEditLoadExecute;
begin
  Result := TFoEditLoadExecute.Create(AOwner);
end;

(*
  procedure TFoPump.criaScript(S: TStrings);
  var
  i: Integer;
  t, u, tab: String;
  fs: TFormatSettings;

  function ValorStr(F: TField): ShortString;
  begin
  if (F is TFloatField) or (F is TCurrencyField) {or (F is TIBBCDField)} then
  Result := FormatFloat('0.###', F.AsFloat, fs)
  else if (F is TIntegerField) or (F is TLargeintField) then
  begin
  Result := F.AsString;
  if Result='' then
  Result := 'NULL';
  end
  else if (F is TDateField) then
  Result := '"' +FormatDateTime('yyyy-MM-dd', F.AsDateTime) + '"'
  else if (F is TDateTimeField) then
  Result := '"' +FormatDateTime('yyyy-MM-dd hh:nn:ss', F.AsDateTime) + '"'
  else if (F is TTimeField) then
  Result := '"' +FormatDateTime('hh:nn:ss', F.AsDateTime) + '"'
  else
  Result := '"' + StringReplace(F.AsString, '"', '""', [rfReplaceAll]) + '"'
  end;

  begin
  fs.DecimalSeparator := '.';
  Conexao.DataSetQuery.First;
  S.Clear;
  u := trim(EdNovoBanco.Text);
  if (u <> '') then
  S.Add('USE ' + u + ';');

  tab := trim(EdNovaTabela.Text);
  if (tab = '') then
  begin
  ShowMessage('Falta digitar o nome da nova tabela');
  Exit;
  end;

  if RadioGroup1.ItemIndex = 1 then
  begin
  while not Conexao.DataSetQuery.Eof do
  begin
  t := 'UPDATE ' + tab + ' SET ';
  for i := 1 to Conexao.DataSetQuery.FieldCount - 1 do
  if Conexao.DataSetQuery.Fields[i].Visible then
  t := t + LV.Items[i].Caption + '=' + ValorStr(Conexao.DataSetQuery.Fields[i]) + ',';
  t := copy(t, 1, length(t) - 1) + ' WHERE ' + LV.Items[0].Caption + '=' + ValorStr
  (Conexao.DataSetQuery.Fields[0]) + ';';
  S.Add(t);
  Conexao.DataSetQuery.Next;
  end;
  end
  else
  begin
  if CbCreate.Checked then
  begin
  t := 'CREATE TABLE '+ tab +' (';
  for i := 0 to Conexao.DataSetQuery.FieldCount - 1 do
  if Conexao.DataSetQuery.Fields[i].Visible then
  begin
  u := trim(Conexao.DataSetQuery.Fields[i].FieldName);
  if (Conexao.DataSetQuery.Fields[i] is TFloatField)
  or (Conexao.DataSetQuery.Fields[i] is TCurrencyField)
  {or (Conexao.DataSetQuery.Fields[i] is TIBBCDField)} then
  u := u + ' Float'
  else if (Conexao.DataSetQuery.Fields[i] is TIntegerField)
  or (Conexao.DataSetQuery.Fields[i] is TLargeintField)
  or (Conexao.DataSetQuery.Fields[i] is TSmallintField) then
  u := u + ' Integer'
  else if (Conexao.DataSetQuery.Fields[i] is TDateField) then
  u := u + ' Date'
  else if (Conexao.DataSetQuery.Fields[i] is TDateTimeField) then
  u := u + ' Datetime'
  else if (Conexao.DataSetQuery.Fields[i] is TTimeField) then
  u := u + ' Time'
  else
  u := u + ' Varchar(' + IntToStr(Conexao.DataSetQuery.Fields[i].size) + ')';
  t := t + u + ','#13#10;
  end;
  t := copy(t, 1, length(t) - 3) + ');';
  S.Add(t);
  end;

  t := 'INSERT INTO ' + tab + '(';

  for i := 0 to LV.Items.Count - 1 do
  if LV.Items[i].Checked then
  begin
  u := trim(LV.Items[i].Caption);
  if (u <> '') then
  t := t + u + ',';
  end;
  S.Add(copy(t, 1, length(t) - 1) + ') VALUES ');
  while not Conexao.DataSetQuery.Eof do
  begin
  t := '(';
  for i := 0 to Conexao.DataSetQuery.FieldCount - 1 do
  if Conexao.DataSetQuery.Fields[i].Visible then
  t := t + ValorStr(Conexao.DataSetQuery.Fields[i]) + ',';
  S.Add(copy(t, 1, length(t) - 1) + '),');
  Conexao.DataSetQuery.Next;
  end;
  S[S.Count - 1] := copy(S[S.Count - 1], 1, length(S[S.Count - 1]) - 1) + ';';
  end;
  S.Add('COMMIT;');

  end; *)


end.
