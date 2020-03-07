unit ETL.Component.Load.Script;

interface

uses
  ETL.Component.Load,
  ETL.Form.Edit.Load.Script,
  ETL.Form.Script;

type
  TCompScript = class(TCompLoad)
  strict private
    FForm: TFoScript;
    function GetInstanceFormEdit: TFoEditLoadScript;
  strict protected
    FFormEdit: TFoEditLoadScript;
    function GetScript: string; override;
    procedure setScript(const AScript: string); override;
    procedure Preview; override;
    // procedure RefreshGrid(var AFormGrid: TFoGrid); override;
  public
    procedure Edit; override;
  end;

implementation

uses
  ETL.FileProject.Interfaces,
  System.SysUtils,
  ETL.Form.Grid,
  ETL.Form.Main;

{ TCompScript }

function TCompScript.GetInstanceFormEdit: TFoEditLoadScript;
var
  LSources: IListSources;
  i, j: integer;
begin
  if not Assigned(FFormEdit) then
  begin
    FFormEdit := TFoEditLoadScript.New(Self);
    TFoEditLoadScript(FFormEdit).Gr.ClearRows;
    LSources := GetSources;
    if Assigned(LSources) then
      for i := 0 to LSources.Count - 1 do
      begin
        with LSources.GetItem(i).GetGrid do
        begin
          for j := 0 to tv.ColumnCount - 1 do
            if tv.Columns[j].Visible then
              FFormEdit.AddField(tv.Columns[j].Caption)
        end;
      end;
  end;
  Result := TFoEditLoadScript(FFormEdit);
end;

function TCompScript.GetScript: string;
begin
  Result := '';
  if Assigned(FFormEdit) then
    Result := TFoEditLoadScript(FFormEdit).ToString;
end;

procedure TCompScript.setScript(const AScript: string);
var
  p: integer;

  function nextValue: string;
  var
    i: integer;
  begin
    Result := '';
    for i := p to AScript.Length do
      if AScript[i] = SEPARATE_SCRIPT_CHAR then
      begin
        p := i + 1;
        break;
      end
      else
        Result := Result + AScript[i];
  end;

var
  LValue: string;
  i, j: integer;
begin
  j := 0;
  p := 1;
  GetInstanceFormEdit;

  FFormEdit.RgCommand.ItemIndex := StrToIntDef(nextValue, -1);
  FFormEdit.CbDisableFK.Checked := nextValue = '1';
  FFormEdit.CbUse.Checked := nextValue = '1';
  FFormEdit.EdSchema.Text := nextValue;
  FFormEdit.CbCommit.Checked := nextValue = '1';
  FFormEdit.EdBlock.Text := nextValue;

  LValue := '';
  for i := p to AScript.Length do
    if AScript[i] = SEPARATE_SCRIPT_FIELDS_CHAR then
    begin
      if LValue <> '' then
      begin
        FFormEdit.SetValue(j, LValue);
        j := j + 1;
        LValue := '';
      end;
    end
    else
      LValue := LValue + AScript[i];
end;

procedure TCompScript.Preview;

// function ValorStr(F: TField): ShortString;
// begin
// if (F is TFloatField) or (F is TCurrencyField) {or (F is TIBBCDField)} then
// Result := FormatFloat('0.###', F.AsFloat, fs)
// else if (F is TIntegerField) or (F is TLargeintField) then
// begin
// Result := F.AsString;
// if Result='' then
// Result := 'NULL';
// end
// else if (F is TDateField) then
// Result := '"' +FormatDateTime('yyyy-MM-dd', F.AsDateTime) + '"'
// else if (F is TDateTimeField) then
// Result := '"' +FormatDateTime('yyyy-MM-dd hh:nn:ss', F.AsDateTime) + '"'
// else if (F is TTimeField) then
// Result := '"' +FormatDateTime('hh:nn:ss', F.AsDateTime) + '"'
// else
// Result := '"' + StringReplace(F.AsString, '"', '""', [rfReplaceAll]) + '"'
// end;

var
  i, j, p: integer;
  s, LTabelName, LFieldName: String; // t, u, tab: ;
  // fs: TFormatSettings;
  LFormGrid: TFoGrid;
begin
  LFormGrid := GetGrid;
  if not Assigned(FForm) then
    FForm := TFoScript.New(Owner);

  FForm.MemoScript.Clear;

  GetInstanceFormEdit;

  if FFormEdit.CbUse.Checked then
  begin
    s := trim(FFormEdit.EdSchema.Text);
    if (s <> '') then
      FForm.MemoScript.Lines.Add('USE ' + s + ';');
  end;

  if FFormEdit.CbDisableFK.Checked then
    FForm.MemoScript.Lines.Add('SET FOREIGN_KEY_CHECKS=0;');

  LFormGrid.tv.BeginUpdate;
  FoMain.ProgressBar.Show;
  try
    // FForm.MemoScript.Lines.Add('CREATE TABLE '+ tab +' (';
    for i := 0 to LFormGrid.tv.ColumnCount - 1 do // FFormEdit.Gr.Rows.Count
    begin
      LFieldName := FFormEdit.getValue(i);
      p := pos('.', LFieldName);
      if p > 0 then
      begin
        LTabelName := Copy(LFieldName, 1, p - 1);
        LFieldName := Copy(LFieldName, p + 1);
      end;

      // LFormGrid.tv.Columns[i].Caption;
      // u := trim(Fields[i].FieldName);
      // if (Fields[i] is TFloatField)
      // or (Fields[i] is TCurrencyField)
      // or (Fields[i] is TIBBCDField) then
      // u := u + ' Float'
      // else if (Fields[i] is TIntegerField)
      // or (Fields[i] is TLargeintField)
      // or (Fields[i] is TSmallintField) then
      // u := u + ' Integer'
      // else if (Fields[i] is TDateField) then
      // u := u + ' Date'
      // else if (Fields[i] is TDateTimeField) then
      /// u := u + ' Datetime'
      // else if (Fields[i] is TTimeField) then
      // u := u + ' Time'
      // else
      // u := u + ' Varchar(' + IntToStr(Fields[i].size) + ')';
      // t := t + u + ','#13#10;
      // end;
      // t := copy(t, 1, length(t) - 3) + ');';
    end;
    FoMain.ProgressBar.Max := LFormGrid.tv.DataController.RowCount;
    for i := 0 to LFormGrid.tv.DataController.RowCount - 1 do
      for j := 0 to LFormGrid.tv.ColumnCount - 1 do
      begin
        FoMain.ProgressBar.Position := i;
        // LFormGrid.tv.DataController.Values[k, j] :=;

        // FFormEdit.RgCommand.ItemIndex;
        // FFormEdit.CbCommit.Checked;
        // FFormEdit.EdBlock.Text;
        // FForm.MemoScript.Lines.Add()

        // t := 'INSERT INTO ' + tab + '(';
        // for i := 0 to LV.Items.Count - 1 do
        // if LV.Items[i].Checked then
        // begin
        // u := trim(LV.Items[i].Caption);
        // if (u <> '') then
        // t := t + u + ',';
        // end;
        // S.Add(copy(t, 1, length(t) - 1) + ') VALUES ');
        // while not Conexao.DataSetQuery.Eof do
        // begin
        // t := '(';
        // for i := 0 to Conexao.DataSetQuery.FieldCount - 1 do
        // if Conexao.DataSetQuery.Fields[i].Visible then
        // t := t + ValorStr(Conexao.DataSetQuery.Fields[i]) + ',';
        // S.Add(copy(t, 1, length(t) - 1) + '),');
        // Conexao.DataSetQuery.Next;
        // end;
        // S[S.Count - 1] := copy(S[S.Count - 1], 1, length(S[S.Count - 1]) - 1) + ';';

      end;
  finally
    LFormGrid.tv.EndUpdate;
    FoMain.ProgressBar.Hide;
  end;

  // Result := VarToStr(TcxEditorRow(Gr.Rows.Items[i]).Properties.Value);
  // fs.DecimalSeparator := '.';
  (*
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
  *)

  FForm.MemoScript.Lines.Add('COMMIT;');
  FForm.Show;
end;

procedure TCompScript.Edit;
begin
  with GetInstanceFormEdit do
  begin
    Caption := Title;
    ShowModal;
  end;
end;

end.
