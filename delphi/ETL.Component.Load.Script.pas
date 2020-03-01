unit ETL.Component.Load.Script;

interface

uses ETL.Component.Load, ETL.Form.Edit.Load.Script, ETL.Form.Script;

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
  ETL.Form.Grid;

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
              TFoEditLoadScript(FFormEdit).AddField(tv.Columns[j].Caption)
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
  i: integer;
  s: String; // t, u, tab: ;
  // fs: TFormatSettings;
  LFormGrid: TFoGrid;
begin
  GetGrid;
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

  // FFormEdit.RgCommand.ItemIndex;
  // FFormEdit.CbCommit.Checked;
  // FFormEdit.EdBlock.Text;

{

      try
        for i := 0 to AFormGrid.tv.ColumnCount - 1 do
          if (AFormGrid.tv.Columns[i].Tag = TAG_COL_BEST) or
            (AFormGrid.tv.Columns[i].Tag = TAG_COL_GROUP_BY) then
            with LNewFoGrid.tv.CreateColumn do
            begin
              Caption := AFormGrid.tv.Columns[i].Caption;
              Tag := i;
            end;

        with LNewFoGrid.tv.CreateColumn do
        begin
          Caption := 'hash';
          Tag := TAG_COL_MD5;
        end;

        k := 0;
        FoMain.ProgressBar.Max := AFormGrid.tv.DataController.RowCount;
        for i := 0 to AFormGrid.tv.DataController.RowCount - 1 do
        begin
          FoMain.ProgressBar.Position := i;
          LNewFoGrid.tv.DataController.RecordCount := k + 1;
          LNewRow := True;
          for j := 0 to LNewFoGrid.tv.ColumnCount - 1 do
            if LNewFoGrid.tv.Columns[j].Tag = TAG_COL_MD5 then
            begin
              LMd5 := StrToMd5(LMd5);
              if LMd5 = LLastMD5 then
              begin
                LNewRow := False;
                Break;
              end;
              LLastMD5 := LMd5;
              LNewFoGrid.tv.DataController.Values[k, j] := LMd5;
              LMd5 := '';
            end
            else
            begin
              LMd5 := LMd5 + VarToStr(AFormGrid.tv.DataController.Values[i,
                LNewFoGrid.tv.Columns[j].Tag]) + ',';
              LNewFoGrid.tv.DataController.Values[k, j] := AFormGrid.tv.DataController.Values
                [i, LNewFoGrid.tv.Columns[j].Tag];
            end;
          if LNewRow then
            k := k + 1;
        end;

      finally
        AFormGrid.DisposeOf;
        LNewFoGrid.tv.EndUpdate;
        AFormGrid := LNewFoGrid;
        FoMain.ProgressBar.Hide;
      end;

}


  // for i := 0 to FFormEdit.Gr.Rows.Count - 1 do

  /// Result := VarToStr(TcxEditorRow(Gr.Rows.Items[i]).Properties.Value);

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
