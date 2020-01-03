unit ETL.Component.Load.Script;

interface

uses ETL.Component.Load, ETL.Form.Edit.Load.Script;

type
  TCompScript = class(TCompLoad)
  strict private
    function GetInstanceFormEdit: TFoEditLoadScript;
  strict protected
    FFormEdit: TFoEditLoadScript;
    function GetScript: string; override;
    procedure setScript(const AScript: string); override;
    // procedure RefreshGrid(var AFormGrid: TFoGrid); override;
  public
    procedure Edit; override;
  end;

implementation

uses ETL.FileProject.Interfaces, SysUtils;

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
  LFormEdit: TFoEditLoadScript;
  i, j: integer;
begin
  j := 0;
  p := 1;
  LFormEdit := GetInstanceFormEdit;

  LFormEdit.RgCommand.ItemIndex := StrToIntDef(nextValue, -1);

  LFormEdit.CbDisableFK.Checked := nextValue = '1';

  LFormEdit.CbUse.Checked := nextValue = '1';

  LFormEdit.EdSchema.Text := nextValue;

  LFormEdit.CbCommit.Checked := nextValue = '1';

  LFormEdit.EdBlock.Text := nextValue;

  LValue := '';
  for i := p to AScript.Length do
    if AScript[i] = SEPARATE_SCRIPT_FIELDS_CHAR then
    begin
      if LValue <> '' then
      begin
        LFormEdit.SetValue(j, LValue);
        j := j + 1;
        LValue := '';
      end;
    end
    else
      LValue := LValue + AScript[i];
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
