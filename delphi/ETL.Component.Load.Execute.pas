unit ETL.Component.Load.Execute;

interface

uses ETL.Component.Load, ETL.Form.Edit.Load.Execute;

const
  SEPARATE_SCRIPT_CHAR = '|';
  SEPARATE_SCRIPT_FIELDS_CHAR = ',';

type
  TCompExecute = class(TCompLoad)
  strict private
    function GetInstanceFormEdit: TFoEditLoadExecute;
  strict protected
    FFormEdit: TFoEditLoadExecute;
    function GetScript: string; override;
    procedure setScript(const AScript: string); override;
  public
    procedure Edit; override;
  end;

implementation

uses ETL.FileProject.Interfaces, SysUtils;

{ TCompExecute }

function TCompExecute.GetInstanceFormEdit: TFoEditLoadExecute;
var
  LSources: IListSources;
  i, j: integer;
begin
  if not Assigned(FFormEdit) then
  begin
    FFormEdit := TFoEditLoadExecute.New(Self);
    LSources := GetSources;
    if Assigned(LSources) then
      for i := 0 to LSources.Count - 1 do
      begin
        with LSources.GetItem(i).GetGrid do
        begin
          // for j := 0 to tv.ColumnCount - 1 do
          // if tv.Columns[j].Visible then
          // TFoEditLoadExecute(FFormEdit).AddField(tv.Columns[j].Caption)
        end;
      end;
  end;
  Result := TFoEditLoadExecute(FFormEdit);
end;

function TCompExecute.GetScript: string;
begin
  Result := '';
  if Assigned(FFormEdit) then
    Result := TFoEditLoadExecute(FFormEdit).ToString;
end;

procedure TCompExecute.setScript(const AScript: string);
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
  LFormEdit: TFoEditLoadExecute;
  i, j: integer;
begin
  j := 0;
  p := 1;
  LFormEdit := GetInstanceFormEdit;

//  LFormEdit.RgCommand.ItemIndex := StrToIntDef(nextValue, -1);

 // LFormEdit.CbDisableFK.Checked := nextValue = '1';

 // LFormEdit.CbUse.Checked := nextValue = '1';

//  LFormEdit.EdSchema.Text := nextValue;

//  LFormEdit.CbCommit.Checked := nextValue = '1';

//  LFormEdit.EdBlock.Text := nextValue;

  LValue := '';
//  for i := p to AScript.Length do
//    if AScript[i] = SEPARATE_SCRIPT_FIELDS_CHAR then
//    begin
//      if LValue <> '' then
//      begin
//        LFormEdit.SetValue(j, LValue);
//        j := j + 1;
//        LValue := '';
//      end;
//    end
//    else
//      LValue := LValue + AScript[i];
end;

procedure TCompExecute.Edit;
begin
  with GetInstanceFormEdit do
  begin
    Caption := Title;
    ShowModal;
  end;
end;

end.
