unit ETL.Component.Transform;

interface

uses
  ETL.Component,
  ETL.Form.Edit.Transform,
  ETL.Form.Edit.Transform.Condensation,
  ETL.FileProject.Interfaces;

type
  TCompTransform = class(TComponentETL, IComponentTransform)
  strict protected
    FFormEdit: TFoEditTransform;
  public
    procedure setTitle(const ATitle: string); override;
    procedure Edit; override;
  end;

  TCompFilter = class(TCompTransform)
  end;

  TCompConversion = class(TCompTransform)
  end;

  TCompDerivation = class(TCompTransform)
  end;

  TCompJoin = class(TCompTransform)
  end;

  TCompCondensation = class(TCompTransform)
  strict private
    function GetInstanceFormEdit: TFoEditCondensation;
  strict protected
    function GetScript: string; override;
    procedure setScript(const AScript: string); override;
  public
    procedure Edit; override;
  end;

implementation

uses System.SysUtils;

{ TCompTransform }

procedure TCompTransform.Edit;
begin
  if not Assigned(FFormEdit) then
  begin
    FFormEdit := TFoEditTransform.New(Self);
    FFormEdit.Caption := Title;
  end;
  FFormEdit.ShowModal;
end;

procedure TCompTransform.setTitle(const ATitle: string);
begin
  inherited setTitle(ATitle);
  if Assigned(FFormEdit) then
    FFormEdit.Caption := ATitle;
end;

{ TCompCondensation }

function TCompCondensation.GetInstanceFormEdit: TFoEditCondensation;
var
  LSources: IListSources;
  i, j: Integer;
begin
  if not Assigned(FFormEdit) then
    FFormEdit := TFoEditCondensation.New(Self);
  FFormEdit.Caption := Title;
  LSources := GetSources;
  if Assigned(LSources) then
    for i := 0 to LSources.Count - 1 do
    begin
      with LSources.GetItem(i).GetGrid do
      begin
        for j := 0 to tv.ColumnCount - 1 do
          if tv.Columns[j].Visible then
            TFoEditCondensation(FFormEdit).AddField(tv.Columns[j].Caption)
      end;
    end;
  Result := TFoEditCondensation(FFormEdit);
end;

procedure TCompCondensation.Edit;
begin
  with GetInstanceFormEdit do
  begin
    ShowModal;
  end;
end;

function TCompCondensation.GetScript: string;
begin
  Result := '';
  if Assigned(FFormEdit) then
    Result := TFoEditCondensation(FFormEdit).ToString;
end;

procedure TCompCondensation.setScript(const AScript: string);
var
  LValue: string;
  i, j: Integer;
  LFormEdit: TFoEditCondensation;
begin
  j := 0;
  LValue := '';
  LFormEdit := GetInstanceFormEdit;
  for i := 1 to AScript.Length do
    if AScript[i] = SEPARATE_CONDENSATION_CHAR then
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

end.
