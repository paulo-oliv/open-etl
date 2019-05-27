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
begin
  if not Assigned(FFormEdit) then
  begin
    FFormEdit := TFoEditCondensation.New(Self);
    Caption := Title;
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

const
  SEPARATE_CONDENSATION_CHAR = '|';

function TCompCondensation.GetScript: string;
var
  i: Integer;
begin
  Result := '';
  if Assigned(FFormEdit) then
  begin
    for i := 0 to TFoEditCondensation(FFormEdit).ClColumns.Count - 1 do
      if TFoEditCondensation(FFormEdit).ClColumns.Checked[i] then
        Result := Result + IntToStr(i) + ',';
    Result := IntToStr(TFoEditCondensation(FFormEdit).RgKind.ItemIndex) +
      SEPARATE_CONDENSATION_CHAR + Result;
  end;
end;

procedure TCompCondensation.setScript(const AScript: string);
var
  LColumns, LNum: string;
  p, i: Integer;
begin
  p := Pos(SEPARATE_CONDENSATION_CHAR, AScript);
  if TryStrToInt(Copy(AScript, 1, p - 1), i) then
    GetInstanceFormEdit.RgKind.ItemIndex := i;
  LColumns := Copy(AScript, p + 1);
  LNum := '';
  for i := 1 to LColumns.Length do
    if LColumns[i] = ',' then
    begin
      TFoEditCondensation(FFormEdit).ClColumns.Checked[StrToInt(LNum)] := True;
      LNum := '';
    end
    else
      LNum := LNum + LColumns[i];
end;

end.
