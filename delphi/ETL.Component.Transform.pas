unit ETL.Component.Transform;

interface

uses
  ETL.Component,
  ETL.Form.Edit.Transform,
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

implementation

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

end.
