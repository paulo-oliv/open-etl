unit ETL.Component.Load;

interface

uses
  ETL.Component,
  ETL.Form.Edit.Load,
  ETL.FileProject.Interfaces;

type
  TCompLoad = class(TComponentETL, IComponentLoad)
  strict protected
    FFormEdit: TFoEditLoad;
  public
    procedure Edit; override;
  end;

  TCompExecute = class(TCompLoad)
  end;

  TCompScript = class(TCompLoad)
  end;

implementation

{ TCompLoad }

procedure TCompLoad.Edit;
begin
  if not Assigned(FFormEdit) then
    FFormEdit := TFoEditLoad.New(Self);
  FFormEdit.ShowModal;
end;

end.
