unit ETL.Component.Extract;

interface

uses
  ETL.Component,
  ETL.Form.Edit.Extract,
  ETL.Form.Grid,
  ETL.FileProject.Interfaces;

type
  TCompExtract = class(TComponentETL, IComponentExtract)
  strict protected
    function GetInstanceFormEdit: TFoEditExtract; virtual; abstract;
  public
    procedure Edit; override;
  end;

implementation

uses Vcl.Forms;

procedure TCompExtract.Edit;
begin
  GetInstanceFormEdit.ShowModal;
end;

end.
