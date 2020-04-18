unit ETL.Component.Load;

interface

uses
  ETL.Component,
  ETL.FileProject.Interfaces,
  ETL.Form.Grid;

type
  TCompLoad = class(TComponentETL, IComponentLoad)
  strict protected
    procedure RefreshGrid(var AGrid: TFoGrid); override;
  end;

  TCompExecute = class(TComponentETL)
  end;

implementation

{ TCompLoad }

{ TCompLoad }

procedure TCompLoad.RefreshGrid(var AGrid: TFoGrid);
begin
  //
end;

end.
