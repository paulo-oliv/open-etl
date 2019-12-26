unit ETL.Component.Load;

interface

uses
  ETL.Component,
  ETL.FileProject.Interfaces;

type
  TCompLoad = class(TComponentETL, IComponentLoad)
  end;

  TCompExecute = class(TComponentETL)
  end;

implementation

{ TCompLoad }

end.
