program OpenETL;

uses
  Vcl.Forms,
  ETL.Form.Main in 'ETL.Form.Main.pas' {FoMain},
  ETL.Form.Edit in 'ETL.Form.Edit.pas' {FoEdit},
  ETL.Form.Edit.Extract.Query in 'ETL.Form.Edit.Extract.Query.pas' {FoEditQuery},
  ETL.Form.Edit.Transform in 'ETL.Form.Edit.Transform.pas' {FoEditTransform},
  ETL.Form.Grid in 'ETL.Form.Grid.pas' {FoGrid},
  ETL.Form.Edit.Load in 'ETL.Form.Edit.Load.pas' {FoEditLoad},
  ETL.Component in 'ETL.Component.pas',
  ETL.FileProject in 'ETL.FileProject.pas',
  ETL.FileProject.Interfaces in 'ETL.FileProject.Interfaces.pas',
  ETL.Component.Factory in 'ETL.Component.Factory.pas',
  ETL.DataModule.Main in 'ETL.DataModule.Main.pas' {DmMain: TDataModule},
  ETL.Form.Edit.Transform.Condensation in 'ETL.Form.Edit.Transform.Condensation.pas' {FoEditCondensation},
  ETL.Component.Transform in 'ETL.Component.Transform.pas',
  ETL.Link in 'ETL.Link.pas',
  ETL.Component.Extract in 'ETL.Component.Extract.pas',
  ETL.Component.Load in 'ETL.Component.Load.pas',
  ETL.Component.Transform.Condensation in 'ETL.Component.Transform.Condensation.pas';

{$R *.res}

begin
  Application.Initialize;
  ReportMemoryLeaksOnShutdown := True;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TDmMain, DmMain);
  Application.CreateForm(TFoMain, FoMain);
  Application.Run;
end.
