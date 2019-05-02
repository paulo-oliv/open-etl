program ETL;

uses
  Vcl.Forms,
  Form.Main in 'Form.Main.pas' {FoMain},
  Form.Edit in 'Form.Edit.pas' {FoEdit},
  Form.Edit.Query in 'Form.Edit.Query.pas' {FoEditQuery},
  Form.Edit.Transform in 'Form.Edit.Transform.pas' {FoEditTransform},
  Form.Grid in 'Form.Grid.pas' {FoGrid},
  DataModule.Main in 'DataModule.Main.pas' {DmMain: TDataModule},
  Form.Edit.Load in 'Form.Edit.Load.pas' {FoEditLoad},
  ComponentETL in 'ComponentETL.pas',
  FileProjectETL in 'FileProjectETL.pas',
  FileProject.Interfaces in 'FileProject.Interfaces.pas',
  ComponentETL.Factory in 'ComponentETL.Factory.pas';

{$R *.res}

begin
  Application.Initialize;
  ReportMemoryLeaksOnShutdown := True;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TDmMain, DmMain);
  Application.CreateForm(TFoMain, FoMain);
  Application.Run;
end.
