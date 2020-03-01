program OpenETL;

uses
  Vcl.Forms,
  ETL.Form.Main in 'ETL.Form.Main.pas' {FoMain},
  ETL.Form.Edit in 'ETL.Form.Edit.pas' {FoEdit},
  ETL.Form.Edit.Extract.Query in 'ETL.Form.Edit.Extract.Query.pas' {FoEditQuery},
  ETL.Form.Edit.Transform in 'ETL.Form.Edit.Transform.pas' {FoEditTransform},
  ETL.Form.Grid in 'ETL.Form.Grid.pas' {FoGrid},
  ETL.Form.Edit.Load.Script in 'ETL.Form.Edit.Load.Script.pas' {FoEditLoadScript},
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
  ETL.Component.Transform.Condensation in 'ETL.Component.Transform.Condensation.pas',
  uEnum in 'C:\osd\lib\uEnum.pas',
  ETL.Form.Edit.Transform.Filter in 'ETL.Form.Edit.Transform.Filter.pas' {FoEditFilter},
  ETL.Form.Edit.Transform.Join in 'ETL.Form.Edit.Transform.Join.pas' {FoEditJoin},
  ETL.Form.Edit.Transform.Conversion in 'ETL.Form.Edit.Transform.Conversion.pas' {FoEditConversion},
  ETL.Form.Edit.Transform.Derivation in 'ETL.Form.Edit.Transform.Derivation.pas' {FoEditDerivation},
  ETL.Form.Edit.Extract in 'ETL.Form.Edit.Extract.pas' {FoEditExtract},
  ETL.Form.Edit.Extract.Files in 'ETL.Form.Edit.Extract.Files.pas' {FoEditFiles},
  ETL.Component.Load.Script in 'ETL.Component.Load.Script.pas',
  ETL.ListConnections.Singleton in 'ETL.ListConnections.Singleton.pas',
  ETL.Form.Edit.Load.Execute in 'ETL.Form.Edit.Load.Execute.pas' {FoEditLoadExecute},
  ETL.Component.Load.Execute in 'ETL.Component.Load.Execute.pas',
  ETL.Form.Script in 'ETL.Form.Script.pas' {FoScript};

{$R *.res}

begin
  Application.Initialize;
{$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
{$ENDIF}
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TDmMain, DmMain);
  Application.CreateForm(TFoMain, FoMain);
  Application.Run;
end.
