program ETL;

uses
  Vcl.Forms,
  Form.Princ in 'Form.Princ.pas' {FoPrinc},
  Form.Edit in 'Form.Edit.pas' {FoEdit},
  Form.Edit.Query in 'Form.Edit.Query.pas' {FoEditQuery},
  Form.Edit.Transform in 'Form.Edit.Transform.pas' {FoEditTransform},
  Form.Grid in 'Form.Grid.pas' {FoGrid},
  uDmImages in 'uDmImages.pas' {DmImages: TDataModule},
  Form.Edit.Load in 'Form.Edit.Load.pas' {FoEditLoad};

{$R *.res}

begin
  Application.Initialize;
  ReportMemoryLeaksOnShutdown := True;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TDmImages, DmImages);
  Application.CreateForm(TFoPrinc, FoPrinc);
  Application.Run;
end.
