unit ETL.Component.Extract.Files;

interface

uses
  ETL.Component.Extract,
  ETL.Form.Edit.Extract,
  ETL.Form.Edit.Extract.Files,
  ETL.Form.Grid,
  ETL.FileProject.Interfaces;

type

  TCompFiles = class(TCompExtract)
  strict private
    FFormEdit: TFoEditFiles;
    // FConnections: TObjectList<TFDConnection>;
    function GetInstanceFormEdit: TFoEditExtract; override;
  strict protected
    function GetScript: string; override;
    procedure setScript(const AScript: string); override;
    procedure RefreshGrid(var AFormGrid: TFoGrid); override;
  end;

implementation

uses System.SysUtils, FireDAC.Comp.Client, Generics.Collections;

{ TCompQuery }

procedure TCompFiles.RefreshGrid(var AFormGrid: TFoGrid);
var
  i: Integer;
begin
  inherited;
  AFormGrid.tv.DataController.RecordCount := 0;
  // AFormGrid.tv.Bands.Clear;
  AFormGrid.tv.ClearItems;
end;

function TCompFiles.GetInstanceFormEdit: TFoEditExtract;
begin
  if not Assigned(FFormEdit) then
  begin
    FFormEdit := TFoEditFiles.New(Self);
    FFormEdit.OnChange := OnFormEditChange;
  end;
  Result := FFormEdit;
end;

const
  SEPARATE_FILES_CHAR = '|';

function TCompFiles.GetScript: string;
var
  i: Integer;
begin
  Result := '';
  if Assigned(FFormEdit) then
  begin
    Result := FFormEdit.EdPath.Text + SEPARATE_FILES_CHAR + FFormEdit.EdFilter.Text +
      SEPARATE_FILES_CHAR;
    if FFormEdit.CbSubDir.Checked then
      Result := Result + '1'
    else
      Result := Result + '0'
  end;
end;

procedure TCompFiles.setScript(const AScript: string);
var
  s: string;
  i: Integer;
begin
  i := Pos(SEPARATE_FILES_CHAR, AScript);
  TFoEditFiles(GetInstanceFormEdit).EdPath.Text := Copy(AScript, 1, i - 1);
  s := Copy(AScript, i + 1);
  i := Pos(SEPARATE_FILES_CHAR, s);
  FFormEdit.EdFilter.Text := Copy(s, 1, i - 1);
  FFormEdit.CbSubDir.Checked := Copy(s, i + 1) = '1';
end;

end.
