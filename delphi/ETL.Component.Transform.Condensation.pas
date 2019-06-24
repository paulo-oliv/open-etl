unit ETL.Component.Transform.Condensation;

interface

uses
  ETL.Component.Transform,
  ETL.Form.Edit.Transform.Condensation,
  ETL.Form.Grid,
  cxCustomData;

type
  TCompCondensation = class(TCompTransform)
  strict private
    function GetInstanceFormEdit: TFoEditCondensation;
    procedure SummaryBEST(ASender: TcxDataSummaryItems; Arguments: TcxSummaryEventArguments;
      var OutArguments: TcxSummaryEventOutArguments);
  strict protected
    function GetScript: string; override;
    procedure setScript(const AScript: string); override;
    procedure RefreshGrid(const AFormGrid: TFoGrid); override;
  public
    procedure Edit; override;
  end;

implementation

{ TCompCondensation }

uses ETL.FileProject.Interfaces, System.SysUtils;

function TCompCondensation.GetInstanceFormEdit: TFoEditCondensation;
var
  LSources: IListSources;
  i, j: Integer;
begin
  if not Assigned(FFormEdit) then
  begin
    FFormEdit := TFoEditCondensation.New(Self);
    TFoEditCondensation(FFormEdit).Gr.ClearRows;
    LSources := GetSources;
    if Assigned(LSources) then
      for i := 0 to LSources.Count - 1 do
      begin
        with LSources.GetItem(i).GetGrid do
        begin
          for j := 0 to tv.ColumnCount - 1 do
            if tv.Columns[j].Visible then
              TFoEditCondensation(FFormEdit).AddField(tv.Columns[j].Caption)
        end;
      end;
  end;
  Result := TFoEditCondensation(FFormEdit);
end;

procedure TCompCondensation.Edit;
begin
  with GetInstanceFormEdit do
  begin
    Caption := Title;
    ShowModal;
  end;
end;

function TCompCondensation.GetScript: string;
begin
  Result := '';
  if Assigned(FFormEdit) then
    Result := TFoEditCondensation(FFormEdit).ToString;
end;

procedure TCompCondensation.setScript(const AScript: string);
var
  LValue: string;
  i, j: Integer;
  LFormEdit: TFoEditCondensation;
begin
  j := 0;
  LValue := '';
  LFormEdit := GetInstanceFormEdit;
  for i := 1 to AScript.Length do
    if AScript[i] = SEPARATE_CONDENSATION_CHAR then
    begin
      if LValue <> '' then
      begin
        LFormEdit.SetValue(j, LValue);
        j := j + 1;
        LValue := '';
      end;
    end
    else
      LValue := LValue + AScript[i];
end;

procedure TCompCondensation.SummaryBEST(ASender: TcxDataSummaryItems;
  Arguments: TcxSummaryEventArguments; var OutArguments: TcxSummaryEventOutArguments);
var
  AQUANTITY: Integer;
begin
  // AQUANTITY := ASender.DataController.Values[Arguments.RecordIndex, Arguments.SummaryItem.Index];
  // AQUANTITY := ASender.DataController.Values[Arguments.RecordIndex,
  // ASender.DataController.DataSource.DataSet.FieldByName('MHDSasia').Index];
  OutArguments.Value := OutArguments.Value * AQUANTITY;
end;

procedure TCompCondensation.RefreshGrid(const AFormGrid: TFoGrid);

  procedure AddDefaultGroupSummaryItems(const AIndex: Integer; const AKind: TCXSummarykind);
  begin
    AFormGrid.tv.Columns[AIndex].Summary.FooterKind := AKind;
    with AFormGrid.tv.DataController.Summary.DefaultGroupSummaryItems.Add do
    begin
      // Format :=
      Kind := AKind;
    end;
  end;

var
  i, j: Integer;
  LValue: string;
begin
  inherited;
  AFormGrid.tv.DataController.Summary.DefaultGroupSummaryItems.Clear;
  AFormGrid.tv.OptionsView.Footer := True;
  for i := 0 to TFoEditCondensation(FFormEdit).Gr.Rows.Count - 1 do
  begin
    LValue := TFoEditCondensation(FFormEdit).GetValue(i);
    AFormGrid.tv.Columns[i].Visible := LValue <> CONDENSATION_NULL;
    if LValue = CONDENSATION_NULL then
      AFormGrid.tv.Columns[i].Summary.FooterKind := TCXSummarykind.skNone
    else if LValue = CONDENSATION_GROUP_BY then
    begin
      AFormGrid.tv.Columns[i].Summary.FooterKind := TCXSummarykind.skNone;
      AFormGrid.tv.Columns[i].GroupIndex := 0; // g
      AFormGrid.tv.Columns[i].Visible := False;
      // g := g + 1;
    end
    else if LValue = CONDENSATION_SUM then
      AddDefaultGroupSummaryItems(i, TCXSummarykind.skSum)
    else if LValue = CONDENSATION_AVERAGE then
      AddDefaultGroupSummaryItems(i, TCXSummarykind.skAverage)
    else if LValue = CONDENSATION_MAX then
      AddDefaultGroupSummaryItems(i, TCXSummarykind.skMax)
    else if LValue = CONDENSATION_MIN then
      AddDefaultGroupSummaryItems(i, TCXSummarykind.skMin)
    else if LValue = CONDENSATION_COUNT then
      AddDefaultGroupSummaryItems(i, TCXSummarykind.skCount)
    else if LValue = CONDENSATION_FIRST then
      AddDefaultGroupSummaryItems(i, TCXSummarykind.skNone)
    else if LValue = CONDENSATION_LAST then
      AddDefaultGroupSummaryItems(i, TCXSummarykind.skNone)
    else if LValue = CONDENSATION_BEST then
    begin
      AddDefaultGroupSummaryItems(i, TCXSummarykind.skNone);
      for j := 0 to AFormGrid.tv.DataController.RowCount - 1 do
      begin
        // AFormGrid.tv.DataController.Values[j, i]
      end
    end
    else
      AddDefaultGroupSummaryItems(i, TCXSummarykind.skNone);

    AFormGrid.tv.Columns[i].Summary.GroupFooterKind := AFormGrid.tv.Columns[i].Summary.FooterKind;
    AFormGrid.tv.Columns[i].Summary.GroupFooterFormat := AFormGrid.tv.Columns[i]
      .Summary.FooterFormat;

    // AFormGrid.tv.DataController.Summary.FooterSummaryItems.OnSummary := SummaryLAST;
    // AFormGrid.tv.DataController.Summary.Recalculate;
  end;
end;

end.
