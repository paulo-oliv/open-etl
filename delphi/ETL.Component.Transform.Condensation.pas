unit ETL.Component.Transform.Condensation;

interface

uses
  ETL.Component.Transform,
  ETL.Form.Edit.Transform.Condensation,
  ETL.Form.Grid,
  cxCustomData,
  System.Generics.Collections;

type
  TKeyBestSelected = record
    Row, Column: Integer;
  end;

  TCompCondensation = class(TCompTransform)
  strict private
    FSelectedBestDictionary: TDictionary<TKeyBestSelected, Variant>;
    function GetInstanceFormEdit: TFoEditCondensation;
    procedure SummaryBEST(ASender: TcxDataSummaryItems; Arguments: TcxSummaryEventArguments;
      var OutArguments: TcxSummaryEventOutArguments);
  strict protected
    function GetScript: string; override;
    procedure setScript(const AScript: string); override;
    procedure RefreshGrid(const AFormGrid: TFoGrid); override;
  public
    destructor Destroy; override;
    procedure Edit; override;
  end;

implementation

uses ETL.FileProject.Interfaces, System.SysUtils, Classes, Dialogs, Variants;

type
  IBestKey = interface
    ['{98AC9B68-DB33-422D-A8FD-D799F9FDBC14}']
    function AddKey(const AKey: Variant): IBestKey;
  end;

  TBestKey = class(TInterfacedObject, IBestKey)
  strict private
    FKeys: TList<Variant>;
    FValue: Variant;
  public
    constructor Create(const AValue: Variant);
    destructor Destroy; override;
    function AddKey(const AKey: Variant): IBestKey;
  end;

  IBestScore = interface
    ['{69D1AB83-7AF3-4961-B9DB-4104918397C8}']
    function IncScore(const AInc: Integer = 1): IBestScore;
  end;

  TBestScore = class(TInterfacedObject, IBestScore)
  strict private
    FScore: Integer;
  public
    constructor Create;
    function IncScore(const AInc: Integer = 1): IBestScore;
  end;

  TBestDictionary = class(TDictionary<IBestKey, IBestScore>)
  strict private
    FColumn: Integer;
  public
    constructor Create(const AColumn: Integer);
    property Column: Integer read FColumn;
  end;

  { TBestValue }

constructor TBestScore.Create;
begin
  FScore := 0;
end;

function TBestScore.IncScore(const AInc: Integer): IBestScore;
begin
  Result := Self;
  FScore := FScore + AInc;
end;

{ TBestDictionary }

constructor TBestDictionary.Create(const AColumn: Integer);
begin
  inherited Create;
  FColumn := AColumn;
end;

{ TBestKey }

function TBestKey.AddKey(const AKey: Variant): IBestKey;
begin
  if not Assigned(FKeys) then
    FKeys := TList<Variant>.Create;
  FKeys.Add(AKey);
end;

constructor TBestKey.Create(const AValue: Variant);
begin
  inherited Create;
  FValue := AValue;
end;

destructor TBestKey.Destroy;
begin
  try
    if Assigned(FKeys) then
      FKeys.DisposeOf;
  except
  end;

  try
    inherited
  except
  end;
end;

{ TCompCondensation }

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

destructor TCompCondensation.Destroy;
begin
  try
    if Assigned(FSelectedBestDictionary) then
      FSelectedBestDictionary.DisposeOf
  except
  end;
  try
    inherited
  except
  end;
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
  // ShowMessage('Arguments.RecordIndex: ' + IntToStr(Arguments.RecordIndex) +
  // #13'Arguments.SummaryItem.Index: ' + IntToStr(Arguments.SummaryItem.Index) +
  // #13'OutArguments.Value: ' + vartostr(OutArguments.Value));
  // AQUANTITY := ASender.DataController.Values[Arguments.RecordIndex, Arguments.SummaryItem.Index];
  // AQUANTITY := ASender.DataController.Values[Arguments.RecordIndex,
  // ASender.DataController.DataSource.DataSet.FieldByName('MHDSasia').Index];
  if Arguments.SummaryItem.Kind <> skNone then
  begin
    OutArguments.SummaryValue := '333';
    OutArguments.Done := True;
  end;

  // var
  // AIndex, AGroupIndex: integer;
  // AValue: variant;
  // begin
  // with <AcxGridDBTableView>.DataController do
  // begin
  // AGroupIndex := Groups.DataGroupIndexByRowIndex[<ARowIndex>];
  // AIndex := Summary.DefaultGroupSummaryItems.IndexOfItemLink(<AColumn>);
  // AValue := Summary.GroupSummaryValues[AGroupIndex, AIndex];
  // end;
  // Caption := VarToStr(AValue);
  // end;
end;

{ procedure TPortalMatrixFrm.OnSummary(
  ASender: TcxDataSummaryItems; Arguments: TcxSummaryEventArguments;
  var OutArguments: TcxSummaryEventOutArguments);
  var
  AValue: Variant;
  AItem: TcxGridTableSummaryItem;
  begin
  AItem := TcxGridTableSummaryItem(Arguments.SummaryItem);

  AValue := tvMatrix.DataController.Values[Arguments.RecordIndex, Arguments.SummaryItem.Field.Index];
  if (not VarIsNull(AValue)) AND (AValue = DSZUSTAND_OFFLINE) then
  begin
  Dec(OutArguments.CountValue);
  end;
  end; }

procedure TCompCondensation.RefreshGrid(const AFormGrid: TFoGrid);

  procedure AddDefaultGroupSummaryItems(const AIndex: Integer; const AKind: TCXSummarykind);
  begin
    AFormGrid.tv.Columns[AIndex].Summary.FooterKind := AKind;
    with AFormGrid.tv.DataController.Summary.DefaultGroupSummaryItems.Add do
    begin
      // Format := '';
      Kind := AKind;
    end;
  end;

var
  LBestDictionaryList: TObjectList<TBestDictionary>;
  LGroupByIndexList: TList<Integer>;

  procedure populateBestDictionary;
  var
    i, j, k: Integer;
    LKey: IBestKey;
    LValue: IBestScore;
  begin
    for i := 0 to LBestDictionaryList.Count - 1 do
      for j := 0 to AFormGrid.tv.DataController.RowCount - 1 do
      begin
        LKey := TBestKey.Create(AFormGrid.tv.DataController.Values[j,
          LBestDictionaryList[i].Column]);
        for k := 0 to LGroupByIndexList.Count - 1 do
          LKey.AddKey(AFormGrid.tv.DataController.Values[j, LGroupByIndexList[k]]);

        LValue := TBestScore.Create;
        if LBestDictionaryList[i].TryGetValue(LKey, LValue) then
          LValue.IncScore
        else
          LBestDictionaryList[i].Add(LKey, LValue);
      end;
  end;

  procedure selectBests;
  var
    i, j: Integer;
    LKey: TKeyBestSelected;
  begin
    if Assigned(FSelectedBestDictionary) then
      FSelectedBestDictionary.Clear
    else
      FSelectedBestDictionary := TDictionary<TKeyBestSelected, Variant>.Create;
    for i := 0 to LBestDictionaryList.Count - 1 do
      for j := 0 to LBestDictionaryList[i].Count - 1 do
      begin
        LKey.Row := 1;
        LKey.Column := LBestDictionaryList[i].Column;
        // FSelectedBestDictionary.Add(LKey, LBestDictionaryList[i]. );
      end;
  end;

var
  i: Integer;
  LKindSummary: TKindCondensation;
begin
  inherited;
  AFormGrid.tv.DataController.Summary.DefaultGroupSummaryItems.Clear;
  AFormGrid.tv.OptionsView.Footer := True;
  try
    for i := 0 to TFoEditCondensation(FFormEdit).Gr.Rows.Count - 1 do
    begin
      LKindSummary := TFoEditCondensation(FFormEdit).GetKind(i);
      AFormGrid.tv.Columns[i].Visible := LKindSummary <> TKindCondensation.Null;
      case LKindSummary of
        TKindCondensation.GroupBy:
          begin
            if not Assigned(LGroupByIndexList) then
              LGroupByIndexList := TList<Integer>.Create;
            LGroupByIndexList.Add(i);
            AFormGrid.tv.Columns[i].Summary.FooterKind := TCXSummarykind.skNone;
            AFormGrid.tv.Columns[i].GroupIndex := 0; // g
            AFormGrid.tv.Columns[i].Visible := False;
            // g := g + 1;
          end;
        TKindCondensation.Null:
          AFormGrid.tv.Columns[i].Summary.FooterKind := TCXSummarykind.skNone;
        TKindCondensation.Sum:
          AddDefaultGroupSummaryItems(i, TCXSummarykind.skSum);
        TKindCondensation.Average:
          AddDefaultGroupSummaryItems(i, TCXSummarykind.skAverage);
        TKindCondensation.Max:
          AddDefaultGroupSummaryItems(i, TCXSummarykind.skMax);
        TKindCondensation.Min:
          AddDefaultGroupSummaryItems(i, TCXSummarykind.skMin);
        TKindCondensation.Count:
          AddDefaultGroupSummaryItems(i, TCXSummarykind.skCount);
        TKindCondensation.First:
          AddDefaultGroupSummaryItems(i, TCXSummarykind.skSum);
        TKindCondensation.Last:
          AddDefaultGroupSummaryItems(i, TCXSummarykind.skSum);
        TKindCondensation.Best:
          begin
            AddDefaultGroupSummaryItems(i, TCXSummarykind.skSum);

            if not Assigned(LBestDictionaryList) then
              LBestDictionaryList := TObjectList<TBestDictionary>.Create;

            LBestDictionaryList.Add(TBestDictionary.Create(i));
          end
      else
        AddDefaultGroupSummaryItems(i, TCXSummarykind.skNone);
      end;
    end;
    if Assigned(LGroupByIndexList) then
      if Assigned(LBestDictionaryList) then
      begin
        populateBestDictionary;
        selectBests;
      end;

  finally
    if Assigned(LGroupByIndexList) then
      LGroupByIndexList.DisposeOf;
    if Assigned(LBestDictionaryList) then
      LBestDictionaryList.DisposeOf;
  end;
  AFormGrid.tv.Columns[i].Summary.GroupFooterKind := AFormGrid.tv.Columns[i].Summary.FooterKind;
  AFormGrid.tv.Columns[i].Summary.GroupFooterFormat := AFormGrid.tv.Columns[i].Summary.FooterFormat;

  AFormGrid.tv.DataController.Summary.FooterSummaryItems.OnSummary := SummaryBEST;
  // AFormGrid.tv.DataController.Summary.DefaultGroupSummaryItems.OnSummary := SummaryBEST;
  // AFormGrid.tv.DataController.Summary.Recalculate;
end;

end.
