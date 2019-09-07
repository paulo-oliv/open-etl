unit ETL.Component.Transform.Condensation;

interface

uses
  ETL.Component.Transform,
  ETL.Form.Edit.Transform.Condensation,
  ETL.Form.Grid;

type
  TCompCondensation = class(TCompTransform)
  strict private
    function GetInstanceFormEdit: TFoEditCondensation;
  strict protected
    function GetScript: string; override;
    procedure setScript(const AScript: string); override;
    procedure RefreshGrid(var AFormGrid: TFoGrid); override;
  public
    procedure Edit; override;
  end;

implementation

uses ETL.FileProject.Interfaces, System.SysUtils, System.Generics.Collections, cxCustomData,
  Variants, IdHashMessageDigest;

type
  TBestItem = record
    Key: string;
    Value: Variant;
    Score: integer;
    procedure IncScore(const AInc: integer = 1);
  end;

  TBest = class
  public
    FItens: TList<TBestItem>;
    FCol: integer;
    constructor Create(const ACol: integer);
    destructor Destroy; override;
    procedure Add(const AKey: string; const AValue: Variant);
    function BestValue(const AKey: string): Variant;
  end;

  { TBestItem }

procedure TBestItem.IncScore(const AInc: integer);
begin
  Score := Score + AInc;
end;

{ TBest }

procedure TBest.Add(const AKey: string; const AValue: Variant);

  function LocateValue: Boolean;
  var
    i: integer;
  begin
    Result := False;
    for i := 0 to FItens.Count - 1 do
      if FItens[i].Key = AKey then
        if FItens[i].Value = AValue then
        begin
          FItens[i].IncScore;
          exit(True);
        end;
  end;

var
  LItem: TBestItem;
begin
  if not LocateValue then
  begin
    LItem.Key := AKey;
    LItem.Value := AValue;
    LItem.Score := 1;
    FItens.Add(LItem);
  end;
end;

constructor TBest.Create(const ACol: integer);
begin
  FCol := ACol;
  FItens := TList<TBestItem>.Create;
end;

destructor TBest.Destroy;
begin
  try
    FItens.DisposeOf
  except
  end;
  try
    inherited
  except
  end;
end;

function TBest.BestValue(const AKey: string): Variant;
var
  LItem: TBestItem;
  LScore: integer;
begin
  LScore := 0;
  Result := Null;
  for LItem in FItens do
    if LItem.Key = AKey then
    begin
      if LItem.Score > LScore then
      begin
        Result := LItem.Value;
        LScore := LItem.Score;
      end;
    end;
end;

{ TCompCondensation }

function TCompCondensation.GetInstanceFormEdit: TFoEditCondensation;
var
  LSources: IListSources;
  i, j: integer;
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
  i, j: integer;
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

procedure TCompCondensation.RefreshGrid(var AFormGrid: TFoGrid);
const
  TAG_COL_GROUP_BY = 1;
  TAG_COL_BEST = 22;
  TAG_COL_NEW_GROUP_BY = 23;
var
  LGroupByCols: TList<integer>;
  LBestList: TObjectList<TBest>;

  function KeyRow(const ARow: integer): string;
  var
    i: integer;
  begin
    Result := '';
    for i := 0 to LGroupByCols.Count - 1 do
      Result := VarToStr(AFormGrid.tv.DataController.Values[ARow, i]) + ',';
  end;

  procedure populateBests;
  var
    i, j: integer;
    LKey: string;
  begin
    for i := 0 to AFormGrid.tv.DataController.RowCount - 1 do
    begin
      LKey := KeyRow(i);
      for j := 0 to LBestList.Count - 1 do
      begin
        LBestList[j].Add(LKey, AFormGrid.tv.DataController.Values[i, LBestList[j].FCol]);
      end;
    end;
  end;

  procedure selectBests;
  var
    i, j, LCol: integer;

    { procedure createColunmGroupBy; // apenas como apresentação
      var
      i, j: integer;
      LValue: string;
      begin
      for i := AFormGrid.tv.ColumnCount - 1 downto 0 do
      if AFormGrid.tv.Columns[i].Tag = TAG_COL_NEW_GROUP_BY then
      AFormGrid.tv.Columns[i].DisposeOf;
      with AFormGrid.tv.CreateColumn do
      begin
      LCol := Index;
      Tag := TAG_COL_NEW_GROUP_BY;
      Caption := 'GROUP BY';
      // Position.BandIndex := 1;
      Visible := False;
      end;

      for i := 0 to AFormGrid.tv.DataController.RowCount - 1 do
      begin
      LValue := '';
      for j := 0 to AFormGrid.tv.ColumnCount - 1 do
      if (AFormGrid.tv.Columns[i].Tag = TAG_COL_BEST) or
      (AFormGrid.tv.Columns[i].Tag = TAG_COL_GROUP_BY) then
      LValue := LValue + VarToStr(AFormGrid.tv.DataController.Values[i, j]) + '|';
      AFormGrid.tv.DataController.Values[i, LCol] := LValue;
      end;
      AFormGrid.tv.Columns[LCol].GroupIndex := 0;
      end; }

    procedure createGridFinal;

      function StrToMd5(const ATxt: string): string;
      var
        idmd5: TIdHashMessageDigest5;
      begin
        idmd5 := TIdHashMessageDigest5.Create;
        try
          Result := idmd5.HashStringAsHex(ATxt);
        finally
          idmd5.DisposeOf;
        end;
      end;

    const
      TAG_COL_MD5 = 999;
    var
      i, j, k: integer;
      LNewFoGrid: TFoGrid;
      LMd5, LLastMD5: string;
      LNewRow: Boolean;
    begin
      LMd5 := '';
      LLastMD5 := 'zzzzzzz';
      LNewFoGrid := TFoGrid.Create(Self);
      LNewFoGrid.tv.BeginUpdate;
      try
        for i := 0 to AFormGrid.tv.ColumnCount - 1 do
          if (AFormGrid.tv.Columns[i].Tag = TAG_COL_BEST) or
            (AFormGrid.tv.Columns[i].Tag = TAG_COL_GROUP_BY) then
            with LNewFoGrid.tv.CreateColumn do
            begin
              Caption := AFormGrid.tv.Columns[i].Caption;
              Tag := i;
            end;

        with LNewFoGrid.tv.CreateColumn do
        begin
          Caption := 'hash';
          Tag := TAG_COL_MD5;
        end;

        k := 0;
        for i := 0 to AFormGrid.tv.DataController.RowCount - 1 do
        begin
          LNewFoGrid.tv.DataController.RecordCount := k + 1;
          LNewRow := True;
          for j := 0 to LNewFoGrid.tv.ColumnCount - 1 do
            if LNewFoGrid.tv.Columns[j].Tag = TAG_COL_MD5 then
            begin
              LMd5 := StrToMd5(LMd5);
              if LMd5 = LLastMD5 then
              begin
                LNewRow := False;
                break;
              end;
              LLastMD5 := LMd5;
              LNewFoGrid.tv.DataController.Values[k, j] := LMd5;
              LMd5 := '';
            end
            else
            begin
              LMd5 := LMd5 + AFormGrid.tv.DataController.Values
                [i, LNewFoGrid.tv.Columns[j].Tag] + ',';
              LNewFoGrid.tv.DataController.Values[k, j] := AFormGrid.tv.DataController.Values
                [i, LNewFoGrid.tv.Columns[j].Tag];
            end;
          if LNewRow then
            k := k + 1;
        end;

      finally
        AFormGrid.DisposeOf;
        LNewFoGrid.tv.EndUpdate;
        AFormGrid := LNewFoGrid;
      end;
    end;

  begin
    for i := AFormGrid.tv.ColumnCount - 1 downto 0 do
      if AFormGrid.tv.Columns[i].Tag = TAG_COL_BEST then
        AFormGrid.tv.Columns[i].DisposeOf;
    for i := 0 to LBestList.Count - 1 do
    begin
      with AFormGrid.tv.CreateColumn do
      begin
        LCol := Index;
        Tag := TAG_COL_BEST;
        Caption := 'best_' + AFormGrid.tv.Columns[LBestList[i].FCol].Caption;
        // Position.BandIndex := 1;
      end;
      for j := 0 to AFormGrid.tv.DataController.RowCount - 1 do
      begin
        AFormGrid.tv.DataController.Values[j, LCol] := LBestList[i].BestValue(KeyRow(j))
      end;
    end;
    // createColunmGroupBy;
    createGridFinal;
  end;

var
  i: integer;
  LKindSummary: TKindCondensation;
begin
  inherited;
  AFormGrid.tv.DataController.Summary.DefaultGroupSummaryItems.Clear;
  LGroupByCols := TList<integer>.Create;
  LBestList := nil;
  try
    for i := 0 to TFoEditCondensation(FFormEdit).Gr.Rows.Count - 1 do
    begin
      LKindSummary := TFoEditCondensation(FFormEdit).GetKind(i);
      AFormGrid.tv.Columns[i].Visible := LKindSummary <> TKindCondensation.Null;
      case LKindSummary of
        TKindCondensation.GroupBy:
          begin
            LGroupByCols.Add(i);
            AFormGrid.tv.Columns[i].Tag := TAG_COL_GROUP_BY;
            AFormGrid.tv.Columns[i].Summary.FooterKind := TCXSummarykind.skNone;
            // AFormGrid.tv.Columns[i].GroupIndex := 0; // g
            // AFormGrid.tv.Columns[i].Visible := False;
            // g := g + 1;
          end;
        TKindCondensation.Null:
          AFormGrid.tv.Columns[i].Summary.FooterKind := TCXSummarykind.skNone;
        TKindCondensation.Sum:
          begin
            AFormGrid.tv.Columns[i].Summary.FooterKind := TCXSummarykind.skSum;
            with AFormGrid.tv.DataController.Summary.DefaultGroupSummaryItems.Add do
            begin
              // Format := '';
              Kind := TCXSummarykind.skSum;

            end;
          end;
        TKindCondensation.Average:
          begin
            AFormGrid.tv.Columns[i].Summary.FooterKind := TCXSummarykind.skAverage;
            with AFormGrid.tv.DataController.Summary.DefaultGroupSummaryItems.Add do
            begin
              // Format := '';
              Kind := TCXSummarykind.skAverage;
            end;
          end;
        TKindCondensation.Max:
          begin
            AFormGrid.tv.Columns[i].Summary.FooterKind := TCXSummarykind.skMax;
            with AFormGrid.tv.DataController.Summary.DefaultGroupSummaryItems.Add do
            begin
              // Format := '';
              Kind := TCXSummarykind.skMax;
            end;
          end;
        TKindCondensation.Min:
          begin
            AFormGrid.tv.Columns[i].Summary.FooterKind := TCXSummarykind.skMin;
            with AFormGrid.tv.DataController.Summary.DefaultGroupSummaryItems.Add do
            begin
              // Format := '';
              Kind := TCXSummarykind.skMin;
            end;
          end;
        TKindCondensation.Count:
          begin
            AFormGrid.tv.Columns[i].Summary.FooterKind := TCXSummarykind.skCount;
            with AFormGrid.tv.DataController.Summary.DefaultGroupSummaryItems.Add do
            begin
              // Format := '';
              Kind := TCXSummarykind.skCount;
            end;
          end;
        TKindCondensation.First:
          begin
            AFormGrid.tv.Columns[i].Summary.FooterKind := TCXSummarykind.skCount;
            with AFormGrid.tv.DataController.Summary.DefaultGroupSummaryItems.Add do
            begin
              // Format := '';
              Kind := TCXSummarykind.skCount;
            end;
          end;
        TKindCondensation.Last:
          begin
            AFormGrid.tv.Columns[i].Summary.FooterKind := TCXSummarykind.skCount;
            with AFormGrid.tv.DataController.Summary.DefaultGroupSummaryItems.Add do
            begin
              // Format := '';
              Kind := TCXSummarykind.skCount;
            end;
          end;
        TKindCondensation.Best:
          begin
            if LBestList = nil then
              LBestList := TObjectList<TBest>.Create;

            LBestList.Add(TBest.Create(i));
          end;
      else
        begin
          AFormGrid.tv.Columns[i].Summary.FooterKind := TCXSummarykind.skNone;
          // with AFormGrid.tv.DataController.Summary.DefaultGroupSummaryItems.Add do
          // begin
          // Format := '';
          // Kind := TCXSummarykind.skNone;
          // end;

          // LBestDictionaryList.Add(TBestDictionary.Create(i));
        end;
      end;
    end;
    AFormGrid.tv.DataController.Groups.FullExpand;
    AFormGrid.tv.ViewData.Expand(True);
    if LBestList <> nil then
    begin
      populateBests;
      selectBests;
    end;
  finally
    try
      LGroupByCols.DisposeOf;
    except
    end;
    if LBestList <> nil then
      try
        LBestList.DisposeOf;
      except
      end;
  end;
  // AFormGrid.tv.Columns[i].Summary.GroupFooterKind := AFormGrid.tv.Columns[i].Summary.FooterKind;
  // AFormGrid.tv.Columns[i].Summary.GroupFooterFormat := AFormGrid.tv.Columns[i].Summary.FooterFormat;
end;

end.
