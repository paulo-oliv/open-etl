unit ETL.Component.Extract;

interface

uses
  ETL.Component,
  ETL.Form.Edit.Extract.Query,
  ETL.Form.Grid,
  ETL.FileProject.Interfaces;

type
  TCompExtract = class(TComponentETL, IComponentExtract)
  end;

  TCompQuery = class(TCompExtract)
  strict private
    FFormEdit: TFoEditQuery;
    // FConnections: TObjectList<TFDConnection>;
    function GetInstanceFormEdit: TFoEditQuery;
  strict protected
    function GetScript: string; override;
    procedure setScript(const AScript: string); override;
    procedure RefreshGrid(var AFormGrid: TFoGrid); override;
  public
    procedure Edit; override;
    destructor Destroy; override;
  end;

  (* TKindComponentQuery = (Json, Xml, UnionAll, Union, Join, Filter, Conversion, Derivation,
    Condensation);

    IComponentQuery = interface
    ['{EE590AE6-0D13-424E-8EBB-83C5E8B1B92F}']
    function getKind: TKindComponentQuery;
    function getScript: string;
    procedure setKind(const AKind: TKindComponentQuery);
    procedure setScript(const AScript: string);
    property Kind: TKindComponentQuery read getKind write setKind;
    property Script: string read getScript write setScript;
    end;

    TComponentQuery = class(TInterfacedObject, IComponentQuery)
    strict private
    FKind: TKindComponentQuery;
    FScript: string;
    strict protected
    function getKind: TKindComponentQuery;
    function getScript: string;
    procedure setKind(const AKind: TKindComponentQuery);
    procedure setScript(const AScript: string);
    public
    class function New: IComponentQuery; overload;
    class function New(const AScript: string; const AKind: TKindComponentQuery)
    : IComponentQuery; overload;
    end;

    class function TComponentQuery.New: IComponentQuery;
    begin
    Result := TComponentQuery.Create;
    end;

    class function TComponentQuery.New(const AScript: string; const AKind: TKindComponentQuery)
    : IComponentQuery;
    begin
    Result := New;
    Result.Kind := AKind;
    Result.Script := AScript;
    end;

    function TComponentQuery.getScript: string;
    begin
    Result := FScript;
    end;

    function TComponentQuery.getKind: TKindComponentQuery;
    begin
    Result := FKind;
    end;

    procedure TComponentQuery.setScript(const AScript: string);
    begin
    FScript := AScript;
    end;

    procedure TComponentQuery.setKind(const AKind: TKindComponentQuery);
    begin
    FKind := AKind;
    end;
  *)

  TCompFile = class(TCompExtract)
  end;

implementation

uses System.SysUtils, FireDAC.Comp.Client, Generics.Collections;

{ TCompQuery }

procedure TCompQuery.RefreshGrid(var AFormGrid: TFoGrid);
var
  LQr: TFDQuery;

  procedure chargeConnection(const AConn: TFDConnection);
  var
    i: Integer;
  begin
    LQr.Close;
    LQr.Connection := AConn;
    // Qr.ConnectionName := 'testar';
    FFormEdit.UpdateSqlOut;
    LQr.SQL.Text := FFormEdit.MmOut.Lines.Text;
    // Qr.Filter := Trim(AFilter);
    LQr.Filtered := LQr.Filter <> '';
    LQr.Open;
    i := AFormGrid.tv.ColumnCount;
    while i < LQr.FieldDefs.Count do
    begin
      with AFormGrid.tv.CreateColumn do
      begin
        Caption := LQr.Fields[i].DisplayLabel;
        // Name := '';
        // DataBinding.ValueTypeClass := TcxStringValueType;
        // DataBinding.FieldName := LQr.Fields[i].FieldName;
        // Text := LQr.Fields[i].DisplayLabel;
      end;
      i := i + 1;
    end;
    while not LQr.Eof do
    begin
      AFormGrid.tv.DataController.RecordCount := AFormGrid.tv.DataController.RecordCount + 1;
      for i := 0 to LQr.FieldDefs.Count - 1 do
        AFormGrid.tv.DataController.Values[AFormGrid.tv.DataController.RecordCount - 1, i] :=
          LQr.Fields[i].Value;
      LQr.Next;
    end;
  end;

var
  i: Integer;
begin
  inherited;
  LQr := TFDQuery.Create(nil);
  try
    AFormGrid.tv.DataController.RecordCount := 0;
    // AFormGrid.tv.Bands.Clear;
    AFormGrid.tv.ClearItems;
    for i := 0 to FFormEdit.ClConexoes.Count - 1 do
      if FFormEdit.ClConexoes.Checked[i] then
        chargeConnection(FFormEdit.GetConnection(i));
  finally
    LQr.DisposeOf;
  end;
end;

destructor TCompQuery.Destroy;
begin
  // if Assigned(FConnections) then
  // try
  // FConnections.DisposeOf
  // except
  // end;
  try
    inherited
  except
  end;
end;

function TCompQuery.GetInstanceFormEdit: TFoEditQuery;
begin
  if not Assigned(FFormEdit) then
  begin
    FFormEdit := TFoEditQuery.New(Self);
    FFormEdit.OnChange := OnFormEditChange;
  end;
  Result := FFormEdit;
end;

procedure TCompQuery.Edit;
begin
  GetInstanceFormEdit.ShowModal;
end;

const
  SEPARATE_QUERY_CHAR = '|';

function TCompQuery.GetScript: string;
var
  i: Integer;
begin
  Result := '';
  if Assigned(FFormEdit) then
  begin
    for i := 0 to FFormEdit.ClConexoes.Count - 1 do
      if FFormEdit.ClConexoes.Checked[i] then
        Result := Result + IntToStr(i) + ',';
    Result := Result + SEPARATE_QUERY_CHAR + FFormEdit.MmIn.Text;
  end;
end;

procedure TCompQuery.setScript(const AScript: string);
var
  LConnections, LNum: string;
  i: Integer;
begin
  i := Pos(SEPARATE_QUERY_CHAR, AScript);
  GetInstanceFormEdit.MmIn.Text := Copy(AScript, i + 1);
  LConnections := Copy(AScript, 1, i - 1);
  LNum := '';
  for i := 1 to LConnections.Length do
    if LConnections[i] = ',' then
    begin
      FFormEdit.ClConexoes.Checked[StrToInt(LNum)] := True;
      LNum := '';
    end
    else
      LNum := LNum + LConnections[i];
end;

end.
