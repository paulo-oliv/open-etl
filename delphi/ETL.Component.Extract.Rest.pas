unit ETL.Component.Extract.Rest;

interface

uses
  ETL.Component,
  ETL.Form.Edit.Extract.Query,
  ETL.Form.Grid,
  ETL.FileProject.Interfaces;

type

  TCompRest = class(TCompExtract)
  strict private
    FFormEdit: TFoEditRest;
    function GetInstanceFormEdit: TFoEditRest;
  strict protected
    function GetScript: string; override;
    procedure setScript(const AScript: string); override;
    procedure RefreshGrid(var AFormGrid: TFoGrid); override;
  public
    procedure Edit; override;
    destructor Destroy; override;
  end;

  (* TKindComponentRest = (Json, Xml, UnionAll, Union, Join, Filter, Conversion, Derivation,
    Condensation);

    IComponentRest = interface
    ['{EE590AE6-0D13-424E-8EBB-83C5E8B1B92F}']
    function getKind: TKindComponentRest;
    function getScript: string;
    procedure setKind(const AKind: TKindComponentRest);
    procedure setScript(const AScript: string);
    property Kind: TKindComponentRest read getKind write setKind;
    property Script: string read getScript write setScript;
    end;

    TComponentRest = class(TInterfacedObject, IComponentRest)
    strict private
    FKind: TKindComponentRest;
    FScript: string;
    strict protected
    function getKind: TKindComponentRest;
    function getScript: string;
    procedure setKind(const AKind: TKindComponentRest);
    procedure setScript(const AScript: string);
    public
    class function New: IComponentRest; overload;
    class function New(const AScript: string; const AKind: TKindComponentRest)
    : IComponentRest; overload;
    end;

    class function TComponentRest.New: IComponentRest;
    begin
    Result := TComponentRest.Create;
    end;

    class function TComponentRest.New(const AScript: string; const AKind: TKindComponentRest)
    : IComponentRest;
    begin
    Result := New;
    Result.Kind := AKind;
    Result.Script := AScript;
    end;

    function TComponentRest.getScript: string;
    begin
    Result := FScript;
    end;

    function TComponentRest.getKind: TKindComponentRest;
    begin
    Result := FKind;
    end;

    procedure TComponentRest.setScript(const AScript: string);
    begin
    FScript := AScript;
    end;

    procedure TComponentRest.setKind(const AKind: TKindComponentRest);
    begin
    FKind := AKind;
    end;
  *)

  TCompFile = class(TCompExtract)
  end;

implementation

uses System.SysUtils, FireDAC.Comp.Client, Generics.Collections;

{ TCompRest }

procedure TCompRest.RefreshGrid(var AFormGrid: TFoGrid);
var
  LQr: TFDRest;

  procedure chargeConnection(const AConn: TFDConnection);
  var
    i: Integer;
  begin
    LQr.Close;
    LQr.Connection := AConn;
    FFormEdit.UpdateSqlOut;
    LQr.SQL.Text := FFormEdit.MmOut.Lines.Text;
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
  LQr := TFDRest.Create(nil);
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

destructor TCompRest.Destroy;
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

function TCompRest.GetInstanceFormEdit: TFoEditRest;
begin
  if not Assigned(FFormEdit) then
  begin
    FFormEdit := TFoEditRest.New(Self);
    FFormEdit.OnChange := OnFormEditChange;
  end;
  Result := FFormEdit;
end;

procedure TCompRest.Edit;
begin
  GetInstanceFormEdit.ShowModal;
end;

const
  SEPARATE_Rest_CHAR = '|';

function TCompRest.GetScript: string;
var
  i: Integer;
begin
  Result := '';
  if Assigned(FFormEdit) then
  begin
    for i := 0 to FFormEdit.ClConexoes.Count - 1 do
      if FFormEdit.ClConexoes.Checked[i] then
        Result := Result + IntToStr(i) + ',';
    Result := Result + SEPARATE_Rest_CHAR + FFormEdit.MmIn.Text;
  end;
end;

procedure TCompRest.setScript(const AScript: string);
var
  LConnections, LNum: string;
  i: Integer;
begin
  i := Pos(SEPARATE_Rest_CHAR, AScript);
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
