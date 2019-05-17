unit ComponentETL.Factory;

interface

uses ComponentETL,
  Form.Edit.Query,
  Form.Edit.Transform,
  Form.Edit.Load,
  Form.Grid,
  Vcl.Controls,
  FireDAC.Comp.Client,
  Generics.Collections;

const
  KIND_COMPONENT_QUERY = 0;
  KIND_COMPONENT_FILE = 1;
  KIND_COMPONENT_FILTER = 2;
  KIND_COMPONENT_CONVERSION = 3;
  KIND_COMPONENT_DERIVATION = 4;
  KIND_COMPONENT_JOIN = 5;
  KIND_COMPONENT_CONDENSATION = 6;
  KIND_COMPONENT_EXECUTE = 7;
  KIND_COMPONENT_SCRIPT = 8;

type
  TCompExtract = class(TComponentETL)
  end;

  TCompTransform = class(TComponentETL)
  strict protected
    FFormEdit: TFoEditTransform;
  public
    procedure setTitle(const ATitle: string); override;
    procedure Edit; override;
  end;

  TCompLoad = class(TComponentETL)
  strict protected
    FFormEdit: TFoEditLoad;
  public
    procedure Edit; override;
  end;

  TCompQuery = class(TCompExtract)
  strict private
    FFormGrid: TFoGrid;
    FFormEdit: TFoEditQuery;
    // FConnections: TObjectList<TFDConnection>;
    function GetInstanceFormEdit: TFoEditQuery;
  strict protected
    function GetScript: string; override;
    procedure setScript(const AScript: string); override;
    procedure Preview; override;
  public
    procedure Edit; override;
    destructor Destroy; override;
  end;

  {
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
  }

  TCompFile = class(TCompExtract)
  end;

  TCompFilter = class(TCompTransform)
  end;

  TCompConversion = class(TCompTransform)
  end;

  TCompDerivation = class(TCompTransform)
  end;

  TCompJoin = class(TCompTransform)
  end;

  TComCondensation = class(TCompTransform)
  end;

  TCompExecute = class(TCompLoad)
  end;

  TCompScript = class(TCompLoad)
  end;

  TComponentETLFactory = class
  public
    class function New(const AParent: TWinControl; const AKind: Byte): TComponentETL;
  end;

implementation

uses SysUtils;

{ TCompQuery }

procedure TCompQuery.Preview;
var
  LQr: TFDQuery;

  procedure chargeConnection(const AConnectionName: string);
  var
    LConn: TFDConnection;
    i: Integer;
  begin
    LConn := TFDConnection.Create(nil);
    FFormGrid.tv.BeginUpdate;
    try
      LQr.Close;
      LConn.ConnectionDefName := AConnectionName;
      LQr.Connection := LConn;
      LConn.Connected := True;
      // Qr.ConnectionName := 'testar';
      LQr.SQL.Text := FFormEdit.MM.Lines.Text;
      // Qr.Filter := Trim(AFilter);
      LQr.Filtered := LQr.Filter <> '';
      LQr.Open;
      i := FFormGrid.tv.ColumnCount;
      while i < LQr.FieldDefs.Count do
      begin
        with FFormGrid.tv.CreateColumn do
        begin
          Caption := LQr.Fields[i].DisplayLabel;
          // Name := '';
          // DataBinding.ValueTypeClass := TcxStringValueType;
          // Text := LQr.Fields[i].DisplayLabel;
        end;
        i := i + 1;
      end;
      while not LQr.Eof do
      begin
        FFormGrid.tv.DataController.RecordCount := FFormGrid.tv.DataController.RecordCount + 1;
        for i := 0 to LQr.FieldDefs.Count - 1 do
          FFormGrid.tv.DataController.Values[FFormGrid.tv.DataController.RecordCount - 1, i] :=
            LQr.Fields[i].Value;
        LQr.Next;
      end;
    finally
      FFormGrid.tv.EndUpdate;
      LConn.DisposeOf;
    end;
  end;

var
  i: Integer;
begin
  if not Assigned(FFormGrid) then
  begin
    FFormGrid := TFoGrid.New(Self);
    // FConnections := TObjectList<TFDConnection>.Create;
  end;
  LQr := TFDQuery.Create(nil);
  try
    FFormGrid.tv.DataController.RecordCount := 0;
    FFormGrid.tv.ClearItems;
    for i := 0 to FFormEdit.ClConexoes.Count - 1 do
      if FFormEdit.ClConexoes.Checked[i] then
        chargeConnection(FFormEdit.ClConexoes.Items[i]);
    FFormGrid.ShowModal;
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
    FFormEdit := TFoEditQuery.New(Self);
  Result := FFormEdit;
end;

procedure TCompQuery.Edit;
begin
  GetInstanceFormEdit.ShowModal;
end;

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
    Result := Result + '|';
    Result := FFormEdit.MM.Text;
  end;
end;

procedure TCompQuery.setScript(const AScript: string);
var
  LConnections: string;
  p: Integer;
begin
  p := Pos('|', AScript);
  GetInstanceFormEdit.MM.Text := Copy(AScript, p + 1);
  LConnections := Copy(AScript, 1, p - 1);
end;

{ TCompTransform }

procedure TCompTransform.Edit;
begin
  if not Assigned(FFormEdit) then
  begin
    FFormEdit := TFoEditTransform.New(Self);
    FFormEdit.Caption := Title;
  end;
  FFormEdit.ShowModal;
end;

procedure TCompTransform.setTitle(const ATitle: string);
begin
  inherited setTitle(ATitle);
  if Assigned(FFormEdit) then
    FFormEdit.Caption := ATitle;
end;

{ TCompLoad }

procedure TCompLoad.Edit;
begin
  if not Assigned(FFormEdit) then
    FFormEdit := TFoEditLoad.New(Self);
  FFormEdit.ShowModal;
end;

{ TComponentETLFactory }

class function TComponentETLFactory.New(const AParent: TWinControl; const AKind: Byte)
  : TComponentETL;
begin
  case AKind of
    KIND_COMPONENT_QUERY:
      Result := TCompQuery.Create(AParent, AParent);
    KIND_COMPONENT_FILE:
      Result := TCompFile.Create(AParent, AParent);
    KIND_COMPONENT_FILTER:
      Result := TCompFilter.Create(AParent, AParent);
    KIND_COMPONENT_CONVERSION:
      Result := TCompConversion.Create(AParent, AParent);
    KIND_COMPONENT_DERIVATION:
      Result := TCompDerivation.Create(AParent, AParent);
    KIND_COMPONENT_JOIN:
      Result := TCompJoin.Create(AParent, AParent);
    KIND_COMPONENT_CONDENSATION:
      Result := TCompConversion.Create(AParent, AParent);
    KIND_COMPONENT_EXECUTE:
      Result := TCompExecute.Create(AParent, AParent);
  else
    // KIND_COMPONENT_SCRIPT:
    Result := TCompScript.Create(AParent, AParent);
  end;
  Result.Tag := AKind;
end;

end.
