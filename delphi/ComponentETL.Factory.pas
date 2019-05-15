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
    FConnections: TObjectList<TFDConnection>;
  strict protected
    procedure Preview; override;
  public
    function GetScript: string; override;
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

// uses // FireDAC.Stan.Intf, FireDAC.Stan.Option,
// FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool,
// FireDAC.Stan.Async, FireDAC.Phys, FireDAC.VCLUI.Wait, Data.DB;

{ TCompQuery }

procedure TCompQuery.Preview;
var
  Qr: TFDQuery;
  i: Integer;
begin
  if not Assigned(FFormGrid) then
  begin
    FFormGrid := TFoGrid.New(Self);
    FConnections := TObjectList<TFDConnection>.Create;
  end;

  if FConnections.Count = 0 then
  begin
    FConnections.Add(TFDConnection.Create(Self));
    FConnections[0].ConnectionDefName := FFormEdit.ClConexoes.Items[FFormEdit.ClConexoes.ItemIndex];
  end;

  Qr := TFDQuery.Create(Self);
  try
    Qr.Connection := FConnections[0];
    // Qr.ConnectionName := 'testar';
    FConnections[0].Connected := True;
    Qr.SQL.Text := FFormEdit.MM.Lines.Text;
    // Qr.Filter := Trim(AFilter);
    Qr.Filtered := Qr.Filter <> '';
    Qr.Open;
    FFormGrid.tv.ClearItems;
    for i := 0 to Qr.FieldDefs.Count - 1 do
      with FFormGrid.tv.CreateColumn do
      begin
        Text := Qr.Fields[i].FieldName;
      end;

    FFormGrid.ShowModal;
  finally
    Qr.DisposeOf;
  end;
end;

destructor TCompQuery.Destroy;
begin
  if Assigned(FConnections) then
    try
      FConnections.DisposeOf
    except
    end;
  try
    inherited
  except
  end;
end;

procedure TCompQuery.Edit;
begin
  if not Assigned(FFormEdit) then
    FFormEdit := TFoEditQuery.New(Self);
  FFormEdit.ShowModal;
end;

function TCompQuery.GetScript: string;
begin
  if Assigned(FFormEdit) then
  begin
    Result := FFormEdit.MM.Text;
  end
  else
    Result := '';
end;

{ TCompTransform }

procedure TCompTransform.Edit;
begin
  if not Assigned(FFormEdit) then
    FFormEdit := TFoEditTransform.New(Self);
  FFormEdit.ShowModal;
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
