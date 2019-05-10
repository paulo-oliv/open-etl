unit ComponentETL.Factory;

interface

uses ComponentETL,
  Form.Edit.Query,
  Form.Edit.Transform,
  Form.Edit.Load,
  Form.Grid,
  Vcl.Controls;

const
  TIPO_COMPONENT_QUERY = 0;
  TIPO_COMPONENT_FILE = 1;
  TIPO_COMPONENT_FILTER = 2;
  TIPO_COMPONENT_CONVERSION = 3;
  TIPO_COMPONENT_DERIVATION = 4;
  TIPO_COMPONENT_JOIN = 5;
  TIPO_COMPONENT_CONDENSATION = 6;
  TIPO_COMPONENT_EXECUTE = 7;
  TIPO_COMPONENT_SCRIPT = 8;

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
  strict protected
    procedure Preview; override;
  public
    procedure Edit; override;
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
    class function New(const AParent: TWinControl; const AType: Byte): TComponentETL;
  end;

implementation

uses // FireDAC.Stan.Intf, FireDAC.Stan.Option,
  // FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool,
  // FireDAC.Stan.Async, FireDAC.Phys, FireDAC.VCLUI.Wait, Data.DB,
  FireDAC.Comp.Client;

{ TCompQuery }

procedure TCompQuery.Preview;
var
  Conn: TFDConnection;
  Qr: TFDQuery;
begin
  if not Assigned(FFormGrid) then
    FFormGrid := TFoGrid.New(Self);

  // FFormGrid.tv.
  Conn := TFDConnection.Create(Self);
  Qr := TFDQuery.Create(Self);
  try
    FFormGrid.ShowModal;
    {

      // Conn.ConnectionDefName := FFormEdit.ClConexoes.Items[FFormEdit.ClConexoes.ItemIndex];
      // Conn.Connected := True;

      // Qr.ConnectionName := 'testar';
      // Qr.Connection := Conn;

      // Qr.SQL.Text := FFormEdit.MM.Lines.Text;
      // Qr.Filter := Trim(AFilter);
      // Qr.Filtered := Qr.Filter <> '';
      // Qr.Open;
      for i := 0 to Qr.FieldDefs.Count - 1 do
      with tv.CreateColumn do
      begin
      Text := Qr.Fields[i].FieldName;
      end;

      while not Qr.Eof do
      begin

      end;

    }
  finally
    Qr.DisposeOf;
    Conn.DisposeOf;
  end;

end;

procedure TCompQuery.Edit;
begin
  if not Assigned(FFormEdit) then
    FFormEdit := TFoEditQuery.New(Self);
  FFormEdit.ShowModal;
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

class function TComponentETLFactory.New(const AParent: TWinControl; const AType: Byte)
  : TComponentETL;
begin
  case AType of
    TIPO_COMPONENT_QUERY:
      begin
        Result := TCompQuery.Create(AParent, AParent);
        Result.Title := 'Query';
      end;
    TIPO_COMPONENT_FILE:
      begin
        Result := TCompFile.Create(AParent, AParent);
        Result.Title := 'File';
      end;
    TIPO_COMPONENT_FILTER:
      begin
        Result := TCompFilter.Create(AParent, AParent);
        Result.Title := 'Filter';
      end;
    TIPO_COMPONENT_CONVERSION:
      begin
        Result := TCompConversion.Create(AParent, AParent);
        Result.Title := 'Conversion';
      end;
    TIPO_COMPONENT_DERIVATION:
      begin
        Result := TCompDerivation.Create(AParent, AParent);
        Result.Title := 'Derivation';
      end;
    TIPO_COMPONENT_JOIN:
      begin
        Result := TCompJoin.Create(AParent, AParent);
        Result.Title := 'Join';
      end;
    TIPO_COMPONENT_CONDENSATION:
      begin
        Result := TCompConversion.Create(AParent, AParent);
        Result.Title := 'Condensation';
      end;
    TIPO_COMPONENT_EXECUTE:
      begin
        Result := TCompExecute.Create(AParent, AParent);
        Result.Title := 'Execute';
      end;
  else
    // TIPO_COMPONENT_SCRIPT:
    begin
      Result := TCompScript.Create(AParent, AParent);
      Result.Title := 'Script';
    end;
  end;
  Result.Tag := AType;
end;

end.
