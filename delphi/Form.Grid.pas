unit Form.Grid;

interface

uses cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxStyles, cxCustomData,
  cxFilter, cxData, cxDataStorage, cxEdit, cxNavigator, Data.DB, cxDBData, FireDAC.Stan.Intf,
  FireDAC.Stan.Option, FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf,
  FireDAC.DApt.Intf, FireDAC.Stan.Async, FireDAC.DApt, FireDAC.Comp.DataSet, FireDAC.Comp.Client,
  cxGridLevel, cxClasses, cxGridCustomView, cxGridCustomTableView, cxGridTableView,
  cxGridDBTableView, System.Classes, Vcl.Controls, cxGrid, Vcl.Forms, FireDAC.UI.Intf,
  FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Phys, FireDAC.VCLUI.Wait, cxGridBandedTableView,
  Generics.Collections;

type
  TTipoComponentQuery = (Json, Xml, UnionAll, Union, Join, Filter, Conversion, Derivation,
    Condensation);

  IComponentQuery = interface
    ['{EE590AE6-0D13-424E-8EBB-83C5E8B1B92F}']
    function getTipo: TTipoComponentQuery;
    function getScript: string;
    procedure setTipo(const ATipo: TTipoComponentQuery);
    procedure setScript(const AScript: string);
    property Tipo: TTipoComponentQuery read getTipo write setTipo;
    property Script: string read getScript write setScript;
  end;

  TComponentQuery = class(TInterfacedObject, IComponentQuery)
  strict private
    FTipo: TTipoComponentQuery;
    FScript: string;
  strict protected
    function getTipo: TTipoComponentQuery;
    function getScript: string;
    procedure setTipo(const ATipo: TTipoComponentQuery);
    procedure setScript(const AScript: string);
  public
    class function New: IComponentQuery; overload;
    class function New(const AScript: string; const ATipo: TTipoComponentQuery)
      : IComponentQuery; overload;
  end;

  TFoGrid = class(TForm)
    Lv: TcxGridLevel;
    Gr: TcxGrid;
    tv: TcxGridBandedTableView;
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  strict private
    FListComponentQuery: TList<IComponentQuery>;
  public
    procedure AddComponentQuery(const AComponent: IComponentQuery); overload;
    procedure AddComponentQuery(const AScript: string;
      const ATipo: TTipoComponentQuery = UnionAll); overload;
    class function New(const AOwner: TComponent): TFoGrid;
  end;

implementation

{$R *.dfm}
{ TFoGrid }

procedure TFoGrid.AddComponentQuery(const AComponent: IComponentQuery);
begin
  FListComponentQuery.Add(AComponent);
end;

procedure TFoGrid.AddComponentQuery(const AScript: string; const ATipo: TTipoComponentQuery);
begin
  AddComponentQuery(TComponentQuery.New(AScript, ATipo));
end;

procedure TFoGrid.FormDestroy(Sender: TObject);
begin
  FListComponentQuery.DisposeOf;
end;

procedure TFoGrid.FormShow(Sender: TObject);
var
  Conn: TFDConnection;
  Qr: TFDQuery;
  i: Integer;
begin
  Conn := TFDConnection.Create(Self);
  Qr := TFDQuery.Create(Self);
  try
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

  finally
    Qr.DisposeOf;
    Conn.DisposeOf;
  end;
end;

class function TFoGrid.New(const AOwner: TComponent): TFoGrid;
begin
  Result := TFoGrid.Create(AOwner);
  Result.FListComponentQuery := TList<IComponentQuery>.Create;
end;

{ TComponentQuery }

class function TComponentQuery.New: IComponentQuery;
begin
  Result := TComponentQuery.Create;
end;

class function TComponentQuery.New(const AScript: string; const ATipo: TTipoComponentQuery)
  : IComponentQuery;
begin
  Result := New;
  Result.Tipo := ATipo;
  Result.Script := AScript;
end;

function TComponentQuery.getScript: string;
begin
  Result := FScript;
end;

function TComponentQuery.getTipo: TTipoComponentQuery;
begin
  Result := FTipo;
end;

procedure TComponentQuery.setScript(const AScript: string);
begin
  FScript := AScript;
end;

procedure TComponentQuery.setTipo(const ATipo: TTipoComponentQuery);
begin
  FTipo := ATipo;
end;

end.
