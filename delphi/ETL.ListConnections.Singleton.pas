unit ETL.ListConnections.Singleton;

interface

uses Generics.Collections, uList, FireDAC.Comp.Client;

type
  IConnectionDatabase = interface
    ['{9EF4CE2D-850E-49D9-B692-2A107EEB670D}']
    function GetName: string;
    function GetConnection: TFDConnection;
    property Name: string read GetName;
    property Connection: TFDConnection read GetConnection;
  end;

  TConnectionDatabase = class(TInterfacedObject, IConnectionDatabase)
  strict private
    FConnection: TFDConnection;
    class function CreateConnection(const AName: string): TFDConnection;
  strict protected
    function GetName: string;
    function GetConnection: TFDConnection;
  public
    constructor Create(const AName: string);
    destructor Destroy; override;
    property Connection: TFDConnection read GetConnection;
    class function CreateMetaInfoTables: TFDMetaInfoQuery;
    class function CreateMetaInfoFields: TFDMetaInfoQuery;
    class function CreateMetaInfoSchemas: TFDMetaInfoQuery;
  end;

  IListConnections = interface(IList<IConnectionDatabase>)
    ['{E78AD476-370C-423C-A7E3-9844D9A5DD3C}']

  end;

  TListConnections = class(TInterfacedList<IConnectionDatabase>, IListConnections)
  strict private
    class var FInstance: IListConnections;
  public
    class function GetInstance: IListConnections;
    class function GetConnection(const AName: string): TFDConnection;
  end;

implementation

uses
  FireDAC.Stan.Def, FireDAC.Phys.Intf, FireDAC.Stan.ExprFuncs,
  FireDAC.Phys.SQLiteDef, FireDAC.UI.Intf, FireDAC.VCLUI.Wait, FireDAC.Phys.IBDef,
  FireDAC.Phys.MSAccDef, FireDAC.Phys.MySQLDef, FireDAC.Phys.ADSDef, FireDAC.Phys.FBDef,
  FireDAC.Phys.PGDef, FireDAC.Phys.PG, FireDAC.Phys.FB, FireDAC.Phys.ADS,
  FireDAC.Phys.MySQL, FireDAC.Phys.MSAcc, FireDAC.Stan.StorageJSON, FireDAC.Stan.StorageXML,
  FireDAC.Stan.StorageBin, FireDAC.Moni.FlatFile, FireDAC.Moni.Custom, FireDAC.Moni.Base,
  FireDAC.Moni.RemoteClient, FireDAC.Phys.IBBase, FireDAC.Phys.IB, FireDAC.Comp.UI,
  FireDAC.Phys.SQLite, FireDAC.Phys.ODBCBase, FireDAC.Stan.Intf, FireDAC.Phys
  // FireDAC.Phys.TDBXDef, FireDAC.Phys.DSDef, FireDAC.Phys.MongoDBDef, FireDAC.Phys.TDataDef,
  // FireDAC.Phys.MSSQLDef, FireDAC.Phys.InfxDef, FireDAC.Phys.DB2Def, FireDAC.Phys.OracleDef,
  // FireDAC.Phys.ODBCDef, FireDAC.Phys.ASADef, FireDAC.Phys.ASA, FireDAC.Phys.ODBC,
  // FireDAC.Phys.Oracle, FireDAC.Phys.DB2, FireDAC.Phys.Infx, FireDAC.Phys.MSSQL,
  // FireDAC.Phys.TData, FireDAC.Phys.MongoDB, FireDAC.Phys.DS, FireDAC.Phys.TDBXBase,
  // FireDAC.Phys.TDBX
    ;

{ TConnectionDatabase }

class function TConnectionDatabase.CreateConnection(const AName: string): TFDConnection;
begin
  Result := TFDConnection.Create(nil);
  Result.ConnectionDefName := AName;
  Result.Connected := True;
end;

constructor TConnectionDatabase.Create(const AName: string);
begin
  inherited Create;
  FConnection := CreateConnection(AName);
end;

class function TConnectionDatabase.CreateMetaInfoFields: TFDMetaInfoQuery;
begin
  Result := TFDMetaInfoQuery.Create(nil);
  Result.MetaInfoKind := TFDPhysMetaInfoKind.mkTableFields;
end;

class function TConnectionDatabase.CreateMetaInfoSchemas: TFDMetaInfoQuery;
begin
  Result := TFDMetaInfoQuery.Create(nil);
  Result.MetaInfoKind := TFDPhysMetaInfoKind.mkSchemas;
end;

class function TConnectionDatabase.CreateMetaInfoTables: TFDMetaInfoQuery;
begin
  Result := TFDMetaInfoQuery.Create(nil);
  Result.MetaInfoKind := TFDPhysMetaInfoKind.mkTables;
end;

destructor TConnectionDatabase.Destroy;
begin
  try
    FConnection.DisposeOf;
  except
  end;
  inherited;
end;

function TConnectionDatabase.GetConnection: TFDConnection;
begin
  Result := FConnection;
end;

function TConnectionDatabase.GetName: string;
begin
  Result := FConnection.ConnectionDefName;
end;

{ TListConnections }

class function TListConnections.GetConnection(const AName: string): TFDConnection;
var
  LConn: IConnectionDatabase;
  LList: IListConnections;
begin
  LList := GetInstance;
  for LConn in LList.GetList do
    if LConn.Name = AName then
      exit(LConn.Connection);

  Result := LList.Add(TConnectionDatabase.Create(AName)).Connection;
end;

class function TListConnections.GetInstance: IListConnections;
begin
  if not Assigned(FInstance) then
    FInstance := TListConnections.Create;
  Result := FInstance;
end;

end.
