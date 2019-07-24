unit ETL.Form.Edit.Extract.Query;

interface

uses ETL.Form.Edit.Extract, Vcl.Menus, Vcl.StdCtrls, Vcl.Controls, Vcl.CheckLst, Vcl.ExtCtrls,
  System.Classes, System.Actions, Vcl.ActnList, cxSplitter, upControls, cxGraphics, cxControls,
  cxLookAndFeels, cxLookAndFeelPainters, Vcl.ComCtrls, System.ImageList, Vcl.ImgList,
  FireDAC.Comp.Client;

type
  TFoEditQuery = class(TFoEditExtract)
    PnEsquerda: TPanel;
    ClConexoes: TCheckListBox;
    Button1: TButton;
    PopupMenu1: TPopupMenu;
    New1: TMenuItem;
    Edit1: TMenuItem;
    N1: TMenuItem;
    Delete1: TMenuItem;
    AcReverseChecks: TAction;
    AcEditConnection: TAction;
    AcDeleteConnection: TAction;
    AcNewConnection: TAction;
    AcCheckAll: TAction;
    AcUncheckAll: TAction;
    pSplitter1: TpSplitter;
    PcQuery: TPageControl;
    TsIn: TTabSheet;
    TsOut: TTabSheet;
    MmIn: TMemo;
    MmOut: TMemo;
    TvTable: TTreeView;
    IL: TImageList;
    pSplitter2: TpSplitter;
    procedure AcReverseChecksExecute(Sender: TObject);
    procedure AcNewConnectionExecute(Sender: TObject);
    procedure AcDeleteConnectionExecute(Sender: TObject);
    procedure AcEditConnectionExecute(Sender: TObject);
    procedure ClConexoesClickCheck(Sender: TObject);
    procedure MmInChange(Sender: TObject);
    procedure TsOutEnter(Sender: TObject);
    procedure ClConexoesDblClick(Sender: TObject);
    procedure ClConexoesClick(Sender: TObject);
    procedure TsInEnter(Sender: TObject);
    procedure TvTableDblClick(Sender: TObject);
  public
    procedure UpdateConnections;
    procedure UpdateTreeTables;
    procedure UpdateSqlOut;
    class function New(const AOwner: TComponent): TFoEditQuery;
    function CreateConnection(const AIndex: Integer): TFDConnection;
  end;

implementation

{$R *.dfm}

uses SectionConexao, System.SysUtils, Vcl.Dialogs,
  FireDAC.VCLUI.ConnEdit, FireDAC.Stan.Def, FireDAC.Phys.Intf, FireDAC.Stan.ExprFuncs,
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

{ TFoEditQuery }

function TFoEditQuery.CreateConnection(const AIndex: Integer): TFDConnection;
begin
  Result := TFDConnection.Create(nil);
  Result.ConnectionDefName := ClConexoes.Items[AIndex];
end;

procedure TFoEditQuery.UpdateTreeTables;
var
  LConn: TFDConnection;
  LTables: TFDMetaInfoQuery;
  LFields: TFDMetaInfoQuery;
  LNode: TTreeNode;
begin
  TvTable.Visible := False;
  TvTable.Items.Clear;
  if PcQuery.ActivePage <> TsIn then
    exit;

  if ClConexoes.ItemIndex < 0 then
    exit;

  if not ClConexoes.Checked[ClConexoes.ItemIndex] then
    exit;

  LConn := CreateConnection(ClConexoes.ItemIndex);
  try
    LTables := TFDMetaInfoQuery.Create(nil); // LConn.GetTableNames();
    try
      LFields := TFDMetaInfoQuery.Create(nil); // LConn.GetFieldNames();
      try
        LTables.Connection := LConn;
        LFields.Connection := LConn;
        LTables.MetaInfoKind := TFDPhysMetaInfoKind.mkTables;
        LFields.MetaInfoKind := TFDPhysMetaInfoKind.mkTableFields;
        LTables.Open;
        while not LTables.Eof do
        begin
          LNode := TvTable.Items.Add(nil, { LTables.Fields[1].AsString + '.' + } LTables.Fields[3]
            .AsString);
          LNode.ImageIndex := 0;
          LFields.Close;
          LFields.ObjectName := LTables.Fields[3].AsString;
          LFields.Open;
          while not LFields.Eof do
          begin
            with TvTable.Items.AddChild(LNode, LFields.Fields[4].AsString) do
            begin
              ImageIndex := 1;
              SelectedIndex := 1;
            end;
            LFields.Next;
          end;
          LTables.Next;
        end;
      finally
        LFields.DisposeOf;
      end;
    finally
      LTables.DisposeOf;
    end;
  //  TvTable.FullExpand;
  finally
    LConn.DisposeOf;
  end;
  TvTable.Visible := True;
end;

procedure TFoEditQuery.ClConexoesClick(Sender: TObject);
begin
  UpdateTreeTables;
end;

procedure TFoEditQuery.UpdateConnections;
begin
  TConnectionDefsIni.GetInstance.ReadSections(ClConexoes.Items);
  UpdateTreeTables;
end;

procedure TFoEditQuery.UpdateSqlOut;
begin
  MmOut.Text := MmIn.Text;
end;

procedure TFoEditQuery.AcReverseChecksExecute(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to ClConexoes.Count - 1 do
    ClConexoes.Checked[i] := not ClConexoes.Checked[i];
end;

procedure TFoEditQuery.ClConexoesClickCheck(Sender: TObject);
begin
  DoChange;
end;

procedure TFoEditQuery.ClConexoesDblClick(Sender: TObject);
begin
  AcEditConnection.Execute;
end;

procedure TFoEditQuery.MmInChange(Sender: TObject);
begin
  DoChange;
end;

procedure TFoEditQuery.AcDeleteConnectionExecute(Sender: TObject);
var
  LSection: string;
begin
  if ClConexoes.ItemIndex < 0 then
    exit;
  LSection := ClConexoes.Items[ClConexoes.ItemIndex];
  TConnectionDefsIni.GetInstance.EraseSection(LSection);
  UpdateConnections;
end;

procedure TFoEditQuery.AcEditConnectionExecute(Sender: TObject);
var
  LConnStr: String;
  LStrings: TStrings;
  LRow, LSection, LIdent: string;
  p: Integer;
  LConn: TFDConnection;
begin
  if ClConexoes.ItemIndex < 0 then
    exit;
  LSection := ClConexoes.Items[ClConexoes.ItemIndex];

  LConn := CreateConnection(ClConexoes.ItemIndex);
  try
    LConnStr := LConn.ResultConnectionDef.BuildString;
  finally
    LConn.DisposeOf;
  end;

  if TfrmFDGUIxFormsConnEdit.Execute(LConnStr, LSection) then
  begin
    LStrings := TStringList.Create;
    try
      LStrings.Delimiter := ';';
      LStrings.DelimitedText := LConnStr;
      for LRow in LStrings do
      begin
        p := Pos('=', LRow);
        LIdent := Copy(LRow, 1, p - 1);
        TConnectionDefsIni.GetInstance.WriteString(LSection, LIdent, Copy(LRow, p + 1));
      end;
      UpdateConnections;
    finally
      LStrings.DisposeOf;
    end;
  end;
end;

procedure TFoEditQuery.AcNewConnectionExecute(Sender: TObject);
const
  DEFAULT_SECTION = 'NewConnection';
var
  LConnStr: String;
  LStrings: TStrings;
  LRow, LSection, LIdent: string;
  p: Integer;
begin
  if TfrmFDGUIxFormsConnEdit.Execute(LConnStr, DEFAULT_SECTION) then
  begin
    LStrings := TStringList.Create;
    try
      LStrings.Delimiter := ';';
      LStrings.DelimitedText := LConnStr;
      LSection := DEFAULT_SECTION;
      for LRow in LStrings do
      begin
        p := Pos('=', LRow);
        LIdent := Copy(LRow, 1, p - 1);
        if LIdent.ToLower = 'connectiondef' then
          LSection := Copy(LRow, p + 1)
        else if LIdent.ToLower = 'database' then
          if LSection = DEFAULT_SECTION then
            LSection := Copy(LRow, p + 1);
      end;
      if LSection = '' then
        LSection := DEFAULT_SECTION;
      LSection := InputBox('Connection Name', 'Set connection name', LSection);
      for LRow in LStrings do
      begin
        p := Pos('=', LRow);
        LIdent := Copy(LRow, 1, p - 1);
        TConnectionDefsIni.GetInstance.WriteString(LSection, LIdent, Copy(LRow, p + 1));
      end;
      UpdateConnections;
    finally
      LStrings.DisposeOf;
    end;
  end;
end;

class function TFoEditQuery.New(const AOwner: TComponent): TFoEditQuery;
begin
  Result := TFoEditQuery.Create(AOwner);
  Result.UpdateConnections;
end;

procedure TFoEditQuery.TsInEnter(Sender: TObject);
begin
  if TvTable.Items.Count = 0 then
    UpdateTreeTables;
end;

procedure TFoEditQuery.TsOutEnter(Sender: TObject);
begin
  UpdateSqlOut
end;

procedure TFoEditQuery.TvTableDblClick(Sender: TObject);
begin
  if Assigned(TvTable.Selected) then
  begin
    MmIn.SelText := TvTable.Selected.Text;
    try
      MmIn.SetFocus
    except
    end;
  end;
end;

end.
