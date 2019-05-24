unit Form.Edit.Query;

interface

uses Form.Edit, Vcl.Menus, Vcl.StdCtrls, Vcl.Controls, Vcl.CheckLst, Vcl.ExtCtrls, System.Classes,
  System.Actions, Vcl.ActnList, cxSplitter, upControls, cxGraphics, cxControls, cxLookAndFeels,
  cxLookAndFeelPainters;

type
  TFoEditQuery = class(TFoEdit)
    MM: TMemo;
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
    procedure AcReverseChecksExecute(Sender: TObject);
    procedure AcNewConnectionExecute(Sender: TObject);
    procedure AcDeleteConnectionExecute(Sender: TObject);
    procedure AcEditConnectionExecute(Sender: TObject);
  public
    procedure UpdateConnections;
    class function New(const AOwner: TComponent): TFoEditQuery;
  end;

implementation

{$R *.dfm}

uses SectionConexao, System.SysUtils, Vcl.Dialogs, FireDAC.Comp.Client,
  FireDAC.VCLUI.ConnEdit, FireDAC.Stan.Def, FireDAC.Phys.Intf,
  // FireDAC.Phys.TDBXDef, FireDAC.Phys.DSDef, FireDAC.Phys.MongoDBDef, FireDAC.Phys.TDataDef,
  // FireDAC.Phys.MSSQLDef, FireDAC.Phys.InfxDef, FireDAC.Phys.DB2Def, FireDAC.Phys.OracleDef,
  // FireDAC.Phys.ODBCDef, FireDAC.Phys.ASADef, FireDAC.Phys.ASA, FireDAC.Phys.ODBC,
  // FireDAC.Phys.Oracle, FireDAC.Phys.DB2, FireDAC.Phys.Infx, FireDAC.Phys.MSSQL,
  // FireDAC.Phys.TData, FireDAC.Phys.MongoDB, FireDAC.Phys.DS, FireDAC.Phys.TDBXBase,
  // FireDAC.Phys.TDBX
  FireDAC.Stan.ExprFuncs,
  FireDAC.Phys.SQLiteDef, FireDAC.UI.Intf, FireDAC.VCLUI.Wait, FireDAC.Phys.IBDef,
  FireDAC.Phys.MSAccDef, FireDAC.Phys.MySQLDef,
  FireDAC.Phys.ADSDef, FireDAC.Phys.FBDef, FireDAC.Phys.PGDef, FireDAC.Phys.PG, FireDAC.Phys.FB,
  FireDAC.Phys.ADS,
  FireDAC.Phys.MySQL, FireDAC.Phys.MSAcc,
  FireDAC.Stan.StorageJSON, FireDAC.Stan.StorageXML, FireDAC.Stan.StorageBin, FireDAC.Moni.FlatFile,
  FireDAC.Moni.Custom, FireDAC.Moni.Base, FireDAC.Moni.RemoteClient, FireDAC.Phys.IBBase,
  FireDAC.Phys.IB, FireDAC.Comp.UI, FireDAC.Phys.SQLite,
  FireDAC.Phys.ODBCBase,
  FireDAC.Stan.Intf, FireDAC.Phys;

{ TFoEditQuery }

procedure TFoEditQuery.UpdateConnections;
begin
  TConnectionDefsIni.GetInstance.ReadSections(ClConexoes.Items);
end;

procedure TFoEditQuery.AcReverseChecksExecute(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to ClConexoes.Count - 1 do
    ClConexoes.Checked[i] := not ClConexoes.Checked[i];
end;

procedure TFoEditQuery.AcDeleteConnectionExecute(Sender: TObject);
var
  LSection: string;
begin
  if ClConexoes.ItemIndex < 0 then
    Exit;
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
    Exit;
  LSection := ClConexoes.Items[ClConexoes.ItemIndex];

  LConn := TFDConnection.Create(nil);
  try
    LConn.ConnectionDefName := LSection;
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

end.
