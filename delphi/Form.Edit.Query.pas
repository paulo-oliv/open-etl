unit Form.Edit.Query;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Form.Edit, cxGraphics, cxControls, cxLookAndFeels,
  cxLookAndFeelPainters, Vcl.StdCtrls, cxSplitter, upControls, Vcl.CheckLst, Vcl.Buttons,
  Vcl.ExtCtrls, System.Actions, Vcl.ActnList, Vcl.Menus;

type
  TFoEditQuery = class(TFoEdit)
    pSplitter1: TpSplitter;
    MM: TMemo;
    PnEsquerda: TPanel;
    ClConexoes: TCheckListBox;
    Button1: TButton;
    PopupMenu1: TPopupMenu;
    New1: TMenuItem;
    Edit1: TMenuItem;
    N1: TMenuItem;
    Delete1: TMenuItem;
    AcInvert: TAction;
    AcEdit: TAction;
    AcDelete: TAction;
    AcNew: TAction;
    procedure AcInvertExecute(Sender: TObject);
    procedure AcNewExecute(Sender: TObject);
  private
    // FAlterado: Boolean;
  public
    class function New(const AOwner: TComponent): TFoEditQuery;
  end;

implementation

{$R *.dfm}

uses SectionConexao, // uMsg
  // IPPeerClient,
  FireDAC.VCLUI.ConnEdit, FireDAC.Stan.Def, FireDAC.Phys.Intf, FireDAC.Phys.TDBXDef,
  FireDAC.Phys.DSDef, FireDAC.Phys.MongoDBDef, FireDAC.Phys.TDataDef, FireDAC.Phys.MSSQLDef,
  FireDAC.Phys.InfxDef, FireDAC.Phys.DB2Def, FireDAC.Phys.OracleDef, FireDAC.Stan.ExprFuncs,
  FireDAC.Phys.SQLiteDef, FireDAC.UI.Intf, FireDAC.VCLUI.Wait, FireDAC.Phys.IBDef,
  FireDAC.Phys.ODBCDef, FireDAC.Phys.MSAccDef, FireDAC.Phys.MySQLDef, FireDAC.Phys.ASADef,
  FireDAC.Phys.ADSDef, FireDAC.Phys.FBDef, FireDAC.Phys.PGDef, FireDAC.Phys.PG, FireDAC.Phys.FB,
  FireDAC.Phys.ADS, FireDAC.Phys.ASA, FireDAC.Phys.MySQL, FireDAC.Phys.MSAcc, FireDAC.Phys.ODBC,
  FireDAC.Stan.StorageJSON, FireDAC.Stan.StorageXML, FireDAC.Stan.StorageBin, FireDAC.Moni.FlatFile,
  FireDAC.Moni.Custom, FireDAC.Moni.Base, FireDAC.Moni.RemoteClient, FireDAC.Phys.IBBase,
  FireDAC.Phys.IB, FireDAC.Comp.UI, FireDAC.Phys.SQLite, FireDAC.Phys.Oracle, FireDAC.Phys.DB2,
  FireDAC.Phys.Infx, FireDAC.Phys.MSSQL, FireDAC.Phys.ODBCBase, FireDAC.Phys.TData,
  FireDAC.Phys.MongoDB, FireDAC.Phys.DS, FireDAC.Stan.Intf, FireDAC.Phys, FireDAC.Phys.TDBXBase,
  FireDAC.Phys.TDBX;

{ TFoEditQuery }

procedure TFoEditQuery.AcInvertExecute(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to ClConexoes.Count - 1 do
    ClConexoes.Checked[i] := not ClConexoes.Checked[i];
end;

procedure TFoEditQuery.AcNewExecute(Sender: TObject);
var
  sConnStr: String;
begin
  // sConnStr := FDConnection1.ResultConnectionDef.BuildString();
  if TfrmFDGUIxFormsConnEdit.Execute(sConnStr, '') then
  begin
    // FDConnection1.ResultConnectionDef.ParseString(sConnStr);
    // FDConnection1.Connected := True;
  end;
end;

class function TFoEditQuery.New(const AOwner: TComponent): TFoEditQuery;
begin
  Result := TFoEditQuery.Create(AOwner);
  TConnectionDefsIni.GetInstance.ReadSections(Result.ClConexoes.Items);
end;

end.
