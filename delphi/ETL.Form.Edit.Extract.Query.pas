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
    function GetConnection(const AIndex: Integer): TFDConnection;
    procedure UpdateConnections;
    procedure UpdateTreeTables;
    procedure UpdateSqlOut;
    class function New(const AOwner: TComponent): TFoEditQuery;
  end;

implementation

{$R *.dfm}

uses
  SectionConexao,
  System.SysUtils,
  Vcl.Dialogs,
  RegularExpressions,
  FireDAC.VCLUI.ConnEdit,
  ETL.ListConnections.Singleton;

{ TFoEditQuery }

function TFoEditQuery.GetConnection(const AIndex: Integer): TFDConnection;
begin
  Result := TListConnections.GetConnection(ClConexoes.Items[AIndex]);
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

  LConn := GetConnection(ClConexoes.ItemIndex);
  LTables := TConnectionDatabase.CreateMetaInfoTables; // LConn.GetTableNames();
  LTables.Connection := LConn;
  try
    LFields := TConnectionDatabase.CreateMetaInfoFields; // LConn.GetFieldNames();
    try
      LFields.Connection := LConn;
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
  // TvTable.FullExpand;
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
var
  LSqlOut, LPattern: string;
  LStartPos: Integer;

  function AddSelect(const ASchema: string): string;
  begin
    Result := Copy(MmOut.Text, 1, LStartPos - 1);
    Result := Result + ASchema + '.' + Copy(MmOut.Text, LStartPos);
  end;

  procedure addSchemas(const AIdxConn: Integer);
  var
    LSchemas: TFDQuery; // TFDMetaInfoQuery
    LConn: TFDConnection;
  begin
    LConn := GetConnection(AIdxConn);
    // LConn.GetSchemaNames('', '', LStrList.Items)
    LSchemas := TFDQuery.Create(nil); // TConnectionDatabase.CreateMetaInfoSchemas;
    try
      LSchemas.Connection := LConn;
      LSchemas.SQL.Text := 'SHOW DATABASES';
      LSchemas.Open;
      while not LSchemas.Eof do
      begin
        if TRegEx.IsMatch(LSchemas.Fields[0].AsString, LPattern) then
          LSqlOut := LSqlOut + AddSelect(LSchemas.Fields[0].AsString);
        LSchemas.Next;
      end;
      LSqlOut := StringReplace(LSqlOut, '-- UNION', 'UNION', [rfReplaceAll, rfIgnoreCase]);
    finally
      LSchemas.DisposeOf;
    end;
  end;

const
  PREFIX_REGEX = '/* ^';
  SUFFIX_REGEX = '*/';
var
  LEndPos, i: Integer;

begin
  MmOut.Text := MmIn.Text;
  LStartPos := Pos(PREFIX_REGEX, MmIn.Text);
  if LStartPos > 0 then
  begin
    LSqlOut := '';
    MmOut.SelStart := LStartPos - 1;
    MmOut.SelLength := Pos(SUFFIX_REGEX, MmOut.Text) - LStartPos + 2;
    LPattern := Copy(MmOut.Text, MmOut.SelStart + 4, MmOut.SelLength - 5);
    MmOut.SelText := '';

    for i := 0 to ClConexoes.Count - 1 do
      if ClConexoes.Checked[i] then
        addSchemas(i);

    MmOut.Text := LSqlOut;
    for i := MmOut.Lines.Count - 1 downto 0 do
    begin
      MmOut.Lines[i] := Trim(MmOut.Lines[i]);
      if MmOut.Lines[i] = '' then
        MmOut.Lines.Delete(i)
      else if UpperCase(Copy(MmOut.Lines[i], 1, 5)) = 'UNION' then
        MmOut.Lines.Delete(i)
      else
        break;
    end;

  end;
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

  LConn := GetConnection(ClConexoes.ItemIndex);
  LConnStr := LConn.ResultConnectionDef.BuildString;

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
