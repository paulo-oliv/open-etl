unit ETL.Form.Main;

interface

uses dxRibbonSkins, dxRibbonCustomizationForm, cxGraphics, cxControls, cxLookAndFeels,
  cxLookAndFeelPainters, FireDAC.UI.Intf, FireDAC.Stan.Async, FireDAC.Comp.ScriptCommands,
  FireDAC.Stan.Util, FireDAC.VCLUI.Script, Vcl.Menus, Vcl.StdActns, Vcl.ActnList, System.Classes,
  System.Actions, dxBar, dxBarApplicationMenu, dxRibbon, cxClasses, FireDAC.Comp.UI,
  FireDAC.Stan.Intf, FireDAC.Comp.Script, dxStatusBar, dxRibbonStatusBar, dxGDIPlusClasses,
  Vcl.Controls, Vcl.ExtCtrls, Vcl.Forms, ETL.Component, FireDAC.Stan.Option, FireDAC.Stan.Param,
  FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf, Data.DB,
  FireDAC.Comp.DataSet, FireDAC.Comp.Client, Vcl.ToolWin, Vcl.ActnMan, Vcl.ActnCtrls, Vcl.ActnMenus,
  Vcl.PlatformDefaultStyleActnCtrls, Vcl.StdCtrls, Vcl.ComCtrls, cxContainer, cxEdit, cxProgressBar;

type
  TFoMain = class(TForm)
    SQL: TFDScript;
    FDScriptDialog: TFDGUIxScriptDialog;
    CategoryPanelGroup1: TCategoryPanelGroup;
    CpLoad: TCategoryPanel;
    CpTransform: TCategoryPanel;
    CpExtract: TCategoryPanel;
    ImCondensation: TImage;
    ImConversion: TImage;
    ImQuery: TImage;
    ImFile: TImage;
    dxRibbonStatusBar1: TdxRibbonStatusBar;
    AlPrinc: TActionList;
    AcOpen: TFileOpen;
    AcNew: TAction;
    AcExecute: TAction;
    AcEditCut: TEditCut;
    AcEditCopy: TEditCopy;
    AcEditPaste: TEditPaste;
    AcEditSelectAll: TEditSelectAll;
    AcEditUndo: TEditUndo;
    AcCommit: TAction;
    AcRollBack: TAction;
    AcSave: TFileSaveAs;
    AcAutoFecha: TAction;
    AcClose: TAction;
    ImExecute: TImage;
    ImScript: TImage;
    ImDerivation: TImage;
    ImFilter: TImage;
    ImJoin: TImage;
    FileMain: TFDMemTable;
    FileMainx: TIntegerField;
    FileMainy: TIntegerField;
    FileMaintype: TShortintField;
    FileMainscript: TMemoField;
    Panel1: TPanel;
    BtOpen: TButton;
    BtSave: TButton;
    PopupMenuOpen: TPopupMenu;
    PopupMenuSave: TPopupMenu;
    ProgressBar: TcxProgressBar;
    procedure FormCreate(Sender: TObject);
    procedure AcExecuteExecute(Sender: TObject);
    procedure AcOpenAccept(Sender: TObject);
    procedure AcSaveAccept(Sender: TObject);
    procedure AcSaveBeforeExecute(Sender: TObject);
    procedure AcOpenBeforeExecute(Sender: TObject);
    procedure AcCommitAutoExecute(Sender: TObject);
    procedure AcNewExecute(Sender: TObject);
    procedure SQLSpoolPut(AEngine: TFDScript; const AMessage: string; AKind: TFDScriptOutputKind);
    procedure AcAddQueryExecute(Sender: TObject);
    procedure AcCloseExecute(Sender: TObject);
    procedure FormDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState;
      var Accept: Boolean);
    procedure FormDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure PopupMenuOpenPopup(Sender: TObject);
  strict private
    procedure updateEnableButtons;
  public
  end;

var
  FoMain: TFoMain;

implementation

{$R *.dfm}

uses uMsg, SysUtils, ETL.DataModule.Main, ETL.FileProject, ETL.Component.Factory,
  ETL.FileProject.Interfaces;

procedure TFoMain.AcNewExecute(Sender: TObject);
begin
  //
end;

procedure TFoMain.AcOpenBeforeExecute(Sender: TObject);
begin
  // if Assigned(ActiveMDIChild) then // abre dialog no mesmo diretorio do arquivo ja aberto
  // AcOpen.Dialog.InitialDir := ExtractFileDir(ActiveMDIChild.Caption);
end;

procedure TFoMain.AcAddQueryExecute(Sender: TObject);

{ c: Integer;
  nome: string;

  procedure geraNome;
  begin
  nome := IntToStr(c) + '.sql';
  c := c + 1;
  end;

  function NomeNaoExiste: Boolean;
  var
  i: Integer;
  begin
  Result := True;
  for i := 0 to Self.MDIChildCount - 1 do
  if Self.MDIChildren[i].Caption = nome then
  Exit(False);
  end; }

begin
  { c := 1;
    repeat
    geraNome;
    until NomeNaoExiste; }

  TProjectETL.AddComponent(Self, KIND_COMPONENT_QUERY, 50, 50);
end;

procedure TFoMain.AcOpenAccept(Sender: TObject);
begin
  if FileExists(AcOpen.Dialog.FileName) then
  begin

    if TProjectETL.GetInstance.getListComponents.Count > 0 then
    begin
      // if TMensagem.Confirmacao('Do you want to keep the existing components?') then
      // else
      TProjectETL.Clear;
    end;

    TProjectETL.Open(AcOpen.Dialog.FileName, Self)
  end
  // else
  // TFoEditQuery.Create(Self).Novo(AFile)

  // if EhArquivo then
  // if UpperCase(ExtractFileExt(AcAbrir.Dialog.FileName))='.CSV' then
  // CriaSQLProduto
  // else
  // visualizaPanel;
end;

procedure TFoMain.AcSaveBeforeExecute(Sender: TObject);
begin
  // if Assigned(ActiveMDIChild) then
  // AcSave.Dialog.InitialDir := ExtractFileDir(ActiveMDIChild.Caption);
end;

procedure TFoMain.AcSaveAccept(Sender: TObject);
begin
  TProjectETL.Save(AcSave.Dialog.FileName)
  { if Assigned(ActiveMDIChild) then
    with ActiveMDIChild as TFoMdiChild do
    begin
    Caption := AcSalvar.Dialog.FileName;
    Salvar;
    end; }
end;

procedure TFoMain.AcCloseExecute(Sender: TObject);
begin
  Close;
end;

procedure TFoMain.AcCommitAutoExecute(Sender: TObject);
begin
  // DmDBRoot.DB.TxOptions.AutoCommit := AcCommitAuto.Checked;
  updateEnableButtons;
end;

procedure TFoMain.updateEnableButtons;
begin
  // AcCommit.Enabled := not DmDBRoot.DB.TxOptions.AutoCommit;
  // AcRollBack.Enabled := not DmDBRoot.DB.TxOptions.AutoCommit;
end;

procedure TFoMain.FormCreate(Sender: TObject);
begin
  { TMensagem.TryExcept(Name, 'Inicialização',
    procedure
    begin
    AcAutoFecha.Checked := False;
    AcCommitAuto.Checked := DmDBRoot.DB.TxOptions.AutoCommit;
    if DmDBRoot.Reconecta then
    begin
    SB.Panels[0].Text := DmDBRoot.DB.Params.Values['Database'];
    SB.Panels[1].Text := DmDBRoot.DB.Params.Values['Server'];
    SB.Panels[2].Text := DmDBRoot.DB.Params.Values['Port'];
    end
    else
    Application.Terminate;
    end); }
end;

procedure TFoMain.FormDragDrop(Sender, Source: TObject; X, Y: Integer);

  procedure AddComp(const AKind: Byte; const APrefix: string);
  begin
    TProjectETL.AddComponent(Self, AKind, X, Y).Title :=
      TProjectETL.GetInstance.getListComponents.GenerateTitle(APrefix)
  end;

const
  STR_QUERY = 'Query';
  STR_FILE = 'File';
  STR_FILTER = 'Filter';
  STR_CONVERSION = 'Conversion';
  STR_DERIVATION = 'Derivation';
  STR_JOIN = 'Join';
  STR_CONDENSATION = 'Condensation';
  STR_EXECUTE = 'Execute';
  STR_SCRIPT = 'Script';
begin
  TImage(Source).EndDrag(True);
  if Source = ImQuery then
    AddComp(KIND_COMPONENT_QUERY, STR_QUERY)
  else if Source = ImFile then
    AddComp(KIND_COMPONENT_FILE, STR_FILE)
  else if Source = ImFilter then
    AddComp(KIND_COMPONENT_FILTER, STR_FILTER)
  else if Source = ImConversion then
    AddComp(KIND_COMPONENT_CONVERSION, STR_CONVERSION)
  else if Source = ImDerivation then
    AddComp(KIND_COMPONENT_DERIVATION, STR_DERIVATION)
  else if Source = ImJoin then
    AddComp(KIND_COMPONENT_JOIN, STR_JOIN)
  else if Source = ImCondensation then
    AddComp(KIND_COMPONENT_CONDENSATION, STR_CONDENSATION)
  else if Source = ImExecute then
    AddComp(KIND_COMPONENT_EXECUTE, STR_EXECUTE)
  else if Source = ImScript then
    AddComp(KIND_COMPONENT_SCRIPT, STR_SCRIPT)
end;

procedure TFoMain.FormDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState;
  var Accept: Boolean);
begin
  Accept := Source is TImage;
end;

procedure TFoMain.PopupMenuOpenPopup(Sender: TObject);
begin
  // if PopupMenuOpen.Items.Count = 0 then

end;

procedure TFoMain.SQLSpoolPut(AEngine: TFDScript; const AMessage: string;
  AKind: TFDScriptOutputKind);
begin
  // SB.Panels[3].Text := AMessage;
end;

procedure TFoMain.AcExecuteExecute(Sender: TObject);

  procedure Execute;

  { procedure executaDiretoArquivo;

    procedure msg(const s: String);
    begin
    if s = '' then
    begin
    TMensagem.Informacao('Script executado com sucesso!', '', 5);
    if AcAutoFecha.Checked then
    Application.Terminate;
    end
    else
    TMensagem.Aviso(s);
    end;

    begin
    try
    msg(DmDBRoot.rodaScriptNoCMD(ActiveMDIChild.Caption));
    except
    on e: exception do
    TMensagem.Erro(e, Name, 'Script SQL');
    end;
    end;

    procedure executaMM;

    procedure copiaLinhas;
    begin
    SQL.SQLScripts.Clear;
    if AcTriggers.Checked then
    SQL.SQLScripts.Add.SQL.Text := 'SET @DISABLE_TRIGGERS=1;';
    if AcForeignKey.Checked then
    SQL.SQLScripts.Add.SQL.Text := 'SET FOREIGN_KEY_CHECKS=0;';

    SQL.SQLScripts.Add.SQL := TFoMdiChild(ActiveMDIChild).MM.Lines;

    if AcTriggers.Checked then
    SQL.SQLScripts.Add.SQL.Text := 'SET @DISABLE_TRIGGERS=NULL;';
    if AcForeignKey.Checked then
    SQL.SQLScripts.Add.SQL.Text := 'SET FOREIGN_KEY_CHECKS=1;';
    end;

    begin
    try
    copiaLinhas;
    try
    SQL.ValidateAll;
    SQL.ExecuteAll;
    finally
    if SQL.TotalErrors = 0 then
    begin
    if AcAutoFecha.Checked then
    Application.Terminate;
    end;
    end;
    except
    on e: exception do
    TMensagem.Erro(e, Name, 'Script');
    end;
    end; }

  begin
    { if Assigned(ActiveMDIChild) then
      if TFoMdiChild(ActiveMDIChild).EhArquivo then
      executaDiretoArquivo
      else
      executaMM; }
  end;

begin
  AcExecute.Enabled := false;
  try
    // SB.Panels[3].Text := '';
    Execute;
  finally
    AcExecute.Enabled := True;
  end;
end;

end.
