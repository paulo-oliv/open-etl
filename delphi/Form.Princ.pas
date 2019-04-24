unit Form.Princ;

interface

uses dxRibbonSkins, dxRibbonCustomizationForm, cxGraphics, cxControls, cxLookAndFeels,
  cxLookAndFeelPainters, FireDAC.UI.Intf, FireDAC.Stan.Async, FireDAC.Comp.ScriptCommands,
  FireDAC.Stan.Util, FireDAC.VCLUI.Script, Vcl.Menus, Vcl.StdActns, Vcl.ActnList, System.Classes,
  System.Actions, dxBar, dxBarApplicationMenu, dxRibbon, cxClasses, FireDAC.Comp.UI,
  FireDAC.Stan.Intf, FireDAC.Comp.Script, dxStatusBar, dxRibbonStatusBar, dxGDIPlusClasses,
  Vcl.Controls, Vcl.ExtCtrls, Vcl.Forms, ComponentETL, FireDAC.Stan.Option, FireDAC.Stan.Param,
  FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf, Data.DB,
  FireDAC.Comp.DataSet, FireDAC.Comp.Client, Vcl.ToolWin, Vcl.ActnMan, Vcl.ActnCtrls, Vcl.ActnMenus,
  Vcl.PlatformDefaultStyleActnCtrls, Vcl.StdCtrls;

type
  TFoPrinc = class(TForm)
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
    AcEditScript: TAction;
    ImExecute: TImage;
    ImScript: TImage;
    ImDerivation: TImage;
    ImFilter: TImage;
    ImJoin: TImage;
    AcPreview: TAction;
    AcEditTitle: TAction;
    PopupComp: TPopupMenu;
    MnEdit: TMenuItem;
    MnPreview: TMenuItem;
    N2: TMenuItem;
    MnEditLabel: TMenuItem;
    N1: TMenuItem;
    MnDeleteComponent: TMenuItem;
    PopupLink: TPopupMenu;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MnDelLink: TMenuItem;
    FileMain: TFDMemTable;
    FileMainx: TIntegerField;
    FileMainy: TIntegerField;
    FileMaintype: TShortintField;
    FileMainscript: TMemoField;
    Button1: TButton;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure AcExecuteExecute(Sender: TObject);
    procedure AcOpenAccept(Sender: TObject);
    procedure AcCommitExecute(Sender: TObject);
    procedure AcSaveAccept(Sender: TObject);
    procedure AcSaveBeforeExecute(Sender: TObject);
    procedure AcOpenBeforeExecute(Sender: TObject);
    procedure AcRollBackExecute(Sender: TObject);
    procedure AcCommitAutoExecute(Sender: TObject);
    procedure AcTriggersExecute(Sender: TObject);
    procedure AcForeignKeyExecute(Sender: TObject);
    procedure AcAutoFechaExecute(Sender: TObject);
    procedure AcNewExecute(Sender: TObject);
    procedure SQLSpoolPut(AEngine: TFDScript; const AMessage: string; AKind: TFDScriptOutputKind);
    procedure AcAddQueryExecute(Sender: TObject);
    procedure AcEditScriptExecute(Sender: TObject);
    procedure MnDeleteComponentClick(Sender: TObject);
    procedure AcPreviewExecute(Sender: TObject);
    procedure AcCloseExecute(Sender: TObject);
    procedure AcEditTitleExecute(Sender: TObject);
    procedure FormDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState;
      var Accept: Boolean);
    procedure FormDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure MnDelLinkClick(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
  strict private
    procedure updateEnableButtons;
    procedure Execute;
  strict protected
    procedure ComponentPaint(Sender: TObject);
    procedure ComponentDragDrop(Sender, Source: TObject; X, Y: Integer);
  public
    function AddComponent(const AType: Byte; const X, Y: Integer): TComponentETL;
  end;

var
  FoPrinc: TFoPrinc;

implementation

{$R *.dfm}

uses uMsg, SysUtils, uDmImages, FileProjectETL;

procedure TFoPrinc.AcNewExecute(Sender: TObject);
begin
  //
end;

procedure TFoPrinc.AcOpenBeforeExecute(Sender: TObject);
begin
  // if Assigned(ActiveMDIChild) then // abre dialog no mesmo diretorio do arquivo ja aberto
  // AcOpen.Dialog.InitialDir := ExtractFileDir(ActiveMDIChild.Caption);
end;

procedure TFoPrinc.AcAddQueryExecute(Sender: TObject);

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

  AddComponent(0, 50, 50);
end;

procedure TFoPrinc.AcOpenAccept(Sender: TObject);
begin
  if FileExists(AcOpen.Dialog.FileName) then
  begin
    TProjectETL.Load(AcOpen.Dialog.FileName)
  end
  // else
  // TFoEditQuery.Create(Self).Novo(AFile)

  // if EhArquivo then
  // if UpperCase(ExtractFileExt(AcAbrir.Dialog.FileName))='.CSV' then
  // CriaSQLProduto
  // else
  // visualizaPanel;
end;

procedure TFoPrinc.AcSaveBeforeExecute(Sender: TObject);
begin
  // if Assigned(ActiveMDIChild) then
  // AcSave.Dialog.InitialDir := ExtractFileDir(ActiveMDIChild.Caption);
end;

procedure TFoPrinc.AcSaveAccept(Sender: TObject);
begin
    TProjectETL.Save(AcSave.Dialog.FileName)
  { if Assigned(ActiveMDIChild) then
    with ActiveMDIChild as TFoMdiChild do
    begin
    Caption := AcSalvar.Dialog.FileName;
    Salvar;
    end; }
end;

procedure TFoPrinc.AcCloseExecute(Sender: TObject);
begin
  Close;
end;

procedure TFoPrinc.AcCommitAutoExecute(Sender: TObject);
begin
  // DmDBRoot.DB.TxOptions.AutoCommit := AcCommitAuto.Checked;
  updateEnableButtons;
end;

procedure TFoPrinc.updateEnableButtons;
begin
  // AcCommit.Enabled := not DmDBRoot.DB.TxOptions.AutoCommit;
  // AcRollBack.Enabled := not DmDBRoot.DB.TxOptions.AutoCommit;
end;

procedure TFoPrinc.Execute;

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

procedure TFoPrinc.FormCreate(Sender: TObject);
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

function TFoPrinc.AddComponent(const AType: Byte; const X, Y: Integer): TComponentETL;
begin
  Result := TComponentETL.Factory(Self, Self, AType, X, Y);
  TProjectETL.AddComponent(Result);
  Result.OnPaint := ComponentPaint;
  Result.OnDragDrop := ComponentDragDrop;
  Result.PopupMenu := PopupComp;
end;

procedure TFoPrinc.FormDragDrop(Sender, Source: TObject; X, Y: Integer);
begin
  TImage(Source).EndDrag(True);
  if Source = ImQuery then
    AddComponent(TIPO_COMPONENT_QUERY, X, Y)
  else if Source = ImFile then
    AddComponent(TIPO_COMPONENT_FILE, X, Y)
  else if Source = ImFilter then
    AddComponent(TIPO_COMPONENT_FILTER, X, Y)
  else if Source = ImConversion then
    AddComponent(TIPO_COMPONENT_CONVERSION, X, Y)
  else if Source = ImDerivation then
    AddComponent(TIPO_COMPONENT_DERIVATION, X, Y)
  else if Source = ImJoin then
    AddComponent(TIPO_COMPONENT_JOIN, X, Y)
  else if Source = ImCondensation then
    AddComponent(TIPO_COMPONENT_CONDENSATION, X, Y)
  else if Source = ImExecute then
    AddComponent(TIPO_COMPONENT_EXECUTE, X, Y)
  else if Source = ImScript then
    AddComponent(TIPO_COMPONENT_SCRIPT, X, Y)
end;

procedure TFoPrinc.FormDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState;
  var Accept: Boolean);
begin
  Accept := Source is TImage;
end;

procedure TFoPrinc.ComponentDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  temp: TLinkComponents;
begin
  TComponentETL(Sender).EndDrag(True);

  temp := TLinkComponents.Create(Self);

  temp.Parent := Self;
  temp.Source := TComponentETL(Sender);
  temp.Target := TComponentETL(Source);
  temp.PopupMenu := PopupLink;
  temp.Text := 'teste';
  temp.RefreshSize;
end;

procedure TFoPrinc.ComponentPaint(Sender: TObject);
begin
  DmImages.ILDev64.Draw(TPaintBox(Sender).Canvas, 0, 0, TComponentETL(Sender).Tag);
end;

procedure TFoPrinc.SQLSpoolPut(AEngine: TFDScript; const AMessage: string;
  AKind: TFDScriptOutputKind);
begin
  // SB.Panels[3].Text := AMessage;
end;

procedure TFoPrinc.AcExecuteExecute(Sender: TObject);
begin
  AcExecute.Enabled := false;
  try
    // SB.Panels[3].Text := '';
    Execute;
  finally
    AcExecute.Enabled := True;
  end;
end;

procedure TFoPrinc.AcCommitExecute(Sender: TObject);
begin
  // DmDBRoot.DB.Commit;
end;

procedure TFoPrinc.AcEditScriptExecute(Sender: TObject);
begin
  TComponentETL(PopupComp.PopupComponent).Edit;
end;

procedure TFoPrinc.AcEditTitleExecute(Sender: TObject);
var
  s: string;
begin
  s := TComponentETL(PopupComp.PopupComponent).Title;
  if TMensagem.InputQuery('Edit Label', s) then
    TComponentETL(PopupComp.PopupComponent).Title := s;
end;

procedure TFoPrinc.AcPreviewExecute(Sender: TObject);
begin
  TComponentETL(PopupComp.PopupComponent).Preview;
end;

procedure TFoPrinc.MnDelLinkClick(Sender: TObject);
begin
  PopupLink.PopupComponent.DisposeOf;
end;

procedure TFoPrinc.MenuItem4Click(Sender: TObject);
var
  s: string;
begin
  s := TLinkComponents(PopupLink.PopupComponent).Text;
  if TMensagem.InputQuery('Edit Label', s) then
    TLinkComponents(PopupLink.PopupComponent).Text := s;
end;

procedure TFoPrinc.MnDeleteComponentClick(Sender: TObject);
var
  i: Integer;
begin
  // if PopupComp.PopupComponent is TComponentETL then
  // begin
  for i := ControlCount - 1 downto 0 do
    if Controls[i] is TLinkComponents then
    begin
      if (PopupComp.PopupComponent = TLinkComponents(Controls[i]).Source) or
        (PopupComp.PopupComponent = TLinkComponents(Controls[i]).Target) then
        Controls[i].DisposeOf;
    end;

  TComponentETL(PopupComp.PopupComponent).Delete
  // end;
end;

procedure TFoPrinc.AcRollBackExecute(Sender: TObject);
begin
  // DmDBRoot.DB.Rollback;
end;

procedure TFoPrinc.AcTriggersExecute(Sender: TObject);
begin
  //
end;

procedure TFoPrinc.AcAutoFechaExecute(Sender: TObject);
begin
  //
end;

procedure TFoPrinc.AcForeignKeyExecute(Sender: TObject);
begin
  //
end;

end.
