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
  Vcl.PlatformDefaultStyleActnCtrls;

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
    procedure Open(const AFile: string);
    procedure Execute;
  strict protected
    procedure ComponentPaint(Sender: TObject);
    procedure ComponentResize(Sender: TObject);
    procedure ComponentMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
    procedure ComponentMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure ComponentMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
    procedure ComponentDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure ComponentDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState;
      var Accept: Boolean);
    procedure ComponentDblClick(Sender: TObject);
  public
    function AddComponent(const AType: Byte): TComponentETL;
  end;

var
  FoPrinc: TFoPrinc;
  moveX, moveY: Integer;
  mover: Boolean;

implementation

{$R *.dfm}

uses uMsg, SysUtils, uDmImages;

procedure TFoPrinc.AcNewExecute(Sender: TObject);
begin
  //
end;

procedure TFoPrinc.Open(const AFile: string);
begin
  if FileExists(AFile) then
  begin
    // FileMain.LoadFromFile(AFile);
  end
  // else
  // TFoEditQuery.Create(Self).Novo(AFile)
end;

procedure TFoPrinc.AcOpenBeforeExecute(Sender: TObject);
begin
  if Assigned(ActiveMDIChild) then // abre dialog no mesmo diretorio do arquivo ja aberto
    AcOpen.Dialog.InitialDir := ExtractFileDir(ActiveMDIChild.Caption);
end;

function TFoPrinc.AddComponent(const AType: Byte): TComponentETL;

  function GenerateTitle(APrefix: string): string;

    function NovoTitle(const ATitle: string): Boolean;
    var
      i: Integer;
    begin
      Result := True;
      for i := 0 to ControlCount - 1 do
        if Controls[i] is TComponentETL then
          if ATitle = TComponentETL(Controls[i]).Title then
            exit(false)
    end;

  var
    LNum: Word;
  begin
    LNum := 0;
    repeat
      LNum := LNum + 1;
      Result := APrefix + IntToStr(LNum);
    until NovoTitle(Result);
  end;

begin
  Result := TComponentETL.Factory(Self, AType);
  Result.Title := GenerateTitle(Result.Title);
  Result.OnPaint := ComponentPaint;
  Result.OnMouseMove := ComponentMouseMove;
  Result.OnMouseUp := ComponentMouseUp;
  Result.OnMouseDown := ComponentMouseDown;
  Result.OnResize := ComponentResize;
  Result.OnDragDrop := ComponentDragDrop;
  Result.OnDragOver := ComponentDragOver;
  Result.OnDblClick := ComponentDblClick;
  Result.PopupMenu := PopupComp;
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

  AddComponent(0).SetBounds(50, 50, 100, 100);
end;

procedure TFoPrinc.AcOpenAccept(Sender: TObject);
begin
  Open(AcOpen.Dialog.FileName);

  // if EhArquivo then
  // if UpperCase(ExtractFileExt(AcAbrir.Dialog.FileName))='.CSV' then
  // CriaSQLProduto
  // else
  // visualizaPanel;
end;

procedure TFoPrinc.AcSaveBeforeExecute(Sender: TObject);
begin
  if Assigned(ActiveMDIChild) then
    AcSave.Dialog.InitialDir := ExtractFileDir(ActiveMDIChild.Caption);
end;

procedure TFoPrinc.AcSaveAccept(Sender: TObject);
begin
  { if Assigned(ActiveMDIChild) then
    with ActiveMDIChild as TFoMdiChild do
    begin
    Caption := AcSalvar.Dialog.FileName;
    Salvar;
    end; }
  FileMain.SaveToFile(AcSave.Dialog.FileName);
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

procedure TFoPrinc.FormDragDrop(Sender, Source: TObject; X, Y: Integer);

  procedure AddComponentPos(const AType: Byte);
  const
    COMP_ETL_WIDTH = 100;
    COMP_ETL_HEIGHT = 85;
  begin
    AddComponent(AType).SetBounds(X, Y, COMP_ETL_WIDTH, COMP_ETL_HEIGHT)
  end;

begin
  TImage(Source).EndDrag(True);
  if Source = ImQuery then
    AddComponentPos(TIPO_COMPONENT_QUERY)
  else if Source = ImFile then
    AddComponentPos(TIPO_COMPONENT_FILE)
  else if Source = ImFilter then
    AddComponentPos(TIPO_COMPONENT_FILTER)
  else if Source = ImConversion then
    AddComponentPos(TIPO_COMPONENT_CONVERSION)
  else if Source = ImDerivation then
    AddComponentPos(TIPO_COMPONENT_DERIVATION)
  else if Source = ImJoin then
    AddComponentPos(TIPO_COMPONENT_JOIN)
  else if Source = ImCondensation then
    AddComponentPos(TIPO_COMPONENT_CONDENSATION)
  else if Source = ImExecute then
    AddComponentPos(TIPO_COMPONENT_EXECUTE)
  else if Source = ImScript then
    AddComponentPos(TIPO_COMPONENT_SCRIPT)
end;

procedure TFoPrinc.FormDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState;
  var Accept: Boolean);
begin
  Accept := Source is TImage;
end;

procedure TFoPrinc.ComponentMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if (Button = mbLeft) then
    if ssShift in Shift then
    begin
      TComponentETL(Sender).BeginDrag(false);
    end
    else
    begin
      moveX := X;
      moveY := Y;
      mover := True;
    end;
end;

procedure TFoPrinc.ComponentDblClick(Sender: TObject);
begin
  TComponentETL(Sender).Edit;
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

procedure TFoPrinc.ComponentDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState;
  var Accept: Boolean);
begin
  Accept := (Source is TComponentETL) and (Sender <> Source);
end;

procedure TFoPrinc.ComponentMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
const
  MIN_LEFT = 120;
  MIN_TOP = 60;
begin
  if mover then
    with Sender as TControl do
    begin
      X := X - moveX + Left;
      Y := Y - moveY + Top;

      if X < MIN_LEFT then
        X := MIN_LEFT;
      if Y < MIN_TOP then
        Y := MIN_TOP;

      if X > Parent.Width - Width then
        X := Parent.Width - Width;
      if Y > Parent.Height - Height - MIN_TOP then
        Y := Parent.Height - Height - MIN_TOP;

      Left := X;
      Top := Y;
    end;
end;

procedure TFoPrinc.ComponentMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if mover then
    mover := false;
end;

procedure TFoPrinc.ComponentPaint(Sender: TObject);
begin
  DmImages.ILDev64.Draw(TPaintBox(Sender).Canvas, 0, 0, TComponentETL(Sender).Tag);
  TComponentETL(Sender).Canvas.TextOut(0, 64, TComponentETL(Sender).Title);
end;

procedure TFoPrinc.ComponentResize(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to ControlCount - 1 do
    if Controls[i] is TLinkComponents then
    begin
      if (Sender = TLinkComponents(Controls[i]).Source) or
        (Sender = TLinkComponents(Controls[i]).Target) then
        TLinkComponents(Controls[i]).RefreshSize;
    end;
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
