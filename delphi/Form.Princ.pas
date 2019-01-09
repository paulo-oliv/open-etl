unit Form.Princ;

interface

uses dxRibbonSkins, dxRibbonCustomizationForm, cxGraphics, cxControls, cxLookAndFeels,
  cxLookAndFeelPainters, FireDAC.UI.Intf, FireDAC.Stan.Async, FireDAC.Comp.ScriptCommands,
  FireDAC.Stan.Util, FireDAC.VCLUI.Script, Vcl.Menus, Vcl.StdActns, Vcl.ActnList, System.Classes,
  System.Actions, dxBar, dxBarApplicationMenu, dxRibbon, cxClasses, FireDAC.Comp.UI,
  FireDAC.Stan.Intf, FireDAC.Comp.Script, dxStatusBar, dxRibbonStatusBar, dxGDIPlusClasses,
  Vcl.Controls, Vcl.ExtCtrls, Vcl.Forms, Form.Grid, Form.Edit.Query, Form.Edit.Transform,
  Form.Edit.Load;

type
  TComponentETL = class(TPaintBox)
  strict private
    FFormGrid: TFoGrid;
    FTitle: string;
  strict protected
    procedure configQuery; virtual;
    function getTitle: string;
    procedure setTitle(const ATitle: string);
  public
    procedure Edit; virtual; abstract;
    procedure Preview;
    procedure Delete;
  published
    property Title: string read getTitle write setTitle;
    // constructor Create(AOwner: TComponent); override;
  end;

  TLinkComponents = class(TPaintBox)
  strict private
    FText: string;
    FSource, FTarget: TComponentETL;
  strict protected
    procedure Paint; override;
  public
    property Text: string read FText write FText;
    property Source: TComponentETL read FSource write FSource;
    property Target: TComponentETL read FTarget write FTarget;
    procedure RefreshSize;
  end;

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
    FFormEdit: TFoEditQuery;
  strict protected
    procedure configQuery; override;
  public
    procedure Edit; override;
  end;

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
    dxBarManager1: TdxBarManager;
    dxBarManager1Bar1: TdxBar;
    dxBarApplicationMenu1: TdxBarApplicationMenu;
    dxRibbon1: TdxRibbon;
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
    dxBarButton1: TdxBarButton;
    dxBarButton2: TdxBarButton;
    dxBarButton3: TdxBarButton;
    dxBarButton4: TdxBarButton;
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
    function AddComponent(const ATipo: Byte): TComponentETL;
  end;

var
  FoPrinc: TFoPrinc;
  moveX, moveY: Integer;
  mover: Boolean;

implementation

{$R *.dfm}

uses uMsg, Windows, SysUtils, uDmImages, Vcl.Graphics;

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

procedure TFoPrinc.AcNewExecute(Sender: TObject);
begin
  //
end;

procedure TFoPrinc.Open(const AFile: string);
begin
  if FileExists(AFile) then
  begin
    // if TFoSqlProdutos.EhSped(AFile) then
    // TFoSqlProdutos.Create(Self).Open(AFile)
    // else
    // TFoEditQuery.Create(Self).Open(AFile)
  end
  // else
  // TFoEditQuery.Create(Self).Novo(AFile)
end;

procedure TFoPrinc.AcOpenBeforeExecute(Sender: TObject);
begin
  if Assigned(ActiveMDIChild) then // abre dialog no mesmo diretorio do arquivo ja aberto
    AcOpen.Dialog.InitialDir := ExtractFileDir(ActiveMDIChild.Caption);
end;

function TFoPrinc.AddComponent(const ATipo: Byte): TComponentETL;
var
  LPrefix: string;

  function GenerateTitle: string;

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
      Result := LPrefix + IntToStr(LNum);
    until NovoTitle(Result);
  end;

begin
  case ATipo of
    TIPO_COMPONENT_QUERY:
      begin
        Result := TCompQuery.Create(Self);
        LPrefix := 'Query';
      end;
    TIPO_COMPONENT_FILE:
      begin
        Result := TCompFile.Create(Self);
        LPrefix := 'File';
      end;
    TIPO_COMPONENT_FILTER:
      begin
        Result := TCompFilter.Create(Self);
        LPrefix := 'Filter';
      end;
    TIPO_COMPONENT_CONVERSION:
      begin
        Result := TCompConversion.Create(Self);
        LPrefix := 'Conversion';
      end;
    TIPO_COMPONENT_DERIVATION:
      begin
        Result := TCompDerivation.Create(Self);
        LPrefix := 'Derivation';
      end;
    TIPO_COMPONENT_JOIN:
      begin
        Result := TCompJoin.Create(Self);
        LPrefix := 'Join';
      end;
    TIPO_COMPONENT_CONDENSATION:
      begin
        Result := TCompConversion.Create(Self);
        LPrefix := 'Condensation';
      end;
    TIPO_COMPONENT_EXECUTE:
      begin
        Result := TCompExecute.Create(Self);
        LPrefix := 'Execute';
      end;
    TIPO_COMPONENT_SCRIPT:
      begin
        Result := TCompScript.Create(Self);
        LPrefix := 'Script';
      end;
  end;

  Result.Title := GenerateTitle;
  Result.Parent := Self;
  Result.OnPaint := ComponentPaint;
  Result.OnMouseMove := ComponentMouseMove;
  Result.OnMouseUp := ComponentMouseUp;
  Result.OnMouseDown := ComponentMouseDown;
  Result.OnResize := ComponentResize;
  Result.OnDragDrop := ComponentDragDrop;
  Result.OnDragOver := ComponentDragOver;
  Result.OnDblClick := ComponentDblClick;
  Result.Tag := ATipo;
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
const
  COMP_ETL_WIDTH = 100;
  COMP_ETL_HEIGHT = 85;
begin
  TImage(Source).EndDrag(True);
  if Source = ImQuery then
    AddComponent(TIPO_COMPONENT_QUERY).SetBounds(X, Y, COMP_ETL_WIDTH, COMP_ETL_HEIGHT)
  else if Source = ImFile then
    AddComponent(TIPO_COMPONENT_FILE).SetBounds(X, Y, COMP_ETL_WIDTH, COMP_ETL_HEIGHT)
  else if Source = ImFilter then
    AddComponent(TIPO_COMPONENT_FILTER).SetBounds(X, Y, COMP_ETL_WIDTH, COMP_ETL_HEIGHT)
  else if Source = ImConversion then
    AddComponent(TIPO_COMPONENT_CONVERSION).SetBounds(X, Y, COMP_ETL_WIDTH, COMP_ETL_HEIGHT)
  else if Source = ImDerivation then
    AddComponent(TIPO_COMPONENT_DERIVATION).SetBounds(X, Y, COMP_ETL_WIDTH, COMP_ETL_HEIGHT)
  else if Source = ImJoin then
    AddComponent(TIPO_COMPONENT_JOIN).SetBounds(X, Y, COMP_ETL_WIDTH, COMP_ETL_HEIGHT)
  else if Source = ImCondensation then
    AddComponent(TIPO_COMPONENT_CONDENSATION).SetBounds(X, Y, COMP_ETL_WIDTH, COMP_ETL_HEIGHT)
  else if Source = ImExecute then
    AddComponent(TIPO_COMPONENT_EXECUTE).SetBounds(X, Y, COMP_ETL_WIDTH, COMP_ETL_HEIGHT)
  else if Source = ImScript then
    AddComponent(TIPO_COMPONENT_SCRIPT).SetBounds(X, Y, COMP_ETL_WIDTH, COMP_ETL_HEIGHT)
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
  minWidth = 120;
  minHeight = 60;
begin
  if mover then
    with Sender as TControl do
    begin
      X := X - moveX + Left;
      Y := Y - moveY + Top;

      if X < minWidth then
        X := minWidth;
      if Y < minHeight then
        Y := minHeight;

      if X > Parent.Width - Width then
        X := Parent.Width - Width;
      if Y > Parent.Height - Height - minHeight then
        Y := Parent.Height - Height - minHeight;

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

{ TComponenteETL }

procedure TComponentETL.configQuery;
begin
  //
end;

procedure TComponentETL.Delete;
begin
  DisposeOf;
end;

function TComponentETL.getTitle: string;
begin
  Result := FTitle;
end;

procedure TComponentETL.setTitle(const ATitle: string);
begin
  FTitle := ATitle;
  Repaint;
end;

procedure TComponentETL.Preview;
begin
  if not Assigned(FFormGrid) then
    FFormGrid := TFoGrid.New(Self);

  configQuery;

  FFormGrid.ShowModal;
end;

{ TCompQuery }

procedure TCompQuery.configQuery;
begin
  if Assigned(FFormEdit) then
  begin

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

{ TLinkComponents }

procedure TLinkComponents.Paint;

  procedure DesenharSeta(Origem, Destino: TPoint);
  const
    ANGULO = 30;
    PONTA = 25;
  var
    AlphaRota, Alpha, Beta: Extended;
    vertice1, vertice2, vertice3: TPoint;
  begin
    Canvas.MoveTo(Origem.X, Origem.Y);
    Canvas.LineTo(Destino.X, Destino.Y);

    if (Destino.X >= Origem.X) then
    begin
      if (Destino.Y >= Origem.Y) then
      begin
        AlphaRota := Destino.X - Origem.X;
        if (AlphaRota <> 0) then
          Alpha := ArcTan((Destino.Y - Origem.Y) / AlphaRota)
        else
          Alpha := ArcTan(Destino.Y - Origem.Y);
        Beta := (ANGULO * (PI / 180)) / 2;
        vertice1.X := Destino.X - Round(Cos(Alpha + Beta));
        vertice1.Y := Destino.Y - Round(Sin(Alpha - Beta));
        vertice2.X := Round(vertice1.X - PONTA * Cos(Alpha + Beta));
        vertice2.Y := Round(vertice1.Y - PONTA * Sin(Alpha + Beta));
        vertice3.X := Round(vertice1.X - PONTA * Cos(Alpha - Beta));
        vertice3.Y := Round(vertice1.Y - PONTA * Sin(Alpha - Beta));
        Self.Canvas.Polygon([vertice1, vertice2, vertice3]);
      end
      else
      begin
        AlphaRota := Destino.Y - Origem.Y;
        if (AlphaRota <> 0) then
          Alpha := ArcTan((Destino.X - Origem.X) / AlphaRota)
        else
          Alpha := ArcTan(Destino.X - Origem.X);
        Beta := (ANGULO * (PI / 180)) / 2;
        vertice1.X := Destino.X - Round(Cos(Alpha + Beta));
        vertice1.Y := Destino.Y - Round(Sin(Alpha - Beta));
        vertice2.X := Round(vertice1.X + PONTA * Sin(Alpha + Beta));
        vertice2.Y := Round(vertice1.Y + PONTA * Cos(Alpha + Beta));
        vertice3.X := Round(vertice1.X + PONTA * Sin(Alpha - Beta));
        vertice3.Y := Round(vertice1.Y + PONTA * Cos(Alpha - Beta));
        Self.Canvas.Polygon([vertice1, vertice2, vertice3]);
      end;
    end
    else
    begin
      Alpha := ArcTan((Destino.Y - Origem.Y) / (Destino.X - Origem.X));
      Beta := (ANGULO * (PI / 180)) / 2;
      vertice1.X := Destino.X - Round(Cos(Alpha + Beta));
      vertice1.Y := Destino.Y - Round(Sin(Alpha - Beta));
      vertice2.X := Round(vertice1.X + PONTA * Cos(Alpha + Beta));
      vertice2.Y := Round(vertice1.Y + PONTA * Sin(Alpha + Beta));
      vertice3.X := Round(vertice1.X + PONTA * Cos(Alpha - Beta));
      vertice3.Y := Round(vertice1.Y + PONTA * Sin(Alpha - Beta));
      Self.Canvas.Polygon([vertice1, vertice2, vertice3]);
    end;
  end;

const
  POSICAO_MIN = 8;
begin
  Canvas.Pen.Width := 2;
  Canvas.Pen.Color := clGray;
  if Tag = 0 then
    DesenharSeta(Point(0, Height), Point(Width - POSICAO_MIN, POSICAO_MIN))
  else if Tag = 1 then
    DesenharSeta(Point(Width, Height - POSICAO_MIN), Point(POSICAO_MIN, POSICAO_MIN))
  else if Tag = 2 then
    DesenharSeta(Point(0, 0), Point(Width - POSICAO_MIN, Height - POSICAO_MIN))
  else
    DesenharSeta(Point(Width, 0), Point(POSICAO_MIN, Height - POSICAO_MIN));
  Canvas.Brush.Color := clWhite;
  Canvas.TextOut(Width div 2 - Canvas.TextWidth(FText) div 2, Height div 2, IntToStr(Tag));
end;

procedure TLinkComponents.RefreshSize;
var
  x1, x2, y1, y2, a: Integer;
  b: Boolean;
begin
  x1 := FSource.Left;
  x2 := FTarget.Left;
  if x1 < x2 then
  begin
    x2 := x2 - x1;
    b := false;
  end
  else
  begin
    a := x1;
    x1 := x2;
    x2 := a - x1;
    b := True;
  end;

  y1 := FSource.Top;
  y2 := FTarget.Top;
  if y1 < y2 then
  begin
    y2 := y2 - y1;
    if b then
      Tag := 0
    else
      Tag := 1
  end
  else
  begin
    a := y1;
    y1 := y2;
    y2 := a - y1;
    if b then
      Tag := 2
    else
      Tag := 3;
  end;
  x1 := x1 + 64;
  y1 := y1 + 64;
  x2 := x2 - 64;
  y2 := y2 - 64;
  if (x2 < 64) then
  begin
    a := (64 - x2) div 2;
    x1 := x1 - a;
    x2 := x2 + a;
  end;
  if (y2 < 64) then
  begin
    a := (64 - y2) div 2;
    y1 := y1 - a;
    y2 := y2 + a;
  end;
  SetBounds(x1, y1, x2, y2);
  Repaint;
end;

end.
