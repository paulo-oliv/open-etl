unit ETL.DataModule.Main;

interface

uses Vcl.Controls, System.Classes, System.ImageList, Vcl.ImgList, cxGraphics, Vcl.Menus,
  System.Actions, Vcl.ActnList;

type
  TDmMain = class(TDataModule)
    IL32: TcxImageList;
    IL64: TcxImageList;
    ILOld16: TImageList;
    PopupLink: TPopupMenu;
    MnEditLabel: TMenuItem;
    MenuItem5: TMenuItem;
    MnDelLink: TMenuItem;
    AlComp: TActionList;
    AcEditScript: TAction;
    AcPreview: TAction;
    AcEditTitle: TAction;
    PopupComp: TPopupMenu;
    MnEdit: TMenuItem;
    MnPreview: TMenuItem;
    N2: TMenuItem;
    MenuItem1: TMenuItem;
    N1: TMenuItem;
    MnDeleteComponent: TMenuItem;
    AcDelete: TAction;
    IL16: TcxImageList;
    AcRefresh: TAction;
    Refresh1: TMenuItem;
    procedure MnDelLinkClick(Sender: TObject);
    procedure MnEditLabelClick(Sender: TObject);
    procedure AcEditScriptExecute(Sender: TObject);
    procedure AcPreviewExecute(Sender: TObject);
    procedure AcEditTitleExecute(Sender: TObject);
    procedure AcDeleteExecute(Sender: TObject);
    procedure AcRefreshExecute(Sender: TObject);
  private
  public
  end;

var
  DmMain: TDmMain;

implementation

{ %CLASSGROUP 'Vcl.Controls.TControl' }

{$R *.dfm}

uses uMsg, ETL.Component, ETL.Link;

procedure TDmMain.AcDeleteExecute(Sender: TObject);
begin
  TComponentETL(PopupComp.PopupComponent).Delete
end;

procedure TDmMain.AcEditScriptExecute(Sender: TObject);
begin
  TComponentETL(PopupComp.PopupComponent).Edit;
end;

procedure TDmMain.AcEditTitleExecute(Sender: TObject);
var
  s: string;
begin
  s := TComponentETL(PopupComp.PopupComponent).Title;
  if TMensagem.InputQuery('Edit Label', s) then
    TComponentETL(PopupComp.PopupComponent).Title := s;
end;

procedure TDmMain.AcPreviewExecute(Sender: TObject);
var
  LComponentETL: TComponentETL;
begin
  LComponentETL := TComponentETL(PopupComp.PopupComponent);
  if LComponentETL.Updated then
    if TMensagem.Confirmacao('Do you want to update the data?') then
      // if TMensagem.Confirmacao('Deseja atualizar os dados?') then
      LComponentETL.RefreshPreviewForm;

  LComponentETL.Preview;
end;

procedure TDmMain.AcRefreshExecute(Sender: TObject);
begin
  TComponentETL(PopupComp.PopupComponent).RefreshPreviewForm;
end;

procedure TDmMain.MnDelLinkClick(Sender: TObject);
begin
  PopupLink.PopupComponent.DisposeOf;
end;

procedure TDmMain.MnEditLabelClick(Sender: TObject);
var
  s: string;
begin
  s := TLinkComponents(PopupLink.PopupComponent).Text;
  if TMensagem.InputQuery('Edit Label', s) then
    TLinkComponents(PopupLink.PopupComponent).Text := s;
end;

end.
