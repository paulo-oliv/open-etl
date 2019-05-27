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
    procedure MnDelLinkClick(Sender: TObject);
    procedure MnEditLabelClick(Sender: TObject);
    procedure AcEditScriptExecute(Sender: TObject);
    procedure AcPreviewExecute(Sender: TObject);
    procedure AcEditTitleExecute(Sender: TObject);
    procedure AcDeleteExecute(Sender: TObject);
  private
  public
  end;

var
  DmMain: TDmMain;

implementation

{ %CLASSGROUP 'Vcl.Controls.TControl' }

{$R *.dfm}

uses uMsg, ETL.Component;

procedure TDmMain.AcDeleteExecute(Sender: TObject);
// var
// i: Integer;
begin
  // for i := ControlCount - 1 downto 0 do
  // if Controls[i] is TLinkComponents then
  // begin
  // if (PopupComp.PopupComponent = TLinkComponents(Controls[i]).Source) or
  // (PopupComp.PopupComponent = TLinkComponents(Controls[i]).Target) then
  // Controls[i].DisposeOf;
  // end;

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
begin
  TComponentETL(PopupComp.PopupComponent).Preview;
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
