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
    ActionList1: TActionList;
    AcInvert: TAction;
    Button1: TButton;
    PopupMenu1: TPopupMenu;
    AcEdit: TAction;
    AcDelete: TAction;
    AcNew: TAction;
    New1: TMenuItem;
    Edit1: TMenuItem;
    N1: TMenuItem;
    Delete1: TMenuItem;
    procedure AcInvertExecute(Sender: TObject);
    procedure AcNewExecute(Sender: TObject);
  private
    // FAlterado: Boolean;
  public
    class function New(const AOwner: TComponent): TFoEditQuery;
  end;

implementation

{$R *.dfm}

uses SectionConexao, FireDAC.VCLUI.ConnEdit; // uMsg

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
//    FDConnection1.ResultConnectionDef.ParseString(sConnStr);
//    FDConnection1.Connected := True;
  end;
end;

class function TFoEditQuery.New(const AOwner: TComponent): TFoEditQuery;
begin
  Result := TFoEditQuery.Create(AOwner);
  TConnectionDefsIni.GetInstance.ReadSections(Result.ClConexoes.Items);
end;

end.
