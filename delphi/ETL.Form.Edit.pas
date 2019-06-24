unit ETL.Form.Edit;

interface

uses Vcl.Controls, Vcl.StdCtrls, Vcl.CheckLst, System.Classes, Vcl.Forms, cxGraphics, cxControls,
  cxLookAndFeels, cxLookAndFeelPainters, cxSplitter, upControls, System.Actions, Vcl.ActnList;

type
  TFoEdit = class(TForm)
    ActionList: TActionList;
    AcCloseWindow: TAction;
    procedure AcCloseWindowExecute(Sender: TObject);
  strict private
    FOnChange: TNotifyEvent;
  strict protected
    procedure DoChange;
  public
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    // procedure SetTitle(const ATitle: string);
  end;

implementation

{$R *.dfm}

procedure TFoEdit.AcCloseWindowExecute(Sender: TObject);
begin
  Close;
end;

{
  procedure TFoEdit.SetTitle(const ATitle: string);
  begin
  Caption := ATitle;
  end; }

procedure TFoEdit.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

end.
