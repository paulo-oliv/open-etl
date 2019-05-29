unit ETL.Form.Edit.Transform.Condensation;

interface

uses ETL.Form.Edit.Transform, Vcl.Controls, Vcl.StdCtrls, Vcl.CheckLst, Vcl.ExtCtrls,
  System.Classes, System.Actions, Vcl.ActnList, cxGraphics, cxControls, cxLookAndFeels,
  cxLookAndFeelPainters, cxStyles, cxEdit, cxInplaceContainer, cxVGrid, cxDropDownEdit;

type
  TFoEditCondensation = class(TFoEditTransform)
    Gr: TcxVerticalGrid;
    GrEditorRow1: TcxEditorRow;
  public
    procedure SetValue(const ARow: Integer; const AValue: string);
    procedure AddField(const AField: string);
    function ToString: string; override;
    class function New(const AOwner: TComponent): TFoEditCondensation;
  end;

const
  SEPARATE_CONDENSATION_CHAR = '|';

implementation

{$R *.dfm}

uses Variants;

{ TFoEditCondensation }

class function TFoEditCondensation.New(const AOwner: TComponent): TFoEditCondensation;
begin
  Result := TFoEditCondensation.Create(AOwner);
  Result.Gr.ClearRows;
end;

procedure TFoEditCondensation.SetValue(const ARow: Integer; const AValue: string);
begin
  TcxEditorRow(Gr.Rows.Items[ARow]).Properties.Value := AValue;
end;

procedure TFoEditCondensation.AddField(const AField: string);
begin
  with Gr.Add(TcxEditorRow) as TcxEditorRow do
  begin
    Properties.EditPropertiesClass := TcxComboBoxProperties;
    Properties.Caption := AField;
    with TcxComboBoxProperties(Properties.EditProperties) do
    begin
      DropDownListStyle := lsEditFixedList;
      Items.Add('Null');
      Items.Add('GROUP BY');
      Items.Add('Sum');
      Items.Add('Average');
      Items.Add('Max');
      Items.Add('Min');
      Items.Add('Count');
      Items.Add('First');
      Items.Add('Last');
      Items.Add('Best');
    end;
  end;
end;

function TFoEditCondensation.ToString: string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to Gr.Rows.Count - 1 do
    Result := Result + VarToStr(TcxEditorRow(Gr.Rows.Items[i]).Properties.Value) +
      SEPARATE_CONDENSATION_CHAR;
end;

end.
