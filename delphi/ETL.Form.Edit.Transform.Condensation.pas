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
    function GetValue(const ARow: Integer): string;
    procedure AddField(const AField: string);
    function ToString: string; override;
    class function New(const AOwner: TComponent): TFoEditCondensation;
  end;

const
  SEPARATE_CONDENSATION_CHAR = '|';

  CONDENSATION_NULL = 'Null';
  CONDENSATION_GROUP_BY = 'GROUP BY';
  CONDENSATION_SUM = 'Sum';
  CONDENSATION_AVERAGE = 'Average';
  CONDENSATION_MAX = 'Max';
  CONDENSATION_MIN = 'Min';
  CONDENSATION_COUNT = 'Count';
  CONDENSATION_FIRST = 'First';
  CONDENSATION_LAST = 'Last';
  CONDENSATION_BEST = 'Best';

implementation

{$R *.dfm}

uses Variants;

{ TFoEditCondensation }

function TFoEditCondensation.GetValue(const ARow: Integer): string;
begin
  Result := VarToStr(TcxEditorRow(Gr.Rows.Items[ARow]).Properties.Value);
end;

class function TFoEditCondensation.New(const AOwner: TComponent): TFoEditCondensation;
begin
  Result := TFoEditCondensation.Create(AOwner);
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
      Items.Add(CONDENSATION_NULL);
      Items.Add(CONDENSATION_GROUP_BY);
      Items.Add(CONDENSATION_SUM);
      Items.Add(CONDENSATION_AVERAGE);
      Items.Add(CONDENSATION_MAX);
      Items.Add(CONDENSATION_MIN);
      Items.Add(CONDENSATION_COUNT);
      Items.Add(CONDENSATION_FIRST);
      Items.Add(CONDENSATION_LAST);
      Items.Add(CONDENSATION_BEST);
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
