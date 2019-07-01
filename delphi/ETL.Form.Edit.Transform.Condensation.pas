unit ETL.Form.Edit.Transform.Condensation;

interface

uses ETL.Form.Edit.Transform, Vcl.Controls, Vcl.StdCtrls, Vcl.CheckLst, Vcl.ExtCtrls,
  System.Classes, System.Actions, Vcl.ActnList, cxGraphics, cxControls, cxLookAndFeels,
  cxLookAndFeelPainters, cxStyles, cxEdit, cxInplaceContainer, cxVGrid, cxDropDownEdit;

type
  TKindCondensation = (Null = 0, GroupBy, Sum, Average, Max, Min, Count, First, Last, Best);

  TFoEditCondensation = class(TFoEditTransform)
    Gr: TcxVerticalGrid;
    GrEditorRow1: TcxEditorRow;
    procedure GrEditValueChanged(Sender: TObject; ARowProperties: TcxCustomEditorRowProperties);
  public
    procedure SetValue(const ARow: Integer; const AValue: string);
    function GetKind(const ARow: Integer): TKindCondensation;
    function GetValue(const ARow: Integer): string;
    procedure AddField(const AField: string);
    function ToString: string; override;
    class function New(const AOwner: TComponent): TFoEditCondensation;
  end;

  TKindCondensationHelper = record helper for TKindCondensation
    function AsByte: byte;
    function ToString: string;
  end;

  TConversions<T> = class
  strict private
  public
    class function StringToEnumeration(x: String): T;
    class function EnumerationToString(x: T): String;
  end;

const
  SEPARATE_CONDENSATION_CHAR = '|';

  CONDENSATION_NULL = 'Null';
  CONDENSATION_GROUP_BY = 'GroupBy';
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

uses Variants, System.TypInfo;

{ TConversions }

class function TConversions<T>.EnumerationToString(x: T): String;
begin
  case Sizeof(T) of
    1: Result := GetEnumName(TypeInfo(T), PByte(@x)^);
    2: Result := GetEnumName(TypeInfo(T), PWord(@x)^);
    4: Result := GetEnumName(TypeInfo(T), PCardinal(@x)^);
  end;
end;

class function TConversions<T>.StringToEnumeration(x: String): T;
begin
  case Sizeof(T) of
    1: PByte(@Result)^ := GetEnumValue(TypeInfo(T), x);
    2: PWord(@Result)^ := GetEnumValue(TypeInfo(T), x);
    4: PCardinal(@Result)^ := GetEnumValue(TypeInfo(T), x);
  end;
end;

{ TFoEditCondensation }

function TFoEditCondensation.GetKind(const ARow: Integer): TKindCondensation;
begin
  Result := TConversions<TKindCondensation>.StringToEnumeration(GetValue(ARow));
end;

function TFoEditCondensation.GetValue(const ARow: Integer): string;
begin
  Result := VarToStr(TcxEditorRow(Gr.Rows.Items[ARow]).Properties.Value);
end;

procedure TFoEditCondensation.GrEditValueChanged(Sender: TObject;
  ARowProperties: TcxCustomEditorRowProperties);
begin
  DoChange;
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

{ TKindCondensationHelper }

function TKindCondensationHelper.AsByte: byte;
begin
  Result := byte(Self);
end;

function TKindCondensationHelper.ToString: string;
begin
  case Self of
    Null:
      Result := CONDENSATION_NULL;
    GroupBy:
      Result := CONDENSATION_GROUP_BY;
    Sum:
      Result := CONDENSATION_SUM;
    Average:
      Result := CONDENSATION_AVERAGE;
    Max:
      Result := CONDENSATION_MAX;
    Min:
      Result := CONDENSATION_MIN;
    Count:
      Result := CONDENSATION_COUNT;
    First:
      Result := CONDENSATION_FIRST;
    Last:
      Result := CONDENSATION_LAST;
    Best:
      Result := CONDENSATION_BEST;
  end;
end;

end.
