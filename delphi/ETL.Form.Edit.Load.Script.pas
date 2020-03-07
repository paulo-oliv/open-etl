unit ETL.Form.Edit.Load.Script;

interface

uses ETL.Form.Edit, Vcl.StdCtrls, Vcl.Samples.Spin, Vcl.Controls, Vcl.ExtCtrls, System.Classes,
  System.Actions, Vcl.ActnList, Vcl.ComCtrls, cxGraphics, cxControls, cxLookAndFeels,
  cxLookAndFeelPainters, cxStyles, cxEdit, cxDropDownEdit, cxVGrid, cxInplaceContainer, cxTextEdit;

const
  SEPARATE_SCRIPT_CHAR = '|';
  SEPARATE_SCRIPT_FIELDS_CHAR = ',';

type
  TFoEditLoadScript = class(TFoEdit)
    PageControl1: TPageControl;
    TsSettings: TTabSheet;
    Label2: TLabel;
    CbCommit: TCheckBox;
    CbDisableFK: TCheckBox;
    CbUse: TCheckBox;
    RgCommand: TRadioGroup;
    EdBlock: TSpinEdit;
    TsNames: TTabSheet;
    EdSchema: TEdit;
    Label1: TLabel;
    Gr: TcxVerticalGrid;
    GrEditorRow1: TcxEditorRow;
  strict private
  public
    procedure AddField(const AField: string);
    function ToString: string; override;
    procedure SetValue(const ARow: Integer; const AValue: string);
    function getValue(const ARow: Integer): Variant;
    class function New(const AOwner: TComponent): TFoEditLoadScript;
  end;

implementation

{$R *.dfm}

uses SysUtils, Variants;

{ TFoEditLoadScript }

function TFoEditLoadScript.getValue(const ARow: Integer): Variant;
begin
  Result := TcxEditorRow(Gr.Rows.Items[ARow]).Properties.Value;
end;

class function TFoEditLoadScript.New(const AOwner: TComponent): TFoEditLoadScript;
begin
  Result := TFoEditLoadScript.Create(AOwner);
end;

procedure TFoEditLoadScript.SetValue(const ARow: Integer; const AValue: string);
begin
  TcxEditorRow(Gr.Rows.Items[ARow]).Properties.Value := AValue;
end;

procedure TFoEditLoadScript.AddField(const AField: string);
begin
  with Gr.Add(TcxEditorRow) as TcxEditorRow do
  begin
    Properties.EditPropertiesClass := TcxTextEditProperties;
    Properties.Caption := AField;
    Properties.Value := AField;
  end;
end;

function TFoEditLoadScript.ToString: string;
var
  i: Integer;
begin
  Result := IntToStr(RgCommand.ItemIndex) + SEPARATE_SCRIPT_CHAR;

  if CbDisableFK.Checked then
    Result := Result + '1';
  Result := Result + SEPARATE_SCRIPT_CHAR;

  if CbUse.Checked then
    Result := Result + '1';
  Result := Result + SEPARATE_SCRIPT_CHAR;

  Result := Result + EdSchema.Text + SEPARATE_SCRIPT_CHAR;

  if CbCommit.Checked then
    Result := Result + '1';
  Result := Result + SEPARATE_SCRIPT_CHAR;

  Result := Result + EdBlock.Text + SEPARATE_SCRIPT_CHAR;

  for i := 0 to Gr.Rows.Count - 1 do
    Result := Result + VarToStr(TcxEditorRow(Gr.Rows.Items[i]).Properties.Value) +
      SEPARATE_SCRIPT_FIELDS_CHAR;
end;

end.
