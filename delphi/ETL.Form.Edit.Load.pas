unit ETL.Form.Edit.Load;

interface

uses ETL.Form.Edit, Vcl.StdCtrls, Vcl.Samples.Spin, Vcl.Controls, Vcl.ExtCtrls, System.Classes,
  System.Actions, Vcl.ActnList;

type
  TFoEditLoad = class(TFoEdit)
    EdSchema: TEdit;
    CbCommit: TCheckBox;
    CbDisableFK: TCheckBox;
    CbUse: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    EdFileName: TEdit;
    Label3: TLabel;
    RgDestination: TRadioGroup;
    RgCommand: TRadioGroup;
    LbConnection: TLabel;
    EdConnection: TEdit;
    EdBlock: TSpinEdit;
    Button1: TButton;
    Button2: TButton;
  private
  public
    class function New(const AOwner: TComponent): TFoEditLoad;
  end;


implementation

{$R *.dfm}

{ TFoEditLoad }

class function TFoEditLoad.New(const AOwner: TComponent): TFoEditLoad;
begin
  Result := TFoEditLoad.Create(AOwner);
end;

end.
