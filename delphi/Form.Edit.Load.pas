unit Form.Edit.Load;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Form.Edit, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Samples.Spin;

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
