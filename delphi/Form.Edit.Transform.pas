unit Form.Edit.Transform;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Form.Edit, Vcl.StdCtrls;

type
  TFoEditTransform = class(TFoEdit)
    Memo1: TMemo;
  private
  public
    class function New(const AOwner: TComponent): TFoEditTransform;
  end;

implementation

{$R *.dfm}
{ TFoEditTransform }

class function TFoEditTransform.New(const AOwner: TComponent): TFoEditTransform;
begin
  Result := TFoEditTransform.Create(AOwner);
end;

end.
