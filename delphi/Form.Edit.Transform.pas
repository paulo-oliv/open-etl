unit Form.Edit.Transform;

interface

uses Form.Edit, System.Classes, System.Actions, Vcl.ActnList;

type
  TFoEditTransform = class(TFoEdit)
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
