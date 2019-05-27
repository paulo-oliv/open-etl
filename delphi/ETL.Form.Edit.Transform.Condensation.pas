unit ETL.Form.Edit.Transform.Condensation;

interface

uses ETL.Form.Edit.Transform, Vcl.Controls, Vcl.StdCtrls, Vcl.CheckLst, Vcl.ExtCtrls,
  System.Classes, System.Actions, Vcl.ActnList;

type
  TFoEditCondensation = class(TFoEditTransform)
    RgKind: TRadioGroup;
    ClColumns: TCheckListBox;
  public
    class function New(const AOwner: TComponent): TFoEditCondensation;
  end;

implementation

{$R *.dfm}
{ TFoEditCondensation }

class function TFoEditCondensation.New(const AOwner: TComponent): TFoEditCondensation;
begin
  Result := TFoEditCondensation.Create(AOwner);
end;

end.
