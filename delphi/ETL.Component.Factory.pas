unit ETL.Component.Factory;

interface

uses
  ETL.Component,
  Vcl.Controls;

const
  KIND_COMPONENT_QUERY = 0;
  KIND_COMPONENT_FILE = 1;
  KIND_COMPONENT_FILTER = 2;
  KIND_COMPONENT_CONVERSION = 3;
  KIND_COMPONENT_DERIVATION = 4;
  KIND_COMPONENT_JOIN = 5;
  KIND_COMPONENT_CONDENSATION = 6;
  KIND_COMPONENT_EXECUTE = 7;
  KIND_COMPONENT_SCRIPT = 8;

type
  TComponentETLFactory = class
  public
    class function New(const AParent: TWinControl; const AKind: Byte; const AGUID: string)
      : TComponentETL;
  end;

implementation

uses ETL.Component.Extract, ETL.Component.Transform, ETL.Component.Transform.Condensation,
  ETL.Component.Load, ETL.Component.Load.Script;

{ TComponentETLFactory }

class function TComponentETLFactory.New(const AParent: TWinControl; const AKind: Byte;
  const AGUID: string): TComponentETL;
begin
  case AKind of
    KIND_COMPONENT_QUERY:
      Result := TCompQuery.Create(AParent, AGUID);
    KIND_COMPONENT_FILE:
      Result := TCompFile.Create(AParent, AGUID);
    KIND_COMPONENT_FILTER:
      Result := TCompFilter.Create(AParent, AGUID);
    KIND_COMPONENT_CONVERSION:
      Result := TCompConversion.Create(AParent, AGUID);
    KIND_COMPONENT_DERIVATION:
      Result := TCompDerivation.Create(AParent, AGUID);
    KIND_COMPONENT_JOIN:
      Result := TCompJoin.Create(AParent, AGUID);
    KIND_COMPONENT_CONDENSATION:
      Result := TCompCondensation.Create(AParent, AGUID);
    KIND_COMPONENT_EXECUTE:
      Result := TCompExecute.Create(AParent, AGUID);
  else
    // KIND_COMPONENT_SCRIPT:
    Result := TCompScript.Create(AParent, AGUID);
  end;
  Result.Tag := AKind;
end;

end.
