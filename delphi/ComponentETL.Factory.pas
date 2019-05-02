unit ComponentETL.Factory;

interface

uses ComponentETL,
  Form.Edit.Query,
  Form.Edit.Transform,
  Form.Edit.Load,
  Vcl.Controls;

const
  TIPO_COMPONENT_QUERY = 0;
  TIPO_COMPONENT_FILE = 1;
  TIPO_COMPONENT_FILTER = 2;
  TIPO_COMPONENT_CONVERSION = 3;
  TIPO_COMPONENT_DERIVATION = 4;
  TIPO_COMPONENT_JOIN = 5;
  TIPO_COMPONENT_CONDENSATION = 6;
  TIPO_COMPONENT_EXECUTE = 7;
  TIPO_COMPONENT_SCRIPT = 8;

type
  TCompExtract = class(TComponentETL)
  end;

  TCompTransform = class(TComponentETL)
  strict protected
    FFormEdit: TFoEditTransform;
  public
    procedure Edit; override;
  end;

  TCompLoad = class(TComponentETL)
  strict protected
    FFormEdit: TFoEditLoad;
  public
    procedure Edit; override;
  end;

  TCompQuery = class(TCompExtract)
  strict private
    FFormEdit: TFoEditQuery;
  strict protected
    procedure configQuery; override;
  public
    procedure Edit; override;
  end;

  TCompFile = class(TCompExtract)
  end;

  TCompFilter = class(TCompTransform)
  end;

  TCompConversion = class(TCompTransform)
  end;

  TCompDerivation = class(TCompTransform)
  end;

  TCompJoin = class(TCompTransform)
  end;

  TComCondensation = class(TCompTransform)
  end;

  TCompExecute = class(TCompLoad)
  end;

  TCompScript = class(TCompLoad)
  end;

  TComponentETLFactory = class
  public
    class function New(const AParent: TWinControl; const AType: Byte): TComponentETL;
  end;

implementation

{ TCompQuery }

procedure TCompQuery.configQuery;
begin
  if Assigned(FFormEdit) then
  begin

  end;
end;

procedure TCompQuery.Edit;
begin
  if not Assigned(FFormEdit) then
    FFormEdit := TFoEditQuery.New(Self);
  FFormEdit.ShowModal;
end;

{ TCompTransform }

procedure TCompTransform.Edit;
begin
  if not Assigned(FFormEdit) then
    FFormEdit := TFoEditTransform.New(Self);
  FFormEdit.ShowModal;
end;

{ TCompLoad }

procedure TCompLoad.Edit;
begin
  if not Assigned(FFormEdit) then
    FFormEdit := TFoEditLoad.New(Self);
  FFormEdit.ShowModal;
end;

{ TComponentETLFactory }

class function TComponentETLFactory.New(const AParent: TWinControl; const AType: Byte)
  : TComponentETL;
begin
  case AType of
    TIPO_COMPONENT_QUERY:
      begin
        Result := TCompQuery.Create(AParent, AParent);
        Result.Title := 'Query';
      end;
    TIPO_COMPONENT_FILE:
      begin
        Result := TCompFile.Create(AParent, AParent);
        Result.Title := 'File';
      end;
    TIPO_COMPONENT_FILTER:
      begin
        Result := TCompFilter.Create(AParent, AParent);
        Result.Title := 'Filter';
      end;
    TIPO_COMPONENT_CONVERSION:
      begin
        Result := TCompConversion.Create(AParent, AParent);
        Result.Title := 'Conversion';
      end;
    TIPO_COMPONENT_DERIVATION:
      begin
        Result := TCompDerivation.Create(AParent, AParent);
        Result.Title := 'Derivation';
      end;
    TIPO_COMPONENT_JOIN:
      begin
        Result := TCompJoin.Create(AParent, AParent);
        Result.Title := 'Join';
      end;
    TIPO_COMPONENT_CONDENSATION:
      begin
        Result := TCompConversion.Create(AParent, AParent);
        Result.Title := 'Condensation';
      end;
    TIPO_COMPONENT_EXECUTE:
      begin
        Result := TCompExecute.Create(AParent, AParent);
        Result.Title := 'Execute';
      end;
  else
    // TIPO_COMPONENT_SCRIPT:
    begin
      Result := TCompScript.Create(AParent, AParent);
      Result.Title := 'Script';
    end;
  end;
  Result.Tag := AType;
end;

end.
