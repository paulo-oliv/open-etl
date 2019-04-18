unit FileProjectETL;

interface

uses Generics.Collections, Controls, FileProject.Interfaces;

type
  TInterfacedList<T> = class(TInterfacedObject, IList)
  strict protected
    FList: TList<T>;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TListComponentsETL = class(TInterfacedList<IComponentETL>, IListComponentsETL)
    function Add(const AComponent: IComponentETL): IListComponentsETL;
  end;

  TListLinks = class(TInterfacedList<ILinkComponents>, IListLinks)
  end;

  TProjectETL = class(TInterfacedObject, IProjectETL)
  strict private
    FListComponentsETL: IListComponentsETL;
    FListLinks: IListLinks;
    class var FInstance: IProjectETL;
  public
    function getListComponents: IListComponentsETL;
    class function GetInstance: IProjectETL;
    class function AddComponent(const AComponent: IComponentETL): IProjectETL;
    class function Load(const AFileName: string): Boolean;
    class function Save(const AFileName: string): Boolean;
    constructor Create;
    // procedure BeginSave;
    // procedure AddComponent(ADataComponentETL: IDataComponentETL);
    // procedure AddLink(ALink: IDataComponentETL);
    // procedure EndSave;
    // procedure LoadComponents(const ADataComponentETL: IDataComponentETL);
    // procedure LoadLinks(const ADataComponentETL: IDataComponentETL);
  end;

implementation

uses Classes, SysUtils {, REST.Json};

{ TInterfacedList }
constructor TInterfacedList<T>.Create;
begin
  FList := TList<T>.Create;
end;

destructor TInterfacedList<T>.Destroy;
begin
  try
    FList.DisposeOf
  except
  end;
  try
    inherited
  except
  end;
end;

{ TProjectETL }

function TProjectETL.getListComponents: IListComponentsETL;
begin
  Result := FListComponentsETL;
end;

class function TProjectETL.GetInstance: IProjectETL;
begin
  if not Assigned(FInstance) then
    FInstance := TProjectETL.Create;
  Result := FInstance;
end;

class function TProjectETL.AddComponent(const AComponent: IComponentETL): IProjectETL;
begin
  Result := GetInstance;
  Result.getListComponents.Add(AComponent);
end;

constructor TProjectETL.Create;
begin
  FListComponentsETL := TListComponentsETL.Create;
  FListLinks := TListLinks.Create;
end;

class function TProjectETL.Load(const AFileName: string): Boolean;
begin
  {
    var
    lSerialize : TXmlSerializer<TDataComponentETL>;
    lOwner : TComponent;
    lDoc   : TxmlDocument;
    begin
    lOwner := TComponent.Create(nil);  // Required to make TXmlDocument work!
    try
    lDoc := TXmlDocument.Create(lOwner);  // will be freed with lOwner.Free
    lDoc.LoadFromFile(FileName);
    lSerialize := TXmlSerializer<TPerson>.Create;
    try
    result := lSerialize.Deserialize(lDoc);
    finally
    lSerialize.Free;
    end;
    finally
    lOwner.Free;
    end; }
end;

class function TProjectETL.Save(const AFileName: string): Boolean;
// var
// FileMain: TFDMemTable;
// TJson.ObjectToJsonString(Self);
// TJson.JsonToObject(Self);
begin

  {
    procedure TDataComponentETL.Save;
    var
    lSerialize : TXmlSerializer<TPerson>;
    lOwner : TComponent;
    lDoc   : TxmlDocument;
    begin
    lOwner := TComponent.Create(nil);  // Required to make TXmlDocument work!
    try
    lDoc := TXmlDocument.Create(lOwner);  // will be freed with lOwner.Free
    lSerialize := TXmlSerializer<TPerson>.Create;
    try
    lSerialize.Serialize(lDoc,Self);
    lDoc.SaveToFile(FileName);
    finally
    lSerialize.Free;
    end;
    finally
    lOwner.Free;
    end;
    end;
  }
end;

{ TListComponentsETL }

function TListComponentsETL.Add(const AComponent: IComponentETL): IListComponentsETL;

  function GenerateTitle(APrefix: string): string;

    function NewTitle(const ATitle: string): Boolean;
    var
      i: Integer;
    begin
      Result := True;
      for i := 0 to FList.Count - 1 do
        if ATitle = FList[i].Title then
          exit(false)
    end;

  var
    LNum: Word;
  begin
    LNum := 0;
    repeat
      LNum := LNum + 1;
      Result := APrefix + IntToStr(LNum);
    until NewTitle(Result);
  end;

begin
  Result := Self;
  AComponent.Title := GenerateTitle(AComponent.Title);
  FList.Add(AComponent);
end;

end.
