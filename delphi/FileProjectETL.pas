unit FileProjectETL;

interface

uses Generics.Collections, Controls, FileProject.Interfaces;

type
  TInterfacedList<T> = class(TInterfacedObject, IList)
  strict protected
    FList: TList<T>;
  public
    function Count: Integer;
    constructor Create;
    destructor Destroy; override;
  end;

  TListComponentsETL = class(TInterfacedList<IComponentETL>, IListComponentsETL)
  public
    function GetItem(const AIndex: Integer): IComponentETL;
    function Add(const AComponent: IComponentETL): IListComponentsETL;
    function GenerateTitle(APrefix: string): string;
  end;

  TListLinks = class(TInterfacedList<ILinkComponents>, IListLinks)
  end;

  TProjectETL = class(TInterfacedObject, IProjectETL)
  strict private
    FFileName: string;
    FListComponentsETL: IListComponentsETL;
    FListLinks: IListLinks;
    class var FInstance: IProjectETL;
  strict protected
    function getFileName: string;
    procedure setFileName(const AFileName: string);
  public
    function getListComponents: IListComponentsETL;
    class function GetInstance: IProjectETL;
    class function AddComponent(const AParent: TWinControl; const AKind: Byte;
      const Ax, Ay: Integer): IComponentETL;
    class function Load(const AFileName: string; const AParent: TWinControl): IProjectETL;
    class function Save(const AFileName: string): IProjectETL; overload;
    class function Save: IProjectETL; overload;

    constructor Create;
    // procedure BeginSave;
    // procedure AddLink(ALink: IDataComponentETL);
    // procedure EndSave;
    // procedure LoadComponents(const ADataComponentETL: IDataComponentETL);
    // procedure LoadLinks(const ADataComponentETL: IDataComponentETL);
  end;

implementation

uses Classes, SysUtils, System.JSON, ComponentETL.Factory;
// ,  REST.Json; // XMLDoc, xmldom, XMLIntf, msxmldom, ,

const
  VAR_KIND = 'Kind';
  VAR_TITLE = 'Title';
  VAR_SCRIPT = 'Script';
  VAR_X = 'X';
  VAR_Y = 'Y';

  { TInterfacedList }
function TInterfacedList<T>.Count: Integer;
begin
  Result := FList.Count;
end;

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

class function TProjectETL.AddComponent(const AParent: TWinControl; const AKind: Byte;
  const Ax, Ay: Integer): IComponentETL;
begin
  Result := TComponentETLFactory.New(AParent, AKind);
  Result.setPosition(Ax, Ay);
  GetInstance.getListComponents.Add(Result);
end;

constructor TProjectETL.Create;
begin
  FListComponentsETL := TListComponentsETL.Create;
  FListLinks := TListLinks.Create;
end;

function TProjectETL.getFileName: string;
begin
  Result := FFileName;
end;

procedure TProjectETL.setFileName(const AFileName: string);
begin
  FFileName := AFileName;
end;

class function TProjectETL.Save(const AFileName: string): IProjectETL;
var
  LJSONArray: TJSONArray;
  LJSONObject: TJSONObject;
  i: Integer;
  f: TextFile;
begin
  Result := GetInstance;
  Result.FileName := AFileName;
  AssignFile(f, AFileName);
  try
    Rewrite(f);
    LJSONArray := TJSONArray.Create;
    try
      for i := 0 to GetInstance.getListComponents.Count - 1 do
      begin
        LJSONObject := TJSONObject.Create;
        LJSONObject.AddPair(VAR_KIND, IntToStr(GetInstance.getListComponents.GetItem(i).GetKind));
        LJSONObject.AddPair(VAR_TITLE, GetInstance.getListComponents.GetItem(i).Title);
        LJSONObject.AddPair(VAR_SCRIPT, GetInstance.getListComponents.GetItem(i).GetScript);
        LJSONObject.AddPair(VAR_X, IntToStr(GetInstance.getListComponents.GetItem(i).GetLeft));
        LJSONObject.AddPair(VAR_Y, IntToStr(GetInstance.getListComponents.GetItem(i).GetTop));
        LJSONArray.AddElement(LJSONObject);
      end;
      Writeln(f, LJSONArray.ToJSON);
    finally
      LJSONArray.DisposeOf
    end;
  finally
    CloseFile(f);
  end;
end;

class function TProjectETL.Save: IProjectETL;
begin
  Result := GetInstance;
  Save(Result.FileName);
end;

class function TProjectETL.Load(const AFileName: string; const AParent: TWinControl): IProjectETL;
var
  LJSONArray: TJSONArray;
  LJSONObject: TJSONObject;
  s: string;
  i: Integer;
  f: TextFile;
begin
  Result := GetInstance;
  Result.FileName := AFileName;
  AssignFile(f, AFileName);
  try
    Reset(f);
    Read(f, s);
  finally
    CloseFile(f);
  end;
  // try
  LJSONArray := TJSONObject.ParseJSONValue(s) as TJSONArray;
  try
    for i := 0 to LJSONArray.Count - 1 do
    begin
      LJSONObject := TJSONObject(LJSONArray.Items[i]);
      with AddComponent(AParent, LJSONObject.GetValue<Byte>(VAR_KIND),
        LJSONObject.GetValue<Integer>(VAR_X), LJSONObject.GetValue<Integer>(VAR_Y)) do
      begin
        Title := LJSONObject.GetValue<string>(VAR_TITLE);
        Script := LJSONObject.GetValue<string>(VAR_SCRIPT);
      end;
    end;
  finally
    LJSONArray.DisposeOf
  end;

  // except
  // raise Exception.Create('Error Message');
  // end;
end;

{ TListComponentsETL }

function TListComponentsETL.GenerateTitle(APrefix: string): string;

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

function TListComponentsETL.GetItem(const AIndex: Integer): IComponentETL;
begin
  Result := FList[AIndex];
end;

function TListComponentsETL.Add(const AComponent: IComponentETL): IListComponentsETL;
begin
  Result := Self;
  FList.Add(AComponent);
end;

end.
