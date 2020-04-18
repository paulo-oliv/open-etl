unit ETL.FileProject;

interface

uses Controls, ETL.FileProject.Interfaces;

type
  TListComponentsETL = class(TInterfacedListComponentsETL, IListComponentsETL)
  public
    function Locate(const AId: string): IComponentETL;
    function GenerateTitle(APrefix: string): string;
  end;

  TProjectETL = class(TInterfacedObject, IProjectETL)
  strict private
    FFileName: string;
    FListComponentsETL: IListComponentsETL;
    class var FInstance: IProjectETL;
  strict protected
    function getFileName: string;
    procedure setFileName(const AFileName: string);
  public
    function getListComponents: IListComponentsETL;
    class function GetInstance: IProjectETL;
    class function AddComponent(const AParent: TWinControl; const AKind: Byte;
      const Ax, Ay: Integer; const AGUID: string): IComponentETL; overload;
    class function AddComponent(const AParent: TWinControl; const AKind: Byte;
      const Ax, Ay: Integer): IComponentETL; overload;
    class function Open(const AFileName: string; const AParent: TWinControl): IProjectETL;
    class function Save(const AFileName: string): IProjectETL; overload;
    class function Save: IProjectETL; overload;

    class procedure Clear;

    constructor Create;
    property FileName: string read FFileName { write setFileName };
    // procedure BeginSave;
    // procedure AddLink(ALink: IDataComponentETL);
    // procedure EndSave;
    // procedure LoadComponents(const ADataComponentETL: IDataComponentETL);
    // procedure LoadLinks(const ADataComponentETL: IDataComponentETL);
  end;

implementation

uses Classes, SysUtils, System.JSON, ETL.Component.Factory, Dialogs;

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
  const Ax, Ay: Integer; const AGUID: string): IComponentETL;
begin
  Result := TComponentETLFactory.New(AParent, AKind, AGUID);
  Result.setPosition(Ax, Ay);
  GetInstance.getListComponents.Add(Result);
end;

class function TProjectETL.AddComponent(const AParent: TWinControl; const AKind: Byte;
  const Ax, Ay: Integer): IComponentETL;
var
  LGUID: TGUID;
begin
  if CreateGUID(LGUID) <> 0 then
  begin
    // Creating GUID failed!'
  end;
  Result := AddComponent(AParent, AKind, Ax, Ay, GUIDToString(LGUID))
end;

class procedure TProjectETL.Clear;
var
  i: Integer;
begin
  for i := GetInstance.getListComponents.Count - 1 downto 0 do
    GetInstance.getListComponents[i].Delete;
end;

constructor TProjectETL.Create;
begin
  FListComponentsETL := TListComponentsETL.Create;
end;

function TProjectETL.getFileName: string;
begin
  Result := FFileName;
end;

procedure TProjectETL.setFileName(const AFileName: string);
begin
  FFileName := AFileName;
end;

const
  VAR_ID = 'Id';
  VAR_KIND = 'Kind';
  VAR_TITLE = 'Title';
  VAR_SCRIPT = 'Script';
  VAR_X = 'X';
  VAR_Y = 'Y';
  VAR_SOURCES = 'Sources';

class function TProjectETL.Save(const AFileName: string): IProjectETL;

  function sourcesToJSONArray(ASources: IListSources): TJSONArray;
  var
    i: Integer;
  begin
    Result := TJSONArray.Create;
    if Assigned(ASources) then
      for i := 0 to ASources.Count - 1 do
        Result.Add(ASources.GetItem(i).getID)
  end;

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
      for i := 0 to Result.getListComponents.Count - 1 do
      begin
        LJSONObject := TJSONObject.Create;
        LJSONObject.AddPair(VAR_ID, Result.getListComponents.GetItem(i).getID);
        LJSONObject.AddPair(VAR_KIND, IntToStr(Result.getListComponents.GetItem(i).GetKind));
        LJSONObject.AddPair(VAR_TITLE, Result.getListComponents.GetItem(i).Title);
        LJSONObject.AddPair(VAR_SCRIPT, Result.getListComponents.GetItem(i).GetScript);
        LJSONObject.AddPair(VAR_X, IntToStr(Result.getListComponents.GetItem(i).GetLeft));
        LJSONObject.AddPair(VAR_Y, IntToStr(Result.getListComponents.GetItem(i).GetTop));
        LJSONObject.AddPair(VAR_SOURCES,
          sourcesToJSONArray(IComponentTransform(Result.getListComponents.GetItem(i)).getSources));

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
  Result := Save(Result.FileName);
end;

class function TProjectETL.Open(const AFileName: string; const AParent: TWinControl): IProjectETL;

  procedure LoadLinks(const AId: string; const ASourcesJSONArray: TJSONArray);
  var
    i: Integer;
    LSource, LTarget: IComponentETL;
  begin
    LTarget := Result.getListComponents.Locate(AId);
    for i := 0 to ASourcesJSONArray.Count - 1 do
    begin
      LSource := Result.getListComponents.Locate(ASourcesJSONArray.Items[i].Value);
      if LSource <> nil then
        LTarget.AddSource(LSource);
    end;
  end;

var
  LJSONArray: TJSONArray;
  LJSONObject: TJSONObject;
  s: string;
  i: Integer;
  f: TextFile;
  LComponent: IComponentETL;
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
      LComponent := AddComponent(AParent, LJSONObject.GetValue<Byte>(VAR_KIND),
        LJSONObject.GetValue<Integer>(VAR_X), LJSONObject.GetValue<Integer>(VAR_Y),
        LJSONObject.GetValue<string>(VAR_ID));
      LComponent.Title := LJSONObject.GetValue<string>(VAR_TITLE)
    end;
    for i := 0 to LJSONArray.Count - 1 do
    begin
      LJSONObject := TJSONObject(LJSONArray.Items[i]);
      LoadLinks(LJSONObject.GetValue<string>(VAR_ID), LJSONObject.GetValue<TJSONArray>(VAR_SOURCES))
    end;
    for i := 0 to LJSONArray.Count - 1 do
    begin
      LJSONObject := TJSONObject(LJSONArray.Items[i]);
      LComponent := Result.getListComponents.Locate(LJSONObject.GetValue<string>(VAR_ID));
      LComponent.Script := LJSONObject.GetValue<string>(VAR_SCRIPT);
    end;
  finally
    LJSONArray.DisposeOf
  end;

  // except
  // raise Exception.Create('Error Message');
  // end;
end;

{ TListComponentsETL }

function TListComponentsETL.Locate(const AId: string): IComponentETL;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if GetItem(i).getID = AId then
      exit(GetItem(i));
end;

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

end.
