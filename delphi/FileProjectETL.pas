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
    class function AddComponent(const AComponent: IComponentETL): IProjectETL;
    class function Load(const AFileName: string): IProjectETL;
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

uses Classes, SysUtils, System.JSON; // ,  REST.Json; // XMLDoc, xmldom, XMLIntf, msxmldom, ,

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
        LJSONObject.AddPair('Type', IntToStr(GetInstance.getListComponents.GetItem(i).GetType));
        LJSONObject.AddPair('Title', GetInstance.getListComponents.GetItem(i).Title);
        LJSONObject.AddPair('X', IntToStr(GetInstance.getListComponents.GetItem(i).GetLeft));
        LJSONObject.AddPair('Y', IntToStr(GetInstance.getListComponents.GetItem(i).GetTop));
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
(*
  class function TProjectETL.Save(const AFileName: string): Boolean;
  var
  lDoc: TXMLDocument;
  i: Integer;
  LComp: IComponentETL;
  //  LNodeElement, NodeCData, NodeText: IXMLNode;
  begin
  lDoc := TXMLDocument.Create(nil);
  try
  lDoc.Active := True;
  for i := 0 to GetInstance.getListComponents.Count - 1 do
  begin
  LComp := GetInstance.getListComponents.GetItem(i);
  lDoc.AddChild(LComp.Title);
  end;
  lDoc.SaveToFile(AFileName);
  finally
  lDoc.DisposeOf
  end;

  var
  begin
  { Define document content. }
  LDocument.DocumentElement := LDocument.CreateNode('ThisIsTheDocumentElement', ntElement, '');
  LDocument.DocumentElement.Attributes['attrName'] := 'attrValue';
  LNodeElement := LDocument.DocumentElement.AddChild('ThisElementHasText', -1);
  LNodeElement.Text := 'Inner text.';
  NodeCData := LDocument.CreateNode('any characters here', ntCData, '');
  LDocument.DocumentElement.ChildNodes.Add(NodeCData);
  NodeText := LDocument.CreateNode('This is a text node.', ntText, '');
  LDocument.DocumentElement.ChildNodes.Add(NodeText);

  LDocument.SaveToFile(DestPath);
  end;

  end;
*)

(* class function TProjectETL.Save(const AFileName: string): Boolean;
  var
  FileMain: TFDMemTable;
  begin
  FileMain.SaveToFile(AcSave.Dialog.FileName);
  end;
*)

class function TProjectETL.Load(const AFileName: string): IProjectETL;
var
  LJSONArray: TJSONArray;
  // LJSONObject: TJSONObject;
  s: string;
  // i: Integer;
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
  // for i := 0 to LJSONArray.Count - 1 do
  // AddComponent()

  // except
  // raise Exception.Create('Error Message');
  // end;
end;

(*
  class function TProjectETL.Load(const AFileName: string): Boolean;
  var
  lDoc: TXMLDocument;
  i: Integer;
  begin
  lDoc := TXMLDocument.Create(AFileName); // will be freed with lOwner.Free
  try
  lDoc.LoadFromFile(AFileName);
  for i := 0 to lDoc.DocumentElement.ChildNodes.Count - 1 do
  begin
  lDoc.DocumentElement.ChildNodes[i].ChildNodes['PARA'].Text;
  end;
  finally
  lDoc.DisposeOf;
  end;
  procedure RetrieveDocument;
  const
  CAttrName = 'attrName';
  HTAB = #9;
  var
  LDocument: IXMLDocument;
  LNodeElement, LNode: IXMLNode;
  LAttrValue: string;
  I: Integer;
  begin
  LDocument := TXMLDocument.Create(nil);
  LDocument.LoadFromFile(SrcPath); { File should exist. }

  { Find a specific node. }
  LNodeElement := LDocument.ChildNodes.FindNode('ThisIsTheDocumentElement');

  if (LNodeElement <> nil) then
  begin
  { Get a specific attribute. }
  Writeln('Getting attribute...');
  if (LNodeElement.HasAttribute(CAttrName)) then
  begin
  LAttrValue := LNodeElement.Attributes[CAttrName];
  Writeln('Attribute value: ' + LAttrValue);
  end;

  { Traverse child nodes. }
  Writeln(sLineBreak, 'Traversing child nodes...' + sLineBreak);
  for I := 0 to LNodeElement.ChildNodes.Count - 1 do
  begin
  LNode := LNodeElement.ChildNodes.Get(I);
  { Display node name. }
  Writeln(sLineBreak + 'Node name: ' + LNode.NodeName);
  { Check whether the node type is Text. }
  if LNode.NodeType = ntText then
  begin
  Writeln(HTAB + 'This is a node of type Text. The text is: ' + LNode.Text);
  end;
  { Check whether the node is text element. }
  if LNode.IsTextElement then
  begin
  Writeln(HTAB + 'This is a text element. The text is: ' + LNode.Text);
  end;
  end;
  end;
  end;

  end;
*)
(* class function TProjectETL.Load(const AFileName: string): Boolean;
  var
  FileMain: TFDMemTable;
  begin
  FileMain.LoadFromFile(AFile);
  end; *)

{ TListComponentsETL }

function TListComponentsETL.GetItem(const AIndex: Integer): IComponentETL;
begin
  Result := FList[AIndex];
end;

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
