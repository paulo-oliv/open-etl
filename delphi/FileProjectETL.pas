unit FileProjectETL;

interface

uses Generics.Collections, Controls;

type
  IComponentETL = interface
    ['{EF7C533C-DE01-4BD9-87E3-4A4E31A7ABB5}']
  end;

  ILinkComponents = interface
    ['{8D0C91ED-10E1-4C14-9E35-7A01237BD8A4}']
  end;

  IListComponentsETL = interface
    ['{829D88B9-7255-40AB-8486-2BCE1A6112AC}']
  end;

  IListLinks = interface
    ['{65B200A3-F463-43D7-9F70-3AFA7881B6FB}']
  end;

  // TListComponentETL = array of TComponentETL;

  TListComponentETL = class(TList<IComponentETL>)
  end;

  // TListLinks = array of TLinkComponents;

  TListLinks = class(TList<ILinkComponents>)
  end;

  IProjectETL = interface
    ['{14B85495-D30F-45B6-BBE9-36438F0E0094}']
  end;

  TProjectETL = class(TInterfacedObject, IProjectETL)
  strict private
    FListComponentETL: TListComponentETL;
    FListLinks: TListLinks;
    class var FInstance: IProjectETL;
  public
    // function AddComponent(const AType: Byte): TComponentETL;
    class function Load(const AFileName: string): Boolean;
    class function Save(const AFileName: string): Boolean;
    constructor Create(const AContainer: TWinControl);
    // procedure BeginSave;
    // procedure AddComponent(ADataComponentETL: IDataComponentETL);
    // procedure AddLink(ALink: IDataComponentETL);
    // procedure EndSave;
    // procedure LoadComponents(const ADataComponentETL: IDataComponentETL);
    // procedure LoadLinks(const ADataComponentETL: IDataComponentETL);
  end;

implementation

uses Classes {, REST.Json};

{ TDataComponentETL }

// var
// FileMain: TFDMemTable;
// TJson.ObjectToJsonString(Self);
// TJson.JsonToObject(Self);

{ TProjectETL }

{function TProjectETL.AddComponent(const AType: Byte): TComponentETL;

  function GenerateTitle(APrefix: string): string;

    function NovoTitle(const ATitle: string): Boolean;
    var
      i: Integer;
    begin
      Result := True;
      for i := 0 to ControlCount - 1 do
        if Controls[i] is TComponentETL then
          if ATitle = TComponentETL(Controls[i]).Title then
            exit(false)
    end;

  var
    LNum: Word;
  begin
    LNum := 0;
    repeat
      LNum := LNum + 1;
      Result := APrefix + IntToStr(LNum);
    until NovoTitle(Result);
  end;

begin
  Result := TComponentETL.Factory(Self, AType);
  Result.Title := GenerateTitle(Result.Title);
  Result.OnPaint := ComponentPaint;
  Result.OnMouseMove := ComponentMouseMove;
  Result.OnMouseUp := ComponentMouseUp;
  Result.OnMouseDown := ComponentMouseDown;
  Result.OnResize := ComponentResize;
  Result.OnDragDrop := ComponentDragDrop;
  Result.OnDragOver := ComponentDragOver;
  Result.OnDblClick := ComponentDblClick;
  Result.PopupMenu := PopupComp;
end;     }

constructor TProjectETL.Create(const AContainer: TWinControl);
begin

end;

class function TProjectETL.Load(const AFileName: string): Boolean;
begin
  //
end;

class function TProjectETL.Save(const AFileName: string): Boolean;
begin
  //
end;

{ procedure TDataComponentETL.Load;
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
  end;
  end;

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

end.
