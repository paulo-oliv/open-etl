unit FileProjectETL;

interface

type
  IDataComponentETL = interface
    ['{EF7C533C-DE01-4BD9-87E3-4A4E31A7ABB5}']
  end;

  TDataComponentETL = class(TInterfacedObject, IDataComponentETL)
    procedure Save;
    procedure Load;
  end;

implementation

uses Classes {, REST.Json};

{ TDataComponentETL }

procedure TDataComponentETL.Save;
begin
  // TJson.ObjectToJsonString(Self);
end;

procedure TDataComponentETL.Load;
begin
  // TJson.JsonToObject(Self);
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
