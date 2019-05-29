unit ETL.FileProject.Interfaces;

interface

uses ETL.Form.Grid, Generics.Collections;

type
  ISourceETL = interface
    ['{8D0C91ED-10E1-4C14-9E35-7A01237BD8A4}']
    function GetID: string;
    function GetGrid: TFoGrid;
  end;

  IList = interface
    ['{43DB5D2D-7418-41AB-94E7-FD036C99DD4B}']
    function Count: Integer;
  end;

  IListSources = interface(IList)
    ['{65B200A3-F463-43D7-9F70-3AFA7881B6FB}']
    function GetItem(const AIndex: Integer): ISourceETL;
    function Add(const ASource: ISourceETL): IListSources;
  end;

  IComponentETL = interface(ISourceETL)
    ['{EF7C533C-DE01-4BD9-87E3-4A4E31A7ABB5}']
    function getTitle: string;
    procedure setTitle(const ATitle: string);
    procedure setPosition(const Ax, Ay: Integer);
    procedure setScript(const AScript: string);
    function GetKind: Integer;
    function GetLeft: Integer;
    function GetTop: Integer;
    function getScript: string;
    function GetSources: IListSources;
    procedure AddSource(const ASource: IComponentETL);
    property Title: string read getTitle write setTitle;
    property Script: string read getScript write setScript;
  end;

  IListComponentsETL = interface(IList)
    ['{829D88B9-7255-40AB-8486-2BCE1A6112AC}']
    function GetItem(const AIndex: Integer): IComponentETL;
    function Add(const AComponent: IComponentETL): IListComponentsETL;
    function Locate(const AId: string): IComponentETL;
    function GenerateTitle(APrefix: string): string;
  end;

  IComponentExtract = interface(IComponentETL)
    ['{06C636E3-93E2-420B-AD22-CD3F5B4C9F83}']
  end;

  IComponentTransform = interface(IComponentETL)
    ['{FCB1B851-02E5-4768-9903-763034678F39}']
  end;

  IComponentLoad = interface(IComponentETL)
    ['{AA5EB842-3CF8-4E57-81D0-520929331B4D}']
  end;

  IProjectETL = interface
    ['{14B85495-D30F-45B6-BBE9-36438F0E0094}']
    function getListComponents: IListComponentsETL;
    function getFileName: string;
    procedure setFileName(const AFileName: string);
    property FileName: string read getFileName write setFileName;
  end;

  TInterfacedList<T> = class(TInterfacedObject, IList)
  strict protected
    FList: TList<T>;
  public
    function Count: Integer;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

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

end.
