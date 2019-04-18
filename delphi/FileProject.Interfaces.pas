unit FileProject.Interfaces;

interface

type
  IComponentETL = interface
    ['{EF7C533C-DE01-4BD9-87E3-4A4E31A7ABB5}']
    function getTitle: string;
    procedure setTitle(const ATitle: string);
    property Title: string read getTitle write setTitle;
  end;

  ILinkComponents = interface
    ['{8D0C91ED-10E1-4C14-9E35-7A01237BD8A4}']
  end;

  IList = interface
    ['{43DB5D2D-7418-41AB-94E7-FD036C99DD4B}']
  end;

  IListComponentsETL = interface(IList)
    ['{829D88B9-7255-40AB-8486-2BCE1A6112AC}']
    function Add(const AComponent: IComponentETL): IListComponentsETL;
  end;

  IListLinks = interface(IList)
    ['{65B200A3-F463-43D7-9F70-3AFA7881B6FB}']
  end;

  IProjectETL = interface
    ['{14B85495-D30F-45B6-BBE9-36438F0E0094}']
    function getListComponents: IListComponentsETL;
  end;

implementation

end.