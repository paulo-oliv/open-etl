unit uDmImages;

interface

uses
  System.SysUtils, System.Classes, System.ImageList, Vcl.ImgList, Vcl.Controls, cxGraphics;

type
  TDmImages = class(TDataModule)
    ILDev32: TcxImageList;
    ILDev64: TcxImageList;
    IL16: TImageList;
  private
  public
  end;

var
  DmImages: TDmImages;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

end.
