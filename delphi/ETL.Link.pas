unit ETL.Link;

interface

uses Vcl.ExtCtrls, ETL.Component, Vcl.Controls, ETL.FileProject.Interfaces;

type
  TLinkComponents = class(TPaintBox)
  strict private
    FText: string;
    FSource, FTarget: IComponentETL;
  strict protected
    procedure Paint; override;
  public
    procedure RefreshSize;
    constructor Create(const AOwnerAndParent: TWinControl);

    class function New(const AOwnerAndParent: TWinControl; const ASource, ATarget: IComponentETL)
      : TLinkComponents;
  published
    property Text: string read FText write FText;
    property Source: IComponentETL read FSource write FSource;
    property Target: IComponentETL read FTarget write FTarget;
  end;

implementation

{ TListSources }

uses ETL.Datamodule.Main, Math, Types, Vcl.Graphics;

{ TLinkComponents }

constructor TLinkComponents.Create(const AOwnerAndParent: TWinControl);
begin
  inherited Create(AOwnerAndParent);
  Parent := AOwnerAndParent;
  PopupMenu := DmMain.PopupLink;
  Text := '';
end;

class function TLinkComponents.New(const AOwnerAndParent: TWinControl;
  const ASource, ATarget: IComponentETL): TLinkComponents;
begin
  Result := TLinkComponents.Create(AOwnerAndParent);
  Result.Source := ASource;
  Result.Target := ATarget;
  Result.RefreshSize;
end;

procedure TLinkComponents.Paint;

  procedure DesenharSeta(Origem, Destino: TPoint);
  const
    ANGULO = 30;
    PONTA = 25;
  var
    AlphaRota, Alpha, Beta: Extended;
    vertice1, vertice2, vertice3: TPoint;
  begin
    Canvas.MoveTo(Origem.X, Origem.Y);
    Canvas.LineTo(Destino.X, Destino.Y);

    if (Destino.X >= Origem.X) then
    begin
      if (Destino.Y >= Origem.Y) then
      begin
        AlphaRota := Destino.X - Origem.X;
        if (AlphaRota <> 0) then
          Alpha := ArcTan((Destino.Y - Origem.Y) / AlphaRota)
        else
          Alpha := ArcTan(Destino.Y - Origem.Y);
        Beta := (ANGULO * (PI / 180)) / 2;
        vertice1.X := Destino.X - Round(Cos(Alpha + Beta));
        vertice1.Y := Destino.Y - Round(Sin(Alpha - Beta));
        vertice2.X := Round(vertice1.X - PONTA * Cos(Alpha + Beta));
        vertice2.Y := Round(vertice1.Y - PONTA * Sin(Alpha + Beta));
        vertice3.X := Round(vertice1.X - PONTA * Cos(Alpha - Beta));
        vertice3.Y := Round(vertice1.Y - PONTA * Sin(Alpha - Beta));
        Self.Canvas.Polygon([vertice1, vertice2, vertice3]);
      end
      else
      begin
        AlphaRota := Destino.Y - Origem.Y;
        if (AlphaRota <> 0) then
          Alpha := ArcTan((Destino.X - Origem.X) / AlphaRota)
        else
          Alpha := ArcTan(Destino.X - Origem.X);
        Beta := (ANGULO * (PI / 180)) / 2;
        vertice1.X := Destino.X - Round(Cos(Alpha + Beta));
        vertice1.Y := Destino.Y - Round(Sin(Alpha - Beta));
        vertice2.X := Round(vertice1.X + PONTA * Sin(Alpha + Beta));
        vertice2.Y := Round(vertice1.Y + PONTA * Cos(Alpha + Beta));
        vertice3.X := Round(vertice1.X + PONTA * Sin(Alpha - Beta));
        vertice3.Y := Round(vertice1.Y + PONTA * Cos(Alpha - Beta));
        Self.Canvas.Polygon([vertice1, vertice2, vertice3]);
      end;
    end
    else
    begin
      Alpha := ArcTan((Destino.Y - Origem.Y) / (Destino.X - Origem.X));
      Beta := (ANGULO * (PI / 180)) / 2;
      vertice1.X := Destino.X - Round(Cos(Alpha + Beta));
      vertice1.Y := Destino.Y - Round(Sin(Alpha - Beta));
      vertice2.X := Round(vertice1.X + PONTA * Cos(Alpha + Beta));
      vertice2.Y := Round(vertice1.Y + PONTA * Sin(Alpha + Beta));
      vertice3.X := Round(vertice1.X + PONTA * Cos(Alpha - Beta));
      vertice3.Y := Round(vertice1.Y + PONTA * Sin(Alpha - Beta));
      Self.Canvas.Polygon([vertice1, vertice2, vertice3]);
    end;
  end;

const
  POSICAO_MIN = 8;
begin
  Canvas.Pen.Width := 2;
  Canvas.Pen.Color := clGray;
  if Tag = 3 then
    DesenharSeta(Point(0, Height), Point(Width - POSICAO_MIN, POSICAO_MIN))
  else if Tag = 2 then
    DesenharSeta(Point(Width, Height - POSICAO_MIN), Point(POSICAO_MIN, POSICAO_MIN))
  else if Tag = 1 then
    DesenharSeta(Point(0, 0), Point(Width - POSICAO_MIN, Height - POSICAO_MIN))
  else
    DesenharSeta(Point(Width, 0), Point(POSICAO_MIN, Height - POSICAO_MIN));
  Canvas.Brush.Color := clWhite;
end;

procedure TLinkComponents.RefreshSize;
var
  x1, x2, y1, y2, a: Integer;
  LSetaDireita: Boolean;
begin
  x1 := FSource.GetLeft;
  x2 := FTarget.GetLeft;
  if x1 < x2 then
  begin
    x2 := x2 - x1;
    LSetaDireita := True;
  end
  else
  begin
    a := x1;
    x1 := x2;
    x2 := a - x1;
    LSetaDireita := false;
  end;

  y1 := FSource.GetTop;
  y2 := FTarget.GetTop;
  if y1 < y2 then
  begin
    y2 := y2 - y1;
    if LSetaDireita then
      Tag := 1
    else
      Tag := 0;
  end
  else
  begin
    a := y1;
    y1 := y2;
    y2 := a - y1;
    if LSetaDireita then
      Tag := 3
    else
      Tag := 2
  end;
  x1 := x1 + 64;
  y1 := y1 + 64;
  x2 := x2 - 64;
  y2 := y2 - 64;

  if (x2 < 64) then
  begin
    a := (101 - x2) div 2;
    x1 := x1 - a;
    x2 := x2 + a;
  end;
  if (y2 < 64) then
  begin
    a := (101 - y2) div 2;
    y1 := y1 - a;
    y2 := y2 + a;
  end;

  a := Min(FSource.GetTop, FTarget.GetTop);
  if (y1 < a) then
  begin
    y2 := y2 + (a - y1);
    y1 := a;
  end;

  a := Min(FSource.GetLeft, FTarget.GetLeft);
  if (x1 < a) then
  begin
    x2 := x2 + (a - x1);
    x1 := a;
  end;

  SetBounds(x1, y1, x2, y2);
  Repaint;
end;

end.
