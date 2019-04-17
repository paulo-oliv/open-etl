unit ComponentETL;

interface

uses
  Form.Grid,
  Vcl.ExtCtrls,
  System.Classes,
  Form.Edit.Query,
  Form.Edit.Transform,
  Form.Edit.Load,
  Vcl.StdCtrls;

const
  TIPO_COMPONENT_QUERY = 0;
  TIPO_COMPONENT_FILE = 1;
  TIPO_COMPONENT_FILTER = 2;
  TIPO_COMPONENT_CONVERSION = 3;
  TIPO_COMPONENT_DERIVATION = 4;
  TIPO_COMPONENT_JOIN = 5;
  TIPO_COMPONENT_CONDENSATION = 6;
  TIPO_COMPONENT_EXECUTE = 7;
  TIPO_COMPONENT_SCRIPT = 8;

type
  TComponentETL = class(TPaintBox)
  strict private
    FLabel: TCustomLabel;
    FFormGrid: TFoGrid;
    FTitle: string;
  strict protected
    procedure configQuery; virtual;
    function getTitle: string;
    procedure setTitle(const ATitle: string);
  public
    procedure Edit; virtual; abstract;
    procedure Preview;
    procedure Delete;
    class function Factory(const AOwner: TComponent; const AType: Byte): TComponentETL;
  published
    property Title: string read getTitle write setTitle;
    property OnResize;
    constructor Create(AOwner: TComponent); override;
  end;

  TLinkComponents = class(TPaintBox)
  strict private
    FText: string;
    FSource, FTarget: TComponentETL;
  strict protected
    procedure Paint; override;
  public
    property Text: string read FText write FText;
    property Source: TComponentETL read FSource write FSource;
    property Target: TComponentETL read FTarget write FTarget;
    procedure RefreshSize;
  end;

  TCompExtract = class(TComponentETL)
  end;

  TCompTransform = class(TComponentETL)
  strict protected
    FFormEdit: TFoEditTransform;
  public
    procedure Edit; override;
  end;

  TCompLoad = class(TComponentETL)
  strict protected
    FFormEdit: TFoEditLoad;
  public
    procedure Edit; override;
  end;

  TCompQuery = class(TCompExtract)
  strict private
    FFormEdit: TFoEditQuery;
  strict protected
    procedure configQuery; override;
  public
    procedure Edit; override;
  end;

  TCompFile = class(TCompExtract)
  end;

  TCompFilter = class(TCompTransform)
  end;

  TCompConversion = class(TCompTransform)
  end;

  TCompDerivation = class(TCompTransform)
  end;

  TCompJoin = class(TCompTransform)
  end;

  TComCondensation = class(TCompTransform)
  end;

  TCompExecute = class(TCompLoad)
  end;

  TCompScript = class(TCompLoad)
  end;

implementation

{ TComponentETL }

uses Controls, Vcl.Graphics, Windows, SysUtils, Math;

constructor TComponentETL.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLabel := TCustomLabel.Create(AOwner);
end;

class function TComponentETL.Factory(const AOwner: TComponent; const AType: Byte): TComponentETL;
begin
  case AType of
    TIPO_COMPONENT_QUERY:
      begin
        Result := TCompQuery.Create(AOwner);
        Result.Title := 'Query';
      end;
    TIPO_COMPONENT_FILE:
      begin
        Result := TCompFile.Create(AOwner);
        Result.Title := 'File';
      end;
    TIPO_COMPONENT_FILTER:
      begin
        Result := TCompFilter.Create(AOwner);
        Result.Title := 'Filter';
      end;
    TIPO_COMPONENT_CONVERSION:
      begin
        Result := TCompConversion.Create(AOwner);
        Result.Title := 'Conversion';
      end;
    TIPO_COMPONENT_DERIVATION:
      begin
        Result := TCompDerivation.Create(AOwner);
        Result.Title := 'Derivation';
      end;
    TIPO_COMPONENT_JOIN:
      begin
        Result := TCompJoin.Create(AOwner);
        Result.Title := 'Join';
      end;
    TIPO_COMPONENT_CONDENSATION:
      begin
        Result := TCompConversion.Create(AOwner);
        Result.Title := 'Condensation';
      end;
    TIPO_COMPONENT_EXECUTE:
      begin
        Result := TCompExecute.Create(AOwner);
        Result.Title := 'Execute';
      end;
    TIPO_COMPONENT_SCRIPT:
      begin
        Result := TCompScript.Create(AOwner);
        Result.Title := 'Script';
      end;
  end;
  Result.Parent := TWinControl(AOwner);
  Result.Tag := AType;
end;

{ TComponenteETL }

procedure TComponentETL.configQuery;
begin
  //
end;

procedure TComponentETL.Delete;
begin
  DisposeOf;
end;

function TComponentETL.getTitle: string;
begin
  Result := FTitle;
end;

procedure TComponentETL.setTitle(const ATitle: string);
begin
  FTitle := ATitle;
  Repaint;
end;

procedure TComponentETL.Preview;
begin
  if not Assigned(FFormGrid) then
    FFormGrid := TFoGrid.New(Self);

  configQuery;

  FFormGrid.ShowModal;
end;

{ TCompQuery }

procedure TCompQuery.configQuery;
begin
  if Assigned(FFormEdit) then
  begin

  end;
end;

procedure TCompQuery.Edit;
begin
  if not Assigned(FFormEdit) then
    FFormEdit := TFoEditQuery.New(Self);
  FFormEdit.ShowModal;
end;

{ TCompTransform }

procedure TCompTransform.Edit;
begin
  if not Assigned(FFormEdit) then
    FFormEdit := TFoEditTransform.New(Self);
  FFormEdit.ShowModal;
end;

{ TCompLoad }

procedure TCompLoad.Edit;
begin
  if not Assigned(FFormEdit) then
    FFormEdit := TFoEditLoad.New(Self);
  FFormEdit.ShowModal;
end;

{ TLinkComponents }

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
  if Tag = 0 then
    DesenharSeta(Point(0, Height), Point(Width - POSICAO_MIN, POSICAO_MIN))
  else if Tag = 1 then
    DesenharSeta(Point(Width, Height - POSICAO_MIN), Point(POSICAO_MIN, POSICAO_MIN))
  else if Tag = 2 then
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
  x1 := FSource.Left;
  x2 := FTarget.Left;
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
    LSetaDireita := False;
  end;

  y1 := FSource.Top;
  y2 := FTarget.Top;
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

  a := Min(FSource.Top, FTarget.Top);
  if (y1 < a) then
  begin
    y2 := y2 + (a - y1);
    y1 := a;
  end;

  a := Min(FSource.Left, FTarget.Left);
  if (x1 < a) then
  begin
    x2 := x2 + (a - x1);
    x1 := a;
  end;

  SetBounds(x1, y1, x2, y2);
  Repaint;
end;

end.
