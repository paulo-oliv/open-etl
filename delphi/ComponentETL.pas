unit ComponentETL;

interface

uses
  Vcl.ExtCtrls,
  System.Classes,
  Vcl.StdCtrls,
  Vcl.Controls,
  FileProject.Interfaces;

type
  TComponentETL = class(TPaintBox, IComponentETL)
  strict private
    FLabel: TCustomLabel;
    class var FMoveX, FMoveY: Integer;
    class var FMover: Boolean;
  strict protected
    procedure RefreshPositionLabel;
    procedure Resize; override;
    function getTitle: string;
    procedure setTitle(const ATitle: string); virtual;
    function GetScript: string; virtual; abstract;
    procedure setScript(const AScript: string); virtual; abstract;
    procedure SetParent(AParent: TWinControl); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure DblClick; override;
    procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState;
      var Accept: Boolean); override;
    procedure DragDrop(Source: TObject; X, Y: Integer); override;
    procedure Paint; override;
  public
    procedure setPosition(const Ax, Ay: Integer);
    function GetKind: Integer;
    procedure Edit; virtual; abstract;
    procedure Preview; virtual; abstract;
    procedure Delete;
    function GetLeft: Integer;
    function GetTop: Integer;
  published
    property Title: string read getTitle write setTitle;
    constructor Create(const AOwner: TComponent; const AParent: TWinControl);
  end;

  TLinkComponents = class(TPaintBox, ILinkComponents)
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

implementation

{ TComponentETL }

uses Vcl.Graphics, Windows, SysUtils, Math, Types, DataModule.Main;

const
  COMP_WIDTH = 64;
  COMP_HEIGHT = 64;

procedure TComponentETL.RefreshPositionLabel;
begin
  FLabel.Left := Left + ((COMP_WIDTH - FLabel.Width) div 2);
  FLabel.Top := Top + Height;
end;

procedure TComponentETL.Resize;
var
  i: Integer;
begin
  inherited;
  RefreshPositionLabel;

  if Assigned(Parent) then
    for i := 0 to Parent.ControlCount - 1 do
      if Parent.Controls[i] is TLinkComponents then
        if (TLinkComponents(Parent.Controls[i]).Target = Self) or
          (TLinkComponents(Parent.Controls[i]).Source = Self) then
          TLinkComponents(Parent.Controls[i]).RefreshSize;
end;

procedure TComponentETL.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  FLabel.Parent := AParent;
end;

constructor TComponentETL.Create(const AOwner: TComponent; const AParent: TWinControl);
begin
  FLabel := TCustomLabel.Create(AOwner);
  inherited Create(AOwner);
  Parent := AParent;
  FLabel.Canvas.Font.Name := 'Segoe UI';
  FLabel.Canvas.Font.Size := 10;
  PopupMenu := DmMain.PopupComp;
end;

{ TComponenteETL }

function TComponentETL.GetLeft: Integer;
begin
  Result := Left;
end;

function TComponentETL.GetTop: Integer;
begin
  Result := Top;
end;

procedure TComponentETL.DblClick;
begin
  inherited DblClick;
  Edit;
  Abort;
end;

procedure TComponentETL.Delete;
begin
  DisposeOf;
end;

procedure TComponentETL.DragDrop(Source: TObject; X, Y: Integer);
var
  temp: TLinkComponents;
begin
  inherited DragDrop(Source, X, Y);
  EndDrag(True);

  temp := TLinkComponents.Create(Self);

  temp.Parent := Parent;
  temp.Source := Self;
  temp.Target := TComponentETL(Source);
  temp.PopupMenu := DmMain.PopupLink;
  temp.Text := 'teste';
  temp.RefreshSize;
end;

procedure TComponentETL.DragOver(Source: TObject; X, Y: Integer; State: TDragState;
  var Accept: Boolean);
begin
  inherited DragOver(Source, X, Y, State, Accept);
  Accept := (Source is TComponentETL) and (Self <> Source);
end;

function TComponentETL.getTitle: string;
begin
  Result := FLabel.Caption;
end;

function TComponentETL.GetKind: Integer;
begin
  Result := Tag;
end;

procedure TComponentETL.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if (Button = mbLeft) then
    if ssShift in Shift then
    begin
      BeginDrag(false);
    end
    else
    begin
      FMoveX := X;
      FMoveY := Y;
      FMover := True;
    end;
end;

procedure TComponentETL.MouseMove(Shift: TShiftState; X, Y: Integer);
const
  MIN_LEFT = 120;
  MIN_TOP = 60;
begin
  inherited MouseMove(Shift, X, Y);
  if FMover then
  begin
    X := X - FMoveX + Left;
    Y := Y - FMoveY + Top;

    if X < MIN_LEFT then
      X := MIN_LEFT;
    if Y < MIN_TOP then
      Y := MIN_TOP;

    if X > Parent.Width - Width then
      X := Parent.Width - Width;
    if Y > Parent.Height - Height - MIN_TOP then
      Y := Parent.Height - Height - MIN_TOP;

    Left := X;
    Top := Y;
  end;
end;

procedure TComponentETL.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if FMover then
    FMover := false;
end;

procedure TComponentETL.setPosition(const Ax, Ay: Integer);
begin
  SetBounds(Ax, Ay, COMP_WIDTH, COMP_HEIGHT);
end;

procedure TComponentETL.setTitle(const ATitle: string);
begin
  FLabel.Caption := ATitle;
  RefreshPositionLabel;
end;

procedure TComponentETL.Paint;
begin
  // inherited;
  DmMain.IL64.Draw(Canvas, 0, 0, Tag);
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
    LSetaDireita := false;
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
