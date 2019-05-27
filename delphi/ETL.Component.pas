unit ETL.Component;

interface

uses
  Vcl.ExtCtrls,
  System.Classes,
  Vcl.StdCtrls,
  Vcl.Controls,
  ETL.FileProject.Interfaces;

type
  TComponentETL = class(TPaintBox, IComponentETL, ISourceETL)
  strict private
    FLabel: TCustomLabel;
    FSources: IListSources;
    FGUID: string;
    class var FMoveX, FMoveY: Integer;
    class var FMover: Boolean;
  strict protected
    procedure RefreshPositionLabel;
    procedure Resize; override;
    function getTitle: string;
    procedure setTitle(const ATitle: string); virtual;
    function getScript: string; virtual; abstract;
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
    function GetId: string;
    function GetSources: IListSources;
    procedure AddSource(const ASource: IComponentETL);
    function GetKind: Integer;
    procedure Edit; virtual; abstract;
    procedure Preview; virtual; abstract;
    procedure Delete;
    function GetLeft: Integer;
    function GetTop: Integer;
    constructor Create(const AOwnerAndParent: TWinControl; const AGUID: string);
  published
    property Title: string read getTitle write setTitle;
  end;

implementation

{ TComponentETL }

uses SysUtils, ETL.DataModule.Main, ETL.Link;

const
  COMP_WIDTH = 64;
  COMP_HEIGHT = 64;

procedure TComponentETL.RefreshPositionLabel;
begin
  FLabel.Left := Left + ((COMP_WIDTH - FLabel.Width) div 2);
  FLabel.Top := Top + Height;
end;

function TComponentETL.GetSources: IListSources;
begin
  Result := FSources;
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
        if (TLinkComponents(Parent.Controls[i]).Target = IComponentETL(Self)) or
          (TLinkComponents(Parent.Controls[i]).Source = IComponentETL(Self)) then
          TLinkComponents(Parent.Controls[i]).RefreshSize;
end;

procedure TComponentETL.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  FLabel.Parent := AParent;
end;

constructor TComponentETL.Create(const AOwnerAndParent: TWinControl; const AGUID: string);
begin
  FLabel := TCustomLabel.Create(AOwnerAndParent);
  inherited Create(AOwnerAndParent);
  Parent := AOwnerAndParent;
  FLabel.Canvas.Font.Name := 'Segoe UI';
  FLabel.Canvas.Font.Size := 10;
  FGUID := AGUID;
  PopupMenu := DmMain.PopupComp;
end;

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

function TComponentETL.GetId: string;
begin
  Result := FGUID;
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

procedure TComponentETL.AddSource(const ASource: IComponentETL);
begin
  TLinkComponents.New(Parent, ASource, Self);

  if not Assigned(FSources) then
    FSources := TListSources.Create;
  FSources.Add(ASource);
end;

procedure TComponentETL.DragDrop(Source: TObject; X, Y: Integer);
begin
  inherited DragDrop(Source, X, Y);
  EndDrag(True);

  AddSource(TComponentETL(Source));
end;

end.
