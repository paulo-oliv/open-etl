unit ETL.Component;

interface

uses
  Vcl.ExtCtrls,
  System.Classes,
  Vcl.StdCtrls,
  Vcl.Controls,
  ETL.Form.Grid,
  ETL.FileProject.Interfaces;

type
  TComponentETL = class(TPaintBox, IComponentETL, ISourceETL)
  strict private
    FUpdated: Boolean;
    FFormGrid: TFoGrid;
    FLabel: TLabel;
    FSources: IListSources;
    FGUID: string;
    class var FMoveX, FMoveY: Integer;
    class var FMover: Boolean;
    function GetGrid: TFoGrid;
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
    procedure RefreshGrid(const AGrid: TFoGrid); virtual; abstract;
    procedure OnFormEditChange(Sender: TObject);
    function GetId: string;
  public
    procedure setPosition(const Ax, Ay: Integer);
    function GetSources: IListSources;
    procedure AddSource(const ASource: IComponentETL);
    function GetKind: Integer;
    procedure Edit; virtual; abstract;
    procedure Preview;
    procedure Delete;
    procedure RefreshPreviewForm;

    function GetLeft: Integer;
    function GetTop: Integer;

    constructor Create(const AOwnerAndParent: TWinControl; const AGUID: string);
  published
    property Updated: Boolean read FUpdated;
    property Title: string read getTitle write setTitle;
  end;

implementation

{ TComponentETL }

uses SysUtils, ETL.DataModule.Main, ETL.Link, graphics;

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
  FLabel := TLabel.Create(AOwnerAndParent);
  inherited Create(AOwnerAndParent);
  FLabel.Font.Name := 'Segoe UI';
  FLabel.Font.Size := 10;
  FLabel.Font.Color := Clwhite;
  Parent := AOwnerAndParent;
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

  procedure DeleteLinks;
  var
    i: Integer;
  begin
    for i := Parent.ControlCount - 1 downto 0 do
      if Parent.Controls[i] is TLinkComponents then
        if (TLinkComponents(Parent.Controls[i]).Target = IComponentETL(Self)) or
          (TLinkComponents(Parent.Controls[i]).Source = IComponentETL(Self)) then
          TLinkComponents(Parent.Controls[i]).DisposeOf;
  end;

begin
  if Assigned(Parent) then
    DeleteLinks;

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

procedure TComponentETL.RefreshPreviewForm;
var
  i: Integer;
  LSource: ISourceETL;
begin
  if not Assigned(FFormGrid) then
  begin
    if Assigned(FSources) then
      for i := 0 to FSources.Count - 1 do
      begin
        LSource := FSources.GetItem(i);
        if Assigned(LSource) then
          FFormGrid := LSource.GetGrid;
      end;
    if not Assigned(FFormGrid) then
      FFormGrid := TFoGrid.New(Owner);
  end;

  FFormGrid.tv.BeginUpdate;
  try
    RefreshGrid(FFormGrid);
    FFormGrid.tv.OptionsView.BandHeaders := (FFormGrid.tv.Bands.Count > 1) or
      (FFormGrid.tv.Bands[0].Caption <> '');
    FUpdated := True;
  finally
    FFormGrid.tv.EndUpdate;
  end;
end;

function TComponentETL.GetGrid: TFoGrid;
begin
  if not FUpdated then
    RefreshPreviewForm;
  Result := FFormGrid;
end;

procedure TComponentETL.Preview;
begin
  GetGrid.Show;
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

procedure TComponentETL.OnFormEditChange(Sender: TObject);
begin
  FUpdated := false;
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
  FUpdated := false;
end;

procedure TComponentETL.DragDrop(Source: TObject; X, Y: Integer);
begin
  inherited DragDrop(Source, X, Y);
  EndDrag(True);

  AddSource(TComponentETL(Source));
end;

end.
