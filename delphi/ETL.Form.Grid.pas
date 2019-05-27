unit ETL.Form.Grid;

interface

uses Vcl.Forms, cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxStyles,
  cxCustomData, cxFilter, cxData, cxDataStorage, cxEdit, cxNavigator, cxGridLevel, cxClasses,
  cxGridCustomView, cxGridCustomTableView, cxGridTableView, cxGridBandedTableView, System.Classes,
  Vcl.Controls, cxGrid, Vcl.StdCtrls;

type
  TFoGrid = class(TForm)
    Lv: TcxGridLevel;
    Gr: TcxGrid;
    tv: TcxGridBandedTableView;
    Button1: TButton;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  strict private
  public
    procedure Save;
    class function New(const AOwner: TComponent): TFoGrid;
  end;

implementation

{$R *.dfm}

uses uMsg;
{ TFoGrid }

procedure TFoGrid.Button1Click(Sender: TObject);
begin
  Save;
end;

procedure TFoGrid.Button2Click(Sender: TObject);
begin
  tv.RestoreFromIniFile('testeINI');
end;

class function TFoGrid.New(const AOwner: TComponent): TFoGrid;
begin
  Result := TFoGrid.Create(AOwner);
end;

procedure TFoGrid.Save;
var
  LStream: TStringStream;
begin
  LStream := TStringStream.Create;
  try
    // tv.StoreToStream(LStream, []); 
    // TMensagem.Informacao(LStream.DataString);
    tv.StoreToIniFile('testeINI', true, [TcxGridStorageOption.gsoUseFilter,
      TcxGridStorageOption.gsoUseSummary]);
    // tv.StoreToStorage('');
    // AStream.
    LStream.Position := 0;
  finally
    LStream.DisposeOf;
  end;
end;

(*
  procedure AbrirFiltro;
  var
  AStream: TMemoryStream;
  begin
  AcAplicar.Execute;
  AStream := TMemoryStream.Create;
  QrFiltrosfiltro.SaveToStream(AStream);
  AStream.Position := 0;
  tvBanded.RestoreFromStream(AStream, False, False, [gsoUseFilter, gsoUseSummary]);
  FreeAndNil(AStream);
  end;
*)

(*
  TcxIniFileReader = class(TcxCustomReader)
  private
    FIniFile: TMemIniFile;
    FPathList: TStringList;
    FObjectNameList: TStringList;
    FClassNameList: TStringList;
    function DecodeString(S: string): string;
    procedure CreateLists;
    procedure GetSectionDetail(const ASection: string; var APath, AObjectName, AClassName: string);
  protected
    procedure BeginRead; override;
    function CanRead: Boolean; override;

    property IniFile: TMemIniFile read FIniFile;
  public
    constructor Create(const AStorageName: string; AStorageStream: TStream); override;
    destructor Destroy; override;
    procedure ReadProperties(const AObjectName, AClassName: string; AProperties: TStrings); override;
    function ReadProperty(const AObjectName, AClassName, AName: string): Variant; override;
    procedure ReadChildren(const AObjectName, AClassName: string; AChildrenNames,
        AChildrenClassNames: TStrings); override;
  end;


  TcxIniFileWriter = class(TcxCustomWriter)
  private
    FIniFile: TMemIniFile;
    function EncodeString(const S: string): string;
  protected
    procedure BeginWrite; override;
    procedure ClearObjectData(const AObjectFullName, AClassName: string); override;
    procedure EndWrite; override;

    property IniFile: TMemIniFile read FIniFile;
  public
    constructor Create(const AStorageName: string; AStream: TStream; AReCreate: Boolean = True); overload; override;
    destructor Destroy; override;
    procedure BeginWriteObject(const AObjectName, AClassName: string); override;
    procedure WriteProperty(const AObjectName, AClassName, AName: string; AValue: Variant); override;
  end;



function TcxIniFileReader.DecodeString(S: string): string;

  function DecodeStringV2: string;
  var
    I: Integer;
  begin
    Result := '';
    I := 1;
    while I <= Length(S)  do
    begin
      if S[I] = '\' then
      begin
        Inc(I);
        if I <= Length(S) then
        begin
          if S[I] = 'n' then
            Result := Result + #13#10
          else if S[I] = 't' then
            Result := Result + #9
          else
            Result := Result + '\';
        end;
      end
      else
        Result := Result + S[I];
      Inc(I);
    end;
  end;

  function DecodeStringV0: string;
  begin
    Result := S;
    Result := StringReplace(Result, ' \n', #13#10, [rfReplaceAll, rfIgnoreCase]);
    Result := StringReplace(Result, '\\n', '\n', [rfReplaceAll, rfIgnoreCase]);
  end;

begin
  S := cxDequotedStr(S);
  case FStorageVersion of
    1, 2: Result := DecodeStringV2;
    0: Result := DecodeStringV0;
  else //-1
    Result := S;
  end;
end;

procedure TcxIniFileReader.CreateLists;
var
  ASectionList: TStringList;
  I: Integer;
  APath: string;
  AObjectName: string;
  AClassName: string;
begin
  if (FPathList = nil) or (FObjectNameList = nil) or (FClassNameList = nil) then
  begin
    FPathList := TStringList.Create;
    FObjectNameList := TStringList.Create;
    FClassNameList := TStringList.Create;
    ASectionList := TStringList.Create;
    try
      IniFile.ReadSections(ASectionList);
      for I := 0 to ASectionList.Count - 1 do
      begin
        GetSectionDetail(ASectionList[I], APath, AObjectName, AClassName);
        FPathList.Add(UpperCase(APath));
        FObjectNameList.Add(AObjectName);
        FClassNameList.Add(AClassName);
      end;
    finally
      ASectionList.Free;
    end;
  end;
end;

procedure TcxIniFileReader.GetSectionDetail(const ASection: string; var APath, AObjectName, AClassName: string);
var
  I: Integer;
  AName: string;
begin
  AName := '';
  APath := '';
  AObjectName := '';
  AClassName := '';

  for I := 1 to Length(ASection) do
    if ASection[I] = '/' then
    begin
      APath := APath + AName + '/';
      AName := '';
    end
    else
      if ASection[I] = ':' then
      begin
        AObjectName := AName;
        AName := '';
      end
      else
        AName := AName + ASection[I];
  AClassName := Trim(AName);
end;


procedure TcxIniFileReader.BeginRead;
begin
  FStorageVersion := IniFile.ReadInteger('Main', 'Version', -1);
end;

function TcxIniFileReader.CanRead: Boolean;
begin
  Result := FileExists(StorageName);
end;

constructor TcxIniFileReader.Create(const AStorageName: string; AStorageStream: TStream);
begin
  inherited Create(AStorageName, AStorageStream);

  FIniFile := TMemIniFile.Create(StorageName);
end;

destructor TcxIniFileReader.Destroy;
begin
  FreeAndNil(FIniFile);
  FreeAndNil(FPathList);
  FreeAndNil(FObjectNameList);
  FreeAndNil(FClassNameList);

  inherited Destroy;
end;

procedure TcxIniFileReader.ReadProperties(const AObjectName, AClassName: string; AProperties: TStrings);
var
  ASectionName: string;
begin
  ASectionName := AObjectName + ': ' + AClassName;
  IniFile.ReadSection(ASectionName, AProperties);
end;

function TcxIniFileReader.ReadProperty(const AObjectName, AClassName, AName: string): Variant;
var
  ASectionName: string;
  AValue: string;
  AIntegerValue: Integer;
  ARealValue: Double;
  ACode: Integer;
begin
  ASectionName := AObjectName + ': ' + AClassName;
  AValue := IniFile.ReadString(ASectionName, AName, '');

  if IsStringValue(AValue) then
    Result := DecodeString(AValue)
  else
  begin
    Val(AValue, AIntegerValue, ACode);
    if ACode = 0 then
      Result := AIntegerValue
    else
    begin
      Val(AValue, ARealValue, ACode);
      if ACode = 0 then
        Result := ARealValue
      else
        Result := DateTimeOrStr(AValue);
    end;
  end;
end;

procedure TcxIniFileReader.ReadChildren(const AObjectName, AClassName: string;
  AChildrenNames, AChildrenClassNames: TStrings);
var
  I: Integer;
  AParentPath: string;
begin
  CreateLists;

  if AObjectName <> '' then
    AParentPath := UpperCase(AObjectName) + '/'
  else
    AParentPath := UpperCase(AObjectName);

  for I := 0 to FPathList.Count - 1 do
  begin
    if FPathList[I] = AParentPath then
    begin
      AChildrenNames.Add(FObjectNameList[I]);
      AChildrenClassNames.Add(FClassNameList[I]);
    end;
  end;
end;






function TcxIniFileWriter.EncodeString(const S: string): string;
begin
  Result := StringReplace(S, '\', '\\', [rfReplaceAll]);
  Result := StringReplace(Result, #13#10, '\n', [rfReplaceAll]);
  Result := StringReplace(Result, #9, '\t', [rfReplaceAll]);
  Result := cxQuotedStr(Result);
end;

procedure TcxIniFileWriter.BeginWrite;
begin
  IniFile.WriteInteger('Main', 'Version', cxIniFileStorageVersion);
end;

procedure TcxIniFileWriter.ClearObjectData(const AObjectFullName, AClassName: string);
var
  ASectionName, AObjectName: string;
  ASections: TStringList;
  I: Integer;
begin
  if AObjectFullName = '' then
    Exit;
  ASections := TStringList.Create;
  try
    IniFile.ReadSections(ASections);
    AObjectName := UpperCase(AObjectFullName);
    for I := 0 to ASections.Count - 1 do
    begin
      ASectionName := UpperCase(ASections[I]);
      if Pos(AObjectName, ASectionName) = 1 then
      begin
        if (Length(ASectionName) > Length(AObjectName)) and (dxCharInSet(ASectionName[Length(AObjectName) + 1], [':', '/'])) then
          IniFile.EraseSection(ASectionName);
      end;
    end;
  finally
    ASections.Free;
  end;
end;

procedure TcxIniFileWriter.EndWrite;
begin
  IniFile.UpdateFile;
end;

constructor TcxIniFileWriter.Create(const AStorageName: string; AStream: TStream; AReCreate: Boolean = True);
begin
  inherited Create(AStorageName, AStream, AReCreate);

  FIniFile := TMemIniFile.Create(StorageName);
  FIniFile.Encoding := TEncoding.UTF8;
  if FReCreate then
    FIniFile.Clear;
  FIniFile.CaseSensitive := False;
end;

destructor TcxIniFileWriter.Destroy;
begin
  FreeAndNil(FIniFile);
  inherited Destroy;
end;

procedure TcxIniFileWriter.BeginWriteObject(const AObjectName, AClassName: string);
begin
  IniFile.WriteString(AObjectName + ': ' + AClassName, '', '');
end;

procedure TcxIniFileWriter.WriteProperty(const AObjectName, AClassName, AName: string;
  AValue: Variant);
var
  ASectionName: string;
begin
  ASectionName := AObjectName + ': ' + AClassName;
  case VarType(AValue) of
    varShortInt, varWord, varLongWord, varInt64,
    varSmallInt, varInteger, varByte:
      IniFile.WriteInteger(ASectionName, AName, AValue);
    varSingle, varDouble, varCurrency:
      IniFile.WriteFloat(ASectionName, AName, AValue);
    varUString, varString, varOleStr:
      IniFile.WriteString(ASectionName, AName, EncodeString(AValue));
    varDate:
      IniFile.WriteDateTime(ASectionName, AName, AValue);
  end;
end;

*)

end.
