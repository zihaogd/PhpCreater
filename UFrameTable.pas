unit UFrameTable;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ComCtrls, Vcl.ExtCtrls, Vcl.Menus, Vcl.ImgList;

type

  TFrameTable = class(TFrame)
    lvColumn: TListView;
    Splitter1: TSplitter;
    pmColumn: TPopupMenu;
    miNew: TMenuItem;
    il16: TImageList;
    ableBaseCreate1: TMenuItem;
    Panel1: TPanel;
    mmoTable: TMemo;
    Panel2: TPanel;
    btnCopy: TButton;
    btnSave: TButton;
    procedure miNewClick(Sender: TObject);
    procedure lvColumnDeletion(Sender: TObject; Item: TListItem);
    procedure lvColumnDblClick(Sender: TObject);
    procedure ableBaseCreate1Click(Sender: TObject);
    procedure btnCopyClick(Sender: TObject);
  public
    ProjectName : string;
    TableName : string;
    function ReadIniFilePath : string;
    procedure SaveColumnIni;
    procedure LoadColumnIni;
  end;

{$Region ' �������ɲ��� ' }

    // Sql ������
  SqlUtil = class
  public
    class function ReadKeyValue : string; // ��������
    class function ReadKeyWhere : string;  // Sql Where
    class function ReadKeySqlDefine( TabStr : string ) : string; // Sql����
  public
    class function ReadColValue : string; // ��������
    class function ReadColSqlValue : string; // Insert��������
    class function ReadColInsertValue : string; // Insert����ʵ��
  end;

    // ��������Ϣ����
  TTableBaseCreater = class
  private
    CreateStr : string;
    TabStr : string;
  private
    TableBaseName : string;
  public
    constructor Create;
    procedure Update;
  private
    procedure AddClassBase;
    procedure AddColumnSql;
    procedure AddGetConn;
    procedure AddSqlQuery;
    procedure AddSelectColQuery;
    procedure AddUpdateColQuery;
  private
    procedure AddTableClass;
    procedure AddTableAddFunction;
    procedure AddTableRemoveFunction;
  private
    procedure AddLine( Str : string );
    procedure AddEmptyLine;
    procedure AddLeftBraces;
    procedure AddRightBraces;
    procedure AddTab;
    procedure RemoveTab;
  end;

{$EndRegion}


{$Region ' ���� ���� ' }

  TLvColumnData = class
  public
    ColName : string;
    IsKey : Boolean;
  public
    constructor Create( _ColName : string );
  end;

{$EndRegion}

{$Region ' ���� ���� ' }

    // Frame
  TFaceTableFrameApi = class
  private
    FrameTable : TFrameTable;
  public
    procedure Activate( _FrameTable : TFrameTable );
  public
    procedure SaveIni;
    function ReadTableName : string;
    function ReadTableBaseName : string;
  end;

    // ��
  TFaceColumnApi = class
  private
    LvColumn : TListView;
  public
    procedure Activate( _LvColumn : TListView );
  public
    procedure AddColumn( ColumnName : string );
    function ReadIsExist( ColumnName : string ): Boolean;
    function ReadIsKey( ColumnName : string ): Boolean;
    procedure SetIsKey( ColumnName : string; IsKey : Boolean );
    function ReadColumnList : TStringList;
    function ReadKeyList : TStringList;
  private
    function ReadColumnItem( ColumnName : string ): TListItem;
    function ReadColumnIndex( ColumnName : string ): Integer;
  end;

    // �������
  TFaceCreaterResultApi = class
  private
    mmoResult : TMemo;
  public
    procedure Activate( _mmoResult : TMemo );
  public
    procedure ShowResult( ResultStr : string );
  end;

{$EndRegion}

{$Region ' �û� ���� ' }

    // ��
  UserColumnApi = class
  public
    class procedure NewColumn;
    class procedure SetKey( ColumnName : string );
  end;

    // ���� Sql
  UserSqlCreateApi = class
  public
    class procedure CreateTableBase;
  end;

{$EndRegion}

const
  Str_StaticVar = 'static $';
  Str_StaticFunction = 'static function';
  Str_Tab = '    ';
  Str_Sql = 'Sql';

const
  Ini_FrameTable = 'FrameTable';
  Ini_ColumnCount = 'ColumnCount';
  Ini_Column = 'Column';
  Ini_ColumnKeyCount = 'ColumnKeyCount';
  Ini_ColumnKey = 'ColumnKey';

var
  FaceTableFrameApi : TFaceTableFrameApi;
  FaceColumnApi : TFaceColumnApi;
  FaceCreaterResultApi : TFaceCreaterResultApi;

implementation

uses IniFiles;

{$R *.dfm}

{ TLvColumnData }

constructor TLvColumnData.Create(_ColName: string);
begin
  ColName := _ColName;
  IsKey := False;
end;

{ TFaceColumnApi }

procedure TFaceColumnApi.Activate(_LvColumn: TListView);
begin
  LvColumn := _LvColumn;
end;

procedure TFaceColumnApi.AddColumn(ColumnName: string);
var
  ColumnData : TLvColumnData;
begin
  ColumnData := TLvColumnData.Create( ColumnName );
  with LvColumn.Items.Add do
  begin
    Caption := ColumnName;
    SubItems.Add( '' );
    Data := ColumnData;
    ImageIndex := 0;
  end;
end;

function TFaceColumnApi.ReadColumnIndex(ColumnName: string): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to LvColumn.Items.Count - 1 do
    if LvColumn.Items[i].Caption = ColumnName then
    begin
      Result := i;
      Break;
    end;
end;

function TFaceColumnApi.ReadColumnItem(ColumnName: string): TListItem;
var
  ColIndex : Integer;
begin
  ColIndex := ReadColumnIndex( ColumnName );
  if ColIndex >= 0 then
    Result := LvColumn.Items[ ColIndex ]
  else
    Result := nil;
end;

function TFaceColumnApi.ReadColumnList: TStringList;
var
  i: Integer;
begin
  Result := TStringList.Create;
  for i := 0 to LvColumn.Items.Count - 1 do
    Result.Add( LvColumn.Items[i].Caption );
end;

function TFaceColumnApi.ReadIsExist(ColumnName: string): Boolean;
begin
  Result := ReadColumnIndex( ColumnName ) >= 0;
end;

function TFaceColumnApi.ReadIsKey(ColumnName: string): Boolean;
var
  ColumnItem : TListItem;
  ColumnData : TLvColumnData;
begin
  Result := False;
  ColumnItem := ReadColumnItem( ColumnName );
  if not Assigned( ColumnItem ) then
    Exit;
  ColumnData := ColumnItem.Data;
  Result := ColumnData.IsKey;
end;

function TFaceColumnApi.ReadKeyList: TStringList;
var
  i: Integer;
  ItemData : TLvColumnData;
begin
  Result := TStringList.Create;
  for i := 0 to LvColumn.Items.Count - 1 do
  begin
    ItemData := LvColumn.Items[i].Data;
    if ItemData.IsKey then
      Result.Add( ItemData.ColName );
  end;
end;

procedure TFaceColumnApi.SetIsKey(ColumnName: string; IsKey: Boolean);
var
  ColumnItem : TListItem;
  ColumnData : TLvColumnData;
  KeyStr : string;
begin
  ColumnItem := ReadColumnItem( ColumnName );
  if not Assigned( ColumnItem ) then
    Exit;
  if IsKey then
    KeyStr := 'Key'
  else
    KeyStr := '';
  ColumnItem.SubItems[0] := KeyStr;

  ColumnData := ColumnItem.Data;
  ColumnData.IsKey := IsKey;
end;
{ UserColumnApi }

class procedure UserColumnApi.NewColumn;
var
  ColumnName : string;
begin
    // �û�����
  if not InputQuery( 'New Column', 'Column Name', ColumnName ) then
    Exit;

    // ���Ѵ���
  if FaceColumnApi.ReadIsExist( ColumnName ) then
    Exit;

    // ������ѡ�б��
  FaceColumnApi.AddColumn( ColumnName );

    // ���̱���������Ϣ
  FaceTableFrameApi.SaveIni;
end;

class procedure UserColumnApi.SetKey(ColumnName: string);
var
  IsKey : Boolean;
begin
    // �����෴�� Key
  IsKey := FaceColumnApi.ReadIsKey( ColumnName );
  FaceColumnApi.SetIsKey( ColumnName, not IsKey );

    // ���̱���������Ϣ
  FaceTableFrameApi.SaveIni;
end;

{ TFaceTableFrameApi }

procedure TFaceTableFrameApi.Activate(_FrameTable: TFrameTable);
begin
  FrameTable := _FrameTable;
  FaceColumnApi.Activate( FrameTable.lvColumn );
  FaceCreaterResultApi.Activate( FrameTable.mmoTable );
end;

procedure TFrameTable.ableBaseCreate1Click(Sender: TObject);
begin
  UserSqlCreateApi.CreateTableBase;
end;

procedure TFrameTable.btnCopyClick(Sender: TObject);
begin
  mmoTable.SelectAll;
  mmoTable.CopyToClipboard;
end;

procedure TFrameTable.LoadColumnIni;
var
  ColumnName, ColumnKey : string;
  IniFile : TIniFile;
  ColumnCount, ColumnKeyCount, i: Integer;
begin
  FaceColumnApi.Activate( lvColumn );
  IniFile := TIniFile.Create( ReadIniFilePath );
    // ��ȡ ��
  ColumnCount := IniFile.ReadInteger( Ini_FrameTable, ProjectName + TableName + Ini_ColumnCount, 0 );
  for i := 0 to ColumnCount - 1 do
  begin
    ColumnName := IniFile.ReadString( Ini_FrameTable, ProjectName + TableName + Ini_Column + IntToStr( i ), '' );
    if ColumnName = '' then
      Continue;
    FaceColumnApi.AddColumn( ColumnName );
  end;
    // ��ȡ �м�
  ColumnKeyCount := IniFile.ReadInteger( Ini_FrameTable, ProjectName + TableName + Ini_ColumnKeyCount, 0 );
  for i := 0 to ColumnKeyCount - 1 do
  begin
    ColumnKey := IniFile.ReadString( Ini_FrameTable, ProjectName + TableName + Ini_ColumnKey + IntToStr( i ), '' );
    if ColumnKey = '' then
      Continue;
    FaceColumnApi.SetIsKey( ColumnKey, True );
  end;
  IniFile.Free;
end;

procedure TFrameTable.lvColumnDblClick(Sender: TObject);
var
  SelectItem : TListItem;
begin
  SelectItem := lvColumn.Selected;
  if not Assigned( SelectItem ) then
    Exit;
  UserColumnApi.SetKey( SelectItem.Caption );
end;

procedure TFrameTable.lvColumnDeletion(Sender: TObject; Item: TListItem);
var
  ObjData : TObject;
begin
  ObjData := Item.Data;
  ObjData.Free;
end;

procedure TFrameTable.miNewClick(Sender: TObject);
begin
  UserColumnApi.NewColumn;
end;

function TFrameTable.ReadIniFilePath: string;
begin
  Result := 'D:\��ʱ�ļ���\Test.ini';
end;

procedure TFrameTable.SaveColumnIni;
var
  IniFile : TIniFile;
  i: Integer;
  KeyList : TStringList;
begin
  IniFile := TIniFile.Create( ReadIniFilePath );
    // ���� ��
  IniFile.WriteInteger( Ini_FrameTable, ProjectName + TableName + Ini_ColumnCount, lvColumn.Items.Count );
  for i := 0 to lvColumn.Items.Count - 1 do
    IniFile.WriteString( Ini_FrameTable, ProjectName + TableName + Ini_Column + IntToStr( i ), lvColumn.Items[i].Caption );
    // ���� ��
  KeyList := FaceColumnApi.ReadKeyList;
  IniFile.WriteInteger( Ini_FrameTable, ProjectName + TableName + Ini_ColumnKeyCount, KeyList.Count );
  for i := 0 to KeyList.Count - 1 do
    IniFile.WriteString( Ini_FrameTable, ProjectName + TableName + Ini_ColumnKey + IntToStr( i ), KeyList[i] );
  KeyList.Free;
  IniFile.Free;
end;

function TFaceTableFrameApi.ReadTableBaseName: string;
begin
  Result := FrameTable.TableName + 'Base';
end;

function TFaceTableFrameApi.ReadTableName: string;
begin
  Result := FrameTable.TableName;
end;

procedure TFaceTableFrameApi.SaveIni;
begin
  FrameTable.SaveColumnIni;
end;

{ TTableBaseCreater }

procedure TTableBaseCreater.AddTab;
begin
  TabStr := TabStr + Str_Tab;
end;

procedure TTableBaseCreater.AddTableAddFunction;
var
  ColumnList : TStringList;
  i: Integer;
  ColumnStr : string;
begin
  AddLine( '// ��������' );
  AddLine( 'static function Add(' + SqlUtil.ReadColValue + ')' );
  AddLeftBraces;
  AddLine( '// Sql ����' );
  AddLine( '$TableSql = ' + TableBaseName + '::$TableSql;' );
  ColumnList := FaceColumnApi.ReadColumnList;
  for i := 0 to ColumnList.Count - 1 do
  begin
    ColumnStr := ColumnList[i];
    ColumnStr := '$' + ColumnStr + Str_Sql + ' = ' + TableBaseName + '::$' + ColumnStr + Str_Sql + ';';
    AddLine( ColumnStr );
  end;
  ColumnList.Free;
  AddEmptyLine;
  AddLine( '// ��װ Sql ���' );
  AddLine( '$InsertSql = "insert into $TableSql ( ' + SqlUtil.ReadColSqlValue + ' )";' );
  AddLine( '$InsertSql = $InsertSql . " values ( ' + SqlUtil.ReadColInsertValue + ' );";' );
  AddEmptyLine;
  AddLine( '// ִ�� Sql ���' );
  AddLine( 'TrialListBase::SqlQuery($InsertSql);' );
  AddRightBraces;
end;

procedure TTableBaseCreater.AddTableClass;
begin
  AddLine( 'class ' + FaceTableFrameApi.ReadTableName );
  AddLeftBraces;
  AddTableAddFunction;
  AddEmptyLine;
  AddTableRemoveFunction;
  AddRightBraces;
end;

procedure TTableBaseCreater.AddTableRemoveFunction;
var
  KeyList : TStringList;
  i: Integer;
  ColumnStr : string;
begin
  AddLine( '// ɾ������' );
  AddLine( 'static function Remove(' + SqlUtil.ReadKeyValue + ')' );
  AddLeftBraces;
  AddLine( '// Sql ����' );
  AddLine( '$TableSql = ' + TableBaseName + '::$TableSql;' );
  KeyList := FaceColumnApi.ReadKeyList;
  for i := 0 to KeyList.Count - 1 do
  begin
    ColumnStr := KeyList[i];
    ColumnStr := '$' + ColumnStr + Str_Sql + ' = ' + TableBaseName + '::$' + ColumnStr + Str_Sql + ';';
    AddLine( ColumnStr );
  end;
  KeyList.Free;
  AddEmptyLine;
  AddLine( '// ��װ Sql ���' );
  AddLine( '$DeleteSql = "delete from $TableSql ' + SqlUtil.ReadKeyWhere + ';";' );
  AddEmptyLine;
  AddLine( '// ִ�� Sql ���' );
  AddLine( 'TrialListBase::SqlQuery($DeleteSql);' );
  AddRightBraces;
end;

procedure TTableBaseCreater.AddUpdateColQuery;
begin
  AddLine( '// ����ĳһ�е�ֵ(�ַ���)' );
  AddLine( Str_StaticFunction + ' UpdateColQuery(' + SqlUtil.ReadKeyValue + ', $ColName, $ColValue)' );
  AddLeftBraces;
  AddLine( '// Sql ����' );
  AddLine( '$TableSql = ' + TableBaseName + '::$TableSql;' );
  AddLine( SqlUtil.ReadKeySqlDefine( TabStr ) );
  AddEmptyLine;
  AddLine( '// ��װ Sql ���' );
  AddLine( '$UpgradeSql = "update $TableSql set $ColName = ''$ColValue''' + SqlUtil.ReadKeyWhere + '";' );
  AddEmptyLine;
  AddLine( '// ִ�� Sql ���' );
  AddLine( 'TrialListBase::SqlQuery($UpgradeSql);' );
  AddRightBraces;
end;

procedure TTableBaseCreater.AddClassBase;
begin
  AddLine( 'class ' + TableBaseName );
  AddLeftBraces;
  AddColumnSql;
  AddEmptyLine;
  AddGetConn;
  AddEmptyLine;
  AddSqlQuery;
  AddEmptyLine;
  AddSelectColQuery;
  AddEmptyLine;
  AddUpdateColQuery;
  AddRightBraces;
end;

procedure TTableBaseCreater.AddColumnSql;
var
  ColumnList : TStringList;
  i: Integer;
  ColumnStr : string;
begin
  AddLine( '// ����' );
  AddLine( Str_StaticVar + 'TableSql = ''' + FaceTableFrameApi.ReadTableName + ''';' );
  ColumnList := FaceColumnApi.ReadColumnList;
  for i := 0 to ColumnList.Count - 1 do
  begin
    ColumnStr := ColumnList[i];
    ColumnStr := Str_StaticVar + ColumnStr + Str_Sql + ' = ''' + ColumnStr + ''';';
    AddLine( ColumnStr );
  end;
  ColumnList.Free;
end;

procedure TTableBaseCreater.AddEmptyLine;
begin
  AddLine( '' );
end;

procedure TTableBaseCreater.AddGetConn;
begin
  AddLine( '// ��ȡ���ݿ�����' );
  AddLine( Str_StaticFunction + ' getConn()' );
  AddLeftBraces;
  AddLine( 'return DataBaseConfigInfo::getConn();' );
  AddRightBraces;
end;

procedure TTableBaseCreater.AddLeftBraces;
begin
  AddLine( '{' );
  AddTab;
end;

procedure TTableBaseCreater.AddLine(Str: string);
begin
  CreateStr := CreateStr + TabStr + Str + #13#10;
end;

procedure TTableBaseCreater.AddRightBraces;
begin
  RemoveTab;
  AddLine( '}' );
end;

procedure TTableBaseCreater.AddSelectColQuery;
begin
  AddLine( '// ��ѯĳһ�е�ֵ(�ַ���)' );
  AddLine( Str_StaticFunction + ' SelectColQuery(' + SqlUtil.ReadKeyValue + ', $ColNameArray)' );
  AddLeftBraces;
  AddLine( '// Sql ����' );
  AddLine( '$TableSql = ' + TableBaseName + '::$TableSql;' );
  AddLine( SqlUtil.ReadKeySqlDefine( TabStr ) );
  AddEmptyLine;
  AddLine( '// Ҫ��ȡ����' );
  AddLine( '$SelectParams = "";' );
  AddLine( 'for($i=0;$i<count($ColNameArray);$i++)' );
  AddLeftBraces;
  AddLine( 'if($SelectParams <> '''')' );
  AddLeftBraces;
  AddLine( '$SelectParams = $SelectParams . ", ";' );
  AddRightBraces;
  AddLine( '$SelectParams = $SelectParams . $ColNameArray[$i]; ' );
  AddRightBraces;
  AddEmptyLine;
  AddLine( '// ��װ Sql ���' );
  AddLine( '$SelectSql = "select $SelectParams from $TableSql' + SqlUtil.ReadKeyWhere + '";' );
  AddEmptyLine;
  AddLine( '// ִ�� Sql ���' );
  AddLine( '$con = ' + TableBaseName + '::getConn();' );
  AddLine( '$result = mysqli_query($con, $SelectSql);' );
  AddLine( 'if ($row = mysqli_fetch_array($result))' );
  AddLeftBraces;
  AddLine( 'for($i=0;$i<count($ColNameArray);$i++)' );
  AddLeftBraces;
  AddLine( '$QueryResult[$i] = $row[$ColNameArray[$i]];' );
  AddRightBraces;
  AddRightBraces;
  AddLine( 'mysqli_close($con);' );
  AddEmptyLine;
  AddLine( 'return $QueryResult;' );
  AddRightBraces;
end;

procedure TTableBaseCreater.AddSqlQuery;
begin
  AddLine( '// ִ�� Sql' );
  AddLine( Str_StaticFunction + ' SqlQuery($SqlStr)' );
  AddLeftBraces;
  AddLine( '// ִ�� Sql ���' );
  AddLine( '$con = ' + TableBaseName + '::getConn();' );
  AddLine( 'mysqli_query($con, $SqlStr);' );
  AddLine( 'mysqli_close($con);' );
  AddRightBraces;
end;

constructor TTableBaseCreater.Create;
begin
  CreateStr := '';
  TabStr := '';
  TableBaseName := FaceTableFrameApi.ReadTableBaseName;
end;

procedure TTableBaseCreater.Update;
begin
    // �������ݿ�����
  AddLine( '<?php' );
  AddEmptyLine;
  AddLine( 'include ''DataBaseConfigInfo.php'';' );
  AddEmptyLine;

    // ��� ����
  AddClassBase;
  AddEmptyLine;

    // ��� ����
  AddTableClass;
  AddEmptyLine;
  AddLine( '?>' );

    // ��ʾ���ɽ��
  FaceCreaterResultApi.ShowResult( CreateStr );
end;

procedure TTableBaseCreater.RemoveTab;
begin
  TabStr := Copy( TabStr, 1, length( TabStr ) - length( Str_Tab ) );
end;

{ TFaceCreaterResultApi }

procedure TFaceCreaterResultApi.Activate(_mmoResult: TMemo);
begin
  mmoResult := _mmoResult;
end;

procedure TFaceCreaterResultApi.ShowResult(ResultStr: string);
begin
  mmoResult.Text := ResultStr;
end;

{ UserSqlCreateApi }

class procedure UserSqlCreateApi.CreateTableBase;
var
  TableBaseCreater : TTableBaseCreater;
begin
  TableBaseCreater := TTableBaseCreater.Create;
  TableBaseCreater.Update;
  TableBaseCreater.Free;
end;

{ SqlUtil }

class function SqlUtil.ReadColInsertValue: string;
var
  ColumnList : TStringList;
  i: Integer;
begin
  ColumnList := FaceColumnApi.ReadColumnList;
  Result := '';
  for i := 0 to ColumnList.Count - 1 do
  begin
    if Result <> '' then
      Result := Result + ', ';
    Result := Result + '''$' + ColumnList[i] + '''';
  end;
  ColumnList.Free;
end;

class function SqlUtil.ReadKeySqlDefine( TabStr : string ): string;
var
  TableBaseName : string;
  KeyList : TStringList;
  i: Integer;
begin
  TableBaseName := FaceTableFrameApi.ReadTableBaseName;

  KeyList := FaceColumnApi.ReadKeyList;
  Result := '';
  for i := 0 to KeyList.Count - 1 do
  begin
    if Result <> '' then
      Result := Result + #13#10 + TabStr;
    Result := Result + '$' + KeyList[i] + Str_Sql + ' = ' + TableBaseName + '::$' + KeyList[i] + Str_Sql + ';';
  end;
  KeyList.Free;
end;

class function SqlUtil.ReadColSqlValue: string;
var
  ColumnList : TStringList;
  i: Integer;
begin
  ColumnList := FaceColumnApi.ReadColumnList;
  Result := '';
  for i := 0 to ColumnList.Count - 1 do
  begin
    if Result <> '' then
      Result := Result + ', ';
    Result := Result + '$' + ColumnList[i] + Str_Sql;
  end;
  ColumnList.Free;
end;

class function SqlUtil.ReadColValue: string;
var
  ColumnList : TStringList;
  i: Integer;
begin
  ColumnList := FaceColumnApi.ReadColumnList;
  Result := '';
  for i := 0 to ColumnList.Count - 1 do
  begin
    if Result <> '' then
      Result := Result + ', ';
    Result := Result + '$' + ColumnList[i];
  end;
  ColumnList.Free;
end;

class function SqlUtil.ReadKeyValue: string;
var
  KeyList : TStringList;
  i: Integer;
begin
  KeyList := FaceColumnApi.ReadKeyList;
  Result := '';
  for i := 0 to KeyList.Count - 1 do
  begin
    if Result <> '' then
      Result := Result + ', ';
    Result := Result + '$' + KeyList[i];
  end;
  KeyList.Free;
end;

class function SqlUtil.ReadKeyWhere: string;
var
  KeyList : TStringList;
  i: Integer;
begin
  KeyList := FaceColumnApi.ReadKeyList;
  Result := ' where ';
  for i := 0 to KeyList.Count - 1 do
  begin
    if Result <> ' where ' then
      Result := Result + ' and ';
    Result := Result + '( ';
    Result := Result + '$' + KeyList[i] + Str_Sql + ' = ''' + '$' + KeyList[i] + '''';
    Result := Result + ' )';
  end;
  KeyList.Free;
end;

end.
