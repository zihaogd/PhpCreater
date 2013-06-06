unit UMainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, RzTabs, IniFiles,
  Vcl.Menus, Vcl.ExtCtrls, Vcl.ImgList;

type

  TfrmMain = class(TForm)
    PcTable: TRzPageControl;
    pmTable: TPopupMenu;
    miNew: TMenuItem;
    plProject: TPanel;
    cbbProject: TComboBox;
    plTitle: TPanel;
    il16: TImageList;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure cbbProjectSelect(Sender: TObject);
    procedure miNewClick(Sender: TObject);
    procedure PcTablePageChange(Sender: TObject);
  private
    function ReadIniFilePath : string;
    procedure SaveIni;
    procedure LoadIni;
    procedure SaveTableIni;
    procedure LoadTableIni;
  private
    procedure MainFormIni;
    procedure MainFornUnini;
  public
    { Public declarations }
  end;

{$Region ' 界面 操作 ' }

    // 项目 界面操作
  TFaceProjectApi = class
  private
    CbbProject : TComboBox;
  public
    constructor Create;
  public
    procedure AddProject( ProjectName : string );
    procedure SelectProject( ProjectName : string );
    function ReadIsExist( ProjectName : string ): Boolean;
  private
    function ReadIndex( ProjectName : string ): Integer;
  end;

    // 表格 界面操作
  TFaceTableApi = class
  private
    PcTable : TRzPageControl;
  public
    constructor Create;
  public
    procedure AddTable( TableName : string );
    procedure SelectTable( TableName : string );
    function ReadIsExist( TableName : string ): Boolean;
    procedure ClearTables;
    function ReadTableList : TStringList;
    procedure EnterTable;
  private
    function ReadTable( TableName : string ): TRzTabSheet;
  end;

{$EndRegion}

{$Region ' 用户 操作 ' }

    // 项目 用户操作
  UserProjectApi = class
  public
    class procedure NewProject;
  end;

    // 表格 用户操作
  UserTableApi = class
  public
    class procedure NewTable;
  end;

{$EndRegion}

const
  Project_New = 'New...';

const
  Ini_Main = 'Main';
  Ini_ProjectCount = 'ProjectCount';
  Ini_Project = 'Project';
  Ini_SelectProject = 'SelectProject';

  Ini_TableCount = 'TableCount';
  Ini_Table = 'Table';
  Ini_SelectTable = 'SelectTable';

var
  FaceProjectApi : TFaceProjectApi;
  FaceTableApi : TFaceTableApi;

var
  frmMain: TfrmMain;

implementation

uses UFrameTable;

{$R *.dfm}

{ UseProjectApi }

class procedure UserProjectApi.NewProject;
var
  ProjectName : string;
begin
    // 用户输入
  if not InputQuery( 'New Project', 'Project Name', ProjectName ) then
  begin
    FaceProjectApi.SelectProject( '' );
    Exit;
  end;

    // 项目已存在
  if FaceProjectApi.ReadIsExist( ProjectName ) then
  begin
    FaceProjectApi.SelectProject( ProjectName );
    Exit;
  end;

    // 创建项目并选中
  FaceProjectApi.AddProject( ProjectName );
  FaceProjectApi.SelectProject( ProjectName );
end;

{ FaceProjectApi }

procedure TFaceProjectApi.AddProject(ProjectName: string);
begin
  CbbProject.Items.Insert( 0, ProjectName );
end;

constructor TFaceProjectApi.Create;
begin
  CbbProject := frmMain.cbbProject;
end;

function TFaceProjectApi.ReadIndex(ProjectName: string): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to CbbProject.Items.Count - 1 do
    if CbbProject.Items[i] = ProjectName then
    begin
      Result := i;
      Break;
    end;
end;

function TFaceProjectApi.ReadIsExist(ProjectName: string): Boolean;
begin
  Result := ReadIndex( ProjectName ) >= 0;
end;

procedure TFaceProjectApi.SelectProject(ProjectName: string);
var
  ProjectIndex : Integer;
begin
  ProjectIndex := ReadIndex( ProjectName );
  if ProjectIndex >= 0 then
    CbbProject.ItemIndex := ProjectIndex
  else
    CbbProject.ItemIndex := -1;
end;

procedure TfrmMain.cbbProjectSelect(Sender: TObject);
begin
  if cbbProject.Text = Project_New then
    UserProjectApi.NewProject
  else
    LoadTableIni;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  MainFormIni;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  MainFornUnini;
end;

procedure TfrmMain.LoadIni;
var
  IniFile : TIniFile;
  ProjectCount, i: Integer;
  ProjectName, SelectProject : string;
begin
  IniFile := TIniFile.Create( ReadIniFilePath );
  ProjectCount := IniFile.ReadInteger( Ini_Main, Ini_ProjectCount, 0 );
  for i := ProjectCount - 1 downto 0 do
  begin
    ProjectName := IniFile.ReadString( Ini_Main, Ini_Project + IntToStr( i ), '' );
    if ProjectName <> '' then
      FaceProjectApi.AddProject( ProjectName );
  end;
  SelectProject := IniFile.ReadString( Ini_Main, Ini_SelectProject, '' );
  if SelectProject <> '' then
  begin
    FaceProjectApi.SelectProject( ProjectName );
    LoadTableIni;
  end;
  IniFile.Free;
end;

procedure TfrmMain.LoadTableIni;
var
  ProjectName, TableName, SelectTable : string;
  IniFile : TIniFile;
  TableCount, i: Integer;
begin
  ProjectName := cbbProject.Text;

  FaceTableApi.ClearTables;
  IniFile := TIniFile.Create( ReadIniFilePath );
  TableCount := IniFile.ReadInteger( Ini_Main, ProjectName + Ini_TableCount, 0 );
  for i := 0 to TableCount - 1 do
  begin
    TableName := IniFile.ReadString( Ini_Main, ProjectName + Ini_Table + IntToStr( i ), '' );
    if TableName = '' then
      Continue;
    FaceTableApi.AddTable( TableName );
  end;
  SelectTable := IniFile.ReadString( Ini_Main, ProjectName + Ini_SelectTable, '' );
  if SelectTable <> '' then
    FaceTableApi.SelectTable( SelectTable );
  IniFile.Free;
end;

procedure TfrmMain.MainFormIni;
begin
  FaceProjectApi := TFaceProjectApi.Create;
  FaceTableApi := TFaceTableApi.Create;
  FaceTableFrameApi := TFaceTableFrameApi.Create;
  FaceColumnApi := TFaceColumnApi.Create;
  FaceCreaterResultApi := TFaceCreaterResultApi.Create;
  LoadIni;
end;

procedure TfrmMain.MainFornUnini;
begin
  SaveIni;
  FaceCreaterResultApi.Free;
  FaceColumnApi.Free;
  FaceTableFrameApi.Free;
  FaceTableApi.Free;
  FaceProjectApi.Free;
end;

procedure TfrmMain.miNewClick(Sender: TObject);
begin
  UserTableApi.NewTable;
  SaveTableIni;
end;

procedure TfrmMain.PcTablePageChange(Sender: TObject);
begin
  FaceTableApi.EnterTable;
end;

function TfrmMain.ReadIniFilePath: string;
begin
  Result := 'D:\临时文件夹\Test.ini';
end;

procedure TfrmMain.SaveIni;
var
  IniFile : TIniFile;
  i: Integer;
begin
  IniFile := TIniFile.Create( ReadIniFilePath );
  IniFile.WriteInteger( Ini_Main, Ini_ProjectCount, cbbProject.Items.Count - 1 );
  for i := 0 to cbbProject.Items.Count - 2 do
    IniFile.WriteString( Ini_Main, Ini_Project + IntToStr( i ), cbbProject.Items[i] );
  IniFile.WriteString( Ini_Main, Ini_SelectProject, cbbProject.Text );
  IniFile.Free;
end;

procedure TfrmMain.SaveTableIni;
var
  ProjectName : string;
  IniFile : TIniFile;
  i: Integer;
begin
  ProjectName := cbbProject.Text;

  IniFile := TIniFile.Create( ReadIniFilePath );
  IniFile.WriteInteger( Ini_Main, ProjectName + Ini_TableCount, PcTable.PageCount );
  for i := 0 to PcTable.PageCount - 1 do
    IniFile.WriteString( Ini_Main, ProjectName + Ini_Table + IntToStr( i ), PcTable.Pages[i].Caption );
  if Assigned( PcTable.ActivePage ) then
    IniFile.WriteString( Ini_Main, ProjectName + Ini_SelectTable, PcTable.ActivePage.Caption );
  IniFile.Free;
end;

{ TFaceProjectTabApi }

procedure TFaceTableApi.AddTable(TableName: string);
var
  tsTable : TRzTabSheet;
  FrameTable : TFrameTable;
begin
  tsTable := TRzTabSheet.Create( PcTable );
  tsTable.Parent := PcTable;
  tsTable.PageControl := PcTable;
  tsTable.Caption := TableName;
  tsTable.ImageIndex := 0;

  FrameTable := TFrameTable.Create( tsTable );
  FrameTable.Parent := tsTable;
  FrameTable.Align := alClient;
  FrameTable.Padding.Top := 5;
  FrameTable.TableName := TableName;
  FrameTable.ProjectName := frmMain.cbbProject.Text;
  FrameTable.LoadColumnIni;
end;

procedure TFaceTableApi.ClearTables;
var
  i: Integer;
begin
  for i := PcTable.PageCount - 1 downto 0 do
    PcTable.Pages[i].Free;
end;

constructor TFaceTableApi.Create;
begin
  PcTable := frmMain.PcTable;
end;

procedure TFaceTableApi.EnterTable;
var
  TsTable : TRzTabSheet;
  FrameTable : TFrameTable;
  i: Integer;
  c : TControl;
begin
  if not Assigned( PcTable.ActivePage ) then
    Exit;
  TsTable := PcTable.ActivePage;
  for i := 0 to TsTable.ControlCount - 1 do
  begin
    c := TsTable.Controls[i];
    if c is TFrameTable then
    begin
      FrameTable := c as TFrameTable;
      FaceTableFrameApi.Activate( FrameTable );
      Break;
    end;
  end;
end;

function TFaceTableApi.ReadIsExist(TableName: string): Boolean;
begin
  Result := Assigned( ReadTable( TableName ) );
end;

function TFaceTableApi.ReadTable(TableName: string): TRzTabSheet;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to PcTable.PageCount - 1 do
    if PcTable.Pages[i].Caption = TableName then
    begin
      Result := PcTable.Pages[i];
      Break;
    end;
end;

function TFaceTableApi.ReadTableList: TStringList;
var
  i: Integer;
begin
  Result := TStringList.Create;
  for i := 0 to PcTable.PageCount - 1 do
    Result.Add( PcTable.Pages[i].Caption );
end;

procedure TFaceTableApi.SelectTable(TableName: string);
var
  tsTable : TRzTabSheet;
begin
  tsTable := ReadTable( TableName );
  if Assigned( tsTable ) then
    PcTable.ActivePage := tsTable;
end;

{ UseTableApi }

class procedure UserTableApi.NewTable;
var
  TableName : string;
begin
    // 用户输入
  if not InputQuery( 'New Table', 'Table Name', TableName ) then
    Exit;

    // 表格已存在
  if FaceTableApi.ReadIsExist( TableName ) then
  begin
    FaceTableApi.SelectTable( TableName );
    Exit;
  end;

    // 创建并选中表格
  FaceTableApi.AddTable( TableName );
  FaceTableApi.SelectTable( TableName );
end;

end.
