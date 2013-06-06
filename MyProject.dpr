program MyProject;

uses
  Vcl.Forms,
  UMainForm in 'UMainForm.pas' {frmMain},
  UFrameTable in 'UFrameTable.pas' {FrameTable: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
