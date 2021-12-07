program AdventOfCode2021;

uses
  Vcl.Forms,
  Main in 'Main.pas' {Form1},
  AOCBase in 'AOCBase.pas',
  AOCSolutions in 'AOCSolutions.pas' {$R *.res},
  uAOCUtils in 'uAOCUtils.pas',
  uAOCTests in 'uAOCTests.pas',
  uAocConfig in 'uAocConfig.pas',
  uIntComputers in 'uIntComputers.pas';

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
