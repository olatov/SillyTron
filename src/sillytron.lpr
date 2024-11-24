program SillyTron;

{$mode objfpc}
{$modeswitch typehelpers}

uses
  Classes, SysUtils, SillyMachine;

var
  ProgamStream: TBytesStream;
  InputFilename: String;
  Machine: TMachine;

begin
  InputFilename := ParamStr(1);
  if InputFilename.IsEmpty then
  begin
    Writeln('Missing program file.');
    Halt(1);
  end;

  ProgamStream := TBytesStream.Create([]);
  ProgamStream.LoadFromFile(InputFilename);
  Machine := TMachine.Create();

  Machine.LoadData(0, ProgamStream);
  FreeAndNil(ProgamStream);

  Machine.Run();

  Writeln();
  Machine.PrintState();

  FreeAndNil(Machine);
end.

