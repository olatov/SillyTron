program SillyAsm;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, StrUtils, StreamEx, SillyAsmProcessor;

var
  InputFilename: String;
  InputStream: TFileStream;
  OutputStream: TFileStream;
  Processor: TSillyAsmProcessor;

begin
  InputFilename := ParamStr(1);
  if InputFilename.IsEmpty then
  begin
    Writeln('Missing input filename.');
    Halt(1);
  end;

  if not FileExists(InputFilename) then
  begin
    Writeln(Format('Input file %s not found.', [InputFilename]));
    Halt(1);
  end;

  InputStream := TFileStream.Create(InputFilename, fmOpenRead);
  OutputStream := TFileStream.Create(
    ChangeFileExt(InputFilename, '.bin'), fmCreate);

  try
    Processor := TSillyAsmProcessor.Create(InputStream, OutputStream);
    try
      Processor.Run();
    except
      on E: ESillyAsmError do
        begin
          WriteLn(Format('Line %d: %s', [E.LineNumber, E.Message]));
          WriteLn(Format('>> %s', [E.LineContents]));
          OutputStream.Size := 0;
          Halt(1);
        end;
    end;

    WriteLn(
      Format(
        'Written %s (%d bytes).',
        [OutputStream.FileName, OutputStream.Size]));
  finally
    FreeAndNil(Processor);
    FreeAndNil(InputStream);
    FreeAndNil(OutputStream);
  end;
end.

