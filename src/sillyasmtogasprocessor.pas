unit sillyasmtogasprocessor;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, FGL;

type

  { ESillyAsmError }

  ESillyAsmError = class(Exception)
  private
    FLineNumber: LongWord;
    FLineContents: String;
  public
    constructor Create(AMessage: String; ALineNumber: LongWord; ALineContents: String);
    property LineNumber: LongWord read FLineNumber;
    property LineContents: String read FLineContents;
  end;

  TCodeSection = (
    csText,
    csData
  );

  TCodeChunk = record
    Section: TCodeSection;
    Lines: TStringList;
    StandardLibraryDependencies: TStringArray;
  end;

  { TSillyAsmToGasProcessor }

  TSillyAsmToGasProcessor = class
  private
    FVerbose: Boolean;
    FInputStream: TStream;
    FOutputStream: TStream;
    function ProcessLine(ALine: String; ALineNumber: LongWord): TCodeChunk;
    procedure ProcessLines();
    function GetStandardLibraryRoutine(AName: String): String;
    function GetResourceContents(AResourceName: String): String;
    function ResolveInternalDependencies(ARoutines: TStringList): TStringList;
  public
    constructor Create(
      const AInputStream, AOutputStream: TStream;
      AVerbose: Boolean = false);
    procedure Run();
  end;

implementation

{$R sillyasmtogas.rc}

uses
  Types, StrUtils, StreamEx, SillyMachine;

{ ESillyAsmError }

constructor ESillyAsmError.Create(
  AMessage: String; ALineNumber: LongWord; ALineContents: String);
begin
  Message := AMessage;
  FLineNumber := ALineNumber;
  FLineContents := ALineContents;
end;

{ TSillyAsmToGasProcessor }

function TSillyAsmToGasProcessor.ProcessLine(ALine: String;
  ALineNumber: LongWord): TCodeChunk;
var
  Tokens: TStringDynArray;
  StringLiteral: String;
begin
  Result.Section := csText;
  Result.Lines := TStringList.Create();
  Result.StandardLibraryDependencies := [];

  Tokens := ALine.ToLower().Split(
    [' ', #9],
    TStringSplitOptions.ExcludeEmpty);

  if (Length(Tokens) = 0) or Tokens[0].StartsWith('//') then
    Exit;

  if Tokens[0].EndsWith(':') then
  begin
    Result.Lines.Add(Tokens[0]);
    Exit;
  end;

  case Tokens[0] of
    'nop':
      Result.Lines.Add('nop');

    'load':
      begin
        Result.Lines.Add('xorq %rax, %rax');
        if Tokens[1] = 'idx' then
          Result.Lines.Add('movq %rbx, %rax')
        else if Tokens[1] = '@idx' then
          Result.Lines.Add('movw (%rbx), %ax')
        else if Tokens[1].StartsWith('$') then
          Result.Lines.Add('movw %s(%%rip), %%ax', [Tokens[1].TrimLeft('$')])
        else if Tokens[1].StartsWith('@') then
          Result.Lines.Add(Format('leaq %s(%%rip), %%rax', [Tokens[1].TrimLeft('@')]))
        else
          Result.Lines.Add(Format('movw $%d, %%ax', [Tokens[1].ToInteger()]));
      end;

    'store':
      begin
        if Tokens[1] = 'idx' then
          Result.Lines.Add('movq %rax, %rbx')
        else if Tokens[1] = '@idx' then
          Result.Lines.Add('movw %ax, (%rbx)')
        else if Tokens[1].StartsWith('$') then
          Result.Lines.Add('movw %%ax, %s(%%rip)', [Tokens[1].TrimLeft('$')])
        else
          raise Exception.Create('Missing operand.');
      end;

    'add':
      begin
        if Tokens[1] = '@idx' then
          Result.Lines.Add('addw (%rbx), %ax')
        else if Tokens[1].StartsWith('$') then
          Result.Lines.Add(Format('addw %s(%%rip), %%ax', [Tokens[1].TrimLeft('$')]))
        else
          Result.Lines.Add(Format('addw $%d, %%ax', [Tokens[1].ToInteger()]));
      end;

    'subtract':
      begin
        if Tokens[1] = '@idx' then
          Result.Lines.Add('subw (%rbx), %ax')
        else if Tokens[1].StartsWith('$') then
          Result.Lines.Add(Format('subw %s(%%rbx), %ax', [Tokens[1].TrimLeft('$')]))
        else
          Result.Lines.Add(Format('subw $%d, %%ax', [Tokens[1].ToInteger()]));
      end;

    'negate':
      Result.Lines.Add('negw %ax');

    'inc':
      begin
        if Length(Tokens) < 2 then
          raise Exception.Create('Missing operand.');
        if Tokens[1] = 'acc' then
          Result.Lines.Add('incw %ax')
        else if Tokens[1] = 'idx' then
          Result.Lines.Add('incw %bx')
        else
          raise Exception.Create(Format('Invalid argument: %s.', [Tokens[1]]));
      end;

    'dec':
      begin
        if Length(Tokens) < 2 then
          raise Exception.Create('Missing operand.');
        if Tokens[1] = 'acc' then
          Result.Lines.Add('decw %ax')
        else if Tokens[1] = 'idx' then
          Result.Lines.Add('decw %bx')
        else
          raise Exception.Create(Format('Invalid argument: %s.', [Tokens[1]]));
      end;

    'jump':
      Result.Lines.Add(Format('jmp %s', [Tokens[1]]));

    'jumpifzero':
      Result.Lines.Add(Format('jz %s', [Tokens[1]]));

    'jumpifsign':
      Result.Lines.Add(Format('js %s', [Tokens[1]]));

    'jumpifnotzero':
      Result.Lines.Add(Format('jnz %s', [Tokens[1]]));

    'jumpifnotsign':
      Result.Lines.Add(Format('jns %s', [Tokens[1]]));

    'push':
      Result.Lines.Add('pushq %rax');

    'pop':
      Result.Lines.Add('popq %rax');

    'call':
      Result.Lines.Add(Format('call %s', [Tokens[1]]));

    'return':
      Result.Lines.Add('ret');

    'printchar':
      begin
        Result.Lines.Add('call __Std__PrintChar');
        Result.StandardLibraryDependencies := ['__Std__PrintChar'];
      end;

    'printinteger':
      begin
        Result.Lines.Add('call __Std__PrintInteger');
        Result.StandardLibraryDependencies := ['__Std__PrintInteger'];
      end;

    'printstring':
      begin
        Result.Lines.Add('call __Std__PrintString');
        Result.StandardLibraryDependencies := ['__Std__PrintString'];
      end;

    'inputinteger':
      begin
        Result.Lines.Add('call __Std__InputInteger');
        Result.StandardLibraryDependencies := ['__Std__InputInteger'];
      end;

    'sleep':
      begin
        Result.Lines.Add('call __Std__Sleep');
        Result.StandardLibraryDependencies := ['__Std__Sleep'];
      end;

    'halt':
      begin
        // Syscall: exit (syscall number 60)
        Result.Lines.AddStrings([
          'movl $60, %eax',
          'xorl %edi, %edi',
          'syscall'
        ]);
      end;

    'var':
      begin
        Result.Section := csData;

        case Tokens[2] of
          'integer':
            Result.Lines.Add(Format('%s: .word %d',
              [Tokens[1], Tokens[3].ToInteger()]));

          'string':
            begin
              StringLiteral := ExtractDelimited(2, ALine, ['''']);
              Result.Lines.Add(Format('%s: .asciz "%s"',
                [Tokens[1], StringLiteral]));
            end;
        end;
        Exit;
      end;
    else
      raise Exception.Create('Syntax error.');
  end;
end;

procedure TSillyAsmToGasProcessor.ProcessLines();
var
  InputReader: TStreamReader;
  OutputWriter: TStreamWriter;
  LineCounter: LongWord;
  Line: String;
  StandardLibraryDependencies: TStringList;
  InternalDependencies: TStringList;
  DataSection: TStringList;
  CodeChunk: TCodeChunk;
  RoutineName: String;
begin
  InputReader := TStreamReader.Create(FInputStream);
  OutputWriter := TStreamWriter.Create(FOutputStream);

  StandardLibraryDependencies := TStringList.Create();
  StandardLibraryDependencies.Sorted := true;
  StandardLibraryDependencies.Duplicates := dupIgnore;

  // Text (code) section
  if FVerbose then
    OutputWriter.WriteLine('# Text (code) section');
  OutputWriter.WriteLine(GetResourceContents('SRC_SECTION_TEXT'));

  try
    LineCounter := 0;
    DataSection := TStringList.Create();
    while not InputReader.Eof do
    begin
      Inc(LineCounter);
      try
        Line := InputReader.ReadLine().Trim();
        if String.IsNullOrWhiteSpace(Line) then
          continue;

        CodeChunk := ProcessLine(Line, LineCounter);
        if Assigned(CodeChunk.Lines) then
        begin
          case CodeChunk.Section of
            csText:
              begin
                if FVerbose then
                begin
                  OutputWriter.WriteLine('# [%d]: %s', [LineCounter, Line]);
                  OutputWriter.WriteLine(CodeChunk.Lines.Text);
                end
                else
                if not String.IsNullOrWhiteSpace(CodeChunk.Lines.Text) then
                  OutputWriter.WriteLine(CodeChunk.Lines.Text.Trim());
              end;
            csData:
              DataSection.AddStrings(CodeChunk.Lines);
          end;
          FreeAndNil(CodeChunk.Lines);
          StandardLibraryDependencies.AddStrings(CodeChunk.StandardLibraryDependencies);
        end;
      except
        on E: Exception do
          raise ESillyAsmError.Create(E.Message, LineCounter, Line);
      end;
    end;

    // Write std routines
    if StandardLibraryDependencies.Count > 0 then
    begin
      if FVerbose then
        OutputWriter.WriteLine('# Standard Library routines');

      InternalDependencies := ResolveInternalDependencies(StandardLibraryDependencies);
      StandardLibraryDependencies.AddStrings(InternalDependencies);
      FreeAndNil(InternalDependencies);

      for RoutineName in StandardLibraryDependencies do
        OutputWriter.WriteLine(GetStandardLibraryRoutine(RoutineName));
    end;

    // Data section
    if FVerbose then
      OutputWriter.WriteLine('# Data section');
    OutputWriter.WriteLine(GetResourceContents('SRC_SECTION_DATA'));

    if DataSection.Count > 0 then
      OutputWriter.WriteLine(DataSection.Text);

  finally
    FreeAndNil(StandardLibraryDependencies);
    FreeAndNil(OutputWriter);
    FreeAndNil(InputReader);
  end;
end;

function TSillyAsmToGasProcessor.GetStandardLibraryRoutine(
  AName: String): String;
var
  ResourceName: String;
begin
  ResourceName := 'LIB' + AName.ToUpper();
  try
    Result := GetResourceContents(ResourceName);
  except
    on E: EResNotFound do
      raise Exception.Create(
        Format('Internal error: library routine "%s" not known.', [AName]));
  end;
end;

function TSillyAsmToGasProcessor.GetResourceContents(
  AResourceName: String): String;
var
  ResourceStream: TResourceStream;
  Content: TStringList;
begin
  ResourceStream := TResourceStream.Create(HInstance, AResourceName, RT_RCDATA);
  Content := TStringList.Create();
  try
    Content.LoadFromStream(ResourceStream);
    Result := Content.Text;
  finally
    FreeAndNil(ResourceStream);
    FreeAndNil(Content)
  end;
end;

function TSillyAsmToGasProcessor.ResolveInternalDependencies(
  ARoutines: TStringList): TStringList;
var
  RoutineName: String;
  OldResultCount: Integer;
begin
  Result := TStringList.Create();
  Result.Sorted := true;
  Result.Duplicates := TDuplicates.dupIgnore;

  repeat
    OldResultCount := Result.Count;
    for RoutineName in ARoutines do
      case RoutineName of
        '__Std__PrintString', '__Std__PrintInteger', '__Std__InputInteger':
          Result.Add('__Std__PrintChar');
      end;
  until Result.Count = OldResultCount;
end;

constructor TSillyAsmToGasProcessor.Create(
  const AInputStream, AOutputStream: TStream;
  AVerbose: Boolean = false);
begin
  FInputStream := AInputStream;
  FOutputStream := AOutputStream;
  FVerbose := AVerbose;
end;

procedure TSillyAsmToGasProcessor.Run();
begin
  ProcessLines();
end;

end.

