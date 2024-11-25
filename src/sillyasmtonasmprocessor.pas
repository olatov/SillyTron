unit SillyAsmToNasmProcessor;

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
  end;

  { TSillyAsmToNasmProcessor }

  TSillyAsmToNasmProcessor = class
  private
    FVerbose: Boolean;
    FInputStream: TStream;
    FOutputStream: TStream;
    function ProcessLine(ALine: String; ALineNumber: LongWord): TCodeChunk;
    procedure ProcessLines();
    function GetStandardLibrary(): TStringList;
  public
    constructor Create(
      const AInputStream, AOutputStream: TStream;
      AVerbose: Boolean = false);
    procedure Run();
  end;

implementation

uses
  Types, StrUtils, StreamEx, SillyMachine;

type
  TParamType = (
    ptNone,
    ptConst,
    ptReference
  );

  TInstruction = record
    Operation: TOperation;
    ParamType: TParamType;
    ParamValue: String;
  end;

{ ESillyAsmError }

constructor ESillyAsmError.Create(
  AMessage: String; ALineNumber: LongWord; ALineContents: String);
begin
  Message := AMessage;
  FLineNumber := ALineNumber;
  FLineContents := ALineContents;
end;

{ TSillyAsmToNasmProcessor }

function TSillyAsmToNasmProcessor.ProcessLine(ALine: String;
  ALineNumber: LongWord): TCodeChunk;
var
  Tokens: TStringDynArray;
  StringLiteral: String;
begin
  Result.Section := csText;
  Result.Lines := TStringList.Create();

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
        Result.Lines.Add('xor rax, rax');
        if Tokens[1] = 'idx' then
          Result.Lines.Add('mov rax, rbx')
        else if Tokens[1] = '@idx' then
          Result.Lines.Add('mov ax, [rbx]')
        else if Tokens[1].StartsWith('$') then
          Result.Lines.Add('mov ax, [%s]', [Tokens[1].TrimLeft('$')])
        else if Tokens[1].StartsWith('@') then
          Result.Lines.Add(Format('lea rax, [%s]', [Tokens[1].TrimLeft('@')]))
        else
          Result.Lines.Add(Format('mov ax, %d', [Tokens[1].ToInteger()]));
      end;

    'store':
      begin
        if Tokens[1] = 'idx' then
          Result.Lines.Add('mov rbx, rax')
        else if Tokens[1] = '@idx' then
          Result.Lines.Add('mov [rbx], ax')
        else if Tokens[1].StartsWith('$') then
          Result.Lines.Add('mov [%s], ax', [Tokens[1].TrimLeft('$')])
        else
          raise Exception.Create('Missing operand.');
      end;

    'add':
      begin
        if Tokens[1] = '@idx' then
          Result.Lines.Add('add ax, [rbx]')
        else if Tokens[1].StartsWith('$') then
          Result.Lines.Add(Format('add ax, [%s]', [Tokens[1].TrimLeft('$')]))
        else
          Result.Lines.Add(Format('add ax, %d', [Tokens[1].ToInteger()]));
      end;

    'subtract':
      begin
        if Tokens[1] = '@idx' then
          Result.Lines.Add('sub ax, [rbx]')
        else if Tokens[1].StartsWith('$') then
          Result.Lines.Add(Format('sub ax, [%s]', [Tokens[1].TrimLeft('$')]))
        else
          Result.Lines.Add(Format('sub ax, %d', [Tokens[1].ToInteger()]));
      end;

    'negate':
      Result.Lines.Add('neg ax');

    'inc':
      begin
        if Length(Tokens) < 2 then
          raise Exception.Create('Missing operand.');
        if Tokens[1] = 'acc' then
          Result.Lines.Add('inc ax')
        else if Tokens[1] = 'idx' then
          Result.Lines.Add('inc bx')
        else
          raise Exception.Create(Format('Invalid argument: %s.', [Tokens[1]]));
      end;

    'dec':
      begin
        if Length(Tokens) < 2 then
          raise Exception.Create('Missing operand.');
        if Tokens[1] = 'acc' then
          Result.Lines.Add('dec ax')
        else if Tokens[1] = 'idx' then
          Result.Lines.Add('dec bx')
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
      Result.Lines.Add('push rax');

    'pop':
      Result.Lines.Add('pop rax');

    'call':
      Result.Lines.Add(Format('call %s', [Tokens[1]]));

    'return':
      Result.Lines.Add('ret');

    'printchar':
      Result.Lines.Add('call __Std__PrintChar');

    'printinteger':
      Result.Lines.Add('call __Std__PrintInteger');

    'printstring':
      Result.Lines.Add('call __Std__PrintString');

    'sleep':
      Result.Lines.Add('call __Std__Sleep');

    'halt':
      begin
        // Syscall: exit (syscall number 60)
        Result.Lines.AddStrings([
          'mov eax, 60',
          'xor edi, edi',
          'syscall'
        ]);
      end;

    'var':
      begin
        Result.Section := csData;

        case Tokens[2] of
          'integer':
            Result.Lines.Add(Format('%s dw %d',
              [Tokens[1], Tokens[3].ToInteger()]));

          'string':
            begin
              StringLiteral := ExtractDelimited(2, ALine, ['''']);
              Result.Lines.Add(Format('%s db "%s", 0',
                [Tokens[1], StringLiteral]));
            end;
        end;
        Exit;
      end;
    else
      raise Exception.Create('Syntax error.');
  end;
end;

procedure TSillyAsmToNasmProcessor.ProcessLines();
var
  InputReader: TStreamReader;
  OutputWriter: TStreamWriter;
  LineCounter: LongWord;
  Line: String;
  StandardLibrary: TStringList;
  DataSection: TStringList;
  CodeChunk: TCodeChunk;
begin
  InputReader := TStreamReader.Create(FInputStream);
  OutputWriter := TStreamWriter.Create(FOutputStream);

  // Text (code) section
  if FVerbose then
    OutputWriter.WriteLine(';; Text (code) section');
  OutputWriter.WriteLine('section .text');
  OutputWriter.WriteLine('global _start');
  OutputWriter.WriteLine('_start:');

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
                  OutputWriter.WriteLine(';; [%d]: %s', [LineCounter, Line]);
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
        end;
      except
        on E: Exception do
          raise ESillyAsmError.Create(E.Message, LineCounter, Line);
      end;
    end;

    // Write std routines
    StandardLibrary := GetStandardLibrary();
    if FVerbose then
      OutputWriter.WriteLine(';; Standard Library routines');
    OutputWriter.WriteLine(StandardLibrary.Text);
    FreeAndNil(StandardLibrary);

    // Data section
    if FVerbose then
      OutputWriter.WriteLine(';; Data section');
    OutputWriter.WriteLine('section .data');
    OutputWriter.WriteLine('__Std__CharBuf db 0');
    OutputWriter.WriteLine('__Std__TimeSpec:');
    OutputWriter.WriteLine('dq 0');
    OutputWriter.WriteLine('dq 1000000');

    if DataSection.Count > 0 then
      OutputWriter.WriteLine(DataSection.Text);

  finally
    FreeAndNil(OutputWriter);
    FreeAndNil(InputReader);
  end;
end;

function TSillyAsmToNasmProcessor.GetStandardLibrary(): TStringList;
begin
  Result := TStringList.Create();
  Result.AddStrings([
    '__Std__PrintChar:',
      'push rax',
      'mov [__Std__CharBuf], al',
      'mov rax, 1',
      'mov rdi, 1',
      'mov rsi, __Std__CharBuf',
      'mov rdx, 1',
      'syscall',
      'pop rax',
      'ret'
  ]);

  Result.AddStrings([
    '__Std__PrintInteger:',
        'push rax',
        'push rbx',
        'test ax, ax',
        'jns __Std__PrintInteger__ProcessNumber',
        'neg ax',
        'mov al, ''-''',
        'call __Std__PrintChar',
        '__Std__PrintInteger__ProcessNumber:',
          'xor rcx, rcx',
        '__Std__PrintInteger__ReverseDigits:',
          'xor dx, dx',
          'mov bx, 10',
          'div bx',
          'push rdx',
          'inc cx',
          'test ax, ax',
          'jnz __Std__PrintInteger__ReverseDigits',
        '__Std__PrintInteger__PrintDigits:',
          'pop rdx',
          'mov al, dl',
          'add al, ''0''',
          'push rcx',
          'call __Std__PrintChar',
          'pop rcx',
          'loop __Std__PrintInteger__PrintDigits',
        'mov al, 10',
        'call __Std__PrintChar',
        'pop rbx',
        'pop rax',
        'ret'
  ]);

  Result.AddStrings([
    '__Std__PrintString:',
      'push rax',
      'mov rsi, rax',
      '__Std__PrintString__Loop:',
        'xor rax, rax',
        'mov al, [rsi]',
        'test al, al',
        'jz __Std__PrintString__Exit',
        'push rsi',
        'call __Std__PrintChar',
        'pop rsi',
        'inc rsi',
        'jmp __Std__PrintString__Loop',
      '__Std__PrintString__Exit:',
        'mov rax, 10',
        'call __Std__PrintChar',
        'pop rax',
        'ret'
  ]);

  Result.AddStrings([
    '__Std__Sleep:',
      'push rax',
      'lea rdi, [__Std__TimeSpec]',
      'xor rsi, rsi',
      'xor rcx, rcx',
      'mov cx, ax',
      '__Std__Sleep__Loop:',
        'push rcx',
        'mov rax, 35',
        'syscall',
        'pop rcx',
        'loop __Std__Sleep__Loop',
      'pop rax',
      'ret'
  ]);

end;

constructor TSillyAsmToNasmProcessor.Create(
  const AInputStream, AOutputStream: TStream;
  AVerbose: Boolean = false);
begin
  FInputStream := AInputStream;
  FOutputStream := AOutputStream;
  FVerbose := AVerbose;
end;

procedure TSillyAsmToNasmProcessor.Run();
begin
  ProcessLines();
end;

end.

