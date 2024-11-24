unit SillyAsmProcessor;

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

  { TSymbolReference }

  TSymbolReference = class
  private
    FOffset: Word;
    FSourceLine: String;
    FSourceLineNumber: LongWord;
    FSymbol: String;
  public
    property Symbol: String read FSymbol;
    property Offset: Word read FOffset;
    property SourceLine: String read FSourceLine;
    property SourceLineNumber: LongWord read FSourceLineNumber;
    constructor Create(
      ASymbol: String; AOffset: Word; ASourceLineNumber: LongWord;
      ASourceLine: String = '');
  end;

  TSymbolMap = specialize TFPGMap<String, Word>;
  TSymbolReferenceList = specialize TFPGList<TSymbolReference>;

  { TSillyAsmProcessor }

  TSillyAsmProcessor = class
  private
    FInputStream: TStream;
    FOutputStream: TStream;
    FSymbolMap: TSymbolMap;
    FSymbolReferences: TSymbolReferenceList;
    procedure AddSymbol(ASymbol: String; AOffset: Word);
    procedure AddSymbolReference(
      ASymbol: String; AOffset: Word; ASourceLineNumber: LongWord;
      ASourceLine: String = '');
    procedure ProcessLine(ALine: String; ALineNumber: LongWord);
    procedure ProcessLines();
    procedure ProcessRefs();
  public
    constructor Create(const AInputStream, AOutputStream: TStream);
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

constructor TSymbolReference.Create(
  ASymbol: String; AOffset: Word; ASourceLineNumber: LongWord;
  ASourceLine: String);
begin
  FSymbol := ASymbol;
  FOffset := AOffset;
  FSourceLineNumber := ASourceLineNumber;
  FSourceLine := ASourceLine;
end;

{ ESillyAsmError }

constructor ESillyAsmError.Create(
  AMessage: String; ALineNumber: LongWord; ALineContents: String);
begin
  Message := AMessage;
  FLineNumber := ALineNumber;
  FLineContents := ALineContents;
end;

{ TSillyAsmProcessor }

procedure TSillyAsmProcessor.AddSymbol(ASymbol: String; AOffset: Word);
begin
  if FSymbolMap.IndexOf(ASymbol) >= 0 then
    raise Exception.Create(Format('Duplicate symbol: %s', [ASymbol]));

  FSymbolMap[ASymbol] := AOffset;
end;

procedure TSillyAsmProcessor.AddSymbolReference(
  ASymbol: String; AOffset: Word; ASourceLineNumber: LongWord;
  ASourceLine: String = '');
begin
  FSymbolReferences.Add(
    TSymbolReference.Create(ASymbol, AOffset, ASourceLineNumber, ASourceLine));
end;

procedure TSillyAsmProcessor.ProcessLine(ALine: String; ALineNumber: LongWord);
var
  Tokens: TStringDynArray;
  Instruction: TInstruction;
  StringChar: Char;
begin
  Tokens := ALine.ToLower().Split(
    [' ', #9],
    TStringSplitOptions.ExcludeEmpty);

  if (Length(Tokens) = 0) or Tokens[0].StartsWith('//') then
    Exit;

  if Tokens[0].EndsWith(':') then
  begin
    AddSymbol(
      Tokens[0].TrimRight(':'),
      FOutputStream.Position div SizeOf(Word));
    Exit;
  end;

  Instruction.ParamType := ptNone;

  case Tokens[0] of
    'nop':
      Instruction.Operation := opNop;

    'load':
      begin
        if Tokens[1] = 'idx' then
          Instruction.Operation := opLoadIndex
        else
        if Tokens[1] = '@idx' then
          Instruction.Operation := opLoadMemIndirect
        else
        if Tokens[1].StartsWith('$') then
        begin
          Instruction.Operation := opLoadMemDirect;
          Instruction.ParamType := ptReference;
          Instruction.ParamValue := Tokens[1].TrimLeft('$');
        end
        else
        if Tokens[1].StartsWith('@') then
        begin
          Instruction.Operation := opLoadConst;
          Instruction.ParamType := ptReference;
          Instruction.ParamValue := Tokens[1].TrimLeft('@');
        end
        else
        begin
          Instruction.Operation := opLoadConst;
          Instruction.ParamType := ptConst;
          Instruction.ParamValue := Tokens[1];
        end;
      end;

    'store':
      begin
        if Tokens[1] = 'idx' then
          Instruction.Operation := opStoreIndex
        else
        if Tokens[1] = '@idx' then
          Instruction.Operation := opStoreMemIndirect
        else
        if Tokens[1].StartsWith('$') then
        begin
          Instruction.Operation := opStoreMemDirect;
          Instruction.ParamType := ptReference;
          Instruction.ParamValue := Tokens[1].TrimLeft('$');
        end
        else
          raise Exception.Create('Missing operand.');
      end;

    'add':
      begin
        if Tokens[1] = '@idx' then
          Instruction.Operation := opAddMemIndirect
        else
        if Tokens[1].StartsWith('$') then
        begin
          Instruction.Operation := opAddMemDirect;
          Instruction.ParamType := ptReference;
          Instruction.ParamValue := Tokens[1].TrimLeft('$');
        end
        else
        begin
          Instruction.Operation := opAddConst;
          Instruction.ParamType := ptConst;
          Instruction.ParamValue := Tokens[1];
        end
      end;

    'subtract':
      begin
        if Tokens[1] = '@idx' then
          Instruction.Operation := opSubtractMemIndirect
        else
        if Tokens[1].StartsWith('$') then
        begin
          Instruction.Operation := opSubtractMemDirect;
          Instruction.ParamType := ptReference;
          Instruction.ParamValue := Tokens[1].TrimLeft('$');
        end
        else
        begin
          Instruction.Operation := opSubtractConst;
          Instruction.ParamType := ptConst;
          Instruction.ParamValue := Tokens[1];
        end
      end;

    'negate':
      Instruction.Operation := opNegate;

    'inc':
      begin
        if Length(Tokens) < 2 then
          raise Exception.Create('Missing operand.');
        if Tokens[1] = 'acc' then
          Instruction.Operation := opIncAccumulator
        else if Tokens[1] = 'idx' then
          Instruction.Operation := opIncIndex
        else
          raise Exception.Create(Format('Invalid argument: %s.', [Tokens[1]]));
      end;

    'dec':
      begin
        if Length(Tokens) < 2 then
          raise Exception.Create('Missing operand.');
        if Tokens[1] = 'acc' then
          Instruction.Operation := opDecAccumulator
        else if Tokens[1] = 'idx' then
          Instruction.Operation := opDecIndex
        else
          raise Exception.Create(Format('Invalid argument: %s.', [Tokens[1]]));
      end;

    'jump':
      begin
        Instruction.Operation := opJump;
        Instruction.ParamType := ptReference;
        Instruction.ParamValue := Tokens[1];
      end;

    'jumpifzero':
      begin
        Instruction.Operation := opJumpIfZero;
        Instruction.ParamType := ptReference;
        Instruction.ParamValue := Tokens[1];
      end;

    'jumpifsign':
      begin
        Instruction.Operation := opJumpIfSign;
        Instruction.ParamType := ptReference;
        Instruction.ParamValue := Tokens[1];
      end;

    'jumpifnotzero':
      begin
        Instruction.Operation := opJumpIfNotZero;
        Instruction.ParamType := ptReference;
        Instruction.ParamValue := Tokens[1];
      end;

    'jumpifnotsign':
      begin
        Instruction.Operation := opJumpIfNotSign;
        Instruction.ParamType := ptReference;
        Instruction.ParamValue := Tokens[1];
      end;

    'push':
      Instruction.Operation := opPush;

    'pop':
      Instruction.Operation := opPop;

    'call':
      begin
        Instruction.Operation := opCall;
        Instruction.ParamType := ptReference;
        Instruction.ParamValue := Tokens[1];
      end;

    'return':
      Instruction.Operation := opReturn;

    'printchar':
      Instruction.Operation := opPrintChar;

    'printinteger':
      Instruction.Operation := opPrintInteger;

    'printstring':
      Instruction.Operation := opPrintString;

    'sleep':
      Instruction.Operation := opSleep;

    'halt':
      Instruction.Operation := opHalt;

    'var':
      begin
        if Tokens[1] <> '_' then
          AddSymbol(Tokens[1], FOutputStream.Position div SizeOf(Word));

        case Tokens[2] of
          'integer':
            FOutputStream.WriteWord(StrToInt(Tokens[3]));

          'string':
            begin
              for StringChar in ExtractDelimited(2, ALine, ['''']) do
                FOutputStream.WriteWord(Word(StringChar));
              FOutputStream.WriteWord(0);
            end;
        end;
        Exit;
      end;
    else
      raise Exception.Create('Syntax error.');
  end;

  FOutputStream.WriteWord(Ord(Instruction.Operation));
  case Instruction.ParamType of
    ptNone:
      begin
        // Nothing to do
      end;

    ptConst:
      FOutputStream.WriteWord(StrToInt(Instruction.ParamValue));

    ptReference:
      begin
        AddSymbolReference(
          Instruction.ParamValue, FOutputStream.Position, ALineNumber, ALine);
        FOutputStream.WriteWord(0);
      end;
  end;
end;

procedure TSillyAsmProcessor.ProcessLines();
var
  InputReader: TStreamReader;
  LineCounter: LongWord;
  Line: String;
begin
  InputReader := TStreamReader.Create(FInputStream);
  try
    LineCounter := 0;
    while not InputReader.Eof do
    begin
      Inc(LineCounter);
      try
        Line := InputReader.ReadLine().Trim();
        ProcessLine(Line, LineCounter);
      except
        on E: Exception do
          raise ESillyAsmError.Create(E.Message, LineCounter, Line);
      end;
    end;
  finally
    FreeAndNil(InputReader);
  end;
end;

procedure TSillyAsmProcessor.ProcessRefs();
var
  SymbolReference: TSymbolReference;
  Offset: Word;
begin
  for SymbolReference in FSymbolReferences do
  begin
    try
      Offset := FSymbolMap[SymbolReference.Symbol];
    except
      on E: EListError do
        raise ESillyAsmError.Create(
          Format('Undefined symbol: %s', [SymbolReference.Symbol]),
          SymbolReference.SourceLineNumber, SymbolReference.FSourceLine);
    end;

    FOutputStream.Seek(SymbolReference.Offset, soBeginning);
    FOutputStream.WriteWord(Offset);
  end;
end;

constructor TSillyAsmProcessor.Create(
  const AInputStream, AOutputStream: TStream);
begin
  FInputStream := AInputStream;
  FOutputStream := AOutputStream;
end;

procedure TSillyAsmProcessor.Run();
begin
  FSymbolMap := TSymbolMap.Create;
  FSymbolReferences := TSymbolReferenceList.Create;
  try
    ProcessLines();
    ProcessRefs();
  finally
    FreeAndNil(FSymbolMap);
    FreeAndNil(FSymbolReferences);
  end;
end;

end.

