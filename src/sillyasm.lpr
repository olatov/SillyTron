program SillyAsm;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, StrUtils, StreamEx, SillyMachine;

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

var
  InputFilename: String;
  InputReader: TFileReader;
  OutputStream: TFileStream;
  Line: String;
  Tokens: TStringArray;
  Symbol: String;
  Symbols: TStringList;
  Refs: TStringList;
  I: Integer;
  LineCounter: Integer;
  StringChar: Char;
  Instruction: Tinstruction;

begin
  InputFilename := ParamStr(1);
  if InputFilename.IsEmpty then
  begin
    Writeln('Missing input filename.');
    Halt(1);
  end;

  OutputStream := TFileStream.Create(
    ChangeFileExt(InputFilename, '.bin'), fmCreate);
  Symbols := TStringList.Create();
  Refs := TStringList.Create();

  InputReader := TFileReader.Create(InputFilename);

  Writeln(Format('Processing %s', [InputFilename]));
  WriteLn();

  LineCounter := 0;
  while not InputReader.Eof do
  begin
    Inc(LineCounter);
    try
      Line := InputReader.ReadLine().Trim();
      if Line.IsEmpty then
        continue;

      Tokens := Line.ToLower().Split(
        [' ', #9],
        TStringSplitOptions.ExcludeEmpty);

      if (Length(Tokens) = 0) or Tokens[0].StartsWith('//') then
        continue;

      if Tokens[0].EndsWith(':') then
      begin
        Symbols.AddPair(
          Tokens[0].TrimRight(':'),
          IntToStr(OutputStream.Position div SizeOf(Word)));
        continue;
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
              Symbols.AddPair(
                Tokens[1],
                IntToStr(OutputStream.Position div SizeOf(Word)));
            case Tokens[2] of
              'integer':
                OutputStream.WriteWord(StrToInt(Tokens[3]));
              'string':
                begin
                  for StringChar in ExtractDelimited(2, Line, ['''']) do
                    OutputStream.WriteWord(Word(StringChar));
                  OutputStream.WriteWord(0);
                end;
            end;
            continue;
          end;
        else
          raise Exception.Create('Syntax error.');
      end;

      OutputStream.WriteWord(Ord(Instruction.Operation));
      case Instruction.ParamType of
        ptConst:
          OutputStream.WriteWord(StrToInt(Instruction.ParamValue));
        ptReference:
          begin
            Refs.AddPair(
              IntToStr(OutputStream.Position),
              Instruction.ParamValue);
            OutputStream.WriteWord(0);
          end;
      end;
    except
      on E: Exception do
        begin
          WriteLn(Format('Line %d: %s', [LineCounter, E.Message]));
          WriteLn(Format('>> %s', [Line]));
          Halt(1);
        end;
    end;
  end;

  FreeAndNil(InputReader);

  WriteLn('Symbols:');
  Writeln(Symbols.Text);

  Writeln('Refs:');
  Writeln(Refs.Text);

  // Resolve refs
  for I := 0 To Refs.Count - 1 do
  begin
    Symbol := Refs.ValueFromIndex[I];
    if Symbols.IndexOfName(Symbol) < 0 then
    begin
      Writeln(Format('Unresolved reference: %s', [Symbol]));
      Halt(1);
    end;

    OutputStream.Seek(Refs.Names[I].ToInt64(), soBeginning);
    OutputStream.WriteWord(Symbols.Values[Symbol].ToInteger());
  end;
  Writeln();

  FreeAndNil(Refs);
  FreeAndNil(Symbols);

  Writeln(
    Format('Written %s (%d bytes length).',
    [OutputStream.FileName, OutputStream.Size]));

  FreeAndNil(OutputStream);
end.

