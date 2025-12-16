program SillyC;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, StrUtils;

type
  TTokenType = (
    ttEOF,
    ttIdentifier,
    ttNumber,
    ttString,
    ttKeyword,
    ttOperator,
    ttDelimiter,
    ttComment
  );

  TToken = record
    TokenType: TTokenType;
    Value: String;
    Line: Integer;
    Column: Integer;
  end;

  TSymbolType = (
    stVariable,
    stFunction
  );

  TSymbol = record
    Name: String;
    SymbolType: TSymbolType;
    Address: Word;
    IsDefined: Boolean;
  end;
  
  TSillyCCompiler = class
  private
    FTokens: array of TToken;
    FSource: TStringList;
    FOutput: TStringList;
    FSymbols: TStringList;
    FTempVars: TStringList;
    FStringVars: TStringList;
    FCurrentToken: Integer;
    FDataAddress: Integer;
    FCodeAddress: Integer;
    FLabelCounter: Integer;
    
  private
    procedure AddToken(TokenType: TTokenType; const Value: String; Line, Column: Integer);
    procedure Tokenize(const Source: String);
    function GetCurrentToken: TToken;
    function PeekToken: TToken;
    procedure NextToken;
    procedure ExpectToken(TokenType: TTokenType; const ExpectedValue: String = '');
    
    function ParseProgram: String;
    procedure ParseExpression;
    procedure ParseDeclarations;
    procedure ParseFunction;
    procedure ParseStatement;
    procedure ParseTerm;
    procedure ParseFactor;
    procedure ParseAssignment;
    procedure ParseIfStatement;
    procedure ParseWhileStatement;
    procedure ParsePrintStatement;
    procedure ParseReturnStatement;
    
    function AddSymbol(const Name: String; SymbolType: TSymbolType): Word;
    function FindSymbol(const Name: String): TSymbol;
    function GenerateLabel: String;
    procedure AddTempVariable(const Name: String);
    procedure AddStringVariable(const Name, Value: String);
    procedure EmitTempVariables;
    
    procedure Emit(const Instruction: String);
    procedure EmitLabel(const LabelName: String);
    procedure EmitComment(const Comment: String);
    
  public
    constructor Create;
    destructor Destroy; override;
    function Compile(const SourceCode: String): String;
  end;

constructor TSillyCCompiler.Create;
begin
  inherited Create;
  FSource := TStringList.Create;
  FOutput := TStringList.Create;
  FSymbols := TStringList.Create;
  FTempVars := TStringList.Create;
  FStringVars := TStringList.Create;
  FDataAddress := 0;
  FCodeAddress := 0;
  FLabelCounter := 0;
end;

destructor TSillyCCompiler.Destroy;
begin
  FSource.Free;
  FOutput.Free;
  FSymbols.Free;
  FTempVars.Free;
  FStringVars.Free;
  inherited Destroy;
end;

procedure TSillyCCompiler.AddToken(TokenType: TTokenType; const Value: String; Line, Column: Integer);
var
  Token: TToken;
begin
  Token.TokenType := TokenType;
  Token.Value := Value;
  Token.Line := Line;
  Token.Column := Column;
  SetLength(FTokens, Length(FTokens) + 1);
  FTokens[High(FTokens)] := Token;
end;

procedure TSillyCCompiler.Tokenize(const Source: String);
var
  Lines: TStringArray;
  Line, Token: String;
  i, j, Pos: Integer;
  c: Char;
  InString: Boolean;
  InLineComment: Boolean;
begin
  SetLength(FTokens, 0);
  Lines := Source.Split([#13, #10], TStringSplitOptions.ExcludeEmpty);
  
  for i := 0 to High(Lines) do
  begin
    Line := Lines[i].Trim;
    j := 1;
    Pos := 1;
    InString := False;
    InLineComment := False;
    
    // Skip line comments starting with //
    if (Length(Line) >= 2) and (Line[1] = '/') and (Line[2] = '/') then
      Continue;
    
    while j <= Length(Line) do
    begin
      c := Line[j];
      
      if InLineComment then
      begin
        Inc(j);
        Continue;
      end;
      
      if InString then
      begin
        if c = '"' then
        begin
          AddToken(ttString, Copy(Line, Pos, j - Pos + 1), i + 1, Pos);
          InString := False;
          Inc(j);
          Pos := j;
        end
        else
        begin
          Inc(j);
        end;
        Continue;
      end;
      
      if c = '"' then
      begin
        InString := True;
        Pos := j;
        Inc(j);
        Continue;
      end;
      
      // Handle line comments
      if (j < Length(Line)) and (Line[j] = '/') and (Line[j+1] = '/') then
      begin
        InLineComment := True;
        Inc(j, 2);
        Continue;
      end;
      
      if c in [' ', #9] then
      begin
        Inc(j);
        Pos := j;
        Continue;
      end;
      
      if c in ['a'..'z', 'A'..'Z', '_'] then
      begin
        Pos := j;
        while (j <= Length(Line)) and (Line[j] in ['a'..'z', 'A'..'Z', '0'..'9', '_']) do
          Inc(j);
        
        Token := Copy(Line, Pos, j - Pos);
        if (Token = 'int') or (Token = 'if') or (Token = 'else') or (Token = 'while') or (Token = 'return') or (Token = 'void') then
          AddToken(ttKeyword, Token, i + 1, Pos)
        else
          AddToken(ttIdentifier, Token, i + 1, Pos);
        Continue;
      end;
      
      if c in ['0'..'9'] then
      begin
        Pos := j;
        while (j <= Length(Line)) and (Line[j] in ['0'..'9']) do
          Inc(j);
        AddToken(ttNumber, Copy(Line, Pos, j - Pos), i + 1, Pos);
        Continue;
      end;
      
      if c in ['+', '-', '*', '/', '=', '<', '>', '&', '|'] then
      begin
        if (j < Length(Line)) and (Line[j+1] in ['=', '+', '-', '&', '|']) then
        begin
          AddToken(ttOperator, Copy(Line, j, 2), i + 1, j);
          Inc(j, 2);
        end
        else
        begin
          AddToken(ttOperator, c, i + 1, j);
          Inc(j);
        end;
        Continue;
      end;
      
      if c in ['(', ')', '{', '}', ';', ','] then
      begin
        AddToken(ttDelimiter, c, i + 1, j);
        Inc(j);
        Continue;
      end;
      
      Inc(j);
    end;
  end;
  
  AddToken(ttEOF, '', 0, 0);
end;

function TSillyCCompiler.GetCurrentToken: TToken;
begin
  if FCurrentToken < Length(FTokens) then
    Result := FTokens[FCurrentToken]
  else
    Result := FTokens[High(FTokens)];
end;

function TSillyCCompiler.PeekToken: TToken;
begin
  if FCurrentToken + 1 < Length(FTokens) then
    Result := FTokens[FCurrentToken + 1]
  else
    Result := FTokens[High(FTokens)];
end;

procedure TSillyCCompiler.NextToken;
begin
  if FCurrentToken < Length(FTokens) then
    Inc(FCurrentToken);
end;

procedure TSillyCCompiler.ExpectToken(TokenType: TTokenType; const ExpectedValue: String);
var
  Token: TToken;
begin
  Token := GetCurrentToken;
  if Token.TokenType <> TokenType then
    raise Exception.CreateFmt('Expected token type %d but found %d at line %d', 
      [Ord(TokenType), Ord(Token.TokenType), Token.Line]);
  
  if (ExpectedValue <> '') and (Token.Value <> ExpectedValue) then
    raise Exception.CreateFmt('Expected "%s" but found "%s" at line %d', 
      [ExpectedValue, Token.Value, Token.Line]);
  
  NextToken;
end;

function TSillyCCompiler.AddSymbol(const Name: String; SymbolType: TSymbolType): Word;
var
  Symbol: TSymbol;
begin
  Symbol.Name := Name;
  Symbol.SymbolType := SymbolType;
  Symbol.IsDefined := False;
  
  if SymbolType = stVariable then
  begin
    Symbol.Address := FDataAddress;
    Inc(FDataAddress);
  end
  else
  begin
    Symbol.Address := FCodeAddress;
  end;
  
  FSymbols.Add(Name + '=' + IntToStr(Symbol.Address));
  Result := Symbol.Address;
end;

procedure TSillyCCompiler.AddTempVariable(const Name: String);
begin
  if FTempVars.IndexOf(Name) < 0 then
    FTempVars.Add(Name);
end;

procedure TSillyCCompiler.AddStringVariable(const Name, Value: String);
begin
  if FStringVars.IndexOf(Name) < 0 then
    FStringVars.Add(Name + '=' + Value);
end;

procedure TSillyCCompiler.EmitTempVariables;
var
  i: Integer;
  Name, Value: String;
  EqPos: Integer;
begin
  Emit('Data:');
  // Emit integer variables
  for i := 0 to FTempVars.Count - 1 do
  begin
    Emit('  var ' + FTempVars[i] + ' integer 0');
  end;
  
  // Emit string variables
  for i := 0 to FStringVars.Count - 1 do
  begin
    EqPos := Pos('=', FStringVars[i]);
    Name := Copy(FStringVars[i], 1, EqPos - 1);
    Value := Copy(FStringVars[i], EqPos + 1, Length(FStringVars[i]));
    // Remove surrounding double quotes from the value
    if (Length(Value) >= 2) and (Value[1] = '"') and (Value[Length(Value)] = '"') then
      Value := Copy(Value, 2, Length(Value) - 2);
    Emit('  var ' + Name + ' string ''' + Value + '''');
  end;
  Emit('');
end;

function TSillyCCompiler.FindSymbol(const Name: String): TSymbol;
var
  Index: Integer;
begin
  Result.Name := '';
  Index := FSymbols.IndexOfName(Name);
  if Index >= 0 then
  begin
    Result.Name := Name;
    Result.Address := StrToIntDef(FSymbols.ValueFromIndex[Index], 0);
  end;
end;

function TSillyCCompiler.GenerateLabel: String;
begin
  Inc(FLabelCounter);
  Result := '__label_' + IntToStr(FLabelCounter);
end;

procedure TSillyCCompiler.Emit(const Instruction: String);
begin
  FOutput.Add(Instruction);
end;

procedure TSillyCCompiler.EmitLabel(const LabelName: String);
begin
  FOutput.Add(LabelName + ':');
end;

procedure TSillyCCompiler.EmitComment(const Comment: String);
begin
  FOutput.Add('  // ' + Comment);
end;

function TSillyCCompiler.Compile(const SourceCode: String): String;
begin
  FOutput.Clear;
  FSymbols.Clear;
  FDataAddress := 0;
  FCodeAddress := 0;
  FLabelCounter := 0;
  
  Tokenize(SourceCode);
  FCurrentToken := 0;
  
  Emit('// Generated by SillyC Compiler');
  Emit('');
  Emit('Start:');
  Emit('  Jump Main');
  Emit('');
  
  ParseProgram;
  
  // Emit Data section at the end with all variables
  EmitTempVariables;
  
  Result := FOutput.Text;
end;

function TSillyCCompiler.ParseProgram: String;
begin
  ParseDeclarations;
  
  while GetCurrentToken.TokenType <> ttEOF do
  begin
    if (GetCurrentToken.Value = 'int') then
      ParseFunction
    else
      NextToken;
  end;
  
  Result := FOutput.Text;
end;

procedure TSillyCCompiler.ParseDeclarations;
var
  VarName: String;
  NextTokenValue: String;
  TempPos: Integer;
  AfterNextToken: String;
begin
  while GetCurrentToken.Value = 'int' do
  begin
    // Look ahead to see if this is a function or variable
    TempPos := FCurrentToken;
    NextToken; // Skip 'int'
    NextTokenValue := GetCurrentToken.Value; // Get identifier
    NextToken; // Get what comes after identifier
    AfterNextToken := GetCurrentToken.Value;
    FCurrentToken := TempPos; // Reset position
    
    if AfterNextToken = '(' then
      Break; // This is a function definition, not a variable declaration
      

    ExpectToken(ttKeyword, 'int');
    VarName := GetCurrentToken.Value;
    ExpectToken(ttIdentifier);
    AddSymbol(VarName, stVariable);
    AddTempVariable(VarName); // Add to temp vars for consistent emission
    ExpectToken(ttDelimiter, ';');
  end;
  Emit('');
end;

procedure TSillyCCompiler.ParseFunction;
var
  FuncName: String;
begin
  ExpectToken(ttKeyword, 'int');
  FuncName := GetCurrentToken.Value;
  ExpectToken(ttIdentifier);
  AddSymbol(FuncName, stFunction);
  Emit('');
  Emit(FuncName + ':');
  
  ExpectToken(ttDelimiter, '(');
  ExpectToken(ttDelimiter, ')');
  ExpectToken(ttDelimiter, '{');
  
  while (GetCurrentToken.Value <> '}') and (GetCurrentToken.TokenType <> ttEOF) do
  begin
    ParseStatement;
  end;
  
  ExpectToken(ttDelimiter, '}');
  Emit('  Return');
end;

procedure TSillyCCompiler.ParseStatement;
var
  Token: TToken;
begin
  Token := GetCurrentToken;
  
  if Token.Value = 'if' then
    ParseIfStatement
  else if Token.Value = 'while' then
    ParseWhileStatement
  else if Token.Value = 'return' then
    ParseReturnStatement
  else if Token.Value = 'printf' then
    ParsePrintStatement
  else if Token.TokenType = ttIdentifier then
    ParseAssignment
  else
    NextToken;
end;

procedure TSillyCCompiler.ParseAssignment;
var
  VarName: String;
  Symbol: TSymbol;
  Op: String;
begin
  VarName := GetCurrentToken.Value;
  ExpectToken(ttIdentifier);
  Symbol := FindSymbol(VarName);
  
  ExpectToken(ttOperator, '=');
  ParseExpression;
  ExpectToken(ttDelimiter, ';');
  
  Emit('  Store $' + VarName);
end;

procedure TSillyCCompiler.ParseExpression;
var
  Op: String;
  TempVar: String;
begin
  ParseTerm;
  
  while (GetCurrentToken.Value = '+') or (GetCurrentToken.Value = '-') do
  begin
    Op := GetCurrentToken.Value;
    NextToken;
    
    // For binary operations, we need to save current result and load second operand
    TempVar := GenerateLabel + '_temp';
    AddTempVariable(TempVar);
    Emit('  Store $' + TempVar);
    ParseTerm;
    
    if Op = '+' then
    begin
      Emit('  Add $' + TempVar);
    end
    else
    begin
      Emit('  Subtract $' + TempVar);
    end;
  end;
end;

procedure TSillyCCompiler.ParseTerm;
var
  Op: String;
begin
  ParseFactor;
  
  // SillyTron doesn't support multiply/divide, so skip them for now
  while (GetCurrentToken.Value = '*') or (GetCurrentToken.Value = '/') do
  begin
    Op := GetCurrentToken.Value;
    NextToken;
    ParseFactor;
    
    if Op = '*' then
      Emit('  // Multiplication not supported')
    else
      Emit('  // Division not supported');
  end;
end;

procedure TSillyCCompiler.ParseFactor;
var
  Token: TToken;
  Symbol: TSymbol;
begin
  Token := GetCurrentToken;
  
  if Token.TokenType = ttNumber then
  begin
    Emit('  Load ' + Token.Value);
    NextToken;
  end
  else if Token.TokenType = ttIdentifier then
  begin
    Symbol := FindSymbol(Token.Value);
    Emit('  Load $' + Token.Value);
    NextToken;
  end
  else if Token.Value = '(' then
  begin
    ExpectToken(ttDelimiter, '(');
    ParseExpression;
    ExpectToken(ttDelimiter, ')');
  end
  else
    NextToken;
end;

procedure TSillyCCompiler.ParseIfStatement;
var
  ElseLabel, EndLabel: String;
begin
  ExpectToken(ttKeyword, 'if');
  ExpectToken(ttDelimiter, '(');
  ParseExpression;
  ExpectToken(ttDelimiter, ')');
  
  ElseLabel := GenerateLabel;
  EndLabel := GenerateLabel;
  
  Emit('  JumpIfZero ' + ElseLabel);
  ExpectToken(ttDelimiter, '{');
  
  while (GetCurrentToken.Value <> '}') and (GetCurrentToken.TokenType <> ttEOF) do
    ParseStatement;
  
  ExpectToken(ttDelimiter, '}');
  
  if GetCurrentToken.Value = 'else' then
  begin
    Emit('  Jump ' + EndLabel);
    EmitLabel(ElseLabel);
    ExpectToken(ttKeyword, 'else');
    ExpectToken(ttDelimiter, '{');
    
    while (GetCurrentToken.Value <> '}') and (GetCurrentToken.TokenType <> ttEOF) do
      ParseStatement;
    
    ExpectToken(ttDelimiter, '}');
    EmitLabel(EndLabel);
  end
  else
  begin
    EmitLabel(ElseLabel);
  end;
end;

procedure TSillyCCompiler.ParseWhileStatement;
var
  StartLabel, EndLabel: String;
begin
  ExpectToken(ttKeyword, 'while');
  StartLabel := GenerateLabel;
  EndLabel := GenerateLabel;
  
  EmitLabel(StartLabel);
  ExpectToken(ttDelimiter, '(');
  ParseExpression;
  ExpectToken(ttDelimiter, ')');
  
  Emit('  JumpIfZero ' + EndLabel);
  ExpectToken(ttDelimiter, '{');
  
  while (GetCurrentToken.Value <> '}') and (GetCurrentToken.TokenType <> ttEOF) do
    ParseStatement;
  
  ExpectToken(ttDelimiter, '}');
  Emit('  Jump ' + StartLabel);
  EmitLabel(EndLabel);
end;

procedure TSillyCCompiler.ParsePrintStatement;
var
  Token: TToken;
  StringLabel: String;
begin
  ExpectToken(ttIdentifier, 'printf');
  ExpectToken(ttDelimiter, '(');
  Token := GetCurrentToken;
  
  if Token.TokenType = ttString then
  begin
    StringLabel := GenerateLabel + '_str';
    AddStringVariable(StringLabel, Token.Value);
    Emit('  Load @' + StringLabel);
    Emit('  PrintString');
    NextToken;
  end
  else
  begin
    ParseExpression;
    Emit('  PrintInteger');
  end;
  
  ExpectToken(ttDelimiter, ')');
  ExpectToken(ttDelimiter, ';');
end;

procedure TSillyCCompiler.ParseReturnStatement;
begin
  ExpectToken(ttKeyword, 'return');
  ParseExpression;
  ExpectToken(ttDelimiter, ';');
  Emit('  Halt');
end;

var
  InputFilename: String;
  SourceCode: String;
  Assembly: String;
  OutputFilename: String;
  Compiler: TSillyCCompiler;
  OutputFile: TextFile;

begin
  InputFilename := ParamStr(1);
  if InputFilename.IsEmpty then
  begin
    Writeln('Usage: sillyc <input.c>');
    Halt(1);
  end;

  if not FileExists(InputFilename) then
  begin
    Writeln('File not found: ', InputFilename);
    Halt(1);
  end;

  try
    with TStringList.Create do
    begin
      LoadFromFile(InputFilename);
      SourceCode := Text;
      Free;
    end;

    Compiler := TSillyCCompiler.Create;
    try
      Assembly := Compiler.Compile(SourceCode);
      OutputFilename := ChangeFileExt(InputFilename, '.sasm');
      
      AssignFile(OutputFile, OutputFilename);
      Rewrite(OutputFile);
      Write(OutputFile, Assembly);
      CloseFile(OutputFile);
      
      Writeln('Compiled ', InputFilename, ' -> ', OutputFilename);
    finally
      Compiler.Free;
    end;
  except
    on E: Exception do
    begin
      Writeln('Error: ', E.Message);
      Halt(1);
    end;
  end;
end.