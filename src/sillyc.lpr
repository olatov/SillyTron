program SillyC;

{$mode objfpc}{$H+}
{$define DEBUG}

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
    FCurrentFunction: String;
    FCurrentFunctionHasReturn: Boolean;
    
  private
    procedure Tokenize(const SourceCode: String);
    procedure AddToken(TokenType: TTokenType; const Value: String; Line, Column: Integer);
    function GetCurrentToken: TToken;
    function PeekToken: TToken;
    procedure NextToken;
    procedure ExpectToken(TokenType: TTokenType; const ExpectedValue: String = '');
    function AddSymbol(const Name: String; SymbolType: TSymbolType): Word;
    procedure AddTempVariable(const Name: String);
    procedure AddStringVariable(const Name, Value: String);
    procedure EmitTempVariables;
    procedure Emit(const Instruction: String);
    procedure EmitLabel(const LabelName: String);
    procedure EmitComment(const Comment: String);
    function FindSymbol(const Name: String): TSymbol;
    function GenerateLabel: String;
    function Compile(const SourceCode: String): String;
    function ParseProgram: String;
    procedure ParseDeclarations;
    procedure ParseFunction;
    procedure ParseStatement;
    procedure ParseAssignment;
    procedure ParseComparison;
    procedure ParseExpression;
    procedure ParseTerm;
    procedure ParseFactor;
    procedure ParseIfStatement;
    procedure ParseWhileStatement;
    procedure ParseForStatement;
    procedure ParsePrintStatement;
    procedure ParseReturnStatement;
    constructor Create;
    destructor Destroy; override;
  end;

procedure TSillyCCompiler.Tokenize(const SourceCode: String);
var
  Lines: TStringList;
  i, j, Pos: Integer;
  Line: String;
  InLineComment, InString: Boolean;
  Token: String;
  c: Char;
begin
  Lines := TStringList.Create;
  try
    Lines.Text := SourceCode;
    SetLength(FTokens, 0);
    // iterate lines
    for i := 0 to Lines.Count - 1 do
    begin
      Line := Lines[i];
      j := 1;
      Pos := 1;
      InLineComment := False;
      InString := False;

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
        if (Token = 'int') or (Token = 'if') or (Token = 'else') or (Token = 'while') or (Token = 'return') or (Token = 'void') or (Token = 'for') then
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
      
      if c in ['+', '-', '*', '/', '=', '<', '>', '&', '|', '!'] then
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
    end; // for loop
    AddToken(ttEOF, '', 0, 0);
  finally
    Lines.Free;
  end;
end;

procedure TSillyCCompiler.AddToken(TokenType: TTokenType; const Value: String; Line, Column: Integer);
var
  t: TToken;
  n: Integer;
begin
  n := Length(FTokens);
  SetLength(FTokens, n + 1);
  t.TokenType := TokenType;
  t.Value := Value;
  t.Line := Line;
  t.Column := Column;
  FTokens[n] := t;
end;

constructor TSillyCCompiler.Create;
begin
  inherited Create;
  FOutput := TStringList.Create;
  FSource := TStringList.Create;
  FSymbols := TStringList.Create;
  FTempVars := TStringList.Create;
  FStringVars := TStringList.Create;
  FCurrentToken := 0;
  FDataAddress := 0;
  FCodeAddress := 0;
  FLabelCounter := 0;
  FCurrentFunction := '';
  FCurrentFunctionHasReturn := False;
end;

destructor TSillyCCompiler.Destroy;
begin
  FOutput.Free;
  FSource.Free;
  FSymbols.Free;
  FTempVars.Free;
  FStringVars.Free;
  inherited Destroy;
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
    raise Exception.CreateFmt('Expected token type %d but found %d ("%s") at line %d', 
      [Ord(TokenType), Ord(Token.TokenType), Token.Value, Token.Line]);
  
  if (ExpectedValue <> '') and (Token.Value <> ExpectedValue) then
    raise Exception.CreateFmt('Expected "%s" but found "%s" at line %d', 
      [ExpectedValue, Token.Value, Token.Line]);
  
  NextToken;
end;

function TSillyCCompiler.AddSymbol(const Name: String; SymbolType: TSymbolType): Word;
var
  Symbol: TSymbol;
  StoredName: String;
begin
  // For variables inside a function, store them with a function prefix
  if (SymbolType = stVariable) and (FCurrentFunction <> '') then
    StoredName := FCurrentFunction + '_' + Name
  else
    StoredName := Name;
  Symbol.Name := StoredName;
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
  
  FSymbols.Add(Symbol.Name + '=' + IntToStr(Symbol.Address));
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
  PrefName: String;
begin
  Result.Name := '';
  // First try function-prefixed name for locals
  if FCurrentFunction <> '' then
  begin
    PrefName := FCurrentFunction + '_' + Name;
    Index := FSymbols.IndexOfName(PrefName);
    if Index >= 0 then
    begin
      Result.Name := PrefName;
      Result.Address := StrToIntDef(FSymbols.ValueFromIndex[Index], 0);
      Exit;
    end;
  end;

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
var
  i: Integer;
begin
  FOutput.Clear;
  FSymbols.Clear;
  FDataAddress := 0;
  FCodeAddress := 0;
  FLabelCounter := 0;
  
  Tokenize(SourceCode);
  FCurrentToken := 0;
  // DEBUG: dump tokens (temporary)
  {$ifdef DEBUG}
  for i := 0 to High(FTokens) do
    Writeln(Format('TOK: type=%d val="%s" line=%d col=%d', [Ord(FTokens[i].TokenType), FTokens[i].Value, FTokens[i].Line, FTokens[i].Column]));
  {$endif}
  
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
    if (GetCurrentToken.Value = 'int') or (GetCurrentToken.Value = 'void') then
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
  Symbol: TSymbol;
begin
  while True do
  begin
    // Skip any comments before declarations
    while GetCurrentToken.TokenType = ttComment do
      NextToken;

    if GetCurrentToken.Value <> 'int' then
      Break;

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
    // If inside a function, store local variable under function-prefixed name
    if FCurrentFunction <> '' then
    begin
      AddSymbol(VarName, stVariable);
      AddTempVariable(FCurrentFunction + '_' + VarName);
    end
    else
    begin
      AddSymbol(VarName, stVariable);
      AddTempVariable(VarName); // Add to temp vars for consistent emission
    end;

    // Optional initializer: handle `= <expression>` and emit store
    if GetCurrentToken.Value = '=' then
    begin
      ExpectToken(ttOperator, '=');
      ParseExpression;
      Symbol := FindSymbol(VarName);
      if Symbol.Name <> '' then
        Emit('  Store $' + Symbol.Name)
      else
        Emit('  Store $' + VarName);
    end;

    ExpectToken(ttDelimiter, ';');
  end;
  Emit('');
end;

procedure TSillyCCompiler.ParseFunction;
var
  FuncName: String;
begin
  // Accept 'int' or 'void' function return type
  if (GetCurrentToken.Value <> 'int') and (GetCurrentToken.Value <> 'void') then
    raise Exception.CreateFmt('Expected function return type but found %s at line %d', [GetCurrentToken.Value, GetCurrentToken.Line]);
  // consume return type
  NextToken;
  FuncName := GetCurrentToken.Value;
  ExpectToken(ttIdentifier);
  AddSymbol(FuncName, stFunction);
  // set current function context for local variables
  FCurrentFunction := FuncName;
  Emit('');
  Emit(FuncName + ':');
  
  ExpectToken(ttDelimiter, '(');

  // Debug: show current token at start of for header
  EmitComment(Format('for-header-start tokenIndex=%d token="%s" peek="%s"',
    [FCurrentToken, GetCurrentToken.Value, PeekToken.Value]));
  ExpectToken(ttDelimiter, ')');
  ExpectToken(ttDelimiter, '{');
  // parse local declarations first
  ParseDeclarations;
  // reset explicit-return tracker for this function
  FCurrentFunctionHasReturn := False;

  while (GetCurrentToken.Value <> '}') and (GetCurrentToken.TokenType <> ttEOF) do
  begin
    ParseStatement;
  end;

  ExpectToken(ttDelimiter, '}');
  // Emit a Return for normal functions only if they didn't contain
  // an explicit return statement which already emitted a Return.
  if (FuncName <> 'main') and (not FCurrentFunctionHasReturn) then
    Emit('  Return');
  // clear function context
  FCurrentFunction := '';
end;

procedure TSillyCCompiler.ParseStatement;
var
  Token: TToken;
  VarName: String;
  Symbol: TSymbol;
  TempName: String;
  Op: String;
  Temp2: String;
begin
  Token := GetCurrentToken;
  
  if Token.Value = 'if' then
    ParseIfStatement
  else if Token.Value = 'int' then
  begin
    // Declaration inside function body (e.g., int x = expr;)
    ExpectToken(ttKeyword, 'int');
    VarName := GetCurrentToken.Value;
    ExpectToken(ttIdentifier);
    if FCurrentFunction <> '' then
    begin
      AddSymbol(VarName, stVariable);
      AddTempVariable(FCurrentFunction + '_' + VarName);
    end
    else
    begin
      AddSymbol(VarName, stVariable);
      AddTempVariable(VarName);
    end;

    if GetCurrentToken.Value = '=' then
    begin
      ExpectToken(ttOperator, '=');
      ParseExpression;
      Symbol := FindSymbol(VarName);
      if Symbol.Name <> '' then
        Emit('  Store $' + Symbol.Name)
      else
        Emit('  Store $' + VarName);
    end;

    ExpectToken(ttDelimiter, ';');
  end
  else if Token.Value = 'while' then
    ParseWhileStatement
  else if Token.Value = 'for' then
    ParseForStatement
  else if Token.Value = 'return' then
    ParseReturnStatement
  else if Token.Value = 'printf' then
    ParsePrintStatement
  else if (Token.TokenType = ttOperator) and ((Token.Value = '++') or (Token.Value = '--')) then
  begin
    // pre-increment/decrement statement: ++i; or --i;
    if Token.Value = '++' then
      NextToken
    else
      NextToken;
    VarName := GetCurrentToken.Value;
    ExpectToken(ttIdentifier);
    Symbol := FindSymbol(VarName);
    // generate temp and emit increment/decrement
    TempName := GenerateLabel + '_temp';
    AddTempVariable(TempName);
    if Token.Value = '++' then
    begin
      if Symbol.Name <> '' then
        Emit('  Load $' + Symbol.Name)
      else
        Emit('  Load $' + VarName);
      Emit('  Store $' + TempName);
      Emit('  Load 1');
      Emit('  Add $' + TempName);
      if Symbol.Name <> '' then
        Emit('  Store $' + Symbol.Name)
      else
        Emit('  Store $' + VarName);
    end
    else
    begin
      if Symbol.Name <> '' then
        Emit('  Load $' + Symbol.Name)
      else
        Emit('  Load $' + VarName);
      Emit('  Store $' + TempName);
      Emit('  Load 1');
      Emit('  Subtract $' + TempName);
      if Symbol.Name <> '' then
        Emit('  Store $' + Symbol.Name)
      else
        Emit('  Store $' + VarName);
    end;
    ExpectToken(ttDelimiter, ';');
  end
  else if Token.TokenType = ttIdentifier then
  begin
    // Distinguish between function call statements and assignments
    if PeekToken.Value = '(' then
    begin
      // function call as a statement: foo();
      // consume identifier and parentheses
      Token := GetCurrentToken;
      ExpectToken(ttIdentifier);
      ExpectToken(ttDelimiter, '(');
      ExpectToken(ttDelimiter, ')');
      // Emit call
      Emit('  Call ' + Token.Value);
      // expect semicolon
      ExpectToken(ttDelimiter, ';');
    end
    else if (PeekToken.Value = '++') or (PeekToken.Value = '--') then
    begin
      // post-increment/decrement statement: i++; or i--;
      VarName := GetCurrentToken.Value;
      Symbol := FindSymbol(VarName);
      ExpectToken(ttIdentifier);
      // consume ++/--
      Op := GetCurrentToken.Value;
      ExpectToken(ttOperator);
      // generate temp and emit
      Temp2 := GenerateLabel + '_temp';
      AddTempVariable(Temp2);
      if Symbol.Name <> '' then
        Emit('  Load $' + Symbol.Name)
      else
        Emit('  Load $' + VarName);
      Emit('  Store $' + Temp2);
      Emit('  Load 1');
      if Op = '++' then
        Emit('  Add $' + Temp2)
      else
        Emit('  Subtract $' + Temp2);
      if Symbol.Name <> '' then
        Emit('  Store $' + Symbol.Name)
      else
        Emit('  Store $' + VarName);
      ExpectToken(ttDelimiter, ';');
    end
    else
      ParseAssignment;
  end
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
  
  if Symbol.Name <> '' then
    Emit('  Store $' + Symbol.Name)
  else
    Emit('  Store $' + VarName);
end;
 

procedure TSillyCCompiler.ParseComparison;
var
  Op: String;
  TempL, TempR: String;
  TrueLabel, FalseLabel, EndLabel: String;
begin
  // handle additive part first
  ParseTerm;

  while (GetCurrentToken.Value = '+') or (GetCurrentToken.Value = '-') do
  begin
    Op := GetCurrentToken.Value;
    NextToken;
    TempL := GenerateLabel + '_temp';
    AddTempVariable(TempL);
    Emit('  Store $' + TempL);
    ParseTerm;
    if Op = '+' then
      Emit('  Add $' + TempL)
    else
      Emit('  Subtract $' + TempL);
  end;

  // Comparison operators: ==, !=, <, >, <=, >=
  while (GetCurrentToken.Value = '==') or (GetCurrentToken.Value = '!=') or
        (GetCurrentToken.Value = '<') or (GetCurrentToken.Value = '>') or
        (GetCurrentToken.Value = '<=') or (GetCurrentToken.Value = '>=') do
  begin
    Op := GetCurrentToken.Value;
    NextToken;

    // Save left operand
    TempL := GenerateLabel + '_L';
    AddTempVariable(TempL);
    Emit('  Store $' + TempL);

    // Parse right operand (leaves right value in Acc)
    ParseTerm;

    // Save right and compute left - right
    TempR := GenerateLabel + '_R';
    AddTempVariable(TempR);
    Emit('  Store $' + TempR);
    Emit('  Load $' + TempL);
    Emit('  Subtract $' + TempR);

    TrueLabel := GenerateLabel + '_cmp_true';
    FalseLabel := GenerateLabel + '_cmp_false';
    EndLabel := GenerateLabel + '_cmp_end';

    if Op = '==' then
    begin
      Emit('  JumpIfZero ' + TrueLabel);
      Emit('  Load 0');
      Emit('  Jump ' + EndLabel);
      EmitLabel(TrueLabel);
      Emit('  Load 1');
      EmitLabel(EndLabel);
    end
    else if Op = '!=' then
    begin
      Emit('  JumpIfZero ' + FalseLabel);
      Emit('  Load 1');
      Emit('  Jump ' + EndLabel);
      EmitLabel(FalseLabel);
      Emit('  Load 0');
      EmitLabel(EndLabel);
    end
    else if Op = '<' then
    begin
      Emit('  JumpIfSign ' + TrueLabel);
      Emit('  Load 0');
      Emit('  Jump ' + EndLabel);
      EmitLabel(TrueLabel);
      Emit('  Load 1');
      EmitLabel(EndLabel);
    end
    else if Op = '<=' then
    begin
      Emit('  JumpIfSign ' + TrueLabel);
      Emit('  JumpIfZero ' + TrueLabel);
      Emit('  Load 0');
      Emit('  Jump ' + EndLabel);
      EmitLabel(TrueLabel);
      Emit('  Load 1');
      EmitLabel(EndLabel);
    end
    else if Op = '>' then
    begin
      Emit('  JumpIfZero ' + FalseLabel);
      Emit('  JumpIfNotSign ' + TrueLabel);
      EmitLabel(FalseLabel);
      Emit('  Load 0');
      Emit('  Jump ' + EndLabel);
      EmitLabel(TrueLabel);
      Emit('  Load 1');
      EmitLabel(EndLabel);
    end
    else if Op = '>=' then
    begin
      Emit('  JumpIfNotSign ' + TrueLabel);
      Emit('  Load 0');
      Emit('  Jump ' + EndLabel);
      EmitLabel(TrueLabel);
      Emit('  Load 1');
      EmitLabel(EndLabel);
    end;
  end;
end;

procedure TSillyCCompiler.ParseExpression;
var
  Op: String;
  LFalse, LTrue, LEnd: String;
begin
  // parse left operand (comparisons/additive)
  ParseComparison;

  while (GetCurrentToken.Value = '&&') or (GetCurrentToken.Value = '||') do
  begin
    Op := GetCurrentToken.Value;
    NextToken;

    if Op = '&&' then
    begin
      LFalse := GenerateLabel + '_and_false';
      LEnd := GenerateLabel + '_and_end';
      // if left == 0 -> false
      Emit('  JumpIfZero ' + LFalse);
      // evaluate right
      ParseComparison;
      // if right == 0 -> false
      Emit('  JumpIfZero ' + LFalse);
      Emit('  Load 1');
      Emit('  Jump ' + LEnd);
      EmitLabel(LFalse);
      Emit('  Load 0');
      EmitLabel(LEnd);
    end
    else // '||'
    begin
      LTrue := GenerateLabel + '_or_true';
      LEnd := GenerateLabel + '_or_end';
      // if left != 0 -> true
      Emit('  JumpIfNotZero ' + LTrue);
      // evaluate right
      ParseComparison;
      Emit('  JumpIfNotZero ' + LTrue);
      Emit('  Load 0');
      Emit('  Jump ' + LEnd);
      EmitLabel(LTrue);
      Emit('  Load 1');
      EmitLabel(LEnd);
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
  LTrue, LEnd: String;
  VarName: String;
  TempName: String;
  Op: String;
  TempPre: String;
begin
  Token := GetCurrentToken;
  // Handle unary minus
  if (Token.TokenType = ttOperator) and (Token.Value = '-') then
  begin
    // consume '-'
    NextToken;
    // parse the factor after unary minus
    ParseFactor;
    // emit negate op
    Emit('  Negate');
    Exit;
  end;

  // Handle logical NOT '!'
  if (Token.TokenType = ttOperator) and (Token.Value = '!') then
  begin
    NextToken;
    ParseFactor;
    // if Acc == 0 -> 1 else 0
    LTrue := GenerateLabel + '_not_true';
    LEnd := GenerateLabel + '_not_end';
    Emit('  JumpIfZero ' + LTrue);
    Emit('  Load 0');
    Emit('  Jump ' + LEnd);
    EmitLabel(LTrue);
    Emit('  Load 1');
    EmitLabel(LEnd);
    Exit;
  end;

  if Token.TokenType = ttNumber then
  begin
    Emit('  Load ' + Token.Value);
    NextToken;
  end
  else if Token.TokenType = ttIdentifier then
  begin
    // handle function call in an expression: ident()
    if PeekToken.Value = '(' then
    begin
      // Call the function and leave return value in Acc
      Symbol := FindSymbol(Token.Value);
      // consume identifier and parentheses
      ExpectToken(ttIdentifier);
      ExpectToken(ttDelimiter, '(');
      ExpectToken(ttDelimiter, ')');
      Emit('  Call ' + Token.Value);
    end
    else
    begin
      // support post-increment/post-decrement: ident++ or ident--
      Symbol := FindSymbol(Token.Value);
      // consume identifier
      VarName := Token.Value;
      ExpectToken(ttIdentifier);
      if (GetCurrentToken.Value = '++') or (GetCurrentToken.Value = '--') then
      begin
        // post inc/dec: preserve old value in Acc, update variable
        TempName := GenerateLabel + '_temp';
        AddTempVariable(TempName);
        if Symbol.Name <> '' then
          Emit('  Load $' + Symbol.Name)
        else
          Emit('  Load $' + VarName);
        Emit('  Store $' + TempName);
        // consume operator
        Op := GetCurrentToken.Value;
        ExpectToken(ttOperator);
        Emit('  Load 1');
        if Op = '++' then
          Emit('  Add $' + TempName)
        else
          Emit('  Subtract $' + TempName);
        if Symbol.Name <> '' then
          Emit('  Store $' + Symbol.Name)
        else
          Emit('  Store $' + VarName);
        // restore original value into Acc (post returns old value)
        Emit('  Load $' + TempName);
      end
      else
      begin
        if Symbol.Name <> '' then
          Emit('  Load $' + Symbol.Name)
        else
          Emit('  Load $' + VarName);
      end;
    end;
  end
  else if (Token.TokenType = ttOperator) and ((Token.Value = '++') or (Token.Value = '--')) then
  begin
    // pre-increment/decrement in expression: ++ident or --ident
    Op := Token.Value;
    NextToken; // consume ++/--
    VarName := GetCurrentToken.Value;
    Symbol := FindSymbol(VarName);
    ExpectToken(ttIdentifier);
    TempPre := GenerateLabel + '_temp';
    AddTempVariable(TempPre);
    if Symbol.Name <> '' then
      Emit('  Load $' + Symbol.Name)
    else
      Emit('  Load $' + VarName);
    Emit('  Store $' + TempPre);
    Emit('  Load 1');
    if Op = '++' then
      Emit('  Add $' + TempPre)
    else
      Emit('  Subtract $' + TempPre);
    if Symbol.Name <> '' then
      Emit('  Store $' + Symbol.Name)
    else
      Emit('  Store $' + VarName);
    // Acc already contains new value after Add/Subtract
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

procedure TSillyCCompiler.ParseForStatement;
var
  InitTokenIndex, CondStart, CondEnd, PostStart, RParenIndex, AfterHeader, AfterBody: Integer;
  idx, level: Integer;
  StartLabel, EndLabel: String;
  VarName: String;
begin
  ExpectToken(ttKeyword, 'for');
  ExpectToken(ttDelimiter, '(');

  

  StartLabel := GenerateLabel;
  EndLabel := GenerateLabel;
  PostStart := 0;

  // --- init ---
  if GetCurrentToken.Value = ';' then
    ExpectToken(ttDelimiter, ';')
  else if GetCurrentToken.Value = 'int' then
  begin
    ExpectToken(ttKeyword, 'int');
    VarName := GetCurrentToken.Value;
    ExpectToken(ttIdentifier);
    // register local
    if FCurrentFunction <> '' then
    begin
      AddSymbol(VarName, stVariable);
      AddTempVariable(FCurrentFunction + '_' + VarName);
    end
    else
    begin
      AddSymbol(VarName, stVariable);
      AddTempVariable(VarName);
    end;

    if GetCurrentToken.Value = '=' then
    begin
      ExpectToken(ttOperator, '=');
      ParseExpression;
      if FCurrentFunction <> '' then
        Emit('  Store $' + FCurrentFunction + '_' + VarName)
      else
        Emit('  Store $' + VarName);
    end;

    ExpectToken(ttDelimiter, ';');
  end
  else if GetCurrentToken.TokenType = ttIdentifier then
  begin
    VarName := GetCurrentToken.Value;
    ExpectToken(ttIdentifier);
    if GetCurrentToken.Value = '=' then
    begin
      ExpectToken(ttOperator, '=');
      ParseExpression;
      ExpectToken(ttDelimiter, ';');
      // store into variable (assume existing symbol)
      if FCurrentFunction <> '' then
        Emit('  Store $' + FCurrentFunction + '_' + VarName)
      else
        Emit('  Store $' + VarName);
    end
    else
      ExpectToken(ttDelimiter, ';');
  end
  else
  begin
    ParseExpression;
    ExpectToken(ttDelimiter, ';');
  end;

  

  if GetCurrentToken.Value = ';' then
  begin
    ExpectToken(ttDelimiter, ';');
    EmitLabel(StartLabel);
  end
  else
  begin
    EmitLabel(StartLabel);
    ParseExpression;
    ExpectToken(ttDelimiter, ';');
    Emit('  JumpIfZero ' + EndLabel);
  end;

  // --- post: remember start index and find closing ) ---
  if GetCurrentToken.Value = ')' then
  begin
    ExpectToken(ttDelimiter, ')');
    AfterHeader := FCurrentToken;
    PostStart := 0;
  end
  else
  begin
    PostStart := FCurrentToken;
    // find matching ')'
    idx := PostStart;
    level := 0;
    while idx < Length(FTokens) do
    begin
      if FTokens[idx].Value = '(' then Inc(level)
      else if FTokens[idx].Value = ')' then
      begin
        if level = 0 then Break
        else Dec(level);
      end;
      Inc(idx);
    end;
    RParenIndex := idx; // index of ')'
    AfterHeader := RParenIndex + 1;
    // move to body start
    FCurrentToken := AfterHeader;
  end;

  // --- body ---
  ExpectToken(ttDelimiter, '{');
  while (GetCurrentToken.Value <> '}') and (GetCurrentToken.TokenType <> ttEOF) do
    ParseStatement;
  ExpectToken(ttDelimiter, '}');
  // remember token after the body so we can resume there
  AfterBody := FCurrentToken;

  // --- emit post expression if present ---
  if PostStart > 0 then
  begin
    FCurrentToken := PostStart;
    // handle a post-assignment like `i = i + 1` which isn't a plain expression
    if (GetCurrentToken.TokenType = ttIdentifier) and (PeekToken.Value = '=') then
    begin
      VarName := GetCurrentToken.Value;
      ExpectToken(ttIdentifier);
      ExpectToken(ttOperator, '=');
      ParseExpression;
      if FCurrentFunction <> '' then
        Emit('  Store $' + FCurrentFunction + '_' + VarName)
      else
        Emit('  Store $' + VarName);
    end
    else
      ParseExpression;
    // consume the closing ')'
    ExpectToken(ttDelimiter, ')');
    // restore token pointer to after the body
    FCurrentToken := AfterBody;
  end;

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
  // If we're returning from 'main' (entered via Jump), halt the machine.
  // Otherwise emit a Return to go back to the caller.
  if FCurrentFunction = 'main' then
    Emit('  Halt')
  else
  begin
    Emit('  Return');
    // mark that this function already emitted a Return
    FCurrentFunctionHasReturn := True;
  end;
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