unit SillyMachine;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type
  TOperation = (
                            {
                              Acc: Accumulator Register
                              Idx: Index Register
                              PC: Program Counter Register
                              SP: Stack Pointer Register
                              ZF: Zero Flag
                              SF: Sign Flag
                              HF: Halt Flag
                            }
    opNop,                  // Nothing (empty cycle)
    opLoadConst,            // Acc := const
    opLoadMemDirect,        // Acc := Mem[const]
    opLoadMemIndirect,      // Acc := Mem[Idx]
    opStoreMemDirect,       // Mem[const] := Acc
    opStoreMemIndirect,     // Mem[Idx] := Acc
    opLoadIndex,            // Acc := Idx
    opStoreIndex,           // Idx := Acc
    opAddConst,             // Acc := Acc + const
    opAddMemDirect,         // Acc := Acc + Mem[const]
    opAddMemIndirect,       // Acc := Acc + Mem[Idx]
    opSubtractConst,        // Acc := Acc - const
    opSubtractMemDirect,    // Acc := Acc - Mem[const]
    opSubtractMemIndirect,  // Acc := Acc - Mem[Idx]
    opNegate,               // Acc := -Acc
    opIncAccumulator,       // Acc := Acc + 1
    opDecAccumulator,       // Acc := Acc - 1
    opIncIndex,             // Idx := Idx + 1
    opDecIndex,             // Idx := Idx - 1
    opJump,                 // PC := const
    opJumpIfZero,           // PC := const IF ZF ELSE PC + 1
    opJumpIfSign,           // PC := const IF SF ELSE PC + 1
    opJumpIfNotZero,        // PC := const IF NOT ZF ELSE PC + 1
    opJumpIfNotSign,        // PC := const IF NOT SF ELSE PC + 1
    opPush,                 // Mem[SP] := Acc; SP := SP - 1
    opPop,                  // SP := SP + 1; Acc := Mem[SP]
    opCall,                 // Mem[SP] := PC + 1; SP := SP - 1; PC := const
    opReturn,               // SP := SP + 1; PC := Mem[SP]
    opPrintChar,            // Stdout <- CHAR(Acc)
    opPrintInteger,         // Stdout <- INTEGER(Acc)
    opPrintString,          // Stdout <- Sequence of chars from memory
                            //   starting with adderss in Accumulator,
                            //   ending with a zero word (null terminated).
    opSleep,                // Pause for amount of ms written in the Accumlator
    opHalt                  // HF := true
  );

  TRegisters = record
    Accumulator: SmallInt;
    Index: Word;
    ProgramCounter: Word;
    StackPointer: Word;
    SignFlag: Boolean;
    ZeroFlag: Boolean;
    HaltFlag: Boolean;
  end;

  { TMachine }

  TMachine = class
  private
    FMemory: array of Word;
    FRegisters: TRegisters;
    FCycles: LongWord;

    procedure Nop;
    procedure LoadConst;
    procedure LoadMemDirect;
    procedure LoadMemIndirect;
    procedure StoreMemDirect;
    procedure StoreMemIndirect;
    procedure StoreIndex;
    procedure LoadIndex;
    procedure AddConst;
    procedure AddMemDirect;
    procedure AddMemIndirect;
    procedure SubtractConst;
    procedure SubtractMemIndirect;
    procedure SubtractMemDirect;
    procedure Negate;
    procedure IncAccumulator;
    procedure DecAccumulator;
    procedure IncIndex;
    procedure DecIndex;
    procedure Jump;
    procedure JumpIfSign;
    procedure JumpIfZero;
    procedure JumpIfNotSign;
    procedure JumpIfNotZero;
    procedure Push;
    procedure Pop;
    procedure Call();
    procedure Return();
    procedure PrintChar();
    procedure PrintInteger();
    procedure PrintString();
    procedure SleepMs();
    procedure Halt();
    procedure UpdateFlags();
  public
    property Cycles: LongWord read FCycles;
    constructor Create(MemorySize: Integer = 65536);
    procedure Reset();
    procedure Run();
    procedure Step();
    procedure LoadData(Address: Word; Data: TBytesStream);
    procedure PrintState();
  end;

implementation

{ TMachine }

procedure TMachine.Nop();
begin
  // Do nothing
end;

procedure TMachine.LoadConst();
begin
  FRegisters.Accumulator := SmallInt(FMemory[FRegisters.ProgramCounter]);
  Inc(FRegisters.ProgramCounter);
  UpdateFlags();
end;

procedure TMachine.LoadMemDirect();
var
  Address: Word;
begin
  Address := FMemory[FRegisters.ProgramCounter];
  Inc(FRegisters.ProgramCounter);
  FRegisters.Accumulator := SmallInt(FMemory[Address]);
  UpdateFlags();
end;

procedure TMachine.LoadMemIndirect();
begin
  FRegisters.Accumulator := SmallInt(FMemory[FRegisters.Index]);
  UpdateFlags();
end;

procedure TMachine.StoreMemIndirect();
begin
  FMemory[FRegisters.Index] := Word(FRegisters.Accumulator);
end;

procedure TMachine.StoreMemDirect();
var
  Address: Integer;
begin
  Address := FMemory[FRegisters.ProgramCounter];
  Inc(FRegisters.ProgramCounter);
  FMemory[Address] := Word(FRegisters.Accumulator);
end;

procedure TMachine.StoreIndex();
begin
  FRegisters.Index := FRegisters.Accumulator;
end;

procedure TMachine.LoadIndex();
begin
  FRegisters.Accumulator := FRegisters.Index;
  UpdateFlags();
end;

procedure TMachine.AddConst();
begin
  Inc(FRegisters.Accumulator, SmallInt(FMemory[FRegisters.ProgramCounter]));
  Inc(FRegisters.ProgramCounter);
  UpdateFlags();
end;

procedure TMachine.AddMemDirect();
var
  Address: Word;
begin
  Address := FMemory[FRegisters.ProgramCounter];
  Inc(FRegisters.ProgramCounter);
  Inc(FRegisters.Accumulator, SmallInt(FMemory[Address]));
  UpdateFlags();
end;

procedure TMachine.AddMemIndirect();
begin
  Inc(FRegisters.Accumulator, SmallInt(FMemory[FRegisters.Index]));
  UpdateFlags();
end;

procedure TMachine.SubtractConst();
begin
  Dec(FRegisters.Accumulator, SmallInt(FMemory[FRegisters.ProgramCounter]));
  Inc(FRegisters.ProgramCounter);
  UpdateFlags();
end;

procedure TMachine.SubtractMemDirect();
var
  Address: Word;
begin
  Address := FMemory[FRegisters.ProgramCounter];
  Inc(FRegisters.ProgramCounter);
  Dec(FRegisters.Accumulator, SmallInt(FMemory[Address]));
  UpdateFlags();
end;

procedure TMachine.SubtractMemIndirect();
begin
  Dec(FRegisters.Accumulator, SmallInt(FMemory[FRegisters.Index]));
  UpdateFlags();
end;

procedure TMachine.Negate();
begin
  FRegisters.Accumulator := -FRegisters.Accumulator;
  UpdateFlags();
end;

procedure TMachine.IncAccumulator();
begin
  Inc(FRegisters.Accumulator);
  UpdateFlags();
end;

procedure TMachine.DecAccumulator();
begin
  Dec(FRegisters.Accumulator);
  UpdateFlags();
end;

procedure TMachine.IncIndex();
begin
  Inc(FRegisters.Index);
end;

procedure TMachine.DecIndex();
begin
  Dec(FRegisters.Index);
end;

procedure TMachine.Jump();
begin
  FRegisters.ProgramCounter := FMemory[FRegisters.ProgramCounter];
end;

procedure TMachine.JumpIfSign();
begin
  if FRegisters.SignFlag then
    FRegisters.ProgramCounter := FMemory[FRegisters.ProgramCounter]
  else
    Inc(Fregisters.ProgramCounter);
end;

procedure TMachine.JumpIfZero();
begin
  if FRegisters.ZeroFlag then
    FRegisters.ProgramCounter := FMemory[FRegisters.ProgramCounter]
  else
    Inc(Fregisters.ProgramCounter);
end;

procedure TMachine.JumpIfNotSign();
begin
  if not FRegisters.SignFlag then
    FRegisters.ProgramCounter := FMemory[FRegisters.ProgramCounter]
  else
    Inc(Fregisters.ProgramCounter);
end;

procedure TMachine.JumpIfNotZero();
begin
  if not FRegisters.ZeroFlag then
    FRegisters.ProgramCounter := FMemory[FRegisters.ProgramCounter]
  else
    Inc(Fregisters.ProgramCounter);
end;

procedure TMachine.Push();
begin
  FMemory[FRegisters.StackPointer] := Word(FRegisters.Accumulator);
  Dec(FRegisters.StackPointer);
end;

procedure TMachine.Pop();
begin
  Inc(FRegisters.StackPointer);
  FRegisters.Accumulator := SmallInt(FMemory[FRegisters.StackPointer]);
  UpdateFlags();
end;

procedure TMachine.Call();
begin
  FMemory[FRegisters.StackPointer] := Word(FRegisters.ProgramCounter + 1);
  Dec(FRegisters.StackPointer);
  FRegisters.ProgramCounter := FMemory[FRegisters.ProgramCounter];
end;

procedure TMachine.Return();
begin
  Inc(FRegisters.StackPointer);
  FRegisters.ProgramCounter := FMemory[FRegisters.StackPointer];
end;

procedure TMachine.PrintChar();
begin
  Write(Chr(FRegisters.Accumulator));
end;

procedure TMachine.PrintInteger();
begin
  Writeln(FRegisters.Accumulator);
end;

procedure TMachine.PrintString();
var
  Address: Word;
begin
  Address := FRegisters.Accumulator;
  while FMemory[Address] <> 0 do
  begin
    Write(Chr(FMemory[Address]));
    Inc(Address);
  end;
  Writeln();
end;

procedure TMachine.SleepMs;
begin
  Sleep(FRegisters.Accumulator);
end;

procedure TMachine.Halt();
begin
  FRegisters.HaltFlag := true;
end;

procedure TMachine.UpdateFlags();
begin
  FRegisters.ZeroFlag := FRegisters.Accumulator = 0;
  FRegisters.SignFlag := FRegisters.Accumulator < 0;
end;

constructor TMachine.Create(MemorySize: Integer = 65536);
begin
  inherited Create();
  SetLength(FMemory, MemorySize);
  Reset();
end;

procedure TMachine.Reset();
begin
  FCycles := 0;
  FRegisters.Accumulator := 0;
  FRegisters.Index := 0;
  FRegisters.ProgramCounter := Low(FMemory);
  FRegisters.StackPointer := High(FMemory);
  FRegisters.SignFlag := false;
  FRegisters.ZeroFlag := false;
  FRegisters.HaltFlag := false;
end;

procedure TMachine.Run();
begin
  while not FRegisters.HaltFlag do
    Step();
end;

procedure TMachine.Step();
var
  Opcode: Word;
  Operation: TOperation;
begin
  Inc(FCycles);
  Opcode := FMemory[FRegisters.ProgramCounter];
  //Write(Opcode, ' ');
  Operation := TOperation(Opcode);
  Inc(FRegisters.ProgramCounter);

  case Operation of
    opNop: Nop();
    opLoadConst: LoadConst();
    opLoadMemDirect: LoadMemDirect();
    opLoadMemIndirect: LoadMemIndirect();
    opStoreMemIndirect: StoreMemIndirect();
    opStoreMemDirect: StoreMemDirect();
    opStoreIndex: StoreIndex();
    opLoadIndex: LoadIndex();
    opAddConst: AddConst();
    opAddMemDirect: AddMemDirect();
    opAddMemIndirect: AddMemIndirect();
    opSubtractConst: SubtractConst();
    opSubtractMemDirect: SubtractMemDirect();
    opSubtractMemIndirect: SubtractMemIndirect();
    opNegate: Negate();
    opIncAccumulator: IncAccumulator();
    opDecAccumulator: DecAccumulator();
    opIncIndex: IncIndex();
    opDecIndex: DecIndex();
    opJump: Jump;
    opJumpIfZero: JumpIfZero();
    opJumpIfSign: JumpIfSign();
    opJumpIfNotZero: JumpIfNotZero();
    opJumpIfNotSign: JumpIfNotSign();
    opPush: Push();
    opPop: Pop();
    opCall: Call();
    opReturn: Return();
    opPrintChar: PrintChar();
    opPrintInteger: PrintInteger();
    opPrintString: PrintString();
    opSleep: SleepMs();
    opHalt: Halt();
  end;
end;

procedure TMachine.LoadData(Address: Word; Data: TBytesStream);
begin
  Data.ReadBuffer(FMemory[Address], Data.Size);
end;

procedure TMachine.PrintState();
begin
  Writeln('------- Machine -------');
  WriteLn(Format('%-16s %d', ['Accumulator:', FRegisters.Accumulator]));
  WriteLn(Format('%-16s %d', ['Index:', FRegisters.Index]));
  WriteLn(Format('%-16s %d', ['Program Counter:', FRegisters.ProgramCounter]));
  WriteLn(Format('%-16s %d', ['Stack Pointer:', FRegisters.StackPointer]));
  WriteLn(Format('%-16s %.0n', ['Cycles:', Double(FCycles)]));
end;

end.

