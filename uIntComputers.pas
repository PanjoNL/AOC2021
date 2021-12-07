unit uIntComputers;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Generics.Defaults, System.Generics.Collections,
  system.Diagnostics, AOCBase, RegularExpressions, System.DateUtils, system.StrUtils,
  system.Math;

type TParameterMode=(PositionMode, ImmediateMode, RelativeMode);
type TOnNoInputValue = procedure(var Input: Int64; Var Stop: Boolean) of object;

type IInstruction = interface
  function Opcode: Integer;
  function ParameterMode(Const aIndex: Integer): TParameterMode;
end;

type TInstruction = class(TInterfacedObject, IInstruction)
  private
    Parametermodes: TDictionary<Integer, TParameterMode>;
    FOpcode: Integer;
    procedure AddParameterMode(const aInstruction: string; Const aIndex: Integer);
  public
    constructor Create(aInstruction: Integer);
    destructor Destroy; override;
    function Opcode: Integer;
    function ParameterMode(Const aIndex: Integer): TParameterMode;
end;

type TBasicIntComputer = class(TPersistent)
private
  MemPos: Integer;
  RelativeBase: Integer;
  FInternalStop: Boolean;
  Fprogram: TDictionary<Integer, int64>;
  InputQueue: TQueue<int64>;
  function CanProcess: Boolean;
  function GetParam(Const aIndex: Integer; Const aInstruction: IInstruction): int64;
  procedure ProcessInstruction(const aInstruction: IInstruction);
  procedure JumpIf(Const JumpIfZero: Boolean; const aInstruction: IInstruction);
  procedure WriteMemoryAtOffset(Const aIndex, ValueToWrite: int64; const aInstruction: IInstruction);
  function GetMemoryAdres(Const aIndex: Integer; Const aInstruction: IInstruction): int64;
public
  LastOutput: int64;
  StopOnOutPut: Boolean;
  OnNoInputValue: TOnNoInputValue;

  constructor Create(aProgram: TDictionary<Integer, int64>); overload;
  constructor Create(Const Input: String); overload;
  destructor Destroy; override;

  function Run: int64;
  function IsStopped: Boolean;
  function GetMemory(Const MemoryIndex: Integer): int64;
  function InstructionQueueCount: Integer;
  procedure WriteMemory(const MemoryIndex, ValueToWrite: int64);
  procedure QueueInput(const Input: int64);
  procedure QueueASCIICode(Const Input: String);
  class function ParseIntput(const aProgram: String): TDictionary<Integer, int64>;
  class function RunProgram(const aProgram: string; aStartInput: Integer): Int64;
end;

implementation

////////////////////////////////// TInstruction //////////////////////////////////

constructor TInstruction.Create(aInstruction: Integer);
var Instruction: string;
begin
  Instruction := RightStr('00000'+IntToStr(aInstruction), 5); //104 -> 00104
  FOpcode := StrToInt(RightStr(Instruction, 2));
  Parametermodes := TDictionary<Integer, TParameterMode>.Create;
  AddParameterMode(Instruction, 1);
  AddParameterMode(Instruction, 2);
  AddParameterMode(Instruction, 3);
end;

destructor TInstruction.Destroy;
begin
  Parametermodes.Free;
end;

procedure TInstruction.AddParameterMode(const aInstruction: string; Const aIndex: Integer);
var Mode: TParameterMode;
begin
  case StrToInt(aInstruction[Length(aInstruction)-1-aIndex]) of
    0: Mode := PositionMode;
    1: Mode := ImmediateMode;
    2: Mode := RelativeMode;
  else
    raise Exception.Create('Unkonw opcode instruction');
  end;

  Parametermodes.Add(aIndex, Mode);
end;

function TInstruction.Opcode: Integer;
begin
  Result := FOpcode;
end;

function TInstruction.ParameterMode(Const aIndex: Integer): TParameterMode;
begin
  Result := Parametermodes[aIndex];
end;

////////////////////////////////// TBasicIntComputer //////////////////////////////////
class function TBasicIntComputer.ParseIntput(const aProgram: String): TDictionary<Integer, int64>;
var Line: TStringList;
    i: Integer;
begin
  Result := TDictionary<Integer, int64>.Create;
  Line := TStringList.Create;
  Line.Delimiter := ',';
  Line.DelimitedText := aProgram;

  for i := 0 to Line.Count - 1 do
    Result.Add(i, StrToInt64(Line[i]));

  Line.Free;
end;

class function TBasicIntComputer.RunProgram(const aProgram: string; aStartInput: Integer): Int64;
var Computer: TBasicIntComputer;
begin
  Computer := TBasicIntComputer.Create(aProgram);
  Computer.LastOutput := aStartInput;
  Result := Computer.Run;
  Computer.Free;
end;

constructor TBasicIntComputer.Create(Const Input: String);
var ComputerProgram :TDictionary<Integer, Int64>;
begin
  ComputerProgram := ParseIntput(Input);
  Create(ComputerProgram);
  ComputerProgram.Free;
end;

constructor TBasicIntComputer.Create(aProgram: TDictionary<Integer, int64>);
begin
  FProgram := TDictionary<Integer, int64>.Create(aProgram);
  InputQueue := TQueue<int64>.Create;
  MemPos := 0;
  RelativeBase := 0;
end;

destructor TBasicIntComputer.Destroy;
begin
  FProgram.Free;
  InputQueue.Free;
end;

function TBasicIntComputer.Run: int64;
var Instruction: IInstruction;
begin
  FInternalStop := False;
  while CanProcess do
  begin
    Instruction := TInstruction.Create(FProgram[MemPos]);
    ProcessInstruction(Instruction);
  end;

  Result := LastOutput;
end;

function TBasicIntComputer.IsStopped: Boolean;
begin
  Result := (FProgram[MemPos] = 99);
end;

procedure TBasicIntComputer.WriteMemory(const MemoryIndex, ValueToWrite: int64);
begin
  Fprogram.AddOrSetValue(MemoryIndex, ValueToWrite);
end;

procedure TBasicIntComputer.QueueInput(const Input: int64);
begin
  InputQueue.Enqueue(Input);
end;

procedure TBasicIntComputer.QueueASCIICode(Const Input: String);
var i: Integer;
begin
  for i := 1 to Length(Input) do
    QueueInput(Ord(Input[i]));
  QueueInput(10); //Final linefeed
end;

function TBasicIntComputer.GetMemory(Const MemoryIndex: Integer): int64;
begin
  if not Fprogram.TryGetValue(MemoryIndex, Result) then
    Result := 0;
end;

function TBasicIntComputer.InstructionQueueCount: Integer;
begin
  Result := InputQueue.Count;
end;

function TBasicIntComputer.CanProcess: Boolean;
begin
  Result := (Not IsStopped) and (Not FInternalStop);
end;

function TBasicIntComputer.GetMemoryAdres(Const aIndex: Integer; Const aInstruction: IInstruction): int64;
begin
  case aInstruction.ParameterMode(aIndex) of
    PositionMode : Result := GetMemory(MemPos + aIndex);
    ImmediateMode: Result := MemPos + aIndex;
    RelativeMode : Result := GetMemory(MemPos + aIndex) + RelativeBase;
  else
    raise Exception.Create('Unknown param mode');
  end;
end;

function TBasicIntComputer.GetParam(Const aIndex: Integer; Const aInstruction: IInstruction): int64;
begin
  Result := GetMemory(GetMemoryAdres(aIndex, aInstruction));
end;

procedure TBasicIntComputer.WriteMemoryAtOffset(Const aIndex, ValueToWrite: int64; const aInstruction: IInstruction);
begin
  WriteMemory(GetMemoryAdres(aIndex, aInstruction), ValueToWrite);
  Inc(MemPos, aIndex + 1);
end;

procedure TBasicIntComputer.JumpIf(Const JumpIfZero: Boolean; const aInstruction: IInstruction);
begin
  if (GetParam(1, aInstruction) = 0) = JumpIfZero then
    MemPos := GetParam(2, aInstruction)
  else
    Inc(MemPos, 3);
end;

procedure TBasicIntComputer.ProcessInstruction(const aInstruction: IInstruction);
var Temp: Int64;
begin
  case aInstruction.Opcode of
    01: WriteMemoryAtOffset(3, GetParam(1, aInstruction) + GetParam(2, aInstruction), aInstruction);
    02: WriteMemoryAtOffset(3, GetParam(1, aInstruction) * GetParam(2, aInstruction), aInstruction);
    03: begin
          if InputQueue.Count > 0 then
            Temp := InputQueue.Dequeue
          else
          begin
            Temp := LastOutput;
            if Assigned(OnNoInputValue) then
              OnNoInputValue(Temp, FInternalStop);
          end;
          WriteMemoryAtOffset(1, Temp, aInstruction);
        end;
    04: begin
          LastOutput := GetParam(1, aInstruction);
          Inc(MemPos, 2);
          FInternalStop := StopOnOutPut;
        end;
    05: JumpIf(False, aInstruction);
    06: JumpIf(True, aInstruction);
    07: WriteMemoryAtOffset(3, Integer(GetParam(1, aInstruction) < GetParam(2, aInstruction)), aInstruction);
    08: WriteMemoryAtOffset(3, Integer(GetParam(1, aInstruction) = GetParam(2, aInstruction)), aInstruction);
    09: begin
          Inc(RelativeBase, GetParam(1, aInstruction));
          Inc(MemPos, 2);
        end;
  else
    raise Exception.CreateFmt('Unknown opcode: %d', [aInstruction.Opcode]);
  end;
end;

end.
