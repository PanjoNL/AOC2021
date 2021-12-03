unit AOCSolutions;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Generics.Defaults, System.Generics.Collections,
  system.Diagnostics, AOCBase, RegularExpressions, System.DateUtils, system.StrUtils,
  system.Math, uAOCUtils, system.Types;

type
  TAdventOfCodeDay1 = class(TAdventOfCode)
  private
    function SonarSweep(Const OffSet: integer): integer;
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  TAdventOfCodeDay2 = class(TAdventOfCode)
  private
    function ProcessCommands(Const PartOne: Boolean): integer;
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  TAdventOfCodeDay3 = class(TAdventOfCode)
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

{$REGION 'placeholder'}
(*
  TAdventOfCodeDay = class(TAdventOfCode)
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
    procedure BeforeSolve; override;
    procedure AfterSolve; override;
  end;
*)
{$ENDREGION}



implementation

{$Region 'TAdventOfCodeDay1'}
function TAdventOfCodeDay1.SolveA: Variant;
begin
  Result := SonarSweep(1);
end;

function TAdventOfCodeDay1.SolveB: Variant;
begin
  Result := SonarSweep(3);
end;

function TAdventOfCodeDay1.SonarSweep(Const OffSet: integer): integer;
var i: integer;
begin
  Result := 0;
  for i := 0 to FInput.Count-OffSet-1 do
    if StrToInt(FInput[i]) < StrToInt(FInput[i+OffSet]) then
      Inc(Result);
end;

{$ENDREGION}
{$Region 'TAdventOfCodeDay2'}
function TAdventOfCodeDay2.SolveA: Variant;
begin
  Result := ProcessCommands(True);
end;

function TAdventOfCodeDay2.SolveB: Variant;
begin
  Result := ProcessCommands(False);
end;

function TAdventOfCodeDay2.ProcessCommands(const PartOne: Boolean): integer;
var
  s: string;
  Split: TStringDynArray;
  depth, hPosition, aim, units: integer;
begin
  depth := 0;
  hPosition := 0;
  aim := 0;

  for s in FInput do
  begin
    Split := SplitString(s, ' ');
    units := StrToInt(Split[1]);
    case IndexText(split[0], ['forward','down','up']) of
      0: begin
          inc(hPosition, units);
          if not PartOne then
            inc(depth, aim * units);
         end;
      1: if PartOne then
          inc(depth, units)
         else
          inc(aim, units);
      2: if PartOne then
          dec(depth, units)
        else
          dec(aim, units);
    else
      Assert(false, s);
    end;
  end;

  Result := depth * hPosition;
end;
{$ENDREGION}
{$Region 'TAdventOfCodeDay3'}
function TAdventOfCodeDay3.SolveA: Variant;
var
  s: string;
  OneBits: array of Integer;
  i, current, GammaRate, EpsilonRate, NoOneBits, NoZeroBits: integer;
begin
  SetLength(OneBits, Length(FInput[0])+1);
  for s in FInput do
    for i := 1 to Length(s) do
      if s[i] = '1' then
        OneBits[i] := OneBits[i] + 1;

  GammaRate := 0;
  EpsilonRate := 0;
  for i := 1 to Length(OneBits) do
  begin
    NoOneBits := OneBits[i];
    NoZeroBits := FInput.Count - NoOneBits;

    GammaRate := GammaRate shl 1;
    EpsilonRate := EpsilonRate shl 1;

    if NoOneBits > NoZeroBits  then
      inc(GammaRate)
    else
      inc(EpsilonRate);
  end;

  Result := EpsilonRate * GammaRate;
end;

function TAdventOfCodeDay3.SolveB: Variant;

  function _FindDiagnosticValue(Const KeepLeastCommenValue: boolean): integer;
  var
    TempInput: TStringList;
    NoOneBits, NoZeroBits, BitIndex, i: Integer;
    KeepZero: boolean;
    s: string;
  begin
    TempInput := TStringList.Create;
    TempInput.Assign(FInput);

    for BitIndex := 1 to Length(FInput[0]) do
    begin
      NoOneBits := 0;
      NoZeroBits := 0;

      for s in TempInput do
      if s[BitIndex] = '1' then
        inc(NoOneBits)
      else
        inc(NoZeroBits);

      KeepZero := NoZeroBits > NoOneBits;
      if KeepLeastCommenValue then
        KeepZero := not KeepZero;

      for i := TemPInput.Count-1 downTo 0 do
        if (TemPInput[i][BitIndex] = '1') and KeepZero then
          TempInput.Delete(i)
        else if (TemPInput[i][BitIndex] = '0') and (not KeepZero) then
          TempInput.Delete(i);

      if TempInput.Count = 1 then
        break;
    end;

    Result := BitStringToInt(TempInput[0]);
    TempInput.Free;
  end;

begin
  Result := _FindDiagnosticValue(False) * _FindDiagnosticValue(True);
end;
{$ENDREGION}



{$Region 'Placeholder'}
(*
procedure TAdventOfCodeDay.BeforeSolve;
begin

end;

procedure TAdventOfCodeDay.AfterSolve;
begin

end;

function TAdventOfCodeDay.SolveA: Variant;
begin

end;

function TAdventOfCodeDay.SolveB: Variant;
begin

end;
*)
{$ENDREGION}

initialization
  RegisterClasses([TAdventOfCodeDay1, TAdventOfCodeDay2, TAdventOfCodeDay3]);

end.

