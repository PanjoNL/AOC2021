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

  TAdventOfCodeDay4 = class(TAdventOfCode)
  private
    GameResults: TList<Int64>;
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
    procedure BeforeSolve; override;
    procedure AfterSolve; override;
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
  i, GammaRate, EpsilonRate, NoOneBits, NoZeroBits: integer;
begin
  SetLength(OneBits, Length(FInput[0]));
  for s in FInput do
    for i := 0 to Length(s) -1 do
      if s[i+1] = '1' then
        OneBits[i] := OneBits[i] + 1;

  GammaRate := 0;
  EpsilonRate := 0;
  for i := 0 to Length(OneBits)-1 do
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
{$Region 'TAdventOfCodeDay4'}
procedure TAdventOfCodeDay4.BeforeSolve;
var
  s: string;
  Numbers, card: TStringDynArray;
  BingcoCard: Array[0..24] of integer;
  Round, GameResult: Int64;
  Base, x,y, CurrentNumber: Integer;
  HasWon: Boolean;
begin
  GameResults := TList<Int64>.create;
  Numbers := SplitString(FInput[0], ',');

  Base := 2;
  while Base < FInput.Count -1 do
  begin
    for y := 0 to 4 do
    begin
      S := Trim(FInput[base].Replace('  ', ' ', [rfReplaceAll]));
      card := SplitString(S, ' ');
      for x := 0 to 4 do
        BingcoCard[x+5*y] := StrToInt(Card[x]);

      Base := Base + 1;
    end;
    Base := Base + 1;

    Round := 0;
    for s in Numbers do
    begin
      CurrentNumber := StrToInt(s);
      inc(round);

      for x := 0 to 24 do
        if BingcoCard[x] = CurrentNumber then
          BingcoCard[x] := -1;

      HasWon := False;
      for x := 0 to 4 do
        HasWon := HasWon
               or ((BingcoCard[x] < 0) and (BingcoCard[x+5] < 0) and (BingcoCard[x+10] < 0) and (BingcoCard[x+15] < 0) and (BingcoCard[x+20] < 0))
               or ((BingcoCard[5*x] < 0) and (BingcoCard[5*x+1] < 0) and (BingcoCard[5*x+2] < 0) and (BingcoCard[5*x+3] < 0) and (BingcoCard[5*x+4] < 0));

      if HasWon then
      begin
        GameResult := 0;
        for x := 0 to 24 do
        if BingcoCard[x] > 0 then
          GameResult := GameResult + CurrentNumber * BingcoCard[x];

        GameResult := (GameResult shl 32) + Round;
        GameResults.Add(GameResult);
        break;
      end;
    end;
  end;
end;

procedure TAdventOfCodeDay4.AfterSolve;
begin
  GameResults.Free;
end;

function TAdventOfCodeDay4.SolveA: Variant;
var
  Round, BestRound: integer;
  Game: Int64;
begin
  BestRound := MaxInt;
  for Game in GameResults do
  begin
    Round := Game and MaxInt;
    if Round < BestRound then
    begin
      Result := Game shr 32;
      BestRound := Round;
    end;
  end;
end;

function TAdventOfCodeDay4.SolveB: Variant;
var
  Round, BestRound: integer;
  Game: Int64;
begin
  BestRound := 0;
  for Game in GameResults do
  begin
    Round := Game and MaxInt;
    if Round > BestRound then
    begin
      Result := Game shr 32;
      BestRound := Round;
    end;
  end;
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
  RegisterClasses([TAdventOfCodeDay1, TAdventOfCodeDay2, TAdventOfCodeDay3, TAdventOfCodeDay4]);

end.

