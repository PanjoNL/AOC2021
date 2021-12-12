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
    BestGame, WorstGame: Int64;
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
    procedure BeforeSolve; override;
  end;

  TAdventOfCodeDay5 = class(TAdventOfCode)
  private
    function SacnHydrothermalVents(Const UseDiagonalLines: Boolean): integer;
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  TAdventOfCodeDay6 = class(TAdventOfCode)
  private
    function SimulateLanternfish(Const Days: Integer): Int64;
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  TAdventOfCodeDay7 = class(TAdventOfCode)
  private
    function CalculateFuelCost(ExpensiveBurning: boolean): integer;
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  TAdventOfCodeDay8 = class(TAdventOfCode)
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  TAdventOfCodeDay9 = class(TAdventOfCode)
  private
    function ExploreBasins(Const OnlyLowestPoints: boolean): integer;
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  TAdventOfCodeDay10 = class(TAdventOfCode)
  private
    SyntaxScore, AutoCompleteScore: int64;
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
    procedure BeforeSolve; override;
  end;

  TAdventOfCodeDay11 = class(TAdventOfCode)
  private
    function ObserveOctopuses(WaitForSync: Boolean): integer;
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  TCave = class
  public
    ConnectedCaves: TList<TCave>;
    IsStartCave, IsEndCave, IsLowercase: boolean;
    CaveId: Integer;
    constructor Create(const aCaveName: string; aCaveId: integer);
    destructor Destroy; override;
  end;

  TAdventOfCodeDay12 = class(TAdventOfCode)
  private
    Caves: TDictionary<string, TCave>;
    function ExploreCaves(Const CanUseLowercaseCaveTwice: boolean): integer;
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
  BingoNumbers, card: TStringDynArray;
  BingcoCard: Array[0..24] of integer;
  Round, GameResult: Int64;
  Base, x, y, CurrentNumber: Integer;
  HasWon: Boolean;
begin
  WorstGame := 0;
  BestGame := MaxInt64;
  Base := 2;

  BingoNumbers := SplitString(FInput[0], ',');

  while Base < FInput.Count -1 do
  begin
    for y := 0 to 4 do
    begin
      S := Trim(FInput[base].Replace('  ', ' ', [rfReplaceAll]));
      card := SplitString(S, ' ');
      for x := 0 to 4 do
        BingcoCard[x+5*y] := StrToInt(Card[x]);

      Inc(Base);
    end;
    Inc(base);

    Round := 0;
    for s in BingoNumbers do
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

        GameResult := (Round shl 32) + GameResult;
        BestGame := Min(BestGame, GameResult);
        WorstGame := Max(WorstGame, GameResult);

        break;
      end;
    end;
  end;
end;

function TAdventOfCodeDay4.SolveA: Variant;
begin
  Result := BestGame and MaxInt;
end;

function TAdventOfCodeDay4.SolveB: Variant;
begin
  Result := WorstGame and MaxInt;
end;

{$ENDREGION}
{$Region 'TAdventOfCodeDay5'}
function TAdventOfCodeDay5.SacnHydrothermalVents(const UseDiagonalLines: Boolean): integer;
const MapSize: integer = 1000;
var
  Map: Array of integer;
  IntersectionsCount: Integer;

  function _GetCoordinates(Const s: string): TPoint;
  var Split: TStringDynArray;
  begin
    Split := SplitString(s, ',');
    Result := TPoint.Create(Split[0].ToInteger, Split[1].ToInteger);
  end;

  procedure _AddPointToMap(Const aX, aY: integer);
  begin
    if Map[aX*MapSize+aY] = 1 then
      Inc(IntersectionsCount);

    Map[aX*MapSize+aY] := Map[aX*MapSize+aY] + 1;
  end;

  function _GetNewValue(Const aStart, aStop, aCurrent: integer): integer;
  begin
    Result := aCurrent + Sign(aStop - aStart);
  end;

var
  Split: TStringDynArray;
  StartCoordinates, StopCoordinates: TPoint;
  s: string;
  X,Y: integer;
begin
  SetLength(Map, MapSize*Mapsize);
  IntersectionsCount := 0;
  for s in FInput do
  begin
    Split := SplitString(s, ' ');
    StartCoordinates := _GetCoordinates(Split[0]);
    StopCoordinates := _GetCoordinates(Split[2]);

    if (not UseDiagonalLines) and (StartCoordinates.X <> StopCoordinates.X) and (StartCoordinates.Y <> StopCoordinates.Y) then
      Continue;

    X := StartCoordinates.X;
    Y := StartCoordinates.Y;
    Repeat
      _AddPointToMap(X,Y);
      X := _GetNewValue(StartCoordinates.X, StopCoordinates.X, X);
      Y := _GetNewValue(StartCoordinates.Y, StopCoordinates.Y, Y);
    until (X = StopCoordinates.X) and (Y = StopCoordinates.Y);
    _AddPointToMap(X,Y);
  end;

  Result := IntersectionsCount;
end;

function TAdventOfCodeDay5.SolveA: Variant;
begin
  Result := SacnHydrothermalVents(False);
end;

function TAdventOfCodeDay5.SolveB: Variant;
begin
  Result := SacnHydrothermalVents(True);
end;
{$ENDREGION}
{$Region 'TAdventOfCodeDay6'}
function TAdventOfCodeDay6.SolveA: Variant;
begin
  Result := SimulateLanternfish(80);
end;

function TAdventOfCodeDay6.SolveB: Variant;
begin
  Result := SimulateLanternfish(256);
end;

function TAdventOfCodeDay6.SimulateLanternfish(Const Days: Integer): Int64;
var
  Fish: array of int64;
  i: integer;
begin
  SetLength(Fish, Days);

  Fish[0] := OccurrencesOfChar(FInput[0],',')+1;
  for i := 1 to 5 do
    Fish[i] := Fish[i-1] + OccurrencesOfChar(FInput[0],i.ToString);

  Fish[6] := 2*Fish[0];
  Fish[7] := 2*Fish[0];
  Fish[8] := Fish[7] + Fish[1] - Fish[0];
  Fish[9] := Fish[8] + Fish[2] - Fish[1];

  for i := 10 to Days do
    Fish[i] := Fish[i-1] - Fish[i-10] - Fish[i-8] + Fish[i-9] + Fish[i-7];

  Result := Fish[Days-1];
end;
{$ENDREGION}
{$Region 'TAdventOfCodeDay7'}
function TAdventOfCodeDay7.SolveA: Variant;
begin
  Result := CalculateFuelCost(False);
end;

function TAdventOfCodeDay7.SolveB: Variant;
begin
  Result := CalculateFuelCost(True);
end;

function TAdventOfCodeDay7.CalculateFuelCost(ExpensiveBurning: boolean): integer;
var
  s: string;
  Split: TStringDynArray;
  BestFuelCost, Count, position, currentFuelCost, StartPosition, Distance: integer;
  Positions: TDictionary<integer, integer>;
  PositionPair: TPair<integer, integer>;
begin
  Positions := TDictionary<integer, integer>.Create();
  StartPosition := MaxInt;
  split := SplitString(FInput[0], ',');
  for s in split do
  begin
    position := s.ToInteger;
    StartPosition := Min(StartPosition, position);
    positions.TryGetValue(position, Count);
    Inc(Count);
    positions.AddOrSetValue(position, count)
  end;

  BestFuelCost := MaxInt;
  repeat
    currentFuelCost := 0;
    for PositionPair in Positions do
    begin
      Distance := abs(PositionPair.key - StartPosition);
      if ExpensiveBurning then
         inc(CurrentFuelCost, PositionPair.Value * Round(Distance * (Distance + 1) / 2))
      else
        inc(CurrentFuelCost, PositionPair.Value * Distance);
    end;

    BestFuelCost := Min(BestFuelCost, currentFuelCost);
    Inc(StartPosition);
  until (currentFuelCost > BestFuelCost);

  Result := BestFuelCost;
  Positions.Free;
end;
{$ENDREGION}
{$Region 'TAdventOfCodeDay8'}
function TAdventOfCodeDay8.SolveA: Variant;
var
  s, d: string;
  Split, display: TStringDynArray;
begin
  Result := 0;
  for s in FInput do
  begin
    Split := SplitString(s, '|');
    d := Trim(Split[1]);
    display := SplitString(d, ' ');
    for d in display do
      case Length(d) of
        2,3,4,7: inc(result);
      end;
  end;
end;

function TAdventOfCodeDay8.SolveB: Variant;

  function CalculateDisplay(Const aCheck, aTotal: string): integer;
  var KnownNumbers: Array[0..9] of Integer;

    function DisplayNumberToInt(aNumber: string): integer;
    var i: integer;
    begin
      Result := 0;
      for i := ord('a') to ord('g') do
      begin
        Result := Result shl 1;
        Result := Result + Sign(pos(chr(i), aNumber));
      end;
    end;

    function FindNumber(Const aNumber: string): integer;
    var int, i: integer;
    begin
      Result := -1;
      int := DisplayNumberToInt(aNumber);
      for i := 0 to 9 do
        if KnownNumbers[i] = int then
          Exit(i);
    end;

  var
    Split: TStringDynArray;
    Number, i: Integer;
  begin
    split := SplitString(aCheck, ' ');
    TArray.Sort<string>(Split, TDelegatedComparer<string>.Construct(
      function(const Left, Right: string): Integer
      begin
        Result := Sign(Length(Left)-Length(Right));
      end));

    KnownNumbers[1] := DisplayNumberToInt(Split[0]);
    KnownNumbers[7] := DisplayNumberToInt(Split[1]);
    KnownNumbers[4] := DisplayNumberToInt(Split[2]);
    KnownNumbers[8] := DisplayNumberToInt(Split[9]);

    for i := 3 to 8 do
    begin
      Number := DisplayNumberToInt(Split[i]);
      if i <= 5 then // Could still be 2,3,5
      begin
        if CountTrueBits(KnownNumbers[1] and Number) = 2 then
          KnownNumbers[3] := number
        else if CountTrueBits(KnownNumbers[4] and Number) = 3 then
          KnownNumbers[5] := number
        else
          KnownNumbers[2] := number;
      end
      else //Could still be 0,6,9
      begin
        if CountTrueBits(Number and KnownNumbers[4]) = 4 then
           KnownNumbers[9] := number
        else if CountTrueBits(KnownNumbers[1] and Number) = 2then
          KnownNumbers[0] := number
        else
          KnownNumbers[6] := number
      end;
    end;

    split := SplitString(aTotal, ' ');
    Result := 1000 * FindNumber(Split[0]);
    Result := Result + 100 * FindNumber(Split[1]);
    Result := Result + 10 * FindNumber(Split[2]);
    Result := Result + 1 * FindNumber(Split[3]);
  end;

var
  s: string;
  Split: TStringDynArray;
begin
  Result := 0;
  for s in FInput do
  begin
    Split := SplitString(s, '|');
    Result := Result + CalculateDisplay(Trim(Split[0]), Trim(Split[1]));
  end;
end;
{$ENDREGION}
{$Region 'TAdventOfCodeDay9'}
function TAdventOfCodeDay9.SolveA: Variant;
begin
  Result := ExploreBasins(True);
end;

function TAdventOfCodeDay9.SolveB: Variant;
begin
  Result := ExploreBasins(False);
end;

function TAdventOfCodeDay9.ExploreBasins(const OnlyLowestPoints: boolean): integer;
Const DeltaX: Array[0..3] of integer = (1,-1,0,0);
      DeltaY: Array[0..3] of integer = (0,0,1,-1);
var MapWidth, MapHeight: integer;

  function _Valid(Const aX, aY: Integer): boolean;
  begin
    Result := (aX >= 0) and (aX <= MapWidth) and (aY > 0) and (aY <= MapHeight);
  end;

  procedure CalcBasinSize(Const aStartX, aStartY, CurrHeight: integer; Const BasinPoints: TList<Integer>);
  var i, xCheck, yCheck, Height: integer;
  begin
    for i := 0 to 3 do
    begin
      xCheck := aStartX + DeltaX[i];
      yCheck := aStartY + DeltaY[i];
      if not _Valid(xCheck, yCheck) then
        Continue;

      if BasinPoints.Contains(xCheck*1000 + yCheck) then
        Continue;

      Height := StrToInt(FInput[xCheck][yCheck]);
      if (Height <= CurrHeight) or (Height >= 9) then
        Continue;

      BasinPoints.Add(xCheck*1000+yCheck);
      CalcBasinSize(xCheck, yCheck, Height, BasinPoints);
    end;
  end;

var
  x, y, xCheck, yCheck, Height, i: Integer;
  BasinPoints, Basins: TList<Integer>;
  IsLowestPoint: Boolean;
begin
  Result := 0;

  BasinPoints := TList<integer>.Create;
  Basins := TList<Integer>.Create;

  MapWidth := FInput.Count-1;
  MapHeight := Length(FInput[0]);

  for x := 0 to MapWidth do
    for y := 1 to MapHeight do
    begin
      Height := StrToInt(FInput[x][y]);
      IsLowestPoint := True;
      for i := 0 to 3 do
      begin
        yCheck := y + DeltaY[i];
        xCheck := x + DeltaX[i];
        if not _Valid(xcheck, yCheck) then
          Continue;

        IsLowestPoint := IsLowestPoint and (Height < StrToInt(FInput[xCheck][yCheck]))
      end;

      if Not IsLowestPoint then
        Continue;

      if OnlyLowestPoints then
        Inc(Result, Height + 1)
      else
      begin
        BasinPoints.Clear;
        BasinPoints.Add(x*1000+Y);
        CalcBasinSize(x,y, Height, BasinPoints);
        Basins.Add(BasinPoints.Count);
      end;
    end;

  if not OnlyLowestPoints then
  begin
    Basins.Sort;
    Basins.Reverse;
    Result := Basins[0] * Basins[1] * Basins[2];
  end;

  BasinPoints.Free;
  Basins.Free;
end;
{$ENDREGION}
{$Region 'TAdventOfCodeDay10'}
procedure TAdventOfCodeDay10.BeforeSolve;
Const open: Array[0..3] of string = ('(', '[', '{', '<');
      close:Array[0..3] of string = (')', ']', '}', '>');
      syntaxCost:Array[0..3] of integer = (3, 57, 1197, 25137);
var
  s, Current: string;
  i, index: integer;
  Stack: TStack<string>;
  AutoCompleteScores: TList<Int64> ;
begin
  SyntaxScore := 0;

  Stack := TStack<string>.Create;
  AutoCompleteScores := TList<Int64>.Create;

  for s in FInput do
  begin
    Stack.Clear;
    for i := 1 to Length(s) do
    begin
      Current := s[i];
      index := IndexStr(Current, open);
      if index >= 0 then
        Stack.Push(close[index])
      else if Stack.Pop <> current then
      begin
        inc(SyntaxScore, syntaxCost[indexStr(Current, Close)]);
        Stack.Clear;
        Break;
      end;
    end;

    if Stack.Count > 0 then
    begin
      AutoCompleteScore := 0;
      while Stack.Count > 0 do
        AutoCompleteScore := AutoCompleteScore * 5 + 1 + IndexStr(Stack.Pop, Close);

      AutoCompleteScores.Add(AutoCompleteScore);
    end;
  end;

  AutoCompleteScores.Sort;
  AutoCompleteScore := AutoCompleteScores[Trunc(AutoCompleteScores.Count/2)];

  AutoCompleteScores.Free;
  Stack.Free;
end;

function TAdventOfCodeDay10.SolveA: Variant;
begin
  Result := SyntaxScore;
end;

function TAdventOfCodeDay10.SolveB: Variant;
begin
  Result := AutoCompleteScore;
end;
{$ENDREGION}
{$REGION 'TAdventOfCodeDay11'}
function TAdventOfCodeDay11.SolveA: Variant;
begin
  Result := ObserveOctopuses(False)
end;

function TAdventOfCodeDay11.SolveB: Variant;
begin
  Result := ObserveOctopuses(True);
end;

function TAdventOfCodeDay11.ObserveOctopuses(WaitForSync: Boolean): integer;
const
  DeltaX: array [0..7] of integer = (1, 1, 1, 0, 0, -1, -1, -1);
  DeltaY: array [0..7] of integer = (1, 0, -1, -1, 1, 1, 0, -1);
var
  i, j, x, y, Count: integer;
  Point, NewPoint: TPoint;
  Octopuses: TDictionary<TPoint, integer>;
  Flashed: TList<TPoint>;
  FlashFound: Boolean;
begin
  Octopuses := TDictionary<TPoint, integer>.Create;
  Flashed := TList<TPoint>.Create;
  try
    for x := 0 to 9 do
      for y := 1 to 10 do
        Octopuses.Add(TPoint.Create(x, y), StrToInt(FInput[x][y]));

    Result := 0;
    i := 1;
    while (i <= 100) or WaitForSync do
    begin
      for point in Octopuses.Keys do
        Octopuses[Point] := Octopuses[point] + 1;

    Flashed.Clear;
    FlashFound := True;
    while FlashFound do
    begin
      FlashFound := False;
      for point in Octopuses.Keys do
        if (Octopuses[point] > 9) and not Flashed.Contains(point) then
        begin
          FlashFound := True;
          Inc(Result);
          Flashed.Add(point);

          for j := 0 to 7 do
          begin
            NewPoint := TPoint.Create(Point);
            NewPoint.Offset(DeltaX[j], DeltaY[j]);
            if Octopuses.TryGetValue(NewPoint, Count) then
              Octopuses[NewPoint] := Count + 1;
          end;
        end;
    end;

    if Flashed.Count = 100 then
      Exit(i);

    for point in Flashed do
      Octopuses[point] := 0;

    inc(i);
  end;
  finally
    Octopuses.Free;
    Flashed.Free;
  end;
end;
{$ENDREGION}
{$REGION 'TCave'}
constructor TCave.Create(const aCaveName: string; aCaveId: integer);
begin
  IsStartCave := SameText(aCaveName, 'start');
  IsEndCave := SameText(aCaveName, 'end');
  IsLowercase := SameStr(aCaveName, LowerCase(aCaveName));

  CaveId := aCaveId;

  ConnectedCaves := TList<TCave>.Create;
end;

destructor TCave.Destroy;
begin
  ConnectedCaves.Free;
end;
{$ENDREGION}
{$REGION 'TAdventOfCodeDay12'}
procedure TAdventOfCodeDay12.BeforeSolve;

  function _Cave(Const aCaveName: String): TCave;
  begin
    if not Caves.TryGetValue(aCaveName, Result) then
    begin
      Result := TCave.Create(aCaveName, Caves.Count);
      Caves.Add(aCaveName, Result);
    end;
  end;

var
  split: TStringDynArray;
  s: string;
  Cave1, Cave2: TCave;
begin
  Caves := TDictionary<string, TCave>.Create;

  for s in FInput do
  begin
    Split := SplitString(s, '-');

    Cave1 := _Cave(Split[0]);
    Cave2 := _Cave(Split[1]);

    Cave1.ConnectedCaves.Add(Cave2);
    Cave2.ConnectedCaves.Add(Cave1);
  end;
end;

procedure TAdventOfCodeDay12.AfterSolve;
var Cave: TCave;
begin
  for Cave in Caves.Values do
    Cave.Free;
  Caves.Free;
end;

function TAdventOfCodeDay12.SolveA: Variant;
begin
  Result := ExploreCaves(False);
end;

function TAdventOfCodeDay12.SolveB: Variant;
begin
  Result := ExploreCaves(True);
end;

type TByteSet = set of byte;
function TAdventOfCodeDay12.ExploreCaves(const CanUseLowercaseCaveTwice: boolean): integer;

  procedure _ExploreCaves(Const aCurrentCave: TCave; aVisitedLowerCaseCaves: TByteSet ; aLowercaseCaveUsedTwice: Boolean);
  var OtherCave: TCave;
      LowercaseCaveUsedTwice: boolean;
      VisitedLowerCaseCaves: TByteSet;
  begin
    for OtherCave in aCurrentCave.ConnectedCaves do
    begin
      if OtherCave.IsEndCave then
        Inc(Result);

      if OtherCave.IsStartCave or OtherCave.IsEndCave then
        Continue;

      LowercaseCaveUsedTwice := aLowercaseCaveUsedTwice;
      VisitedLowerCaseCaves := aVisitedLowerCaseCaves;
      if OtherCave.IsLowercase then
      begin
        if OtherCave.CaveId in VisitedLowerCaseCaves then
        begin
          if LowercaseCaveUsedTwice then
            Continue;

          LowercaseCaveUsedTwice := true;
        end;
        Include(VisitedLowerCaseCaves, OtherCave.CaveId);
      end;

      _ExploreCaves(OtherCave, VisitedLowerCaseCaves, LowercaseCaveUsedTwice);
    end;
  end;

begin
  Result := 0;
  _ExploreCaves(caves['start'], [], not CanUseLowercaseCaveTwice);
end;
{$ENDREGION}


{$REGION 'Placeholder'}
(*
  procedure TAdventOfCodeDay.BeforeSolve;
  begin

  end;

  procedure TAdventOfCodeDay.AfterSolve;
  begin

  end;

  function TAdventOfCodeDay.SolveA: Variant;
  var
  s: string;
  Split: TStringDynArray;
  i: integer;
  begin

  end;

  function TAdventOfCodeDay.SolveB: Variant;
  begin

  end;
*)
{$ENDREGION}

initialization

RegisterClasses([
    TAdventOfCodeDay1, TAdventOfCodeDay2, TAdventOfCodeDay3, TAdventOfCodeDay4, TAdventOfCodeDay5,
    TAdventOfCodeDay6, TAdventOfCodeDay7, TAdventOfCodeDay8, TAdventOfCodeDay9, TAdventOfCodeDay10,
    TAdventOfCodeDay11,TAdventOfCodeDay12]);

end.
