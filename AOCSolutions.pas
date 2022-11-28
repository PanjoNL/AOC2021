unit AOCSolutions;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes,
  Generics.Defaults, System.Generics.Collections,
  System.Diagnostics, AOCBase, RegularExpressions, System.DateUtils,
  System.StrUtils,
  System.Math, uAOCUtils, System.Types, PriorityQueues, System.Json;

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
    function SimulateLanternfish(Const Days: integer): Int64;
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  TAdventOfCodeDay7 = class(TAdventOfCode)
  private
    function CalculateFuelCost(ExpensiveBurning: Boolean): integer;
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
    function ExploreBasins(Const OnlyLowestPoints: Boolean): integer;
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  TAdventOfCodeDay10 = class(TAdventOfCode)
  private
    SyntaxScore, AutoCompleteScore: Int64;
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
    IsStartCave, IsEndCave, IsLowercase: Boolean;
    CaveId: integer;
    constructor Create(const aCaveName: string; aCaveId: integer);
    destructor Destroy; override;
  end;

  TAdventOfCodeDay12 = class(TAdventOfCode)
  private
    Caves: TDictionary<string, TCave>;
    function ExploreCaves(Const CanUseLowercaseCaveTwice: Boolean): integer;
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
    procedure BeforeSolve; override;
    procedure AfterSolve; override;
  end;

  TAdventOfCodeDay13 = class(TAdventOfCode)
  private
    function FoldPaper(Const OnlyOnce: Boolean): Variant;
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  TAdventOfCodeDay14 = class(TAdventOfCode)
  private
    Rules: TDictionary<string, string>;
    function BuildPolymer(Const aRounds: integer): Int64;
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
    procedure BeforeSolve; override;
    procedure AfterSolve; override;
  end;

  TAdventOfCodeDay15 = class(TAdventOfCode)
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
    function CheckCavern(Const MapSizeMultiplier: integer): integer;
  end;

  TAdventOfCodeDay16 = class(TAdventOfCode)
  private
    VersionNumbers, BITSmessage: Int64;
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
    procedure BeforeSolve; override;
  end;

  TAdventOfCodeDay17 = class(TAdventOfCode)
  private
    MaxHeight, VelocityValues: integer;
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
    procedure BeforeSolve; override;
  end;

type
  TSnailFishNode = class
  private
    Parent, Left, Right: TSnailFishNode;
    IntValue: integer;

    function Explode(aDepth: integer): Boolean;
    function Split: Boolean;
    function IsSimpleNode: Boolean;
    function GetNode(aLeft: Boolean): TSnailFishNode;
    procedure UpdateAfterExplosion(aValue: Integer; aExplodingNode: TSnailFishNode; aLeft: Boolean);
  public
    Constructor Create(aString: string); overload;
    Constructor Create(aParent: TSnailFishNode; aJson: TJsonValue); overload;
    Constructor Create(aParent: TSnailFishNode; aIntValue: integer); overload;
    Constructor Create(aLeft, aRight: TSnailFishNode); overload;
    destructor Destroy; override;

    procedure Stabalize;
    function CalcMagnitude: integer;
  end;

  TAdventOfCodeDay18 = class(TAdventOfCode)
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  TPosition3 = record
    x, y, z: int64;
    class function Create(Const aX, aY, aZ: int64): TPosition3; static;
    class operator Add(a, b: TPosition3): TPosition3;
    class operator Subtract(a, b: TPosition3): TPosition3;
    function ManhatenDistance(Other: TPosition3): int64;
  end;

  TScanner = class
  private
    function CorrectPosition(Const aPosition: TPosition3; aOrientation: integer): TPosition3;
  public
    Points, CorretedPoints: TList<TPosition3>;
    Distances: TList<Int64>;
    FinalStartingPoint: TPosition3;
    Name: String;
    constructor Create(); overload;
    destructor Destroy; override;

    procedure AddPoint(Const aString: string);
    function MapBeacons(const aScanner: TScanner; var aBeacons: TList<TPosition3>): Boolean;
  end;

  TAdventOfCodeDay19 = class(TAdventOfCode)
  private
    BeaconCount, BeaconDistance: integer;
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
    procedure BeforeSolve; override;
  end;

  TAdventOfCodeDay20 = class(TAdventOfCode)
  private
    function EnhanceImage(Const aRounds: integer):integer;
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  TAdventOfCodeDay21 = class(TAdventOfCode)
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  TAdventOfCodeDay22 = class(TAdventOfCode)
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  TAmphipod = (A,B,C,D,None);
  TAmphipodState = record
    Places: array[0..26] of TAmphipod; // 0-10 hallway, 11/15/29/23 room A
    TotalEngergyUsed: Integer;
    GuesedTotalEnergy: Integer;
    Path: string; //For debuging
    function AsString: string;
    function RoomIndex(aAmphipod: TAmphipod; aDepth: integer): Integer;
    class function Create: TAmphipodState; static;
  end;

  TAdventOfCodeDay23 = class(TAdventOfCode)
  private
    function CalculateEngergyCost(InstructionFolded: Boolean): integer;
    procedure DumpPath(aPath: string);
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  TAdventOfCodeDay24 = class(TAdventOfCode)
  private
    function CalcMONAD(const aMaxResult: Boolean): Extended ;
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  TAdventOfCodeDay25 = class(TAdventOfCode)
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

implementation

{$REGION 'TAdventOfCodeDay1'}

function TAdventOfCodeDay1.SolveA: Variant;
begin
  Result := SonarSweep(1);
end;

function TAdventOfCodeDay1.SolveB: Variant;
begin
  Result := SonarSweep(3);
end;

function TAdventOfCodeDay1.SonarSweep(Const OffSet: integer): integer;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to FInput.Count - OffSet - 1 do
    if StrToInt(FInput[i]) < StrToInt(FInput[i + OffSet]) then
      Inc(Result);
end;

{$ENDREGION}
{$REGION 'TAdventOfCodeDay2'}

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
    case IndexText(Split[0], ['forward', 'down', 'up']) of
      0:
        begin
          Inc(hPosition, units);
          if not PartOne then
            Inc(depth, aim * units);
        end;
      1:
        if PartOne then
          Inc(depth, units)
        else
          Inc(aim, units);
      2:
        if PartOne then
          dec(depth, units)
        else
          dec(aim, units);
    else
      Assert(False, s);
    end;
  end;

  Result := depth * hPosition;
end;
{$ENDREGION}
{$REGION 'TAdventOfCodeDay3'}

function TAdventOfCodeDay3.SolveA: Variant;
var
  s: string;
  OneBits: array of integer;
  i, GammaRate, EpsilonRate, NoOneBits, NoZeroBits: integer;
begin
  SetLength(OneBits, Length(FInput[0]));
  for s in FInput do
    for i := 0 to Length(s) - 1 do
      if s[i + 1] = '1' then
        OneBits[i] := OneBits[i] + 1;

  GammaRate := 0;
  EpsilonRate := 0;
  for i := 0 to Length(OneBits) - 1 do
  begin
    NoOneBits := OneBits[i];
    NoZeroBits := FInput.Count - NoOneBits;

    GammaRate := GammaRate shl 1;
    EpsilonRate := EpsilonRate shl 1;

    if NoOneBits > NoZeroBits then
      Inc(GammaRate)
    else
      Inc(EpsilonRate);
  end;

  Result := EpsilonRate * GammaRate;
end;

function TAdventOfCodeDay3.SolveB: Variant;

  function _FindDiagnosticValue(Const KeepLeastCommenValue: Boolean): integer;
  var
    TempInput: TStringList;
    NoOneBits, NoZeroBits, BitIndex, i: integer;
    KeepZero: Boolean;
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
          Inc(NoOneBits)
        else
          Inc(NoZeroBits);

      KeepZero := NoZeroBits > NoOneBits;
      if KeepLeastCommenValue then
        KeepZero := not KeepZero;

      for i := TempInput.Count - 1 downTo 0 do
        if (TempInput[i][BitIndex] = '1') and KeepZero then
          TempInput.Delete(i)
        else if (TempInput[i][BitIndex] = '0') and (not KeepZero) then
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
{$REGION 'TAdventOfCodeDay4'}

procedure TAdventOfCodeDay4.BeforeSolve;
var
  s: string;
  BingoNumbers, card: TStringDynArray;
  BingcoCard: Array [0 .. 24] of integer;
  Round, GameResult: Int64;
  Base, x, y, CurrentNumber: integer;
  HasWon: Boolean;
begin
  WorstGame := 0;
  BestGame := MaxInt64;
  Base := 2;

  BingoNumbers := SplitString(FInput[0], ',');

  while Base < FInput.Count - 1 do
  begin
    for y := 0 to 4 do
    begin
      s := Trim(FInput[Base].Replace('  ', ' ', [rfReplaceAll]));
      card := SplitString(s, ' ');
      for x := 0 to 4 do
        BingcoCard[x + 5 * y] := StrToInt(card[x]);

      Inc(Base);
    end;
    Inc(Base);

    Round := 0;
    for s in BingoNumbers do
    begin
      CurrentNumber := StrToInt(s);
      Inc(Round);

      for x := 0 to 24 do
        if BingcoCard[x] = CurrentNumber then
          BingcoCard[x] := -1;

      HasWon := False;
      for x := 0 to 4 do
        HasWon := HasWon or ((BingcoCard[x] < 0) and (BingcoCard[x + 5] < 0) and
          (BingcoCard[x + 10] < 0) and (BingcoCard[x + 15] < 0) and
          (BingcoCard[x + 20] < 0)) or
          ((BingcoCard[5 * x] < 0) and (BingcoCard[5 * x + 1] < 0) and
          (BingcoCard[5 * x + 2] < 0) and (BingcoCard[5 * x + 3] < 0) and
          (BingcoCard[5 * x + 4] < 0));

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
{$REGION 'TAdventOfCodeDay5'}

function TAdventOfCodeDay5.SacnHydrothermalVents(const UseDiagonalLines
  : Boolean): integer;
const
  MapSize: integer = 1000;
var
  Map: Array of integer;
  IntersectionsCount: integer;

  function _GetCoordinates(Const s: string): TPoint;
  var
    Split: TStringDynArray;
  begin
    Split := SplitString(s, ',');
    Result := TPoint.Create(Split[0].ToInteger, Split[1].ToInteger);
  end;

  procedure _AddPointToMap(Const aX, aY: integer);
  begin
    if Map[aX * MapSize + aY] = 1 then
      Inc(IntersectionsCount);

    Map[aX * MapSize + aY] := Map[aX * MapSize + aY] + 1;
  end;

  function _GetNewValue(Const aStart, aStop, aCurrent: integer): integer;
  begin
    Result := aCurrent + Sign(aStop - aStart);
  end;

var
  Split: TStringDynArray;
  StartCoordinates, StopCoordinates: TPoint;
  s: string;
  x, y: integer;
begin
  SetLength(Map, MapSize * MapSize);
  IntersectionsCount := 0;
  for s in FInput do
  begin
    Split := SplitString(s, ' ');
    StartCoordinates := _GetCoordinates(Split[0]);
    StopCoordinates := _GetCoordinates(Split[2]);

    if (not UseDiagonalLines) and (StartCoordinates.x <> StopCoordinates.x) and
      (StartCoordinates.y <> StopCoordinates.y) then
      Continue;

    x := StartCoordinates.x;
    y := StartCoordinates.y;
    Repeat
      _AddPointToMap(x, y);
      x := _GetNewValue(StartCoordinates.x, StopCoordinates.x, x);
      y := _GetNewValue(StartCoordinates.y, StopCoordinates.y, y);
    until (x = StopCoordinates.x) and (y = StopCoordinates.y);
    _AddPointToMap(x, y);
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
{$REGION 'TAdventOfCodeDay6'}

function TAdventOfCodeDay6.SolveA: Variant;
begin
  Result := SimulateLanternfish(80);
end;

function TAdventOfCodeDay6.SolveB: Variant;
begin
  Result := SimulateLanternfish(256);
end;

function TAdventOfCodeDay6.SimulateLanternfish(Const Days: integer): Int64;
var
  Fish: array of Int64;
  i: integer;
begin
  SetLength(Fish, Days);
  Fish[0] := OccurrencesOfChar(FInput[0], ',') + 1;
  for i := 1 to 5 do
    Fish[i] := Fish[i - 1] + OccurrencesOfChar(FInput[0], i.ToString);

  Fish[6] := 2 * Fish[0];
  Fish[7] := 2 * Fish[0];
  Fish[8] := Fish[7] + Fish[1] - Fish[0];
  Fish[9] := Fish[8] + Fish[2] - Fish[1];

  for i := 10 to Days do
    Fish[i] := Fish[i - 1] - Fish[i - 10] - Fish[i - 8] + Fish[i - 9] +
      Fish[i - 7];

  Result := Fish[Days - 1];
end;
{$ENDREGION}
{$REGION 'TAdventOfCodeDay7'}

function TAdventOfCodeDay7.SolveA: Variant;
begin
  Result := CalculateFuelCost(False);
end;

function TAdventOfCodeDay7.SolveB: Variant;
begin
  Result := CalculateFuelCost(True);
end;

function TAdventOfCodeDay7.CalculateFuelCost(ExpensiveBurning: Boolean)
  : integer;
var
  s: string;
  Split: TStringDynArray;
  BestFuelCost, Count, position, currentFuelCost, StartPosition,
    Distance: integer;
  Positions: TDictionary<integer, integer>;
  PositionPair: TPair<integer, integer>;
begin
  Positions := TDictionary<integer, integer>.Create();
  StartPosition := MaxInt;
  Split := SplitString(FInput[0], ',');
  for s in Split do
  begin
    position := s.ToInteger;
    StartPosition := Min(StartPosition, position);
    Positions.TryGetValue(position, Count);
    Inc(Count);
    Positions.AddOrSetValue(position, Count)
  end;

  BestFuelCost := MaxInt;
  repeat
    currentFuelCost := 0;
    for PositionPair in Positions do
    begin
      Distance := abs(PositionPair.key - StartPosition);
      if ExpensiveBurning then
        Inc(currentFuelCost, PositionPair.Value *
          Round(Distance * (Distance + 1) / 2))
      else
        Inc(currentFuelCost, PositionPair.Value * Distance);
    end;

    BestFuelCost := Min(BestFuelCost, currentFuelCost);
    Inc(StartPosition);
  until (currentFuelCost > BestFuelCost);

  Result := BestFuelCost;
  Positions.Free;
end;
{$ENDREGION}
{$REGION 'TAdventOfCodeDay8'}

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
        2, 3, 4, 7:
          Inc(Result);
      end;
  end;
end;

function TAdventOfCodeDay8.SolveB: Variant;

  function CalculateDisplay(Const aCheck, aTotal: string): integer;
  var
    KnownNumbers: Array [0 .. 9] of integer;

    function DisplayNumberToInt(aNumber: string): integer;
    var
      i: integer;
    begin
      Result := 0;
      for i := ord('a') to ord('g') do
      begin
        Result := Result shl 1;
        Result := Result + Sign(pos(chr(i), aNumber));
      end;
    end;

    function FindNumber(Const aNumber: string): integer;
    var
      int, i: integer;
    begin
      Result := -1;
      int := DisplayNumberToInt(aNumber);
      for i := 0 to 9 do
        if KnownNumbers[i] = int then
          Exit(i);
    end;

  var
    Split: TStringDynArray;
    Number, i: integer;
  begin
    Split := SplitString(aCheck, ' ');
    TArray.Sort<string>(Split, TDelegatedComparer<string>.Construct(
      function(const Left, Right: string): integer
      begin
        Result := Sign(Length(Left) - Length(Right));
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
          KnownNumbers[3] := Number
        else if CountTrueBits(KnownNumbers[4] and Number) = 3 then
          KnownNumbers[5] := Number
        else
          KnownNumbers[2] := Number;
      end
      else // Could still be 0,6,9
      begin
        if CountTrueBits(Number and KnownNumbers[4]) = 4 then
          KnownNumbers[9] := Number
        else if CountTrueBits(KnownNumbers[1] and Number) = 2 then
          KnownNumbers[0] := Number
        else
          KnownNumbers[6] := Number
      end;
    end;

    Split := SplitString(aTotal, ' ');
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
{$REGION 'TAdventOfCodeDay9'}

function TAdventOfCodeDay9.SolveA: Variant;
begin
  Result := ExploreBasins(True);
end;

function TAdventOfCodeDay9.SolveB: Variant;
begin
  Result := ExploreBasins(False);
end;

function TAdventOfCodeDay9.ExploreBasins(const OnlyLowestPoints
  : Boolean): integer;
Const
  DeltaX: Array [0 .. 3] of integer = (1, -1, 0, 0);
  DeltaY: Array [0 .. 3] of integer = (0, 0, 1, -1);
var
  MapWidth, MapHeight: integer;

  function _Valid(Const aX, aY: integer): Boolean;
  begin
    Result := (aX >= 0) and (aX <= MapWidth) and (aY > 0) and (aY <= MapHeight);
  end;

  procedure CalcBasinSize(Const aStartX, aStartY, CurrHeight: integer;
  Const BasinPoints: TList<integer>);
  var
    i, xCheck, yCheck, Height: integer;
  begin
    for i := 0 to 3 do
    begin
      xCheck := aStartX + DeltaX[i];
      yCheck := aStartY + DeltaY[i];
      if not _Valid(xCheck, yCheck) then
        Continue;

      if BasinPoints.Contains(xCheck * 1000 + yCheck) then
        Continue;

      Height := StrToInt(FInput[xCheck][yCheck]);
      if (Height <= CurrHeight) or (Height >= 9) then
        Continue;

      BasinPoints.Add(xCheck * 1000 + yCheck);
      CalcBasinSize(xCheck, yCheck, Height, BasinPoints);
    end;
  end;

var
  x, y, xCheck, yCheck, Height, i: integer;
  BasinPoints, Basins: TList<integer>;
  IsLowestPoint: Boolean;
begin
  Result := 0;

  BasinPoints := TList<integer>.Create;
  Basins := TList<integer>.Create;

  MapWidth := FInput.Count - 1;
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
        if not _Valid(xCheck, yCheck) then
          Continue;

        IsLowestPoint := IsLowestPoint and
          (Height < StrToInt(FInput[xCheck][yCheck]))
      end;

      if Not IsLowestPoint then
        Continue;

      if OnlyLowestPoints then
        Inc(Result, Height + 1)
      else
      begin
        BasinPoints.Clear;
        BasinPoints.Add(x * 1000 + y);
        CalcBasinSize(x, y, Height, BasinPoints);
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
{$REGION 'TAdventOfCodeDay10'}

procedure TAdventOfCodeDay10.BeforeSolve;
Const
  open: Array [0 .. 3] of string = ('(', '[', '{', '<');
  close: Array [0 .. 3] of string = (')', ']', '}', '>');
  syntaxCost: Array [0 .. 3] of integer = (3, 57, 1197, 25137);
var
  s, Current: string;
  i, index: integer;
  Stack: TStack<string>;
  AutoCompleteScores: TList<Int64>;
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
      else if Stack.Pop <> Current then
      begin
        Inc(SyntaxScore, syntaxCost[IndexStr(Current, close)]);
        Stack.Clear;
        break;
      end;
    end;

    if Stack.Count > 0 then
    begin
      AutoCompleteScore := 0;
      while Stack.Count > 0 do
        AutoCompleteScore := AutoCompleteScore * 5 + 1 +
          IndexStr(Stack.Pop, close);

      AutoCompleteScores.Add(AutoCompleteScore);
    end;
  end;

  AutoCompleteScores.Sort;
  AutoCompleteScore := AutoCompleteScores[Trunc(AutoCompleteScores.Count / 2)];

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
  DeltaX: array [0 .. 7] of integer = (1, 1, 1, 0, 0, -1, -1, -1);
  DeltaY: array [0 .. 7] of integer = (1, 0, -1, -1, 1, 1, 0, -1);
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
      for Point in Octopuses.Keys do
        Octopuses[Point] := Octopuses[Point] + 1;

      Flashed.Clear;
      FlashFound := True;
      while FlashFound do
      begin
        FlashFound := False;
        for Point in Octopuses.Keys do
          if (Octopuses[Point] > 9) and not Flashed.Contains(Point) then
          begin
            FlashFound := True;
            Inc(Result);
            Flashed.Add(Point);

            for j := 0 to 7 do
            begin
              NewPoint := TPoint.Create(Point);
              NewPoint.OffSet(DeltaX[j], DeltaY[j]);
              if Octopuses.TryGetValue(NewPoint, Count) then
                Octopuses[NewPoint] := Count + 1;
            end;
          end;
      end;

      if Flashed.Count = 100 then
        Exit(i);

      for Point in Flashed do
        Octopuses[Point] := 0;

      Inc(i);
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
  Split: TStringDynArray;
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
var
  Cave: TCave;
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

type
  TByteSet = set of byte;

function TAdventOfCodeDay12.ExploreCaves(const CanUseLowercaseCaveTwice
  : Boolean): integer;

  procedure _ExploreCaves(Const aCurrentCave: TCave;
  aVisitedLowerCaseCaves: TByteSet; aLowercaseCaveUsedTwice: Boolean);
  var
    OtherCave: TCave;
    LowercaseCaveUsedTwice: Boolean;
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

          LowercaseCaveUsedTwice := True;
        end;
        Include(VisitedLowerCaseCaves, OtherCave.CaveId);
      end;

      _ExploreCaves(OtherCave, VisitedLowerCaseCaves, LowercaseCaveUsedTwice);
    end;
  end;

begin
  Result := 0;
  _ExploreCaves(Caves['start'], [], not CanUseLowercaseCaveTwice);
end;
{$ENDREGION}
{$REGION 'TAdventOfCodeDay13'}

function TAdventOfCodeDay13.SolveA: Variant;
begin
  Result := FoldPaper(True);
end;

function TAdventOfCodeDay13.SolveB: Variant;
begin
  Result := FoldPaper(False)
end;

function TAdventOfCodeDay13.FoldPaper(const OnlyOnce: Boolean): Variant;
var
  Points: TList<TPoint>;
  MaxX, MaxY: integer;

  function SaveToFile: string;
  var
    s: string;
    x, y: integer;
    lst: TStringList;
  begin
    Result := SaveFilePath;
    lst := TStringList.Create;
    for y := 0 to MaxY do
    begin
      s := '';
      for x := 0 to MaxX do
        if Points.Contains(TPoint.Create(x, y)) then
          s := s + '#'
        else
          s := s + '.';
      lst.Add(s);
    end;
    lst.SaveToFile(Result);
    lst.Free;
  end;

  function Vold(Const CurrentValue, FoldLine: integer): integer;
  begin
    Result := CurrentValue;
    if CurrentValue > FoldLine then
      Result := 2 * FoldLine - CurrentValue;;
  end;

var
  s: string;
  Split: TStringDynArray;
  i, j: integer;
  FoldLine, x, y: integer;
  FoldOnX: Boolean;
  Point, NewPoint: TPoint;
begin
  Points := TList<TPoint>.Create;
  try
    MaxX := 0;
    MaxY := 0;
    for i := 0 to FInput.Count - 1 do
    begin
      s := Trim(FInput[i]);
      if s = '' then
        break;

      Split := SplitString(s, ',');
      x := Split[0].ToInteger;
      y := Split[1].ToInteger;

      Points.Add(TPoint.Create(x, y));
      MaxX := Max(MaxX, x);
      MaxY := Max(MaxY, y);
    end;

    while i < FInput.Count - 1 do
    begin
      Inc(i);
      s := FInput[i];
      Split := SplitString(s, '=');
      FoldOnX := Split[0].EndsWith('x');
      FoldLine := Split[1].ToInteger;

      for j := Points.Count - 1 downto 0 do
      begin
        Point := Points.ExtractAt(j);
        if FoldOnX then
          Point.x := Vold(Point.x, FoldLine)
        else
          Point.y := Vold(Point.y, FoldLine);

        if not Points.Contains(Point) then
          Points.Add(Point);
      end;

      if FoldOnX then
        MaxX := MaxX Shr 1
      else
        MaxY := MaxY Shr 1;

      if OnlyOnce then
        Exit(Points.Count)
    end;

    Result := Format('Solution saved in: %s', [SaveToFile]);
  finally
    Points.Free;
  end;
end;

{$ENDREGION}
{$REGION 'TAdventOfCodeDay14'}

procedure TAdventOfCodeDay14.BeforeSolve;
var
  Split: TStringDynArray;
  i: integer;
begin
  Rules := TDictionary<String, string>.Create;
  for i := 2 to FInput.Count - 1 do
  begin
    Split := SplitString(FInput[i], ' ');
    Rules.Add(Split[0], Split[2]);
  end;
end;

procedure TAdventOfCodeDay14.AfterSolve;
begin
  Rules.Free;
end;

function TAdventOfCodeDay14.SolveA: Variant;
begin
  Result := BuildPolymer(10);
end;

function TAdventOfCodeDay14.SolveB: Variant;
begin
  Result := BuildPolymer(40);
end;

function TAdventOfCodeDay14.BuildPolymer(const aRounds: integer): Int64;

  procedure AddToDict(aDict: TDictionary<string, Int64>; Const aKey: string;
  aValue: Int64);
  var
    Val: Int64;
  begin
    aDict.TryGetValue(aKey, Val);
    aDict.AddOrSetValue(aKey, Val + aValue);
  end;

var
  Middle: string;
  i: Int64;
  Values: TArray<Int64>;
  Counts, PolymerParts, NewPolymerParts: TDictionary<string, Int64>;
  PolymerPair: TPair<string, Int64>;
begin
  PolymerParts := TDictionary<string, Int64>.Create;
  for i := 1 to Length(FInput[0]) - 1 do
    AddToDict(PolymerParts, FInput[0][i] + FInput[0][i + 1], 1);

  for i := 1 to aRounds do
  begin
    NewPolymerParts := TDictionary<string, Int64>.Create;
    for PolymerPair in PolymerParts do
    begin
      Middle := Rules[PolymerPair.key];

      AddToDict(NewPolymerParts, PolymerPair.key[1] + Middle,
        PolymerPair.Value);
      AddToDict(NewPolymerParts, Middle + PolymerPair.key[2],
        PolymerPair.Value);
    end;

    PolymerParts.Free;
    PolymerParts := NewPolymerParts;
  end;

  Counts := TDictionary<string, Int64>.Create;
  Counts.Add(FInput[0][1], 1);
  for PolymerPair in PolymerParts do
    AddToDict(Counts, PolymerPair.key[2], PolymerPair.Value);

  Values := Counts.Values.ToArray;
  TArray.Sort<Int64>(Values);
  Result := Values[Counts.Count - 1] - Values[0];

  PolymerParts.Free;
  Counts.Free;
end;

{$ENDREGION}
{$REGION 'TAdventOfCodeDay15'}

function TAdventOfCodeDay15.SolveA: Variant;
begin
  Result := CheckCavern(1);
end;

function TAdventOfCodeDay15.SolveB: Variant;
begin
  Result := CheckCavern(5);
end;

type
  RCavernWork = record
    x, y, Risk: integer;
  end;

function TAdventOfCodeDay15.CheckCavern(const MapSizeMultiplier
  : integer): integer;
Const
  DeltaX: Array [0 .. 3] of integer = (1, 0, -1, 0);
  DeltaY: Array [0 .. 3] of integer = (0, 1, 0, -1);
var
  Risk, xExtra, yExtra, BaseRisk, i, x, y, MaxX, MaxY: integer;
  Risks, Checked: Array of Array of integer;
  Work, NewWork: RCavernWork;
  Comparer: IComparer<RCavernWork>;
  Queue: PriorityQueue<RCavernWork>;
begin
  Result := -1;
  Comparer := TComparer<RCavernWork>.Construct(
    function(const Left, Right: RCavernWork): integer
    begin
      Result := Sign(Left.Risk - Right.Risk);
    end);

  Queue := PriorityQueue<RCavernWork>.Create(Comparer, Comparer);

  MaxX := Length(FInput[0]) - 1;
  MaxY := FInput.Count - 1;

  SetLength(Risks, MapSizeMultiplier * MaxX + MapSizeMultiplier);
  SetLength(Checked, MapSizeMultiplier * MaxX + MapSizeMultiplier);
  for x := 0 to MapSizeMultiplier * MaxX + MapSizeMultiplier - 1 do
  begin
    SetLength(Risks[x], MapSizeMultiplier * MaxY + MapSizeMultiplier);
    SetLength(Checked[x], MapSizeMultiplier * MaxY + MapSizeMultiplier);
    for y := 0 to MapSizeMultiplier * MaxY + MapSizeMultiplier - 1 do
      Checked[x][y] := MaxInt;
  end;

  for x := 0 to MaxX do
    for y := 0 to MaxY do
    begin
      BaseRisk := StrToInt(FInput[y][x + 1]);
      for xExtra := 0 to MapSizeMultiplier - 1 do
        for yExtra := 0 to MapSizeMultiplier - 1 do
        begin
          Risk := BaseRisk + xExtra + yExtra;
          if Risk > 9 then
            Risk := Risk - 9;

          Risks[x + xExtra * (MaxX + 1)][y + yExtra * (MaxY + 1)] := Risk
        end;
    end;

  MaxX := (MaxX + 1) * MapSizeMultiplier - 1;
  MaxY := (MaxY + 1) * MapSizeMultiplier - 1;

  Work.x := 0;
  Work.y := 0;
  Work.Risk := 0;
  Queue.Enqueue(Work);

  while Queue.Count > 0 do
  begin
    Work := Queue.Dequeue;

    if (Work.x = MaxX) and (Work.y = MaxY) then
      Exit(Work.Risk);

    if Checked[Work.x][Work.y] <= Work.Risk then
      Continue;

    Checked[Work.x][Work.y] := Work.Risk;

    for i := 0 to 3 do
    begin
      NewWork.x := Work.x + DeltaX[i];
      NewWork.y := Work.y + DeltaY[i];
      if (NewWork.x >= 0) and (NewWork.x <= MaxX) and (NewWork.y >= 0) and
        (NewWork.y <= MaxY) then
      begin
        NewWork.Risk := Risks[NewWork.x][NewWork.y] + Work.Risk;
        Queue.Enqueue(NewWork);
      end;
    end;
  end;
end;
{$ENDREGION}
{$REGION 'TAdventOfCodeDay16'}

procedure TAdventOfCodeDay16.BeforeSolve;

  function _GetBits(Var aBits: string; Const aLength: integer): string;
  begin
    Result := Copy(aBits, 1, aLength);
    Delete(aBits, 1, aLength)
  end;

  procedure _Calc(Var aResult: Int64; const aValue: Int64;
  Const aTypeId: integer);
  begin
    case aTypeId of
      0:
        aResult := aResult + aValue;
      1:
        aResult := aResult * aValue;
      2:
        aResult := Min(aResult, aValue);
      3:
        aResult := Max(aResult, aValue);
    end;
  end;

  function _Compare(Const Val1, Val2: Int64; Const aTypeId: integer): Int64;
  begin
    Result := 0;
    case aTypeId of
      5:
        if Val1 > Val2 then
          Result := 1;
      6:
        if Val1 < Val2 then
          Result := 1;
      7:
        if Val1 = Val2 then
          Result := 1;
    end;
  end;

  function ReadBits(var aBits: string; Const onlyOne: Boolean): Int64;
  var
    NewBinaryString: string;
    PacketLength, TypeId: integer;
  begin
    Result := -1;

    VersionNumbers := VersionNumbers + BitStringToInt(_GetBits(aBits, 3));
    TypeId := BitStringToInt(_GetBits(aBits, 3));

    if TypeId = 4 then // Literal value
    begin
      NewBinaryString := '';
      while _GetBits(aBits, 1) = '1' do
        NewBinaryString := NewBinaryString + _GetBits(aBits, 4);
      NewBinaryString := NewBinaryString + _GetBits(aBits, 4);
      Result := BitStringToInt(NewBinaryString);
    end
    else // Operator;
    begin
      case TypeId of // Defaults
        0, 3, 5, 6, 7:
          Result := 0;
        1:
          Result := 1;
        2:
          Result := MaxInt;
      end;

      if _GetBits(aBits, 1) = '0' then
      begin
        PacketLength := BitStringToInt(_GetBits(aBits, 15));
        NewBinaryString := _GetBits(aBits, PacketLength);

        if TypeId < 4 then
        begin
          while Length(NewBinaryString) > 0 do
            _Calc(Result, ReadBits(NewBinaryString, True), TypeId);
        end
        else
          Result := _Compare(ReadBits(NewBinaryString, True),
            ReadBits(NewBinaryString, True), TypeId);
      end
      else
      begin
        PacketLength := BitStringToInt(_GetBits(aBits, 11));

        while PacketLength > 0 do
        begin
          dec(PacketLength);

          if TypeId < 4 then
            _Calc(Result, ReadBits(aBits, True), TypeId)
          else
          begin
            Result := _Compare(ReadBits(aBits, True),
              ReadBits(aBits, True), TypeId);
            dec(PacketLength);
            Assert(PacketLength = 0)
          end
        end;
      end;
    end;
  end;

var
  BinaryString, s: string;
  i: integer;
begin
  s := FInput[0];
  BinaryString := '';
  for i := 1 to Length(s) do
    case pos(s[i], '0123456789ABCDEF') of
      1:
        BinaryString := BinaryString + '0000';
      2:
        BinaryString := BinaryString + '0001';
      3:
        BinaryString := BinaryString + '0010';
      4:
        BinaryString := BinaryString + '0011';
      5:
        BinaryString := BinaryString + '0100';
      6:
        BinaryString := BinaryString + '0101';
      7:
        BinaryString := BinaryString + '0110';
      8:
        BinaryString := BinaryString + '0111';
      9:
        BinaryString := BinaryString + '1000';
      10:
        BinaryString := BinaryString + '1001';
      11:
        BinaryString := BinaryString + '1010';
      12:
        BinaryString := BinaryString + '1011';
      13:
        BinaryString := BinaryString + '1100';
      14:
        BinaryString := BinaryString + '1101';
      15:
        BinaryString := BinaryString + '1110';
      16:
        BinaryString := BinaryString + '1111';
    else
      Assert(False, s[i]);
    end;

  VersionNumbers := 0;
  BITSmessage := ReadBits(BinaryString, True);
end;

function TAdventOfCodeDay16.SolveA: Variant;
begin
  Result := VersionNumbers;
end;

function TAdventOfCodeDay16.SolveB: Variant;
begin
  Result := BITSmessage;
end;

{$ENDREGION}
{$REGION 'TAdventOfCodeDay17'}

procedure TAdventOfCodeDay17.BeforeSolve;
var
  s: string;
  Split: TStringDynArray;
  Height, TargetMinX, TargetMaxX, TargetMinY, TargetMaxY, x, y: integer;
  Probe, Velocity: TPoint;
begin
  MaxHeight := 0;
  VelocityValues := 0;

  s := FInput[0];
  s := s.Replace('..', ' ', [rfReplaceAll]).Replace('=', ' ', [rfReplaceAll])
    .Replace(',', '', [rfReplaceAll]);
  Split := SplitString(s, ' ');

  TargetMinX := Split[3].ToInteger;
  TargetMaxX := Split[4].ToInteger;
  TargetMinY := Split[6].ToInteger;
  TargetMaxY := Split[7].ToInteger;

  for x := 0 to TargetMaxX do
    for y := TargetMinY to abs(TargetMinY) do
    begin
      Probe := TPoint.Zero;
      Velocity := TPoint.Create(x, y);
      Height := 0;
      while (Probe.y >= TargetMinY) and (Probe.x <= TargetMaxX) do
      begin
        if (Velocity.x = 0) and (Probe.x < TargetMinX) then
          break;

        Probe := Probe.Add(Velocity);
        Velocity.x := Max(Velocity.x - 1, 0);
        Velocity.y := Velocity.y - 1;
        Height := Max(Height, Probe.y);
        if (Probe.x >= TargetMinX) and (Probe.x <= TargetMaxX) and
          (Probe.y >= TargetMinY) and (Probe.y <= TargetMaxY) then
        begin
          MaxHeight := Max(MaxHeight, Height);
          Inc(VelocityValues);
          break;
        end;
      end;
    end;
end;

function TAdventOfCodeDay17.SolveA: Variant;
begin
  Result := MaxHeight;
end;

function TAdventOfCodeDay17.SolveB: Variant;
begin
  Result := VelocityValues;
end;
{$ENDREGION}
{$REGION 'TSnailFishNode'}
constructor TSnailFishNode.Create(aString: string);
var
  Json: TJsonValue;
begin
  Json := TJSONObject.ParseJSONValue(aString);
  Create(nil, Json);
  Json.Free;
end;

constructor TSnailFishNode.Create(aParent: TSnailFishNode; aJson: TJsonValue);
begin
  Parent := aParent;

  if aJson is TJSONArray then
  begin
    Left := TSnailFishNode.Create(self, TJSONArray(aJson).Items[0]);
    Right := TSnailFishNode.Create(self, TJSONArray(aJson).Items[1]);
    IntValue := -1;
  end
  else
    IntValue := TJSONNumber(aJson).AsInt;
end;

constructor TSnailFishNode.Create(aParent: TSnailFishNode; aIntValue: integer);
begin
  Parent := aParent;
  IntValue := aIntValue;
end;

constructor TSnailFishNode.Create(aLeft, aRight: TSnailFishNode);
begin
  Left := aLeft;
  Right := aRight;

  Left.Parent := self;
  Right.Parent := self;
end;

destructor TSnailFishNode.Destroy;
begin
  if Assigned(Left) then
    Left.Free;
  if Assigned(Right) then
    Right.Free;
end;

function TSnailFishNode.Explode(aDepth: integer): Boolean;
begin
  Result := False;

  if Left.IsSimpleNode and Right.IsSimpleNode then
  begin
    if aDepth >= 4 then
    begin
      Parent.UpdateAfterExplosion(Left.IntValue, Self, True);
      Parent.UpdateAfterExplosion(Right.IntValue, Self, False);
      FreeAndNil(Left);
      FreeAndNil(Right);
      IntValue := 0;
      Result := True;
    end;

    Exit;
  end;

  if not Left.IsSimpleNode then
    Result := Left.Explode(aDepth + 1);

  if Result then
    Exit;

  if not Right.IsSimpleNode then
    Result := Right.Explode(aDepth + 1);
end;

function TSnailFishNode.GetNode(aLeft: Boolean): TSnailFishNode;
begin
  if aLeft then
    result := Self.Left
  else
    Result := Self.Right;
end;

function TSnailFishNode.IsSimpleNode: Boolean;
begin
  Result := not(Assigned(Left) or Assigned(Right))
end;

function TSnailFishNode.Split: Boolean;
begin
  Result := False;
  if IsSimpleNode then
  begin
    if IntValue > 9 then
    begin
      Result := True;
      Left := TSnailFishNode.Create(self, Trunc(IntValue / 2));
      Right := TSnailFishNode.Create(self, Ceil(IntValue / 2));
      Self.IntValue := -1;
    end;

    Exit;
  end;

  Result := Left.Split;
  if Result then
    Exit;

  Result := Right.Split;
end;

procedure TSnailFishNode.Stabalize;
begin
  while True do
  begin
    if Explode(0) then
      Continue;

    if Split then
      Continue;

    Exit;
  end;
end;

procedure TSnailFishNode.UpdateAfterExplosion(aValue: Integer; aExplodingNode: TSnailFishNode; aLeft: Boolean);
var
  tmpNode: TSnailFishNode;
begin
  // Select the exploding child
  tmpNode := GetNode(aLeft);

  // Walk up the tree until we find the non exploding root of this number
  if tmpNode = aExplodingNode then
  begin
    if Assigned(Parent) then
      Parent.UpdateAfterExplosion(aValue, self, aLeft);
    Exit;
  end;

  // Walk down until we find a simple node to update
  while not tmpNode.IsSimpleNode do
    tmpNode := tmpNode.GetNode(not aLeft);
  Inc(tmpNode.IntValue, aValue);
end;

function TSnailFishNode.CalcMagnitude: integer;
begin
  if IsSimpleNode then
    Result := IntValue
  else
    Result := 3 * Left.CalcMagnitude + 2 * Right.CalcMagnitude
end;
{$ENDREGION}
{$REGION 'TAdventOfCodeDay18'}

function TAdventOfCodeDay18.SolveA: Variant;
var
  i: integer;
  SnailFishNode: TSnailFishNode;
begin
  SnailFishNode := TSnailFishNode.Create(TSnailFishNode.Create((FInput[0])), TSnailFishNode.Create(FInput[1]));
  SnailFishNode.Stabalize;

  for i := 2 to FInput.Count - 1 do
  begin
    SnailFishNode := TSnailFishNode.Create(SnailFishNode, TSnailFishNode.Create(FInput[i]));
    SnailFishNode.Stabalize;
  end;

  Result := SnailFishNode.CalcMagnitude;
  SnailFishNode.Free;
end;

function TAdventOfCodeDay18.SolveB: Variant;
var
  i, j: integer;
  SnailFishNode: TSnailFishNode;
begin
  Result := 0;

  for i := 0 to FInput.Count - 1 do
    for j := 0 to FInput.Count - 1 do
    begin
      if i = j then
        Continue;

      SnailFishNode := TSnailFishNode.Create(TSnailFishNode.Create(FInput[i]), TSnailFishNode.Create(FInput[j]));
      SnailFishNode.Stabalize;
      Result := Max(Result, SnailFishNode.CalcMagnitude);
      SnailFishNode.Free;
    end;
end;
{$ENDREGION}
{$REGION 'TPosition3'}
class function TPosition3.Create(Const aX, aY, aZ: int64): TPosition3;
begin
  Result.x := aX;
  Result.y := aY;
  Result.z := aZ;
end;

function TPosition3.ManhatenDistance(Other: TPosition3): int64;
begin
  Result :=
    Abs(Other.x - Self.x) +
    Abs(Other.y - Self.y) +
    Abs(Other.z - Self.z);
end;

class operator TPosition3.Add(a, b: TPosition3): TPosition3;
begin
  Result.x := a.x + b.x;
  Result.y := a.y + b.y;
  Result.z := a.z + b.z;
end;

class operator TPosition3.Subtract(a, b: TPosition3): TPosition3;
begin
  Result.x := a.x - b.x;
  Result.y := a.y - b.y;
  Result.z := a.z - b.z;
end;
{$ENDREGION}
{$REGION 'TScanner'}
procedure TScanner.AddPoint(const aString: string);
var
  Split: TStringDynArray;
  Position, Other: TPosition3;
begin
  Split := SplitString(aString, ',');
  Position := TPosition3.Create(StrToInt(Split[0]), StrToInt(Split[1]), StrToInt(Split[2]));

  for Other in Points do
      Distances.Add(Position.ManhatenDistance(Other));

  Points.Add(Position);
end;

function TScanner.CorrectPosition(const aPosition: TPosition3; aOrientation: integer): TPosition3;
begin //Todo refactor
  case aOrientation of
    0:  Result := TPosition3.Create(aPosition.x, aPosition.y, aPosition.z);
    1:  Result := TPosition3.Create(aPosition.x, aPosition.z, -aPosition.y);
    2:  Result := TPosition3.Create(aPosition.x, -aPosition.y, -aPosition.z);
    3:  Result := TPosition3.Create(aPosition.x, -aPosition.z, aPosition.y);
    4:  Result := TPosition3.Create(aPosition.y, aPosition.x, -aPosition.z);
    5:  Result := TPosition3.Create(aPosition.y, aPosition.z, aPosition.x);
    6:  Result := TPosition3.Create(aPosition.y, -aPosition.x, aPosition.z);
    7:  Result := TPosition3.Create(aPosition.y, -aPosition.z, -aPosition.x);
    8:  Result := TPosition3.Create(aPosition.z, aPosition.x, aPosition.y);
    9:  Result := TPosition3.Create(aPosition.z, aPosition.y, -aPosition.x);
    10: Result := TPosition3.Create(aPosition.z, -aPosition.x, -aPosition.y);
    11: Result := TPosition3.Create(aPosition.z, -aPosition.y, aPosition.x);
    12: Result := TPosition3.Create(-aPosition.x, aPosition.y, -aPosition.z);
    13: Result := TPosition3.Create(-aPosition.x, aPosition.z, aPosition.y);
    14: Result := TPosition3.Create(-aPosition.x, -aPosition.y, aPosition.z);
    15: Result := TPosition3.Create(-aPosition.x, -aPosition.z, -aPosition.y);
    16: Result := TPosition3.Create(-aPosition.y, aPosition.x, aPosition.z);
    17: Result := TPosition3.Create(-aPosition.y, aPosition.z, -aPosition.x);
    18: Result := TPosition3.Create(-aPosition.y, -aPosition.x, -aPosition.z);
    19: Result := TPosition3.Create(-aPosition.y, -aPosition.z, aPosition.x);
    20: Result := TPosition3.Create(-aPosition.z, aPosition.x, -aPosition.y);
    21: Result := TPosition3.Create(-aPosition.z, aPosition.y, aPosition.x);
    22: Result := TPosition3.Create(-aPosition.z, -aPosition.x, aPosition.y);
    23: Result := TPosition3.Create(-aPosition.z, -aPosition.y, -aPosition.x);
  end;
end;

constructor TScanner.Create;
begin
  Points := TList<TPosition3>.Create;
  CorretedPoints := TList<TPosition3>.Create;
  Distances := TList<Int64>.Create;
  FinalStartingPoint := TPosition3.Create(0,0,0);
end;

destructor TScanner.Destroy;
begin
  Points.Free;
  CorretedPoints.Free;
  Distances.Free;
end;

function TScanner.MapBeacons(const aScanner: TScanner; var aBeacons: TList<TPosition3>): Boolean;
Var
  PossibleScannerPosition, Beacon, Scan, Corrected, PossibleBeacon : TPosition3;
  FoundPoints, i, j, Orientation: integer;
  Distance: Int64;
begin
  Result := False;

  FoundPoints := 0;
  for Distance in aScanner.Distances do
    if Self.Distances.Contains(Distance) then
      Inc(FoundPoints);

  if FoundPoints < 66 then
    Exit;

  for Orientation := 0 to 23 do // Try all Orientations,
    for j := 0 to Points.Count -1 do // Try all points
      for Beacon in aScanner.CorretedPoints do // Map to all beacons
      begin
        Corrected := CorrectPosition(points[j], Orientation);
        PossibleScannerPosition := Beacon - Corrected;

        FoundPoints := 0;
        for i := 0 to Points.Count -1 do
        begin
          Scan := Points[i];
          Corrected := CorrectPosition(Scan, Orientation);
          PossibleBeacon := PossibleScannerPosition + Corrected;
          if aScanner.CorretedPoints.Contains(PossibleBeacon) then
            Inc(FoundPoints);
          if (FoundPoints + Points.count - i) < 12 then // Not possible to map; break
            Break;
        end;

        if FoundPoints >= 12 then // Found a match
        begin
          Writeln(Name,':',PossibleScannerPosition.x, ',', PossibleScannerPosition.y,',',PossibleScannerPosition.z, ' matching points ', FoundPoints);
          FinalStartingPoint := aScanner.FinalStartingPoint + PossibleScannerPosition;

          for Scan in Points do
          begin
            Corrected := CorrectPosition(Scan, Orientation);
            CorretedPoints.Add(Corrected);

            PossibleBeacon := FinalStartingPoint + Corrected;
            if not aBeacons.Contains(PossibleBeacon) then
              aBeacons.Add(PossibleBeacon);
          end;
          exit(True);
        end;
      end;
end;
{$ENDREGION}
{$REGION 'TAdventOfCodeDay19'}
procedure TAdventOfCodeDay19.BeforeSolve;
var
  i, j: integer;
  Scanner: TScanner;
  MappedScanners, UnMappedScanners: TList<TScanner>;
  FoundBeacons: TList<TPosition3>;
  JustMappedScanners: TQueue<TScanner>;
begin
  MappedScanners := TList<TScanner>.Create;
  UnMappedScanners := TList<TScanner>.Create;
  JustMappedScanners := TQueue<TScanner>.Create;
  Scanner := nil;
  for i := 0 to FInput.Count -1 do
  begin
    if FInput[i].StartsWith('---') then
    begin
      Scanner := TScanner.Create;
      Scanner.Name := FInput[i];
      UnMappedScanners.Add(Scanner);
    end
    else if FInput[i] <> '' then
      Scanner.AddPoint(FInput[i]);
  end;

  FoundBeacons := TList<TPosition3>.Create(UnMappedScanners[0].Points);
  MappedScanners.Add(UnMappedScanners[0]);
  UnMappedScanners.Delete(0);
  JustMappedScanners.Enqueue(MappedScanners[0]);
  MappedScanners[0].CorretedPoints.AddRange(MappedScanners[0].Points);

  while UnMappedScanners.Count > 0 do
  begin
    Scanner := JustMappedScanners.Dequeue;
    for i := UnMappedScanners.Count - 1 downto 0 do
    begin
      if UnMappedScanners[i].MapBeacons(Scanner, FoundBeacons) then
      begin
        MappedScanners.Add(UnMappedScanners[i]);
        JustMappedScanners.EnQueue(UnMappedScanners[i]);
        UnMappedScanners.Delete(i);
      end;
    end;
  end;

  BeaconCount := FoundBeacons.Count;
  BeaconDistance := 0;
    for i := 0 to MappedScanners.Count -1 do
      for j := 0 to MappedScanners.Count -1 do
        BeaconDistance := Max(BeaconDistance,
          Abs(MappedScanners[i].FinalStartingPoint.x - MappedScanners[j].FinalStartingPoint.x) +
          Abs(MappedScanners[i].FinalStartingPoint.y - MappedScanners[j].FinalStartingPoint.y) +
          Abs(MappedScanners[i].FinalStartingPoint.z - MappedScanners[j].FinalStartingPoint.z));

  for scanner in MappedScanners do
    Scanner.Free;
  MappedScanners.Free;
  UnMappedScanners.Free;
  FoundBeacons.Free;
  JustMappedScanners.Free;
end;

function TAdventOfCodeDay19.SolveA: Variant;
begin
  Result := BeaconCount
end;

function TAdventOfCodeDay19.SolveB: Variant;
begin
  Result := BeaconDistance;
end;
{$ENDREGION}
{$REGION 'TAdventOfCodeDay20'}
function TAdventOfCodeDay20.EnhanceImage(const aRounds: integer): integer;
const
  DeltaY: array [0 .. 8] of integer = (-1, -1, -1, 0, 0, 0, 1, 1, 1);
  DeltaX: array [0 .. 8] of integer = (-1, 0, 1, -1, 0, 1, -1, 0, 1);
var
  EnhancementAlgorithm: array[0..511] of boolean;
  Round, i, x, y, number, MaxX, MaxY: integer;
  NewPixels, Pixels, tmp: TDictionary<TPoint, Boolean>;
  point, CheckPoint: TPoint;
  IsOn: boolean;
begin
  Pixels := TDictionary<TPoint, Boolean>.Create;
  NewPixels := TDictionary<TPoint,Boolean>.Create;

  for x := 0 to 511 do
    EnhancementAlgorithm[x] := FInput[0][x+1] = '#';

  for x := 1 to Length(FInput[2]) do
    for y := 2 to FInput.Count -1 do
      Pixels.Add(TPoint.Create(x-1,y-2), FInput[y][x] = '#' );

  MaxX := Length(Finput[2])  + 1;
  MaxY := FInput.Count -2 + 1;
  for Round := 0 to aRounds -1 do
  begin
    for x := -1 - Round*2 to MaxX + Round*2 do
      for y := -1 - Round*2 to MaxY + Round*2 do
      begin
        Point := TPoint.Create(x,y);

        number := 0;
        for i := 0 to 8 do
        begin
          number := number shl 1;
          CheckPoint := TPoint.Create(Point);
          CheckPoint.Offset(DeltaX[i], DeltaY[i]);
          if not Pixels.TryGetValue(CheckPoint, isOn) then
            IsOn := Odd(Round);
          if IsOn then
            number := number + 1;
        end;

        NewPixels.AddOrSetValue(Point, EnhancementAlgorithm[Number]);
      end;

    tmp := Pixels;
    Pixels := NewPixels;
    NewPixels := tmp;
  end;

  Result := 0;
  for IsOn in Pixels.Values do
    if IsOn then
      Inc(Result);
  Pixels.Free;
  NewPixels.Free;
end;

function TAdventOfCodeDay20.SolveA: Variant;
begin
  Result := EnhanceImage(2);
end;

function TAdventOfCodeDay20.SolveB: Variant;
begin
  Result := EnhanceImage(50);
end;
{$ENDREGION}
{$REGION 'TAdventOfCodeDay21'}
type GameResult = record P1, P2: int64 end;
function TAdventOfCodeDay21.SolveA: Variant;

  function IncDIe(aDie: integer): integer;
  begin
    Result := aDie + 1;
    if Result > 100 then
      Result := Result -100;
  end;

  function _PlayGame(CurrentPlayerPostion, NextPlayerPosition, CurrentPlayerScore, NextPlayerScore, Die, DieRolls: integer): integer;
  var Position, DieScore: Integer;
  begin
    if NextPlayerScore >= 1000 then
      Exit(CurrentPlayerScore*DieRolls);

    DieScore := Die;
    Die := IncDIe(Die);
    DieScore := DieScore + Die;
    Die := IncDIe(Die);
    DieScore := DieScore + Die;
    Die := IncDIe(Die);

    Position := CurrentPlayerPostion + DieScore;
    while Position > 10 do
      dec(Position, 10);

    Result := _PlayGame(NextPlayerPosition, Position, NextPlayerScore, CurrentPlayerScore + Position, Die, DieRolls+3);
  end;

begin
  Result := _PlayGame(SplitString(FInput[0], ' ')[4].ToInteger,SplitString(FInput[1], ' ')[4].ToInteger,0,0,1,0);
end;

function TAdventOfCodeDay21.SolveB: Variant;
var Cache: TDictionary<String,GameResult>;

  function PlayGame(PosP1, PosP2, ScoreP1, ScoreP2: integer): GameResult;
  Const DiracDice: Array[3..9] of integer = (1,3,6,7,6,3,1);
  var rslt: GameResult;
      Position, i: Integer;
      CacheKey: String;
  begin
    CacheKey := Format('%d|%d|%d|%d', [PosP1, PosP2, ScoreP1, ScoreP2]);
    if Cache.TryGetValue(CacheKey, Result) then
      Exit;

    if ScoreP2 >= 21 then
    begin
      Result.P1 := 0;
      Result.P2 := 1;
      Exit;
    end;

    Result.P1 := 0;
    Result.P2 := 0;

    for i := 3 to 9 do
    begin
      Position := PosP1 + i;
      if Position > 10 then
        Position := Position - 10;
      rslt := PlayGame(PosP2, Position, ScoreP2, ScoreP1 + Position);
      Result.P1 := Result.P1 + DiracDice[i] * rslt.P2;
      Result.P2 := Result.P2 + DiracDice[i] * rslt.P1;
    end;

    Cache.Add(CacheKey, Result);
  end;

var Rslt: GameResult;
begin
  Cache := TDictionary<String,GameResult>.Create;
  Rslt := PlayGame(SplitString(FInput[0], ' ')[4].ToInteger,SplitString(FInput[1], ' ')[4].ToInteger,0,0);
  Result := Max(Rslt.P1,Rslt.P2);
  Cache.Free;
end;
{$ENDREGION}
{$REGION 'TAdventOfCodeDay22'}
function TAdventOfCodeDay22.SolveA: Variant;
var
  s: string;
  Split: TStringDynArray;
  Points: TDictionary<TPosition3, Integer>;
  X,Y,Z: integer;
begin
  Points := TDictionary<TPosition3, integer>.Create;
  for s in FInput do
  begin
    Split := SplitString(s.Replace('=', ' ', [rfReplaceAll]).Replace('..', ' ', [rfReplaceAll]).Replace(',', ' ', [rfReplaceAll]), ' ');
    for X := Max(StrToInt(Split[2]), -50) to Min(StrToInt(Split[3]), 50) do
      for Y := Max(StrToInt(Split[5]), -50) to Min(StrToInt(Split[6]), 50) do
        for Z := Max(StrToInt(Split[8]), -50) to Min(StrToInt(Split[9]), 50) do
          if SameText(split[0], 'on') then
            Points.AddOrSetValue(TPosition3.Create(X,Y,Z), 0)
          else
            Points.Remove(TPosition3.Create(X,Y,Z));
  end;

  Result := Points.Count;
  Points.Free;
end;

type TCoordinaat = (cX, cY, cZ);
Type TCuboid = record
  Top, Bottom: Array[TCoordinaat] of int64;
  IsOn: boolean;

  Class function Create(aMinX, aMaxX, aMinY, aMaxY, aMinZ, aMaxZ: integer; aIsOn: Boolean): TCuboid; static;
end;

class function TCuboid.Create(aMinX, aMaxX, aMinY, aMaxY, aMinZ, aMaxZ: integer; aIsOn: Boolean): TCuboid;
begin
  Result.Top[cX] := aMinX;
  Result.Top[cY] := aMinY;
  Result.Top[cZ] := aMinZ;

  Result.Bottom[cX] := aMaxX;
  Result.Bottom[cY] := aMaxY;
  Result.Bottom[cZ] := aMaxZ;

  Result.IsOn := aIsOn;
end;

function TAdventOfCodeDay22.SolveB: Variant;

  function _NoOverlap(a,b: TCuboid): Boolean;
  var Coord: TCoordinaat;
  begin
    Result := False;
    for Coord := Low(TCoordinaat) to High(TCoordinaat) do
      if (a.Top[Coord] > b.Bottom[Coord]) or (a.Bottom [Coord]< b.Top[Coord]) then
        Exit(True);
  end;

var
  Cubes: TList<TCuboid>;
  s: string;
  Split: TStringDynArray;
  i: integer;
  Current: Int64;
  CreatedCube, OverlappingCube, NewCube: TCuboid;
  Coord: TCoordinaat;
begin
  Cubes := TList<TCuboid>.Create;

  for s in FInput do
  begin
    Split := SplitString(s.Replace('=', ' ', [rfReplaceAll]).Replace('..', ' ', [rfReplaceAll]).Replace(',', ' ', [rfReplaceAll]), ' ');

    CreatedCube := TCuboid.Create(Split[2].ToInteger, Split[3].ToInteger, Split[5].ToInteger,
      Split[6].ToInteger, Split[8].ToInteger, Split[9].ToInteger, SameText(split[0], 'on'));

    for i := Cubes.Count -1 downto 0 do
    begin
      if _NoOverlap(Cubes[i], CreatedCube) then
        Continue;

      OverlappingCube := Cubes.ExtractAt(i);
      for Coord := Low(TCoordinaat) to High(TCoordinaat) do
      begin
        if CreatedCube.Bottom[Coord] < OverlappingCube.bottom[Coord] then
        begin
          NewCube := OverlappingCube;
          NewCube.top[Coord] := CreatedCube.Bottom[Coord]+1;
          OverlappingCube.Bottom[Coord] := CreatedCube.Bottom[Coord];
          Cubes.Add(NewCube);
        end;

        if CreatedCube.Top[Coord] > OverlappingCube.Top[Coord] then
        begin
          NewCube := OverlappingCube;
          NewCube.bottom[Coord] := CreatedCube.top[Coord]-1;
          OverlappingCube.Top[Coord] := CreatedCube.Top[Coord];
          Cubes.Add(NewCube);
        end;
      end;
    end;

    if CreatedCube.IsOn then
      Cubes.Add(CreatedCube);
  end;

  Result := 0;
  for CreatedCube in Cubes do
  begin
    Current := 1;
    for Coord := Low(TCoordinaat) to High(TCoordinaat) do
      Current := Current * (CreatedCube.Bottom[Coord] - CreatedCube.Top[Coord]+1);
    Result := Result + Current;
  end;
  Cubes.Free;
end;

{$ENDREGION}
{$REGION 'TAdventOfCodeDay23'}
function TAdventOfCodeDay23.SolveA: Variant;
begin
  Result := CalculateEngergyCost(True);
end;

function TAdventOfCodeDay23.SolveB: Variant;
begin
  Result := CalculateEngergyCost(False);
end;

type TAmphipodInfo = record
  AmphipodType: TAmphipod;
  CharCode: string;
  HallWayIndex,
  EnergyCost: Integer;
end;

const AmphipodInfo:  array[TAmphipod] of TAmphipodInfo =
(
 (AmphipodType: A;    CharCode: 'A'; HallWayIndex: 2; EnergyCost: 1),
 (AmphipodType: B;    CharCode: 'B'; HallWayIndex: 4; EnergyCost: 10),
 (AmphipodType: C;    CharCode: 'C'; HallWayIndex: 6; EnergyCost: 100),
 (AmphipodType: D;    CharCode: 'D'; HallWayIndex: 8; EnergyCost: 1000),
 (AmphipodType: None; CharCode: '.'; HallWayIndex: 0; EnergyCost: 0)
);

{ TAmphipodState }

class function TAmphipodState.Create: TAmphipodState;
var
  i: Integer;
begin
  Result.TotalEngergyUsed := 0;
  Result.GuesedTotalEnergy := 0;

  for i := 0 to 26 do
    Result.Places[i] := None;

  Result.Path := '';
end;

function TAmphipodState.AsString: string;
var
  i: integer;
begin
  Result := TotalEngergyUsed.ToString + '_';
  for i := 0 to 26 do
    Result := Result + AmphipodInfo[Places[i]].CharCode
end;

function TAmphipodState.RoomIndex(aAmphipod: TAmphipod; aDepth: integer): Integer;
begin
  Result := 11 + Ord(aAmphipod) + aDepth * 4;
end;

function TAdventOfCodeDay23.CalculateEngergyCost(InstructionFolded: Boolean): integer;
const DebugPath: boolean = True;
var RoomDepth: integer;

  function _RoomIndex(aAmphipod: TAmphipod; aDepth: integer): Integer;
  begin
    Result := 11 + Ord(aAmphipod) + (aDepth-1) * 4;
  end;

  function _RoomReady(state: TAmphipodState; aAmphipod: TAmphipod): boolean;
  var
    i: integer;
    pod: TAmphipod;
  begin
    Result := True;
    for i := 1 to RoomDepth do
    begin
      pod := state.Places[_RoomIndex(aAmphipod, i)];
      if (pod <> None) and (pod <> aAmphipod) then
        Exit(False);
    end;
  end;

  function _TopAmphipod(aState: TAmphipodState;   aRoom: TAmphipod; out aDepth: integer): TAmphipod;
  var
    i: integer;
    pod: TAmphipod;
  begin
    Result := None;
    for i := 1 to RoomDepth do
    begin
      Pod := aState.Places[_RoomIndex(aRoom, i)];
      if (pod <> None) then
      begin
        Result := pod;
        aDepth := i;
        Exit;
      end;
    end;
  end;

  function _Depth(aState: TAmphipodState; aRoom: TAmphipod): integer;
  var i: integer;
  begin
    Result := -99;
    for i := RoomDepth downto 1 do
      if aState.Places[_RoomIndex(aRoom, i)] = None then
        Exit(i);
  end;

  function StrToAmphipod(Const aCharCode: string): TAmphipod;
  var
    CurrentAmpiPod: TAmphipod;
  begin
    Result := None;
    for CurrentAmpiPod := low(TAmphipod) to High(TAmphipod) do
      if AmphipodInfo[CurrentAmpiPod].CharCode = aCharCode then
        Exit(CurrentAmpiPod)
  end;

  function HallWayClear(aState: TAmphipodState; aFrom, aTo: integer): boolean;
  var i: integer;
  begin
    Result := True;
    for i := Min(aFrom, aTo) to Max(aFrom, aTo) do
      if aState.Places[i] <> None then
        Exit(False);
  end;

  function _Distance(a, b: integer): integer;
  begin
    Result := Abs(a-b);
  end;

  function _GuesEnergy(aState: TAmphipodState): integer;
  var
    i, distance: integer;
    CurrentPod, room: TAmphipod;
  begin
    // Total used
    Result := aState.TotalEngergyUsed;

    // Hallway
    for i := 0 to 10 do
    begin
      CurrentPod := aState.Places[i];
      if CurrentPod = none then
        Continue;

      distance := Max(AmphipodInfo[CurrentPod].HallWayIndex, i-1) - Min(AmphipodInfo[CurrentPod].HallWayIndex, i+1) + RoomDepth + 1;
      Result := Result + distance * AmphipodInfo[a].EnergyCost;
    end;

    // Rooms
    for room := low(TAmphipod) to High(TAmphipod) do
    begin
      if room = None then
        Continue;

      for i := 1 to RoomDepth  do
      begin
        CurrentPod := aState.Places[_RoomIndex(room, i)];
        if (CurrentPod <> none) and (CurrentPod <> room) then
        begin
          distance := i + _Distance(AmphipodInfo[CurrentPod].HallWayIndex, AmphipodInfo[Room].HallWayIndex);
          Result := Result + AmphipodInfo[CurrentPod].EnergyCost * distance;
        end;
      end;
    end;
  end;

  function UpdateAndCloneState(aCurrentState: TAmphipodState; aPodTomMove, aRoom: TAmphipod; aDepth, aHallWayPosition: integer): TAmphipodState;
  var
    tmp: TAmphipod;
    roomIndex: integer;
  begin
    // Copy currentState;
    Result := aCurrentState;

    // Calc energy
    Result.TotalEngergyUsed := Result.TotalEngergyUsed + AmphipodInfo[aPodTomMove].EnergyCost * (aDepth + _Distance(AmphipodInfo[aRoom].HallWayIndex, aHallWayPosition));

    // Swap pods
    roomIndex := _RoomIndex(aRoom, aDepth);
    tmp := Result.Places[aHallWayPosition];
    Result.Places[aHallWayPosition] := Result.Places[roomIndex];
    Result.Places[roomIndex] := tmp;

    Result.GuesedTotalEnergy := _GuesEnergy(Result);

    if DebugPath then
      Result.Path := Result.Path + '|' + Result.AsString;
  end;

var
  CurrentState: TAmphipodState;
  Comparer: IComparer<TAmphipodState>;
  Queue: PriorityQueue<TAmphipodState>;
  Depth, i, HallWayIndex, QueueSize, RoomIndex, Distance: Integer;
  Seen: TDictionary<String, Boolean>;
  TopAmphipod, CurrentAmphipod, CurrentRoom: TAmphipod;
  StateChanged, StateChanged2: Boolean;
begin
  Result := MaxInt;

  CurrentState := TAmphipodState.Create;

  RoomDepth := 2;
  if not InstructionFolded then
  begin
    RoomDepth := 4;

    CurrentState.Places[_RoomIndex(A, 2)] := D;
    CurrentState.Places[_RoomIndex(B, 2)] := C;
    CurrentState.Places[_RoomIndex(C, 2)] := B;
    CurrentState.Places[_RoomIndex(D, 2)] := A;

    CurrentState.Places[_RoomIndex(A, 3)] := D;
    CurrentState.Places[_RoomIndex(B, 3)] := B;
    CurrentState.Places[_RoomIndex(C, 3)] := A;
    CurrentState.Places[_RoomIndex(D, 3)] := C;
  end;

  // Process toprow
  for i := 0 to 3 do
    CurrentState.Places[_RoomIndex(TAmphipod(i), 1)] := StrToAmphipod(FInput[2][4 + i*2]);

  // Process bottomrow
  for i := 0 to 3 do
    CurrentState.Places[_RoomIndex(TAmphipod(i), RoomDepth)] := StrToAmphipod(FInput[3][4 + i*2]);

  if DebugPath then
    CurrentState.Path := CurrentState.AsString;

  Comparer := TComparer<TAmphipodState>.Construct(
    function(const Left, Right: TAmphipodState): integer
    begin
     // if Abs(Left.TotalEngergyUsed - Right.TotalEngergyUsed) < 1000 then
        Result := Sign(Left.TotalEngergyUsed - Right.TotalEngergyUsed)
      //else
       // Result := Sign(Left.GuesedTotalEnergy - Right.GuesedTotalEnergy);
    end);

  Queue := PriorityQueue<TAmphipodState>.Create(Comparer, Comparer);
  Queue.Enqueue(CurrentState);
  Seen := TDictionary<string, boolean>.Create;;

  while Queue.Count > 0 do
  begin
    StateChanged2 := False;
    CurrentState := Queue.Dequeue;

    if CurrentState.TotalEngergyUsed > result then
      Continue;

    if Seen.ContainsKey(CurrentState.AsString) then
      Continue;

    Seen.Add(CurrentState.AsString, true);
    QueueSize := Queue.Count;

    // Try to move everything from the hallway to the assinged rooms
    repeat
      StateChanged := False;

      for i := 0 to 10 do
      begin
        CurrentAmphipod := CurrentState.Places[i];
        if (CurrentAmphipod = None) or (not _RoomReady(CurrentState, CurrentAmphipod))  then
          Continue; //Nothing here or room is occupied

        HallWayIndex := AmphipodInfo[CurrentAmphipod].HallWayIndex;

        if not HallWayClear(CurrentState, Min(HallWayIndex, i+1), Max(HallWayIndex, i-1)) then
          continue;

        // Can reach it!, calc depth
        StateChanged := True;

        Depth := _Depth(CurrentState, CurrentAmphipod);

        // Calc energy
        CurrentState.TotalEngergyUsed :=
          CurrentState.TotalEngergyUsed + AmphipodInfo[CurrentAmphipod].EnergyCost * (Depth + 1 + _Distance(AmphipodInfo[CurrentAmphipod].HallWayIndex, i));

        // Swap pods
        RoomIndex := _RoomIndex(CurrentAmphipod, Depth);
        CurrentState.Places[i] := None;
        CurrentState.Places[roomIndex] := CurrentAmphipod;
        StateChanged2 := True;

        if DebugPath then
          CurrentState.Path := CurrentState.Path + '|' + CurrentState.AsString;
      end;
    until (not StateChanged);

//    // Try To Move from room to room
//    repeat
//      StateChanged := False;
//
//      for CurrentRoom := low(TAmphipod) to High(TAmphipod) do
//      begin
//        if (CurrentRoom = none) or _RoomReady(CurrentState, CurrentRoom) then
//          Continue; // Not a room or room is already full
//
//        // Select the pod to move out
//        TopAmphipod := _TopAmphipod(CurrentState, CurrentRoom, Depth);
//
//        // Check if Recieving room is ready;
//        if not _RoomReady(CurrentState, TopAmphipod) then
//          Continue;
//
//        // Check if hallway is clear
//        if not HallWayClear(CurrentState, AmphipodInfo[CurrentRoom].HallWayIndex, AmphipodInfo[TopAmphipod].HallWayIndex) then
//          Continue;
//
//        // Can reach it!
//        StateChanged := True;
//
//        Distance := Depth + 1;
//
//
//
//
//        // Calc energy
//        CurrentState.TotalEngergyUsed := CurrentState.TotalEngergyUsed
//          + AmphipodInfo[TopAmphipod].EnergyCost *
//          (Depth // Depth to move out
//        + _Distance(AmphipodInfo[CurrentRoom].HallWayIndex, AmphipodInfo[TopAmphipod].HallWayIndex)
//        + _Depth(CurrentState, TopAmphipod) + 1);
//
//        // Swap pods
//        RoomIndex := _RoomIndex(TopAmphipod, Depth);
//        CurrentState.Places[roomIndex] := None;
//        RoomIndex := _RoomIndex(TopAmphipod, _Depth(CurrentState, TopAmphipod));
//        CurrentState.Places[RoomIndex] := TopAmphipod;
//
//        if DebugPath then
//          CurrentState.Path := CurrentState.Path + '|' + CurrentState.AsString;
//      end;
//
//    until (not StateChanged);

    // Try to move from room to hallway
    for CurrentRoom := low(TAmphipod) to High(TAmphipod) do
    begin
      if (CurrentRoom = none) or _RoomReady(CurrentState, CurrentRoom) then
        Continue; // Not a room or room is already full

      TopAmphipod := _TopAmphipod(CurrentState, CurrentRoom, Depth);
      HallWayIndex := AmphipodInfo[CurrentRoom].HallWayIndex;

      for i := 0 to 10 do
      begin
        if i in [2,4,6,8] then // skip entrance to room
          continue;

        if not HallWayClear(CurrentState, i, HallWayIndex) then
          Continue; // Hallway is blocked

        // Can reach it;
        Queue.Enqueue(UpdateAndCloneState(CurrentState, TopAmphipod, CurrentRoom, Depth, i));
      end;
    end;

    if HallWayClear(CurrentState, 0, 10) and _RoomReady(CurrentState, A) and _RoomReady(CurrentState, B) and _RoomReady(CurrentState, C)   then
    begin
      if Result > CurrentState.TotalEngergyUsed then

      WriteLn(CurrentState.TotalEngergyUsed, ' (', queue.Count,')');
      Result := Min(Result, CurrentState.TotalEngergyUsed);
    end;


//    if (QueueSize = Queue.Count) and HallWayClear(CurrentState, 0, 10)then
//    begin
//      if DebugPath then
//        DumpPath(CurrentState.Path);
//
//      Exit(CurrentState.TotalEngergyUsed);
//    end;
  end;
end;

procedure TAdventOfCodeDay23.DumpPath(aPath: string);

  procedure _Line(aPrefix, aLine, aSuffix: string);
  begin
    WriteLN(aPrefix, aLine[1], '#', aLine[2], '#', aLine[3], '#', aLine[4], aSuffix);
  end;

Var
  split, Split2: TStringDynArray;
  i: integer;
  s: string;
begin
  split := SplitString(aPath, '|');
  for i := 0 to Length(split)-1 do
  begin
    Writeln('');
    split2 := SplitString(split[i], '_');
    s := Split2[1];

    Writeln('#############');
    Writeln('#', copy(s, 0, 10), '#');
    _Line('###', copy(s, 12, 4), '###' );
    _Line('  #', copy(s, 16, 4), '#  ' );
    _Line('  #', copy(s, 20, 4), '#  ' );
    _Line('  #', copy(s, 24, 4), '#  ' );
    Writeln('  #########  ');
    WriteLn('energy: ', Split2[0]);
  end;
end;
{$ENDREGION}
{$REGION 'TAdventOfCodeDay24'}
function TAdventOfCodeDay24.SolveA: Variant;
begin
  Result := CalcMONAD(True);
end;

function TAdventOfCodeDay24.SolveB: Variant;
begin
  Result := CalcMONAD(False);
end;

function TAdventOfCodeDay24.CalcMONAD(const aMaxResult: Boolean): Extended ;

  function _Value(Const LineNumber: integer): int64;
  begin
    Result := StrToInt64(SplitString(FInput[LineNumber], ' ')[2])
  end;

var
  i, j, Val1, Val2, Val3, Sum, Base: int64;
  Stack: TStack<int64>;
begin
  Stack := TStack<int64>.Create;
  Result := 0;
  Base := 1;
  if aMaxResult then
    Base := 9;

  for i := 0 to 13 do
  begin
    Val1 := _Value(i * 18 + 4);
    Val2 := _Value(i * 18 + 5);
    Val3 := _Value(i * 18 + 15);

    if Val1 = 1 then
      Stack.Push(i shl 32 + Val3)
    else
    begin
      j := Stack.Peek shr 32;
      val3 := Stack.Pop and MaxInt;
      Sum := Val3 + Val2;
      if ((Sum <= 0) and aMaxResult) or ((Sum >= 0) and not aMaxResult)then
      begin
        Result := Result + Base * Power(10, (13-j));
        Result := Result + (Base + Sum) * Power(10, (13-i))
      end
      else
      begin
        Result := Result + (Base - Sum) * Power(10, (13-j));
        Result := Result + Base * Power(10, (13-i))
      end;
    end;
  end;
  Stack.Free;
end;
{$ENDREGION}
{$REGION 'TAdventOfCodeDay25'}
function TAdventOfCodeDay25.SolveA: Variant;
var
  x, y, MapWidth, MapHeight: integer;
  Map: TDictionary<TPoint,Boolean>;
  Moving: Boolean;

  function _Move(aEast: Boolean): boolean;
  var
    Changes: TDictionary<TPoint, TPoint>;
    CurrentPoint, NewPoint: TPoint;
  begin
    Changes := TDictionary<TPoint, TPoint>.Create;
    for CurrentPoint in Map.Keys do
    begin
      if Map[CurrentPoint] <> aEast then
        Continue;

      NewPoint := TPoint.Create(CurrentPoint);
      if aEast then
      begin
        NewPoint.X := NewPoint.X + 1;
        if NewPoint.X > MapWidth then
          NewPoint.X := 0;
      end
      else
      begin
        NewPoint.Y := NewPoint.Y + 1;
        if NewPoint.Y > MapHeight then
          NewPoint.Y := 0;
      end;

      if not Map.ContainsKey(NewPoint) then
        Changes.Add(CurrentPoint, NewPoint);
    end;

    Result := Changes.Count > 0;
    for CurrentPoint in Changes.Keys do
    begin
      Map.Remove(CurrentPoint);
      Map.Add(Changes[CurrentPoint], aEast);
    end;
    Changes.Free;
  end;


begin
  Map :=  TDictionary<TPoint,Boolean>.Create;

  MapWidth := Length(FInput[0]) -1;
  MapHeight := FInput.Count -1;

  for x := 0 to MapWidth do
    for y := 0 to MapHeight do
      if FInput[y][x+1] <> '.' then
        Map.Add(TPoint.Create(x,y), FInput[y][x+1] = '>');

  Result := 0;
  Moving := True;
  while Moving do
  begin
    Result := Result + 1;

    Moving := _Move(True);
    if _Move(False) then
      Moving := True;
  end;

  Map.Free;
end;

function TAdventOfCodeDay25.SolveB: Variant;
begin
  //
end;
{$ENDREGION}

initialization

RegisterClasses([
  TAdventOfCodeDay1, TAdventOfCodeDay2, TAdventOfCodeDay3, TAdventOfCodeDay4, TAdventOfCodeDay5,
  TAdventOfCodeDay6, TAdventOfCodeDay7, TAdventOfCodeDay8, TAdventOfCodeDay9, TAdventOfCodeDay10,
  TAdventOfCodeDay11,TAdventOfCodeDay12,TAdventOfCodeDay13,TAdventOfCodeDay14,TAdventOfCodeDay15,
  TAdventOfCodeDay16,TAdventOfCodeDay17,TAdventOfCodeDay18,TAdventOfCodeDay19,TAdventOfCodeDay20,
  TAdventOfCodeDay21,TAdventOfCodeDay22,TAdventOfCodeDay23,TAdventOfCodeDay24,TAdventOfCodeDay25]);

end.
