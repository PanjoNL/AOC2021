unit uAOCTests;

interface

uses
  System.SysUtils, Winapi.Windows,
  uAocUtils, AocSolutions, AOCBase, uAOCConfig;

type AOCTest = record
  AOCClass: TAdventOfCodeRef;
  ExpectedSolutionA, ExpectedSolutionB, OverRidenTestInput: String;
end;

type AOCTests = class
public
  Class procedure RunTests(aConfig: TAOCConfig);
end;

Const AOCTestData: array[0..24] of AOCTest =
(
 (AOCClass: TAdventOfCodeDay1; ExpectedSolutionA: '1502'; ExpectedSolutionB: '1538'),
 (AOCClass: TAdventOfCodeDay2; ExpectedSolutionA: '2272262'; ExpectedSolutionB: '2134882034'),
 (AOCClass: TAdventOfCodeDay3; ExpectedSolutionA: '1540244'; ExpectedSolutionB: '4203981'),
 (AOCClass: TAdventOfCodeDay4; ExpectedSolutionA: '4662'; ExpectedSolutionB: '12080'),
 (AOCClass: TAdventOfCodeDay5; ExpectedSolutionA: '6007'; ExpectedSolutionB: '19349'),
 (AOCClass: TAdventOfCodeDay6; ExpectedSolutionA: '362740'; ExpectedSolutionB: '1644874076764'),
 (AOCClass: TAdventOfCodeDay7; ExpectedSolutionA: '328187'; ExpectedSolutionB: '91257582'),
 (AOCClass: TAdventOfCodeDay8; ExpectedSolutionA: '367'; ExpectedSolutionB: '974512'),
 (AOCClass: TAdventOfCodeDay9; ExpectedSolutionA: '444'; ExpectedSolutionB: '1168440'),
 (AOCClass: TAdventOfCodeDay10; ExpectedSolutionA: '315693'; ExpectedSolutionB: '1870887234'),
 (AOCClass: TAdventOfCodeDay11; ExpectedSolutionA: '1723'; ExpectedSolutionB: '327'),
 (AOCClass: TAdventOfCodeDay12; ExpectedSolutionA: '5457'; ExpectedSolutionB: '128506'),
 (AOCClass: TAdventOfCodeDay13; ExpectedSolutionA: '735'; ExpectedSolutionB: ''),
 (AOCClass: TAdventOfCodeDay14; ExpectedSolutionA: '2937'; ExpectedSolutionB: '3390034818249'),
 (AOCClass: TAdventOfCodeDay15; ExpectedSolutionA: '589'; ExpectedSolutionB: '2885'),
 (AOCClass: TAdventOfCodeDay16; ExpectedSolutionA: '901'; ExpectedSolutionB: '110434737925'),
 (AOCClass: TAdventOfCodeDay17; ExpectedSolutionA: '13203'; ExpectedSolutionB: '5644'),
 (AOCClass: TAdventOfCodeDay18; ExpectedSolutionA: '3486'; ExpectedSolutionB: '4747'),
 (AOCClass: TAdventOfCodeDay19; ExpectedSolutionA: '432'; ExpectedSolutionB: '14414'),
 (AOCClass: TAdventOfCodeDay20; ExpectedSolutionA: '5461'; ExpectedSolutionB: '18226'),
 (AOCClass: TAdventOfCodeDay21; ExpectedSolutionA: '678468'; ExpectedSolutionB: '131180774190079'),
 (AOCClass: TAdventOfCodeDay22; ExpectedSolutionA: '598616'; ExpectedSolutionB: '1193043154475246'),
 (AOCClass: TAdventOfCodeDay23; ExpectedSolutionA: '15516'; ExpectedSolutionB: '45272'),
 (AOCClass: TAdventOfCodeDay24; ExpectedSolutionA: '96979989692495'; ExpectedSolutionB: '51316214181141'),
 (AOCClass: TAdventOfCodeDay25; ExpectedSolutionA: '321'; ExpectedSolutionB: '')
);

implementation

class procedure AOCTests.RunTests(aConfig: TAOCConfig);

  procedure _Check(const DisplayName, Expected, Actual: String);
  begin
    if Expected <> '' then
      if Expected <> Actual then
      begin
        WriteLn(Format('FAIL, %s Expected: %s, Actual: %s', [DisplayName, Expected, Actual]));
        Assert(False);
      end
      else
        WriteLn(Format('PASS, %s', [DisplayName]))
  end;

Var Test: AOCTest;
    AdventOfCode: TAdventOfCode;
    SolutionA, SolutionB: string;
    StartTickTest, StartTick: Int64;
begin
  Writeln('');
  StartTick := GetTickCount;
  for Test in AOCTestData do
  begin
    Writeln(Format('Running tests for %s', [Test.AOCClass.ClassName]));

    StartTickTest := GetTickCount;
    AdventOfCode := Test.AOCClass.Create(aConfig);
    AdventOfCode.Test(SolutionA, SolutionB, Test.OverRidenTestInput);
    AdventOfCode.Free;

    _Check('Part a', Test.ExpectedSolutionA, SolutionA);
    _Check('Part b', Test.ExpectedSolutionB, SolutionB);
    Writeln(FormAt('Total ticks %d', [GetTickCount - StartTickTest]));
    Writeln('');
  end;

  Writeln(Format('All tests done in %d ms', [GetTickCount - StartTick]));
end;

end.
