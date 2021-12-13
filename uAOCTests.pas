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

Const AOCTestData: array[0..12] of AOCTest =
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
 (AOCClass: TAdventOfCodeDay13; ExpectedSolutionA: '735'; ExpectedSolutionB: '')
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
