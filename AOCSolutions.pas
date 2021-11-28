unit AOCSolutions;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Generics.Defaults, System.Generics.Collections,
  system.Diagnostics, AOCBase, RegularExpressions, System.DateUtils, system.StrUtils,
  system.Math, uAOCUtils, system.Types;

type
  TAdventOfCodeDay1 = class(TAdventOfCode)
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
procedure TAdventOfCodeDay1.BeforeSolve;
begin

end;

procedure TAdventOfCodeDay1.AfterSolve;
begin

end;

function TAdventOfCodeDay1.SolveA: Variant;
//var Split: TStringDynArray;
begin
//  Split := SplitString(FInput[0], ';');

end;

function TAdventOfCodeDay1.SolveB: Variant;
begin

end;
{$ENDREGION}
{$Region 'Placeholder'}
(*
procedure TAdventOfCodeDay.BeforeSolve;
begin

end;

procedure TAdventOfCodeDay.AfterSolve;
begin
  FNumbers.Free;
end;

function TAdventOfCodeDay.SolveA: Variant;
begin

end;

function TAdventOfCodeDay2.SolveB: Variant;
begin

end;
*)
{$ENDREGION}

initialization
  RegisterClasses([TAdventOfCodeDay1]);

end.

