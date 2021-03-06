unit uAOCUtils;

interface

uses
  System.SysUtils, System.Generics.Collections, AOCBase, RTTI, System.Classes,
  System.Net.HttpClient, System.Net.urlclient, system.Generics.Defaults, uAocConfig, vcl.Dialogs, system.uiTypes;

type TAdventOfCodeRef = class of TAdventOfCode;
type TDirection = (Up = 0, Right, Down, Left);

type AOCUtils = class
  public
    class function GetAdventOfCode: TList<TAdventOfCodeRef>;
    class function DayIndexFromClassName(Const aClassName: String): String;
    class procedure DoAdventOfCode(aAdventOfCodeRef: TAdventOfCodeRef; aConfig: TAOCConfig);
    class procedure DownLoadPuzzleInput(var InputList: TStrings; Const DayIndex: String; Config: TAOCConfig);
end;

type TAOCDictionary<TKey,TValue> = class(TDictionary<TKey,TValue>)
  public
    procedure AddOrIgnoreValue(const Key: TKey; const Value: TValue);
    constructor Create(const aOnValueNoify: TCollectionNotifyEvent<TValue>); overload;
    procedure Free; overload;
end;

type
  TPosition = record
    x: integer;
    y: Integer;
    function SetIt(const aX, aY: integer): TPosition;
    function AddDelta(const aX, aY: Integer): TPosition;
    function Equals(Const Other: TPosition): Boolean;
    function Clone: TPosition;
    function ApplyDirection(Const aDirection: TDirection): TPosition;
  end;

function GCD(Number1, Number2: int64): int64;
function OccurrencesOfChar(const S: string; const C: string): integer;
function BitStringToInt(Const aBit: string): int64;
function CountTrueBits(aInt: integer): integer;

Const
  MaxInt64: Int64 = 9223372036854775807;

implementation

uses
  System.strUtils;

class function AOCUtils.GetAdventOfCode: TList<TAdventOfCodeRef>;
var
  ctx: TRttiContext;
  lType: TRttiType;
  AdventOfCode: TAdventOfCodeRef;
  Comparison: TComparison<TAdventOfCodeRef>;
begin
  result := TList<TAdventOfCodeRef>.Create;
  ctx := TRttiContext.Create;
  Writeln('Discovering advent of code');
  for lType in ctx.GetTypes do
    if (lType is TRttiInstanceType) and (TRttiInstanceType(lType).MetaclassType.InheritsFrom(TAdventOfCode))
    then
    begin
      AdventOfCode := TAdventOfCodeRef(TRttiInstanceType(lType).MetaclassType);
      if AdventOfCode.ClassName <> TAdventOfCode.ClassName then
      begin
        Writeln('Found '+ AdventOfCode.ClassName);
        Result.Add(adventOfCode);
      end;
    end;

  Comparison :=
    function(const Left, Right: TAdventOfCodeRef): Integer
    begin
      Result := StrToInt(AOCUtils.DayIndexFromClassName(Left.ClassName)) -
                StrToInt(AOCUtils.DayIndexFromClassName(Right.ClassName));
    end;
  Result.Sort(TComparer<TAdventOfCodeRef>.Construct(Comparison));
end;

class function AOCUtils.DayIndexFromClassName(Const aClassName: String): String;
var i: Integer;
begin
  i := Length('TAdventOfCodeDay');
  Result := Copy(aClassName, i + 1, Length(aClassName) - i); //
end;

class procedure AOCUtils.DoAdventOfCode(aAdventOfCodeRef: TAdventOfCodeRef; aConfig: TAOCConfig);
var AdventOfCode: TAdventOfCode;
begin
  AdventOfCode := aAdventOfCodeRef.Create(aConfig);
  try
    AdventOfCode.Solve;
  finally
    AdventOfCode.Free;
  end;
end;

class procedure AOCUtils.DownLoadPuzzleInput(var InputList: TStrings; Const DayIndex: String; Config: TAOCConfig);
var HttpClient: THttpClient;
    lHeader: TNetHeader;
    Headers: TNetHeaders;
    HttpOutput: IHTTPResponse;
    Url, SessionCookie: string;
begin
  Url := Config.BaseUrl+'/day/'+DayIndex+'/input';
  WriteLn('Downloading puzzle data from ' + Url);

  HttpClient := THTTPClient.Create;
  Headers := nil;
  SessionCookie := Config.SessionCookie;
  if SessionCookie = '' then
  begin
    SessionCookie := InputBox('SessionCookie', 'Whats your sessionCookie?', '');
    if SessionCookie = '' then
      raise Exception.Create('No session cookie provided')
    else
      Config.SessionCookie := SessionCookie;
  end;

  lHeader := LHeader.Create('cookie', 'session=' + SessionCookie);
  SetLength(Headers, 1);
  Headers[0] := lHeader;

  try
    HttpOutput := HttpClient.Get(Url, nil, Headers);
    WriteLn(HttpOutput.StatusCode);
    if HttpOutput.StatusCode = 200 then
      InputList.LoadFromStream(HttpOutput.ContentStream)
    else if HttpOutput.StatusCode = 400 then
    begin
      if MessageDlg('Error conecting to AOC, delete session and try again?', mtError, mbYesNo, 0) = mrYes then
      begin
        Config.SessionCookie := '';
        AOCUtils.DownLoadPuzzleInput(InputList, DayIndex, Config);
      end
    end
    else
      raise Exception.Create(HttpOutput.ContentAsString());
  finally
    HttpClient.Free;
  end;
end;

procedure TAOCDictionary<TKey,TValue>.AddOrIgnoreValue(const Key: TKey; const Value: TValue);
begin
  if not Self.ContainsKey(Key) then
    Self.Add(Key, Value);
end;

constructor TAOCDictionary<TKey,TValue>.Create(const aOnValueNoify: TCollectionNotifyEvent<TValue>);
begin
  inherited Create;
  OnValueNotify := aOnValueNoify;
end;

procedure TAOCDictionary<TKey,TValue>.Free;
begin
  Self.Clear;
  inherited Free;
end;

function TPosition.SetIt(const aX: Integer; const aY: Integer): TPosition;
begin
  x := aX;
  y := aY;
  Result := Self;
end;

function TPosition.AddDelta(const aX, aY: Integer): TPosition;
begin
  x := x + aX;
  y := y + aY;
  Result := Self;
end;

function TPosition.Equals(Const Other: TPosition): Boolean;
begin
  Result := (x = Other.x) and (y = Other.y);
end;

function TPosition.Clone: TPosition;
begin
  Result.x := Self.x;
  Result.y := Self.y;
end;

function TPosition.ApplyDirection(Const aDirection: TDirection): TPosition;
begin
  case aDirection of
    Up: AddDelta(0, -1);
    Right: AddDelta(1, 0);
    Down: AddDelta(0, 1);
    Left: AddDelta(-1, 0);
  end;
  Result := Self
end;

function GCD(Number1, Number2: int64): int64;
var Temp: int64;
begin
  if Number1 < 0 then Number1 := -Number1;
  if Number2 < 0 then Number2 := -Number2;

  repeat
    if Number1 < Number2 then
      begin
        Temp := Number1;
        Number1 := Number2;
        Number2 := Temp;
      end;

    Number1 := Number1 mod Number2;
  until (Number1 = 0);

  result := Number2;
end;

function OccurrencesOfChar(const S: string; const C: string): integer;
var
  i: Integer;
begin
  result := 0;
  for i := 1 to Length(S) do
    if S[i] = C then
      inc(result);
end;

function BitStringToInt(Const aBit: string): int64;
var i: Integer;
begin
  Result := 0;
  for i := 1 to Length(aBit) do
  begin
    Result := Result shl 1;
    case IndexStr(aBit[i], ['0', '1']) of
      0:; //Do nothing
      1: Inc(Result);
    else
      raise Exception.CreateFmt('BitStringToInt encounterd: %s', [aBit[i]]);
    end;
  end;
end;

function CountTrueBits(aInt: integer): integer;
begin
  Result := 0;
  while aInt > 0 do
  begin
    if Odd(aInt) then
      inc(Result);
    aInt := aInt shr 1;
  end;

end;


end.
