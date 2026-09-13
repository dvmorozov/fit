// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(What every mock in this directory shares, and the lifetime rule.)

THE LIFETIME RULE, and why it is stated here once rather than in every mock.
Everything in this project compiles -SIcorba: interfaces are CORBA style, so
there is NO IUnknown, NO _AddRef/_Release and NO REFERENCE COUNTING. An interface
variable is a bare vtable pointer into an object nobody is counting.

So a mock is a plain TObject that implements its interface, and:

  * it does NOT descend from TInterfacedObject. The refcounting there is inert
    under corba mode, so its presence reads as a lifetime guarantee that does not
    exist - which is worse than no guarantee, because a reader stops looking.
  * the FIXTURE owns it. It holds both the object reference (to free through) and
    the interface reference (to call through), nils the INTERFACE first in
    TearDown, and frees the object after. A live interface variable over a freed
    object is a pointer into reclaimed memory, and the next call through it is
    undefined rather than nil.
  * every mock answers AsObject, for the same reason
    int_app_module.IModuleSession does: a corba interface carries no cast back to
    TObject, so a test holding only the interface could not otherwise read the
    call log it came to read.

A MOCK RECORDS; IT DOES NOT ASSERT. A mock that failed inside its own callback
would report the failure from wherever - and on whatever thread - the code under
test happened to call it, and the message would name the mock rather than the
expectation. The test asserts afterwards, on its own thread, against the log.

The only test double that existed before this directory was TFakeLoader in
testcase_data_loader_registry, which is a class rather than an interface and so
teaches none of the above.
}
unit mock_support;

{$MODE Delphi}

interface

uses
    Classes, SysUtils;

type
    { What a mock recorded: method names and arguments, in order.

      ORDER IS PART OF THE CONTRACT for anything that reports progress, and a
      mock that only counted calls could not tell "asked twice" from "asked for
      the wrong thing". }
    TCallLog = class(TObject)
    private
        FCalls: TStringList;
    public
        constructor Create;
        destructor Destroy; override;
        procedure Note(const AMethod: string); overload;
        procedure Note(const AMethod, AArgs: string); overload;
        { How many times AMethod was called, whatever its arguments. }
        function CountOf(const AMethod: string): longint;
        { True when AMethod was called at least once. }
        function Saw(const AMethod: string): boolean;
        { The calls in order, one per line - the message an assertion prints. }
        function AsText: string;
        { The sequence as one comparable string: 'ShowProfile;Done'. }
        function Sequence: string;
        procedure Clear;
        property Calls: TStringList read FCalls;
    end;

    { Base for every mock here: owns a call log and answers AsObject. }
    TMockBase = class(TObject)
    protected
        FLog: TCallLog;
    public
        constructor Create; virtual;
        destructor Destroy; override;
        { A corba interface carries no cast to TObject - see the unit header. }
        function AsObject: TObject;
        property Log: TCallLog read FLog;
    end;

implementation

{ TCallLog }

constructor TCallLog.Create;
begin
    inherited Create;
    FCalls := TStringList.Create;
end;

destructor TCallLog.Destroy;
begin
    FCalls.Free;
    inherited;
end;

procedure TCallLog.Note(const AMethod: string);
begin
    FCalls.Add(AMethod);
end;

procedure TCallLog.Note(const AMethod, AArgs: string);
begin
    //  '(' as the separator so CountOf can match a bare name against a call that
    //  recorded arguments without the two spellings drifting apart.
    FCalls.Add(AMethod + '(' + AArgs + ')');
end;

function TCallLog.CountOf(const AMethod: string): longint;
var
    i: longint;
begin
    Result := 0;
    for i := 0 to FCalls.Count - 1 do
        if (FCalls[i] = AMethod) or
           (Pos(AMethod + '(', FCalls[i]) = 1) then
            Inc(Result);
end;

function TCallLog.Saw(const AMethod: string): boolean;
begin
    Result := CountOf(AMethod) > 0;
end;

function TCallLog.AsText: string;
begin
    Result := FCalls.Text;
end;

function TCallLog.Sequence: string;
var
    i: longint;
begin
    Result := '';
    for i := 0 to FCalls.Count - 1 do
    begin
        if Result <> '' then
            Result := Result + ';';
        Result := Result + FCalls[i];
    end;
end;

procedure TCallLog.Clear;
begin
    FCalls.Clear;
end;

{ TMockBase }

constructor TMockBase.Create;
begin
    inherited Create;
    FLog := TCallLog.Create;
end;

destructor TMockBase.Destroy;
begin
    FLog.Free;
    inherited;
end;

function TMockBase.AsObject: TObject;
begin
    Result := Self;
end;

end.
