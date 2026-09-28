// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(A component name made from data, that the widget set will accept.)

WHY THIS IS NOT STRING CONCATENATION. `TComponent.Name` must be a valid Pascal
identifier and unique among its owner's components, and setting one that is not
raises. Both rules are easy to break from data the user supplies:

  * a curve type called "2 br. Pseudo-Voigt" begins with a digit and carries a
    dot and two spaces;
  * two user-defined curves may be given the same name, and nothing stops them.

So the mapping is a function with tests rather than a `+` inside a build loop,
where the failure would be an exception at start-up on somebody else's data.

WHY NAME GENERATED WIDGETS AT ALL. Nothing looks a generated widget up by name -
there is no FindComponent-by-name over them anywhere. The name is what the Object
Inspector, the debugger's watch list, an exception message and
`MenuEntriesAtRiskOfDangling`'s log line all call the thing, and "CurveType7"
tells the next reader nothing. That is reason enough on its own.
}
unit ui_names;

{$mode objfpc}{$H+}

interface

uses
    SysUtils, Classes;

type
    { Issues names, and remembers what it has issued.

      A CLASS RATHER THAN A FUNCTION because uniqueness is a property of the
      SET, not of any one name: the same source text may arrive twice, and only
      something holding the previous answers can say so. }
    TWidgetNames = class
    private
        FIssued: TStringList;
    public
        constructor Create;
        destructor Destroy; override;

        { APrefix followed by an identifier made from ASource, unique among
          what this instance has already issued.

          Characters an identifier cannot hold are dropped rather than replaced:
          "Asym. Pseudo-Voigt" gives AsymPseudoVoigt, which reads as the name it
          came from. A leading digit is kept - it sits behind the prefix, where
          a digit is legal - so "2 br." keeps the 2 that distinguishes it. A
          collision takes a numeric suffix, which is the one place a number in a
          name means something: "the second one of these".

          A source with nothing usable in it still gets a name, because the
          alternative is an exception in a build loop over data somebody else
          supplied. }
        function NameFor(const APrefix, ASource: string): string;

        { Forgets what has been issued. For a caller that rebuilds a set of
          widgets from scratch, where last time's names went with last time's
          widgets. }
        procedure Clear;
    end;

{ The identifier ASource makes, with no prefix and no uniqueness - the half of
  the rule that is a pure function of one string. Exposed for its tests, and
  because a caller that needs no register should not have to make one. }
function IdentifierFrom(const ASource: string): string;

implementation

const
    { What a name falls back to when the source contributes nothing an
      identifier can hold - a caption of punctuation, or an empty one. Better a
      dull name than an exception. }
    UnnamedSource = 'Item';

function IdentifierFrom(const ASource: string): string;
var
    i: longint;
    C: char;
begin
    Result := '';
    for i := 1 to Length(ASource) do
    begin
        C := ASource[i];
        //  ASCII ONLY, and deliberately: FPC accepts an identifier of these,
        //  and a name is not a caption - it does not have to carry the user's
        //  own alphabet to be useful in a debugger.
        if ((C >= 'A') and (C <= 'Z')) or ((C >= 'a') and (C <= 'z')) or
            ((C >= '0') and (C <= '9')) or (C = '_') then
            Result := Result + C;
    end;
    if Result = '' then
        Result := UnnamedSource;
end;

constructor TWidgetNames.Create;
begin
    inherited Create;
    FIssued := TStringList.Create;
    //  Case-insensitively, because that is how Pascal compares identifiers and
    //  how the widget set compares component names.
    FIssued.CaseSensitive := False;
end;

destructor TWidgetNames.Destroy;
begin
    FIssued.Free;
    inherited Destroy;
end;

procedure TWidgetNames.Clear;
begin
    FIssued.Clear;
end;

function TWidgetNames.NameFor(const APrefix, ASource: string): string;
var
    Base: string;
    N: longint;
begin
    Base := APrefix + IdentifierFrom(ASource);
    //  A prefix of nothing and a source starting with a digit would give a name
    //  starting with a digit, which no identifier may. Guarded here rather than
    //  forbidden of the caller, because the caller is a build loop.
    if not IsValidIdent(Base) then
        Base := UnnamedSource + Base;

    Result := Base;
    N := 1;
    while FIssued.IndexOf(Result) >= 0 do
    begin
        Inc(N);
        Result := Base + IntToStr(N);
    end;
    FIssued.Add(Result);
end;

end.
