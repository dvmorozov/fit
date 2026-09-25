// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definitions of base curve class allowing setting up type name.)

@author(Dmitry Morozov dvmorozov@hotmail.com,
LinkedIn: https://www.linkedin.com/in/dmitry-morozov-79490a59/
Facebook: https://www.facebook.com/dmitry.v.morozov)
}
unit named_points_set;

{$IF NOT DEFINED(FPC)}
{$DEFINE _WINDOWS}
{$ELSEIF DEFINED(WINDOWS)}
{$DEFINE _WINDOWS}
{$ENDIF}

interface

uses
    coordinate_axis, configurable_points_set, curve_points_set, explanation;

type
    TNamedPointsSetClass = class of TNamedPointsSet;
    TExtremumMode   = (
        OnlyMaximums,
        OnlyMinimums,
        MaximumsAndMinimums
        );
    TCurveTypeId    = TGuid;
    { Base curve class allowing setting up type name. Type name distinguishes
      this curve from all other curve types, as opposite to the 'Title' attributes
      which is used to distinguish separate curve instances. }
    TNamedPointsSet = class(TCurvePointsSet)
    private
        { The attribute should not be used in descendants. }
        FName: string;

    public
        { Sets name of curve type. The method is used in deserializing
          objects received from server. }
        procedure SetCurveTypeName(Name: string); virtual;
        { Returns unique name of curve type. }
        class function GetCurveTypeName: string; virtual; abstract;
        { Returns unique type identifier. }
        class function GetCurveTypeId: TCurveTypeId; virtual; abstract;
        { The curve's analytic formula as a text expression in x and its parameter
          names, in numpy syntax (e.g.
          'A/(sigma*sqrt(2*pi))*exp(-(x0-x)**2/(2*sigma**2))'). This is what lets
          the Python backend stay model-agnostic: it evaluates whatever formula it
          is sent instead of re-implementing each curve type. Empty when the curve
          has no closed-form expression the backend can evaluate. }
        function GetCurveExpression: string; virtual;
        { True when instances of this type have a closed-form expression that
          the formula-based backends (Python sidecar, remote compute server) can
          evaluate - i.e. exactly when GetCurveExpression returns non-empty.

          It exists at CLASS level because the UI has to decide whether to offer
          those backends before any instance is placed. The two must agree, and
          a test walks the registry asserting they do, so a future type cannot
          override one and forget the other. }
        class function IsAnalytic: boolean; virtual;
        { Optional grouping for the curve-type menu. Empty (the default) means
          ungrouped, so every existing curve type keeps appearing exactly where
          it does now; a non-empty group asks the UI for a submenu of that name.
          Introduced for a wave-pattern pack, whose six pattern types would
          otherwise crowd the flat list. }
        class function GetCurveTypeGroup: string; virtual;
        { True when this type's amplitude can move freely over orders of
          magnitude during a fit, rather than being pinned near the data by how
          the curve is seeded.

          A capability, not a list of type names (D18): whether a given objective
          may be used with a given curve type is DERIVED from this, so adding a
          seventh pattern type needs no edit to any compatibility table.

          What it guards: an objective normalised by the model's own integral
          (fit_loss.LossIsSelfNormalising) can be reduced by inflating the model
          instead of by fitting it, because the numerator is scale-invariant
          while the denominator is not. A peak never exploits that - its
          amplitude is seeded from the data and stays there - which is why the
          defect went unnoticed for 25 years. A curve free to grow exploits it
          immediately, so that pairing is refused. }
        class function AmplitudeIsUnbounded: boolean; virtual;
        { The named point set instances of this type are PLACED from, or empty
          when they are placed from a single curve position.

          A capability, not a type test (D18). The engine has one decision to
          make before it can build a model - where the curves come from - and
          this is the answer to it: empty means the existing path, one x per
          curve, which is every peak type and therefore the default. A non-empty
          name says the type is placed by marking an extent, and names the point
          set the picks are collected into ('wave-bounds' for a wave
          pattern).

          WHY A NAME AND NOT A FLAG: the same answer tells the client which set
          a pick belongs to and the server which stored set to slice per fit
          interval, so a module can bring its own point set without another
          edit to the engine.

          WHY ASKED OF THE CLASS: the decision is a property of the type, known
          before anything is built. Deriving it from an attempt to build - "did
          the module handle this?" - is what makes an ordinary situation (the
          type is selected, nothing is marked yet) fall through to the
          position-based path, which with nothing marked generates one curve per
          data point. That presented as a hang once already. }
        class function PlacedByPointSet: string; virtual;
        { The axis mode (axis_mode_registry) this curve type's ADimension is
          meant to be shown in, by id, or '' for none.

          A PREFERENCE, NOT AN AXIS. The type names a registered mode and the
          automatic rule (axis_choice) weighs it against the data's and the
          other curves'; the type builds nothing, so it needs no wavelength or
          any other setting, and a module's type can name its module's modes.

          None by default: a type that is about no field in particular (a user
          formula) says nothing, and the data or the general name answers.
          Saying nothing is also right for a coordinate a type cannot know - a
          wave pattern's value is a price, but whether its argument is a bar or
          a date is the data's to say. }
        class function PreferredAxisMode(ADimension: TAxisDimension): string; virtual;
        { A WEAKER preference: the axis mode ADimension is shown in when the
          data says nothing about it. Where PreferredAxisMode outranks the
          data, this yields to it - for a coordinate the type can assume but
          not know. A wave pattern's argument is a bar number unless the series
          was read by date, and then the data says Date. '' for none. }
        class function FallbackAxisMode(ADimension: TAxisDimension): string; virtual;
        { Returns algorithm of searching of extremum points. }
        class function GetExtremumMode: TExtremumMode; virtual; abstract;
        class function GetConfigurablePointsSet: TConfigurablePointsSetClass; virtual;
        { What this curve type is, in words a user can learn from: what its
          formula means, what each parameter controls, where the shape is used,
          what it does not cover, and where to read about it.

          THE CLASS STATES THE CONTENT; the curve-type explanation provider
          stamps the topic and, when this leaves it empty, the title - so an
          override never repeats the type's identity and cannot get it wrong.

          INCOMPLETE BY DEFAULT, deliberately. The base answers with no summary
          and no body, so a type that does not override this fails the
          registry-walking completeness test by name, instead of shipping a
          curve type the application cannot explain. }
        class function Explanation: TExplanation; virtual;
    end;

implementation

uses
    non_configurable_points_set;

{============================ TNamedPointsSet =================================}

procedure TNamedPointsSet.SetCurveTypeName(Name: string);
begin
    FName := Name;
end;

function TNamedPointsSet.GetCurveExpression: string;
begin
    //  No closed-form expression by default; analytic curves override this.
    Result := '';
end;

class function TNamedPointsSet.IsAnalytic: boolean;
begin
    //  Analytic by default: every curve type this framework shipped with had a
    //  formula, so this keeps their behaviour unchanged.
    Result := True;
end;

class function TNamedPointsSet.GetCurveTypeGroup: string;
begin
    //  Ungrouped by default, so existing curve types are unaffected.
    Result := '';
end;

class function TNamedPointsSet.AmplitudeIsUnbounded: boolean;
begin
    //  Bounded by default: every peak type is seeded from the data it sits on.
    Result := False;
end;

class function TNamedPointsSet.PlacedByPointSet: string;
begin
    //  Placed from a single curve position by default, which is what every peak
    //  type does - so adding this capability changes nothing for them.
    Result := '';
end;

{$hints off}
class function TNamedPointsSet.PreferredAxisMode(ADimension: TAxisDimension): string;
begin
    Result := '';
end;

class function TNamedPointsSet.FallbackAxisMode(ADimension: TAxisDimension): string;
begin
    Result := '';
end;
{$hints on}

class function TNamedPointsSet.Explanation: TExplanation;
begin
    //  Nothing to say by default - see the declaration for why that is the
    //  right way to fail.
    Result := Default(TExplanation);
end;

class function TNamedPointsSet.GetConfigurablePointsSet: TConfigurablePointsSetClass;
begin
    Result := TNonConfigurablePointsSet;
end;

end.
