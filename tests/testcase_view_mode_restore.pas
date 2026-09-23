// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(Which argument axis a saved setting actually resolves to when the
window opens.)

A SAVED MODE IS A WISH, NOT A FACT. The setting says which axis the last session
ended on, and start-up has to decide whether that axis can be drawn at all
before it applies it. Two of them cannot always be:

Sin(theta)/lambda DIVIDES BY A WAVELENGTH. Reopened against a profile whose
wavelength is not known, the axis cannot be computed - and the old code's answer
was to ask for one, which means a modal dialog in front of a window that is not
up yet. The user is asked a question about diffraction geometry before they have
seen their data.

A CUSTOM AXIS IS A PAIR OF FORMULAS, one to display a value and one to read it
back. Without them the mode names nothing at all, and a chart set to it has no
way to place a single point.

In both cases the answer is the curve type's own axis, which every curve type
can always supply. Returning the wish instead is how start-up fails on a setting
the user cannot see in order to correct it.

AND WHETHER THE SETTING COUNTS AT ALL comes first: a mode the user never chose
is not a wish, it is whatever the previous session happened to leave behind.
}
unit testcase_view_mode_restore;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry,
    argument_axis, mscr_specimen_list;

type
    TViewModeRestoreTest = class(TTestCase)
    published
        //  Whether the setting counts at all.
        procedure AModeTheUserChoseIsHonoured;
        procedure AModeTheUserNeverChoseIsNot;

        //  The diffraction axis needs a wavelength.
        procedure TheDiffractionAxisIsKeptWhenAWavelengthIsKnown;
        procedure ItFallsBackToTheModelsAxisWithoutOne;
        procedure OnlyTheDiffractionAxisCaresAboutTheWavelength;

        //  The custom axis needs both its formulas.
        procedure TheCustomAxisIsKeptWhenBothFormulasWereSaved;
        procedure ItFallsBackWithNeitherFormula;
        procedure ItFallsBackWithOnlyTheForwardOne;
        procedure ItFallsBackWithOnlyTheInverseOne;
        procedure OnlyTheCustomAxisCaresAboutTheFormulas;

        //  Together.
        procedure APlainModeIsUnaffectedByEither;
        procedure AnUnchosenModeIsRefusedBeforeAnythingElseIsAsked;
        procedure TheFallbackIsAlwaysTheModelsOwnAxis;
    end;

implementation

const
    //  A representative wavelength; only the diffraction family reads it.
    CuKa = 1.54056;
    //  "No wavelength is known" - the value the client reports before a profile
    //  has told it one.
    NoWaveLength = 0;
    Fwd = '1239.84/x';
    Inv = '1239.84/x';

{ ---- whether the setting counts at all ------------------------------------- }

procedure TViewModeRestoreTest.AModeTheUserChoseIsHonoured;
begin
    //  An explicit choice outranks the model's preference - that is what
    //  choosing an axis from the menu means, and it has to survive a restart or
    //  the choice was not saved at all.
    AssertEquals('kept', XCM_2T,
        UsableViewMode(XCM_2T, True, CuKa, '', ''));
end;

procedure TViewModeRestoreTest.AModeTheUserNeverChoseIsNot;
begin
    //  WHAT THE PREVIOUS SESSION LEFT BEHIND IS NOT A CHOICE. Without this the
    //  axis latches on whatever was shown last and stops following the curve
    //  type, so a diffraction peak opened after a wave pattern is captioned as
    //  a plain position and its values are stored through the wrong transform.
    AssertEquals('the model decides', XCM_CURVE,
        UsableViewMode(XCM_2T, False, CuKa, '', ''));
end;

{ ---- the diffraction axis needs a wavelength ------------------------------- }

procedure TViewModeRestoreTest.TheDiffractionAxisIsKeptWhenAWavelengthIsKnown;
begin
    AssertEquals('kept', XCM_SINTL,
        UsableViewMode(XCM_SINTL, True, CuKa, '', ''));
end;

procedure TViewModeRestoreTest.ItFallsBackToTheModelsAxisWithoutOne;
begin
    //  THE ONE THAT WOULD BLOCK START-UP. sin(theta)/lambda divides by the
    //  wavelength; asking the user for one puts a modal dialog in front of a
    //  window that is not up yet, about a quantity they have not been shown any
    //  reason to care about.
    AssertEquals('the model''s axis instead', XCM_CURVE,
        UsableViewMode(XCM_SINTL, True, NoWaveLength, '', ''));
end;

procedure TViewModeRestoreTest.OnlyTheDiffractionAxisCaresAboutTheWavelength;
begin
    //  Theta and 2-theta are angles the instrument measured; they mean the same
    //  thing whether or not anybody recorded what wavelength produced them. A
    //  rule that gated the whole family on a wavelength would drop a user back
    //  to the model's axis for no reason.
    AssertEquals('theta survives', XCM_T,
        UsableViewMode(XCM_T, True, NoWaveLength, '', ''));
    AssertEquals('and so does 2-theta', XCM_2T,
        UsableViewMode(XCM_2T, True, NoWaveLength, '', ''));
end;

{ ---- the custom axis needs both its formulas ------------------------------- }

procedure TViewModeRestoreTest.TheCustomAxisIsKeptWhenBothFormulasWereSaved;
begin
    AssertEquals('kept', XCM_CUSTOM,
        UsableViewMode(XCM_CUSTOM, True, NoWaveLength, Fwd, Inv));
end;

procedure TViewModeRestoreTest.ItFallsBackWithNeitherFormula;
begin
    //  The mode names nothing: there is no axis to build, so a chart set to it
    //  cannot place a single point.
    AssertEquals('the model''s axis instead', XCM_CURVE,
        UsableViewMode(XCM_CUSTOM, True, CuKa, '', ''));
end;

procedure TViewModeRestoreTest.ItFallsBackWithOnlyTheForwardOne;
begin
    //  BOTH, NOT EITHER. With only the forward formula a position can be shown
    //  and not read back, so the parameters grid displays a value that cannot
    //  be typed - and an edit lands on whatever the missing inverse returns.
    AssertEquals('not usable', XCM_CURVE,
        UsableViewMode(XCM_CUSTOM, True, CuKa, Fwd, ''));
end;

procedure TViewModeRestoreTest.ItFallsBackWithOnlyTheInverseOne;
begin
    AssertEquals('nor this way round', XCM_CURVE,
        UsableViewMode(XCM_CUSTOM, True, CuKa, '', Inv));
end;

procedure TViewModeRestoreTest.OnlyTheCustomAxisCaresAboutTheFormulas;
begin
    //  Every other mode carries its own conversion in code, so a session that
    //  never defined a custom axis must not lose the axis it did choose.
    AssertEquals('2-theta is unaffected', XCM_2T,
        UsableViewMode(XCM_2T, True, CuKa, '', ''));
    AssertEquals('and so is the identity axis', XCM_IDENTITY,
        UsableViewMode(XCM_IDENTITY, True, CuKa, '', ''));
end;

{ ---- together -------------------------------------------------------------- }

procedure TViewModeRestoreTest.APlainModeIsUnaffectedByEither;
begin
    AssertEquals('nothing to check', XCM_IDENTITY,
        UsableViewMode(XCM_IDENTITY, True, NoWaveLength, '', ''));
end;

procedure TViewModeRestoreTest.AnUnchosenModeIsRefusedBeforeAnythingElseIsAsked;
begin
    //  ORDER MATTERS. A mode nobody chose is discarded whether or not it would
    //  have been usable - otherwise a wavelength happening to be known would
    //  resurrect a setting the user never made.
    AssertEquals('discarded even though it would work', XCM_CURVE,
        UsableViewMode(XCM_SINTL, False, CuKa, '', ''));
    AssertEquals('and so is a complete custom axis', XCM_CURVE,
        UsableViewMode(XCM_CUSTOM, False, CuKa, Fwd, Inv));
end;

procedure TViewModeRestoreTest.TheFallbackIsAlwaysTheModelsOwnAxis;
begin
    //  ONE FALLBACK, not a different one per failure. The curve type's own axis
    //  is the only one every curve type can always supply, so it is the only
    //  answer that cannot itself fail - which is what a fallback has to be.
    AssertEquals('no wavelength', XCM_CURVE,
        UsableViewMode(XCM_SINTL, True, NoWaveLength, '', ''));
    AssertEquals('no formulas', XCM_CURVE,
        UsableViewMode(XCM_CUSTOM, True, CuKa, '', ''));
    AssertEquals('never chosen', XCM_CURVE,
        UsableViewMode(XCM_T, False, CuKa, '', ''));
end;

initialization
    //  A unit test: five numbers in and one out. No viewer, no settings file.
    RegisterTest('unit', TViewModeRestoreTest);
end.
