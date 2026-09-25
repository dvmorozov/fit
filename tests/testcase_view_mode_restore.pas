// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(Which axis a saved choice actually resolves to when the window opens.)

A SAVED MODE IS A WISH, NOT A FACT. The setting says which axis the last session
ended on, and start-up has to decide whether that axis can be drawn at all
before it applies it. Some cannot always be:

Sin(theta)/lambda DIVIDES BY A WAVELENGTH. Reopened against a profile whose
wavelength is not known, the axis cannot be computed - and the old code's answer
was to ask for one, which means a modal dialog in front of a window that is not
up yet. The user is asked a question about diffraction geometry before they have
seen their data.

A CUSTOM AXIS IS A PAIR OF FORMULAS, one to display a value and one to read it
back. Without them the mode names nothing at all, and a chart set to it has no
way to place a single point.

A MODE A MODULE REGISTERED is gone from a build without that module.

In each case the answer is the automatic entry, which can always be drawn.
Returning the wish instead is how start-up fails on a setting the user cannot
see in order to correct it.

AND WHETHER THE SETTING COUNTS AT ALL comes first: a mode the user never chose
is not a wish, it is whatever the previous session happened to leave behind.

Every test goes through TChartAxes.Restore - the call the window makes with
what it read from the settings file or the project.
}
unit testcase_view_mode_restore;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry,
    coordinate_axis, axis_mode_registry, axis_mode_registration, axis_choice,
    diffraction_axis_modes, chart_axes;

type
    TViewModeRestoreTest = class(TTestCase)
    private
        FAxes: TChartAxes;
        function Restored(ADimension: TAxisDimension; const AStoredId: string;
            AChosen: boolean; AWaveLength: double;
            const AForward, AInverse: string): string;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        //  Whether the setting counts at all.
        procedure AModeTheUserChoseIsHonoured;
        procedure AModeTheUserNeverChoseIsNot;

        //  The diffraction axis needs a wavelength.
        procedure TheDiffractionAxisIsKeptWhenAWavelengthIsKnown;
        procedure ItFallsBackToTheAutomaticAxisWithoutOne;
        procedure OnlyTheDiffractionAxisCaresAboutTheWavelength;

        //  The custom axis needs both its formulas.
        procedure TheCustomAxisIsKeptWhenBothFormulasWereSaved;
        procedure ItFallsBackWithNeitherFormula;
        procedure ItFallsBackWithOnlyTheForwardOne;
        procedure ItFallsBackWithOnlyTheInverseOne;
        procedure OnlyTheCustomAxisCaresAboutTheFormulas;

        //  A module's mode, in a build without the module.
        procedure AModeNoLongerRegisteredFallsBack;

        //  Together.
        procedure APlainModeIsUnaffectedByEither;
        procedure AnUnchosenModeIsRefusedBeforeAnythingElseIsAsked;
        procedure AFallbackIsNoLongerTheUsersChoice;
        procedure TheDefinitionIsKeptEvenWhenItsModeIsNotRestored;
        procedure TheValueAxisIsRestoredOnItsOwn;
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

procedure TViewModeRestoreTest.SetUp;
begin
    RegisterAllAxisModes;
    FAxes := TChartAxes.Create;
end;

procedure TViewModeRestoreTest.TearDown;
begin
    FreeAndNil(FAxes);
end;

function TViewModeRestoreTest.Restored(ADimension: TAxisDimension;
    const AStoredId: string; AChosen: boolean; AWaveLength: double;
    const AForward, AInverse: string): string;
var
    Definition: TAxisDefinition;
begin
    Definition.Name := 'Energy';
    Definition.UnitName := 'nm';
    Definition.Forward := AForward;
    Definition.Inverse := AInverse;
    //  The wavelength is known before the choice is restored, as it is in the
    //  window: the project's settings come first.
    FAxes.SetWaveLength(AWaveLength);
    FAxes.Restore(ADimension, AStoredId, AChosen, Definition);
    Result := FAxes.Choice(ADimension).ModeId;
end;

{ ---- whether the setting counts at all ------------------------------------- }

procedure TViewModeRestoreTest.AModeTheUserChoseIsHonoured;
begin
    AssertEquals('kept', TwoThetaAxisModeId,
        Restored(adArgument, TwoThetaAxisModeId, True, CuKa, '', ''));
    AssertTrue('and still the user''s', FAxes.Choice(adArgument).ChosenByUser);
end;

procedure TViewModeRestoreTest.AModeTheUserNeverChoseIsNot;
begin
    //  A settings file written before the choice was recorded says 2*Theta only
    //  because that was the hard-coded default. Honouring it would pin every
    //  existing user to a diffraction axis whatever they model.
    AssertEquals('the model and the data decide', AutomaticAxisModeId,
        Restored(adArgument, TwoThetaAxisModeId, False, CuKa, '', ''));
end;

{ ---- the diffraction axis needs a wavelength ------------------------------- }

procedure TViewModeRestoreTest.TheDiffractionAxisIsKeptWhenAWavelengthIsKnown;
begin
    AssertEquals('kept', SinThetaOverLambdaAxisModeId,
        Restored(adArgument, SinThetaOverLambdaAxisModeId, True, CuKa, '', ''));
end;

procedure TViewModeRestoreTest.ItFallsBackToTheAutomaticAxisWithoutOne;
begin
    //  THE DEFECT. The old path asked for a wavelength here - a modal dialog
    //  before the main window had even appeared.
    AssertEquals('the automatic axis instead', AutomaticAxisModeId,
        Restored(adArgument, SinThetaOverLambdaAxisModeId, True, NoWaveLength,
            '', ''));
end;

procedure TViewModeRestoreTest.OnlyTheDiffractionAxisCaresAboutTheWavelength;
begin
    //  Theta and 2*Theta are the stored value halved and as-is: neither divides
    //  by anything, so neither needs to wait for a wavelength.
    AssertEquals('theta survives', ThetaAxisModeId,
        Restored(adArgument, ThetaAxisModeId, True, NoWaveLength, '', ''));
    AssertEquals('and so does 2-theta', TwoThetaAxisModeId,
        Restored(adArgument, TwoThetaAxisModeId, True, NoWaveLength, '', ''));
end;

{ ---- the custom axis needs both its formulas ------------------------------- }

procedure TViewModeRestoreTest.TheCustomAxisIsKeptWhenBothFormulasWereSaved;
begin
    AssertEquals('kept', CustomAxisModeId,
        Restored(adArgument, CustomAxisModeId, True, NoWaveLength, Fwd, Inv));
end;

procedure TViewModeRestoreTest.ItFallsBackWithNeitherFormula;
begin
    AssertEquals('the automatic axis instead', AutomaticAxisModeId,
        Restored(adArgument, CustomAxisModeId, True, CuKa, '', ''));
end;

procedure TViewModeRestoreTest.ItFallsBackWithOnlyTheForwardOne;
begin
    //  Values could be drawn but not read back: a click on the chart would
    //  land nowhere, and a typed position could not be converted.
    AssertEquals('not usable', AutomaticAxisModeId,
        Restored(adArgument, CustomAxisModeId, True, CuKa, Fwd, ''));
end;

procedure TViewModeRestoreTest.ItFallsBackWithOnlyTheInverseOne;
begin
    AssertEquals('nor this way round', AutomaticAxisModeId,
        Restored(adArgument, CustomAxisModeId, True, CuKa, '', Inv));
end;

procedure TViewModeRestoreTest.OnlyTheCustomAxisCaresAboutTheFormulas;
begin
    AssertEquals('2-theta is unaffected', TwoThetaAxisModeId,
        Restored(adArgument, TwoThetaAxisModeId, True, CuKa, '', ''));
    AssertEquals('and so is the general axis', PositionAxisModeId,
        Restored(adArgument, PositionAxisModeId, True, CuKa, '', ''));
end;

procedure TViewModeRestoreTest.AModeNoLongerRegisteredFallsBack;
begin
    AssertEquals(AutomaticAxisModeId,
        Restored(adArgument, 'vendor.module-not-in-this-build', True, CuKa,
            '', ''));
end;

{ ---- together --------------------------------------------------------------- }

procedure TViewModeRestoreTest.APlainModeIsUnaffectedByEither;
begin
    AssertEquals('nothing to check', PositionAxisModeId,
        Restored(adArgument, PositionAxisModeId, True, NoWaveLength, '', ''));
end;

procedure TViewModeRestoreTest.AnUnchosenModeIsRefusedBeforeAnythingElseIsAsked;
begin
    //  Even a mode that WOULD work is discarded if the user never picked it -
    //  the order of the checks is part of the rule.
    AssertEquals('discarded even though it would work', AutomaticAxisModeId,
        Restored(adArgument, SinThetaOverLambdaAxisModeId, False, CuKa, '', ''));
    AssertEquals('and so is a complete custom axis', AutomaticAxisModeId,
        Restored(adArgument, CustomAxisModeId, False, CuKa, Fwd, Inv));
end;

procedure TViewModeRestoreTest.AFallbackIsNoLongerTheUsersChoice;
begin
    //  Saved again without it, rather than written back as a choice the
    //  session never showed.
    Restored(adArgument, SinThetaOverLambdaAxisModeId, True, NoWaveLength, '', '');
    AssertFalse(FAxes.Choice(adArgument).ChosenByUser);
end;

procedure TViewModeRestoreTest.TheDefinitionIsKeptEvenWhenItsModeIsNotRestored;
begin
    //  The dialog reopens on what was given, whichever axis is in force.
    Restored(adArgument, TwoThetaAxisModeId, True, CuKa, Fwd, Inv);
    AssertEquals(Fwd, FAxes.Choice(adArgument).Definition.Forward);
    AssertEquals('Energy', FAxes.Choice(adArgument).Definition.Name);
end;

procedure TViewModeRestoreTest.TheValueAxisIsRestoredOnItsOwn;
begin
    AssertEquals(LogarithmicAxisModeId,
        Restored(adValue, LogarithmicAxisModeId, True, NoWaveLength, '', ''));
    AssertEquals('the argument is untouched', AutomaticAxisModeId,
        FAxes.Choice(adArgument).ModeId);
    AssertEquals('an argument mode is no value mode', AutomaticAxisModeId,
        Restored(adValue, ThetaAxisModeId, True, CuKa, '', ''));
end;

initialization
    RegisterTest('unit', TViewModeRestoreTest);
end.
