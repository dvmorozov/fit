// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(How the interface decides what pixel density to lay itself out for.)

WHY THIS IS WORTH TESTING AT ALL. Every one of these decisions is invisible on
the machine that wrote it. A scale factor parsed through the wrong decimal
separator, a resource matched on the wrong line, a ppi accepted that no display
has - each produces a window that is the wrong size on somebody else's desktop
and exactly right on this one. There is no assertion a developer can make by
looking, which is precisely why the arithmetic had to come out of the unit that
opens with `uses Forms`.

The extraction is ui_scaling; ui_dpi keeps the environment reads, the gdk
property and the write into ScreenInfo. Nothing here needs a screen.
}
unit testcase_ui_scaling;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, ui_scaling;

type
    TUiScalingTest = class(TTestCase)
    published
        //  A toolkit scale factor as text.
        procedure AWholeFactorScalesTheDesignDensity;
        procedure AFractionalFactorIsReadWithAFullStop;
        procedure AFactorIsNotReadThroughTheUsersDecimalSeparator;
        procedure AnUnsetVariableScalesNothing;
        procedure AFactorThatIsNotANumberScalesNothing;
        procedure ANegativeOrZeroFactorIsRefused;
        procedure SurroundingSpaceIsTolerated;

        //  A ppi as text.
        procedure APlainPpiIsRead;
        procedure ANonNumericPpiIsZero;
        procedure ANegativePpiIsZero;

        //  The X resource database.
        procedure TheDpiIsReadFromTheDatabase;
        procedure TheResourceIsMatchedOnlyAtTheStartOfALine;
        procedure TheFirstLineOfTheDatabaseCounts;
        procedure ADatabaseWithoutTheResourceAnswersNothing;
        procedure AnEmptyDatabaseAnswersNothing;
        procedure ANonNumericValueIsNotADpi;
        procedure AZeroValueIsNotADpi;
        procedure TheValueIsTakenFromItsOwnLineOnly;
        procedure TheLastLineNeedsNoNewline;

        //  What counts as a display.
        procedure AnOrdinaryDensityIsUsable;
        procedure TheBoundsThemselvesAreUsable;
        procedure ADensityNoDisplayHasIsRefused;

        //  What to do about it.
        procedure ADensityOutsideTheRangeIsRefused;
        procedure TheWidgetSetsOwnAnswerIsNotReapplied;
        procedure ADifferentUsableDensityIsApplied;

        //  How it reads afterwards.
        procedure EverySourceHasAName;
        procedure NotHavingFoundOutIsItsOwnAnswer;

        //  Whether a caption fits.
        procedure ACaptionShorterThanItsControlFits;
        procedure ACaptionAsWideAsItsControlDoesNotFit;
        procedure ThePaddingIsSubtractedFromTheControl;
    end;

implementation

{ ---- a toolkit scale factor ------------------------------------------------ }

procedure TUiScalingTest.AWholeFactorScalesTheDesignDensity;
begin
    //  A 200 % desktop. This is the whole reason the reader exists.
    AssertEquals('twice the design density', 192, PPIFromScaleFactorText('2'));
end;

procedure TUiScalingTest.AFractionalFactorIsReadWithAFullStop;
begin
    AssertEquals('one and a half', 144, PPIFromScaleFactorText('1.5'));
    AssertEquals('and a quarter', 120, PPIFromScaleFactorText('1.25'));
end;

procedure TUiScalingTest.AFactorIsNotReadThroughTheUsersDecimalSeparator;
var
    Saved: char;
begin
    //  THE FAILURE THIS PREVENTS is spectacular and locale-dependent: read
    //  through a comma locale, '1.5' loses its separator, parses as 15, and the
    //  interface comes up ten times too large. The toolkits write a full stop
    //  whatever the locale is, so this must not follow the user's.
    Saved := DefaultFormatSettings.DecimalSeparator;
    try
        DefaultFormatSettings.DecimalSeparator := ',';
        AssertEquals('still one and a half', 144,
            PPIFromScaleFactorText('1.5'));
    finally
        DefaultFormatSettings.DecimalSeparator := Saved;
    end;
end;

procedure TUiScalingTest.AnUnsetVariableScalesNothing;
begin
    //  Zero means "this source did not answer", which is what lets the caller
    //  fall through to the next one. Anything else would stop the search on a
    //  variable nobody set.
    AssertEquals('unset', 0, PPIFromScaleFactorText(''));
    AssertEquals('blank', 0, PPIFromScaleFactorText('   '));
end;

procedure TUiScalingTest.AFactorThatIsNotANumberScalesNothing;
begin
    AssertEquals('a word', 0, PPIFromScaleFactorText('large'));
    AssertEquals('a number with a suffix', 0, PPIFromScaleFactorText('2x'));
end;

procedure TUiScalingTest.ANegativeOrZeroFactorIsRefused;
begin
    //  A zero factor would produce a zero ppi, and the range check downstream
    //  would then have to be the only thing between it and a window of no size.
    AssertEquals('zero', 0, PPIFromScaleFactorText('0'));
    AssertEquals('negative', 0, PPIFromScaleFactorText('-2'));
end;

procedure TUiScalingTest.SurroundingSpaceIsTolerated;
begin
    //  Environment variables acquire spaces from shell profiles.
    AssertEquals('trimmed', 192, PPIFromScaleFactorText('  2  '));
end;

{ ---- a ppi as text --------------------------------------------------------- }

procedure TUiScalingTest.APlainPpiIsRead;
begin
    AssertEquals('read', 192, PPIFromText('192'));
    AssertEquals('trimmed', 144, PPIFromText(' 144 '));
end;

procedure TUiScalingTest.ANonNumericPpiIsZero;
begin
    AssertEquals('a word', 0, PPIFromText('high'));
    AssertEquals('empty', 0, PPIFromText(''));
    AssertEquals('fractional', 0, PPIFromText('1.5'));
end;

procedure TUiScalingTest.ANegativePpiIsZero;
begin
    //  Zero is the "did not answer" signal, so a negative number must become it
    //  rather than travelling on as a value that later fails a range check with
    //  a confusing message.
    AssertEquals('negative', 0, PPIFromText('-96'));
end;

{ ---- the X resource database ----------------------------------------------- }

procedure TUiScalingTest.TheDpiIsReadFromTheDatabase;
var
    PPI: integer;
begin
    AssertTrue('found', XftDpiFromDatabase(
        'Xft.antialias:'#9'1'#10'Xft.dpi:'#9'192'#10'Xcursor.size:'#9'48'#10,
        PPI));
    AssertEquals('the scaled density', 192, PPI);
end;

procedure TUiScalingTest.TheResourceIsMatchedOnlyAtTheStartOfALine;
var
    PPI: integer;
begin
    //  A RESOURCE NAME IS FREE-FORM, so one merely ending in Xft.dpi exists and
    //  must not answer for the display. Matching anywhere in the text would let
    //  an unrelated application's setting decide how this one is laid out.
    AssertFalse('a name that ends in the key is a different resource',
        XftDpiFromDatabase('Someapp*Xft.dpi:'#9'480'#10, PPI));
    AssertEquals('and nothing came back', 0, PPI);
end;

procedure TUiScalingTest.TheFirstLineOfTheDatabaseCounts;
var
    PPI: integer;
begin
    //  Position one is the start of a line too, and a database whose very first
    //  entry is the one wanted is the ordinary case on a minimal setup.
    AssertTrue('found', XftDpiFromDatabase('Xft.dpi:'#9'144'#10, PPI));
    AssertEquals('read', 144, PPI);
end;

procedure TUiScalingTest.ADatabaseWithoutTheResourceAnswersNothing;
var
    PPI: integer;
begin
    //  FALSE, not a guess. The caller falls through to the widget set, which is
    //  right on every display that is not scaled - and inventing a number here
    //  would break the majority case to serve the minority one.
    AssertFalse('absent', XftDpiFromDatabase(
        'Xft.antialias:'#9'1'#10'Xcursor.theme:'#9'Adwaita'#10, PPI));
end;

procedure TUiScalingTest.AnEmptyDatabaseAnswersNothing;
var
    PPI: integer;
begin
    //  A display where nothing has ever set a resource. Must not fault on the
    //  empty string, which is what the property read hands back.
    AssertFalse('nothing at all', XftDpiFromDatabase('', PPI));
end;

procedure TUiScalingTest.ANonNumericValueIsNotADpi;
var
    PPI: integer;
begin
    AssertFalse('not a number', XftDpiFromDatabase('Xft.dpi:'#9'auto'#10, PPI));
    AssertEquals('and nothing came back', 0, PPI);
end;

procedure TUiScalingTest.AZeroValueIsNotADpi;
var
    PPI: integer;
begin
    //  Zero is how some tools write "unset". Accepting it would hand a zero ppi
    //  to the range check and report it as a misconfigured display, which is a
    //  worse message than not having found the resource.
    AssertFalse('zero is not a density', XftDpiFromDatabase(
        'Xft.dpi:'#9'0'#10, PPI));
end;

procedure TUiScalingTest.TheValueIsTakenFromItsOwnLineOnly;
var
    PPI: integer;
begin
    //  The value ends at the newline. Reading past it would swallow the next
    //  resource's name and parse nothing.
    AssertTrue('found', XftDpiFromDatabase(
        'Xft.dpi:'#9'192'#10'Xft.hinting:'#9'1'#10, PPI));
    AssertEquals('and only its own line', 192, PPI);
end;

procedure TUiScalingTest.TheLastLineNeedsNoNewline;
var
    PPI: integer;
begin
    //  The property is not guaranteed to end in a newline, and the scan walks
    //  forward looking for one - so the end of the string has to end the line
    //  too, or the last resource in the database can never be read.
    AssertTrue('found', XftDpiFromDatabase(
        'Xft.antialias:'#9'1'#10'Xft.dpi:'#9'192', PPI));
    AssertEquals('read', 192, PPI);
end;

{ ---- what counts as a display ---------------------------------------------- }

procedure TUiScalingTest.AnOrdinaryDensityIsUsable;
begin
    AssertTrue('96', IsUsablePPI(96));
    AssertTrue('192', IsUsablePPI(192));
end;

procedure TUiScalingTest.TheBoundsThemselvesAreUsable;
begin
    //  Inclusive. An off-by-one here refuses a display that exists.
    AssertTrue('the floor', IsUsablePPI(MinPPI));
    AssertTrue('the ceiling', IsUsablePPI(MaxPPI));
end;

procedure TUiScalingTest.ADensityNoDisplayHasIsRefused;
begin
    //  The difference between a badly scaled window and one whose controls are
    //  larger than the screen.
    AssertFalse('nothing', IsUsablePPI(0));
    AssertFalse('negative', IsUsablePPI(-96));
    AssertFalse('just below the floor', IsUsablePPI(MinPPI - 1));
    AssertFalse('just above the ceiling', IsUsablePPI(MaxPPI + 1));
end;

{ ---- what to do about it --------------------------------------------------- }

procedure TUiScalingTest.ADensityOutsideTheRangeIsRefused;
begin
    //  REFUSED, and the widget set's own answer kept. A misconfigured variable
    //  must not be able to make the application unusable.
    AssertTrue('refused', ScalingAction(0, 96) = saRefuse);
    AssertTrue('refused high', ScalingAction(100000, 96) = saRefuse);
end;

procedure TUiScalingTest.TheWidgetSetsOwnAnswerIsNotReapplied;
begin
    //  Detected and current agree, so there is nothing to correct - but it is
    //  still worth saying, because "we asked and it really is 96" and "we never
    //  found out" must not look the same in the log.
    AssertTrue('already right', ScalingAction(96, 96) = saAlreadyRight);
end;

procedure TUiScalingTest.ADifferentUsableDensityIsApplied;
begin
    //  The case the unit exists for: a 200 % desktop behind a widget set that
    //  reports 96 with complete confidence.
    AssertTrue('applied', ScalingAction(192, 96) = saApply);
    //  And downwards too - an override can be smaller than what was reported.
    AssertTrue('applied downwards', ScalingAction(96, 192) = saApply);
end;

{ ---- how it reads afterwards ----------------------------------------------- }

procedure TUiScalingTest.EverySourceHasAName;
var
    S: TPPISource;
begin
    //  WALKS THE ENUM, so a source added without a name fails here rather than
    //  producing a log line that says the ppi came from nowhere. The log is the
    //  only way anyone finds out why a window is the size it is.
    for S := Low(TPPISource) to High(TPPISource) do
        AssertTrue('source ' + IntToStr(Ord(S)) + ' has a name',
            PPISourceName(S) <> '');
end;

procedure TUiScalingTest.NotHavingFoundOutIsItsOwnAnswer;
begin
    //  Distinct from every real source, and not blank.
    AssertTrue('named', PPISourceName(psNone) <> '');
    AssertTrue('and not confusable with the widget set',
        PPISourceName(psNone) <> PPISourceName(psWidgetSet));
end;

{ ---- whether a caption fits ------------------------------------------------ }

procedure TUiScalingTest.ACaptionShorterThanItsControlFits;
begin
    AssertTrue('room to spare', CaptionFits(50, 100, 4));
end;

procedure TUiScalingTest.ACaptionAsWideAsItsControlDoesNotFit;
begin
    //  A button draws inside a border, so equal widths are already clipped.
    AssertFalse('exactly as wide', CaptionFits(100, 100, 4));
    AssertFalse('one pixel over the padding', CaptionFits(97, 100, 4));
end;

procedure TUiScalingTest.ThePaddingIsSubtractedFromTheControl;
begin
    //  The padding is scaled by the caller, so it is a parameter and not a
    //  literal - and the boundary is where a check like this is wrong or right.
    AssertTrue('right on the boundary', CaptionFits(96, 100, 4));
    AssertFalse('and one past it', CaptionFits(97, 100, 4));
    AssertTrue('with no padding the boundary moves',
        CaptionFits(100, 100, 0));
end;

initialization
    //  A unit test: no screen, no widget set, no environment. Every one of these
    //  decisions is text and arithmetic, which is why it could be moved out.
    RegisterTest('unit', TUiScalingTest);
end.
