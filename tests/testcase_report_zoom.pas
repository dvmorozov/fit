// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(How large the report and the Explain pane are drawn, and what that
does to the page handed to the HTML component.)

WHY THE ARITHMETIC IS WORTH TESTING. A zoom is two buttons and a number, and
every way it goes wrong is silent: a step that lands off the ladder leaves the
next click doing nothing, a value read from an older config.xml that nobody
repairs draws the report at a size nobody chose, and a page whose headings keep
their default size while the text around them grows looks broken rather than
zoomed.

WHY THE HTML IS TOUCHED AT ALL. TIpHtmlPanel.DefaultFontSize scales body text
and nothing else: TIpHtmlNodeHeader.SetProps takes a heading's size from a fixed
array (iphtml.pas, FONTSIZESVALUESARRAY), so <h2> and <h3> ignore it. Asked to
draw a report at 200 %, the component grew the findings to 24 px and left the
section titles above them at 18 - headings smaller than their own text. A
<style> block giving h2 a font-size of its own changed nothing; the same size in the
tag's own style attribute did. Hence the shape asserted below.

The size the panel is told and the page it is given have to agree, and deciding
what that page says is arithmetic - which is why it is here and not in the
view.
}
unit testcase_report_zoom;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, report_zoom;

type
    TReportZoomTest = class(TTestCase)
    published
        //  Which values are a zoom at all.
        procedure TheDefaultIsOnTheLadder;
        procedure AValueNoOneChoseIsTheDefault;
        procedure AValueOffTheLadderIsTheDefault;
        procedure ANegativeValueIsTheDefault;
        procedure AValueOnTheLadderIsKept;

        //  Stepping.
        procedure ZoomingInGoesUpOneStep;
        procedure ZoomingOutGoesDownOneStep;
        procedure ZoomingInStopsAtTheTop;
        procedure ZoomingOutStopsAtTheBottom;
        procedure SteppingRepairsAValueOffTheLadder;
        procedure OutAndBackInReturnsTheSameSize;

        //  What greys the buttons.
        procedure TheTopOffersNoFurtherZoomIn;
        procedure TheBottomOffersNoFurtherZoomOut;
        procedure TheMiddleOffersBoth;
        procedure AValueOffTheLadderOffersBoth;

        //  The size the panel is told.
        procedure TheDefaultIsTheComponentsOwnSize;
        procedure ALargerZoomIsALargerFont;
        procedure ASmallerZoomIsASmallerFont;
        procedure EveryStepIsAVisibleChange;

        //  The page the component is given.
        procedure AtTheDefaultThePageIsUntouched;
        procedure EveryHeadingCarriesTheSizeToDrawItAt;
        procedure AZoomedPageKeepsWhatTheReportSaid;
        procedure AZoomedPageKeepsTheReportsLinks;
        procedure HeadingsStayLargerThanTheTextAtEveryStep;
        procedure HeadingsGrowWithTheText;
        procedure AHeadingWrittenInCapitalsIsSizedToo;
        procedure AHeadingWithAttributesOfItsOwnIsLeftAlone;
        procedure APageWithNoHeadingsIsLeftAsItIs;
        procedure AnEmptyPageIsLeftEmpty;
    end;

implementation

uses
    StrUtils;

{ The size written into a heading tag, in px. -1 when that tag carries none -
  so a heading that was left alone fails as one, and not as a parse of the empty
  string. }
function StyledSize(const AHtml, ATag: string): longint;
var
    At, Stop: integer;
    Text: string;
begin
    Result := -1;
    At := Pos('<' + ATag + ' ', AHtml);
    if At = 0 then
        Exit;
    At := PosEx('font-size:', AHtml, At);
    if At = 0 then
        Exit;
    Inc(At, Length('font-size:'));
    Stop := PosEx('px', AHtml, At);
    if Stop = 0 then
        Exit;
    Text := Trim(Copy(AHtml, At, Stop - At));
    Result := StrToIntDef(Text, -1);
end;

const
    //  A page shaped like the ones report_html writes.
    SampleHtml =
        '<html><body><h2>Impulse (5)</h2>' + LineEnding +
        '<p><b>Verdict: Pass</b> - 1 rule broken</p>' + LineEnding +
        '<h3>Wave 3</h3>' + LineEnding +
        '<ul><li><b>Pass</b> Wave 3 is never the shortest ' +
        '<a href="topic:demo/impulse">why</a></li></ul>' + LineEnding +
        '</body></html>';

{ ---- which values are a zoom ----------------------------------------------- }

procedure TReportZoomTest.TheDefaultIsOnTheLadder;
begin
    //  If the default were not a step, the first click would move to a size
    //  the user cannot get back to.
    AssertEquals('the default survives repair',
        ReportZoomDefault, UsableZoom(ReportZoomDefault));
end;

procedure TReportZoomTest.AValueNoOneChoseIsTheDefault;
begin
    //  What an older config.xml holds: the property existed nowhere, so it
    //  reads as zero.
    AssertEquals('zero is not a size', ReportZoomDefault, UsableZoom(0));
end;

procedure TReportZoomTest.AValueOffTheLadderIsTheDefault;
begin
    AssertEquals('between two steps', ReportZoomDefault, UsableZoom(103));
    AssertEquals('past the top', ReportZoomDefault, UsableZoom(10000));
end;

procedure TReportZoomTest.ANegativeValueIsTheDefault;
begin
    AssertEquals('below nothing', ReportZoomDefault, UsableZoom(-120));
end;

procedure TReportZoomTest.AValueOnTheLadderIsKept;
var
    i: integer;
begin
    for i := Low(ReportZoomSteps) to High(ReportZoomSteps) do
        AssertEquals('a step is its own repair',
            ReportZoomSteps[i], UsableZoom(ReportZoomSteps[i]));
end;

{ ---- stepping -------------------------------------------------------------- }

procedure TReportZoomTest.ZoomingInGoesUpOneStep;
begin
    AssertEquals('one step up from the default',
        ReportZoomSteps[3], ZoomedIn(ReportZoomSteps[2]));
end;

procedure TReportZoomTest.ZoomingOutGoesDownOneStep;
begin
    AssertEquals('one step down from the default',
        ReportZoomSteps[1], ZoomedOut(ReportZoomSteps[2]));
end;

procedure TReportZoomTest.ZoomingInStopsAtTheTop;
begin
    //  Not wrapping round to the smallest: a button clicked once more must not
    //  shrink the page it has been enlarging.
    AssertEquals('the top is the top', ReportZoomSteps[High(ReportZoomSteps)],
        ZoomedIn(ReportZoomSteps[High(ReportZoomSteps)]));
end;

procedure TReportZoomTest.ZoomingOutStopsAtTheBottom;
begin
    AssertEquals('the bottom is the bottom', ReportZoomSteps[Low(ReportZoomSteps)],
        ZoomedOut(ReportZoomSteps[Low(ReportZoomSteps)]));
end;

procedure TReportZoomTest.SteppingRepairsAValueOffTheLadder;
begin
    //  A hand-edited config must not make the buttons dead.
    AssertEquals('in from nonsense', ZoomedIn(ReportZoomDefault), ZoomedIn(103));
    AssertEquals('out from nonsense', ZoomedOut(ReportZoomDefault), ZoomedOut(103));
end;

procedure TReportZoomTest.OutAndBackInReturnsTheSameSize;
var
    i: integer;
begin
    //  Away from the ends, the two buttons undo each other - which is what a
    //  user expects of them and what an unevenly walked ladder would break.
    for i := Low(ReportZoomSteps) + 1 to High(ReportZoomSteps) - 1 do
    begin
        AssertEquals('out then in', ReportZoomSteps[i],
            ZoomedIn(ZoomedOut(ReportZoomSteps[i])));
        AssertEquals('in then out', ReportZoomSteps[i],
            ZoomedOut(ZoomedIn(ReportZoomSteps[i])));
    end;
end;

{ ---- what greys the buttons ------------------------------------------------ }

procedure TReportZoomTest.TheTopOffersNoFurtherZoomIn;
begin
    AssertFalse('nothing above the top',
        CanZoomIn(ReportZoomSteps[High(ReportZoomSteps)]));
    AssertTrue('but it can still come back',
        CanZoomOut(ReportZoomSteps[High(ReportZoomSteps)]));
end;

procedure TReportZoomTest.TheBottomOffersNoFurtherZoomOut;
begin
    AssertFalse('nothing below the bottom',
        CanZoomOut(ReportZoomSteps[Low(ReportZoomSteps)]));
    AssertTrue('but it can still grow',
        CanZoomIn(ReportZoomSteps[Low(ReportZoomSteps)]));
end;

procedure TReportZoomTest.TheMiddleOffersBoth;
begin
    AssertTrue('in from the default', CanZoomIn(ReportZoomDefault));
    AssertTrue('out from the default', CanZoomOut(ReportZoomDefault));
end;

procedure TReportZoomTest.AValueOffTheLadderOffersBoth;
begin
    //  It is repaired to the default, which is not an end.
    AssertTrue('in', CanZoomIn(0));
    AssertTrue('out', CanZoomOut(0));
end;

{ ---- the size the panel is told -------------------------------------------- }

procedure TReportZoomTest.TheDefaultIsTheComponentsOwnSize;
begin
    //  So a build nobody has zoomed draws exactly what it drew before.
    AssertEquals('the component''s own default', ReportBaseFontSize,
        FontSizeAtZoom(ReportZoomDefault));
end;

procedure TReportZoomTest.ALargerZoomIsALargerFont;
begin
    AssertTrue('larger', FontSizeAtZoom(ReportZoomSteps[High(ReportZoomSteps)]) >
        ReportBaseFontSize);
end;

procedure TReportZoomTest.ASmallerZoomIsASmallerFont;
begin
    AssertTrue('smaller', FontSizeAtZoom(ReportZoomSteps[Low(ReportZoomSteps)]) <
        ReportBaseFontSize);
end;

procedure TReportZoomTest.EveryStepIsAVisibleChange;
var
    i: integer;
begin
    //  A step that rounds to the size below it is a click that does nothing.
    for i := Low(ReportZoomSteps) + 1 to High(ReportZoomSteps) do
        AssertTrue('step ' + IntToStr(i) + ' is bigger than the one below',
            FontSizeAtZoom(ReportZoomSteps[i]) >
            FontSizeAtZoom(ReportZoomSteps[i - 1]));
end;

{ ---- the page the component is given --------------------------------------- }

procedure TReportZoomTest.AtTheDefaultThePageIsUntouched;
begin
    //  At the size the component already draws, every heading already has the
    //  size this unit would give it, so there is nothing to say.
    AssertEquals('byte for byte', SampleHtml,
        HtmlAtZoom(SampleHtml, ReportZoomDefault));
end;

procedure TReportZoomTest.EveryHeadingCarriesTheSizeToDrawItAt;
var
    Page: string;
begin
    //  ON THE TAG, because that is the only form the component obeys - a
    //  heading rule in a style sheet was measured to do nothing at all.
    Page := HtmlAtZoom(SampleHtml, ReportZoomSteps[High(ReportZoomSteps)]);
    AssertTrue('no style sheet is emitted', Pos('<style', Page) = 0);
    AssertTrue('the section title is sized',
        Pos('<h2 style="font-size:', Page) > 0);
    AssertTrue('and so is the sub-heading',
        Pos('<h3 style="font-size:', Page) > 0);
    AssertTrue('no bare heading is left behind', Pos('<h2>', Page) = 0);
end;

procedure TReportZoomTest.AZoomedPageKeepsWhatTheReportSaid;
var
    Page: string;
begin
    //  The zoom is a size, not an edit. report_html decides the words.
    Page := HtmlAtZoom(SampleHtml, ReportZoomSteps[0]);
    AssertTrue('the heading', Pos('Impulse (5)', Page) > 0);
    AssertTrue('the verdict', Pos('Verdict: Pass', Page) > 0);
    AssertTrue('the finding', Pos('Wave 3 is never the shortest', Page) > 0);
end;

procedure TReportZoomTest.AZoomedPageKeepsTheReportsLinks;
var
    Page: string;
begin
    //  A zoomed report whose "why" no longer opens anything would be worse
    //  than one that is hard to read.
    Page := HtmlAtZoom(SampleHtml, ReportZoomSteps[0]);
    AssertTrue('the topic link survives',
        Pos('href="topic:demo/impulse"', Page) > 0);
end;

procedure TReportZoomTest.HeadingsStayLargerThanTheTextAtEveryStep;
var
    i: integer;
    Page: string;
begin
    //  The whole reason the page is touched: left to itself the component
    //  draws headings from a fixed table, so at 200 % they are smaller than
    //  the paragraphs under them.
    for i := Low(ReportZoomSteps) to High(ReportZoomSteps) do
    begin
        if ReportZoomSteps[i] = ReportZoomDefault then
            Continue;
        Page := HtmlAtZoom(SampleHtml, ReportZoomSteps[i]);
        AssertTrue('h2 over the body at ' + IntToStr(ReportZoomSteps[i]),
            StyledSize(Page, 'h2') > FontSizeAtZoom(ReportZoomSteps[i]));
        AssertTrue('h3 over the body at ' + IntToStr(ReportZoomSteps[i]),
            StyledSize(Page, 'h3') > FontSizeAtZoom(ReportZoomSteps[i]));
        AssertTrue('h2 over h3 at ' + IntToStr(ReportZoomSteps[i]),
            StyledSize(Page, 'h2') > StyledSize(Page, 'h3'));
    end;
end;

procedure TReportZoomTest.HeadingsGrowWithTheText;
var
    Small, Large: string;
begin
    Small := HtmlAtZoom(SampleHtml, ReportZoomSteps[Low(ReportZoomSteps)]);
    Large := HtmlAtZoom(SampleHtml, ReportZoomSteps[High(ReportZoomSteps)]);
    AssertTrue('h2 grew', StyledSize(Large, 'h2') > StyledSize(Small, 'h2'));
    AssertTrue('h3 grew', StyledSize(Large, 'h3') > StyledSize(Small, 'h3'));
    AssertTrue('and so did the text under them',
        FontSizeAtZoom(ReportZoomSteps[High(ReportZoomSteps)]) >
        FontSizeAtZoom(ReportZoomSteps[Low(ReportZoomSteps)]));
end;

procedure TReportZoomTest.AHeadingWrittenInCapitalsIsSizedToo;
var
    Page: string;
begin
    //  Nothing makes a module write lower case, and a section title left at
    //  18 px in the middle of a page drawn at 24 is the defect this exists to
    //  prevent.
    Page := HtmlAtZoom('<HTML><BODY><H2>Shouted</H2></BODY></HTML>',
        ReportZoomSteps[High(ReportZoomSteps)]);
    AssertTrue('sized whatever case it was written in',
        Pos('style="font-size:', Page) > 0);
    AssertTrue('and still says it', Pos('Shouted', Page) > 0);
end;

procedure TReportZoomTest.AHeadingWithAttributesOfItsOwnIsLeftAlone;
var
    Source, Page: string;
begin
    //  A heading a module wrote with attributes is a page this unit does not
    //  know the shape of. Forcing a size onto it could contradict what it
    //  already says, and guessing is worse than leaving it.
    Source := '<html><body><h2 align="center">Centred</h2></body></html>';
    Page := HtmlAtZoom(Source, ReportZoomSteps[High(ReportZoomSteps)]);
    AssertEquals('exactly as it was written', Source, Page);
end;

procedure TReportZoomTest.APageWithNoHeadingsIsLeftAsItIs;
var
    Source, Page: string;
begin
    //  Body text is the component's own business - it is told that size
    //  directly - so a page with nothing to head has nothing to rewrite.
    Source := '<html><body><p>bare</p></body></html>';
    Page := HtmlAtZoom(Source, ReportZoomSteps[High(ReportZoomSteps)]);
    AssertEquals('untouched', Source, Page);
end;

procedure TReportZoomTest.AnEmptyPageIsLeftEmpty;
begin
    //  There is nothing to head, and nothing to hand over.
    AssertEquals('nothing stays nothing', '',
        HtmlAtZoom('', ReportZoomSteps[High(ReportZoomSteps)]));
end;

initialization
    //  A unit test: a number, a ladder and a string. No component, no screen.
    RegisterTest('unit', TReportZoomTest);
end.
