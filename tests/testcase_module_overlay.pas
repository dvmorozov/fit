// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(The module overlay hook, and the view vocabulary modules draw with.)

int_module_overlay states two contracts in its own comments and nothing enforced
either: registration is idempotent, and DrawModuleOverlays swallows exceptions.
Both matter on every single redraw - a hook registered twice draws its series
twice, and a hook that raises would make the chart unusable whenever a module's
server was momentarily unreachable - so they are exactly the kind of rule that
should fail a build rather than be re-derived by the next reader.

THE REGISTRY IS PROCESS-GLOBAL AND APPEND-ONLY: there is no unregister, by
design, because a module registers once at start-up. So every assertion here is
about a RELATIVE change across the call rather than an absolute count, and the
tests are order-independent as a result.
}
unit testcase_module_overlay;

{$MODE Delphi}

interface

uses
    Classes, SysUtils, fpcunit, testregistry,
    int_module_overlay, module_view_types;

type
    TModuleOverlayTest = class(TTestCase)
    published
        procedure ARegisteredHookIsCalled;
        procedure RegisteringTwiceRegistersOnce;
        procedure ANilHookIsIgnoredRatherThanStored;
        procedure AHookThatRaisesDoesNotStopTheOthers;
        procedure TheClientIsPassedThrough;
        procedure EveryHookRunsOnEveryDraw;
    end;

    TModuleViewTypesTest = class(TTestCase)
    published
        procedure TheNamedColoursAreTheWidgetSetsOwnValues;
        procedure AnOutlineRowCarriesItsOwnIndent;
        procedure TheDefaultSeriesStyleIsUsable;
    end;

implementation

{ ---- what the hooks did, recorded outside them ----------------------------- }

var
    { Module-level rather than fields: a TModuleOverlayProc is a plain procedure
      pointer, so a hook cannot be a method and has nowhere else to record. }
    CallsA, CallsB, CallsRaiser: longint;
    LastClientA: TObject;

procedure HookA(AClient: TObject);
begin
    Inc(CallsA);
    LastClientA := AClient;
end;

{$hints off}
//  AClient is deliberately ignored by these two: what they exist to record is
//  THAT they ran and in what order, and only HookA needs to prove the client is
//  passed through.
procedure HookB(AClient: TObject);
begin
    Inc(CallsB);
end;

procedure HookThatRaises(AClient: TObject);
begin
    Inc(CallsRaiser);
    raise Exception.Create('a module whose server just went away');
end;
{$hints on}

{ ---- the hook registry ----------------------------------------------------- }

procedure TModuleOverlayTest.ARegisteredHookIsCalled;
var
    Before: longint;
begin
    RegisterModuleOverlay(@HookA);
    Before := CallsA;
    DrawModuleOverlays(nil);
    AssertEquals('the hook ran once', Before + 1, CallsA);
end;

procedure TModuleOverlayTest.RegisteringTwiceRegistersOnce;
var
    CountBefore, CallsBefore: longint;
begin
    //  THE FIRST DOCUMENTED CONTRACT. A hook stored twice draws its series twice
    //  per redraw - two identical overlays on the chart, and a module author with
    //  no reason to suspect the framework.
    RegisterModuleOverlay(@HookB);
    CountBefore := ModuleOverlayCount;
    RegisterModuleOverlay(@HookB);
    AssertEquals('the second registration added nothing',
        CountBefore, ModuleOverlayCount);

    CallsBefore := CallsB;
    DrawModuleOverlays(nil);
    AssertEquals('and it is still called exactly once per draw',
        CallsBefore + 1, CallsB);
end;

procedure TModuleOverlayTest.ANilHookIsIgnoredRatherThanStored;
var
    Before: longint;
begin
    //  A nil stored would be called, and calling it would take the chart down on
    //  the next redraw - far from where the nil came from.
    Before := ModuleOverlayCount;
    RegisterModuleOverlay(nil);
    AssertEquals('nothing was stored', Before, ModuleOverlayCount);
end;

procedure TModuleOverlayTest.AHookThatRaisesDoesNotStopTheOthers;
var
    CallsABefore, RaiserBefore: longint;
begin
    //  THE SECOND DOCUMENTED CONTRACT, and the one place swallowing an exception
    //  is right: this runs on every redraw, so one module's momentary failure
    //  must not make the chart unusable. What went wrong is that module's to
    //  report when the user next asks it to act.
    RegisterModuleOverlay(@HookThatRaises);
    RegisterModuleOverlay(@HookA);
    CallsABefore := CallsA;
    RaiserBefore := CallsRaiser;

    //  No try..except here on purpose: if DrawModuleOverlays ever stops
    //  swallowing, this test fails by erroring rather than by asserting - which
    //  is still the right verdict.
    DrawModuleOverlays(nil);

    AssertEquals('the raising hook did run', RaiserBefore + 1, CallsRaiser);
    AssertEquals('and the one after it ran too', CallsABefore + 1, CallsA);
end;

procedure TModuleOverlayTest.TheClientIsPassedThrough;
var
    Client: TObject;
begin
    //  The hook receives the TFitClient as TObject, so this unit names nothing a
    //  module defines. What matters is that it is the SAME object.
    Client := TObject.Create;
    try
        RegisterModuleOverlay(@HookA);
        LastClientA := nil;
        DrawModuleOverlays(Client);
        AssertTrue('the hook got the client it was given',
            LastClientA = Client);
    finally
        Client.Free;
    end;
end;

procedure TModuleOverlayTest.EveryHookRunsOnEveryDraw;
var
    A, B: longint;
begin
    RegisterModuleOverlay(@HookA);
    RegisterModuleOverlay(@HookB);
    A := CallsA;
    B := CallsB;
    DrawModuleOverlays(nil);
    AssertEquals('first hook', A + 1, CallsA);
    AssertEquals('second hook', B + 1, CallsB);
end;

{ ---- the view vocabulary --------------------------------------------------- }

procedure TModuleViewTypesTest.TheNamedColoursAreTheWidgetSetsOwnValues;
begin
    //  These are spelled out rather than imported from Graphics, so that this
    //  unit - and the light suite that compiles it - needs no widget set. The
    //  values must therefore be checked, because nothing else can: a typo here
    //  would show a module's markup in the wrong colour and nothing would fail.
    //  BGR, not RGB, which is exactly the mistake worth catching.
    AssertEquals('navy is full blue in the high byte', $800000, mcNavy);
    AssertEquals('red is in the LOW byte', $0000FF, mcRed);
    AssertEquals('green', $008000, mcGreen);
    AssertEquals('blue is in the HIGH byte', $FF0000, mcBlue);
    AssertEquals('black', $000000, mcBlack);
end;

procedure TModuleViewTypesTest.AnOutlineRowCarriesItsOwnIndent;
var
    Row: TOutlineRow;
    Outline: TOutline;
begin
    //  The outline is a flattened depth-first list, each row carrying the indent
    //  it should be drawn at, so the view holds no tree of its own and the ORDER
    //  is an explicit claim rather than an implicit one.
    //  Initialised explicitly: TOutline is a managed dynamic array, and a
    //  SetLength onto an uninitialised local is what the compiler warns about.
    Outline := nil;
    Row := Default(TOutlineRow);
    AssertEquals('a fresh row is a root', 0, Row.Indent);
    AssertEquals('with no caption', '', Row.Caption);
    AssertFalse('and is not detached', Row.IsDetached);

    SetLength(Outline, 2);
    Outline[0].Caption := 'parent';
    Outline[0].Id := 'p';
    Outline[1].Indent := 1;
    Outline[1].Caption := 'child';
    Outline[1].Id := 'c';
    Outline[1].IsDetached := True;
    AssertEquals('the child is indented under the parent', 1, Outline[1].Indent);
    AssertTrue('and a detached row says so, rather than looking like a root',
        Outline[1].IsDetached);
end;

procedure TModuleViewTypesTest.TheDefaultSeriesStyleIsUsable;
var
    Style: TModuleSeriesStyle;
begin
    //  A module that draws without choosing a style gets this one, so it has to
    //  be visible: a zero width or a marker of no size draws nothing at all, and
    //  the module author sees an empty chart with no error.
    Style := DefaultModuleSeriesStyle;
    AssertTrue('the marker has size, or it draws nothing', Style.Size > 0);
    AssertTrue('a shape is chosen', Style.Shape = msCircle);
    AssertTrue('points are drawn', Style.ShowPoints);
    AssertTrue('and joined, which is what makes a sequence readable',
        Style.ShowLines);
    //  UNSORTED, and this is the one that matters. A module series is a sequence
    //  far more often than a scatter - a path, an ordered set of pivots - and
    //  sorting one by x silently reorders what it says. Defaulting the other way
    //  would corrupt the common case and look like a rendering quirk.
    AssertFalse('and not sorted behind the caller''s back', Style.Sorted);
end;

initialization
    //  Unit tests: procedure pointers, records and constants.
    RegisterTest('unit', TModuleOverlayTest);
    RegisterTest('unit', TModuleViewTypesTest);
end.
