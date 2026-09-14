// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(The maintained half of the model: which curve instance each pick
stands for.)

WHY THIS EXISTS. The server keeps INPUTS - the profile, the fit intervals, the
picked curve positions - and rebuilds every curve instance from them on every
model edit (TFitService.GoToReadyForFit frees the task list; TFitTask.
RecreateCurves builds fresh instances). The instance that comes back is a
different object, so anything the model accumulated and the inputs do not hold -
the fitted parameter values - has to be re-attached to it.

It used to be re-attached by CONTENT: a hash of the instance's initial parameter
values, recomputed after the rebuild and matched against the values stored under
it. That worked only while the seed never moved, and it failed quietly when it
did not hold.

This registry replaces that with the ordinary answer. Identity is ISSUED to the
model INPUT - the pick - and inherited by whatever instance is built from it.
The pick survives the rebuild, so the identity does; and because the identity is
attached to the pick rather than derived from it, MOVING a pick keeps it.

WHAT IT DELIBERATELY DOES NOT HOLD. No parameter values, no curve objects, no
point sets. Those live where they already lived. This is bookkeeping only, which
is what lets it be tested without a profile, a fit or a server - and what keeps
it free of the LCL, so it is measured in the unit suite where coverage is taken.

@author(Dmitry Morozov dvmorozov@hotmail.com)
}
unit curve_identity_registry;

{$mode objfpc}{$H+}

interface

uses
    SysUtils, curve_instance_id;

type
    { One instance the model contains. }
    TCurveIdentity = record
        { The handle. Issued once, never recomputed. }
        Id:     TCurveInstanceId;
        { The pick this instance is built from - the x of a real profile sample.
          Meaningless when Positionless is set. }
        Seed:   double;
        { True for an instance that has no position parameter at all, so no pick
          places it: the user-defined formula curve, of which there is exactly
          one per fit interval. Keyed by Slot instead. }
        Positionless: boolean;
        { Which positionless instance this is - the index of the fit interval it
          belongs to. Unused when Positionless is false. }
        Slot:   longint;
        { True once an optimiser has actually produced values for this instance.
          NOT the same as "it has parameter values": every instance has those
          from the moment it is seeded. Only this distinguishes a fitted curve
          from one that has merely been placed. }
        Fitted: boolean;
        { Set when this instance's pick has MOVED since the values stored for it
          were found, so its position must come from the new pick rather than
          from those values. See TakeSeedFrom. }
        ReseedOnRestore: boolean;
        { True for an instance an analysis pack placed from its own markup. It
          HAS a seed - the position it was built at - but no pick put it there,
          so the pick set cannot make it stale. See KeepOnlySeeds. }
        PlacedByModule: boolean;
    end;

    { What has to go with an instance for its deletion to survive a rebuild.
      See RemovalOf. }
    TCurveRemoval = (
        { The pick that placed it, and the handle. }
        crPickAndIdentity,
        { The handle alone: nothing placed it at a position. }
        crIdentityOnly,
        { The markup that placed it, which is a module's to remove. }
        crMarkupThatPlacedIt);

    { The instances the model contains, and their handles.

      Ordering is not part of the contract: this is a set addressed by handle or
      by seed. What the client is shown is ordered by the report the service
      builds each round (interval order, then creation order), which is derived
      and cannot be maintained here. }
    TCurveIdentityRegistry = class(TObject)
    private
        FItems: array of TCurveIdentity;

        function IndexOfId(const AId: TCurveInstanceId): longint;
        function IndexOfSeed(ASeed: double): longint;
        function IndexOfSlot(ASlot: longint): longint;
        procedure RemoveAt(AIndex: longint);
        function Append(const AId: TCurveInstanceId; ASeed: double;
            APositionless: boolean; ASlot: longint): TCurveInstanceId;

    public
        destructor Destroy; override;

        { Forgets everything. A new profile is a new problem. }
        procedure Clear;
        function Count: longint;
        { The entry at APosition, for callers that enumerate. }
        function Item(APosition: longint): TCurveIdentity;

        { Issues a handle for a pick, and returns it. Re-picking the same x
          returns the handle already issued for it rather than a second one:
          a pick set holds unique x values, so two entries for one x would mean
          two instances where the model has one. }
        function IssueForSeed(ASeed: double): TCurveInstanceId;
        { Issues a handle for the positionless instance of fit interval ASlot. }
        function IssueForSlot(ASlot: longint): TCurveInstanceId;
        { Adopts a handle the caller already owns - a module that identifies its
          own curves.

          The same handle at a DIFFERENT seed is that instance having MOVED, not
          a clash: a module's markup can be dragged, and the identity belongs to
          the markup rather than to where it happens to sit. So the seed is
          updated and the handle kept, exactly as TakeSeedFrom does for a pick.

          Two LIVE instances sharing one handle is a clash, and it is caught
          where it can actually be seen - TFitTask.AddBuiltCurve, which knows
          which curves this build pass has produced. This registry spans
          rebuilds and cannot tell one pass from the next. }
        procedure Adopt(const AId: TCurveInstanceId; ASeed: double;
            APlacedByModule: boolean = False);

        { Drops every instance a pick used to place whose pick is no longer in
          ASeeds - and keeps everything no pick placed.

          HERE RATHER THAN IN THE SERVICE, where the loop used to be, because
          what it keeps is a rule about identity and the service could not be
          asked about it without an engine, a module and a rebuild. It kept
          positionless instances and dropped everything else, so an instance a
          MODULE placed - which has a seed, and no pick - was dropped on every
          rebuild. The handle was re-adopted immediately afterwards, so nothing
          looked wrong; what went each time was everything the registry knew
          about it, including whether an optimiser had produced its values. A
          project saved from such a model recorded fitted=false for every curve
          in it. }
        procedure KeepOnlySeeds(const ASeeds: array of double);

        { The handle for a pick, or NoCurveInstanceId if that x has none. }
        function IdForSeed(ASeed: double): TCurveInstanceId;
        { The handle for a fit interval's positionless instance. }
        function IdForSlot(ASlot: longint): TCurveInstanceId;
        function Has(const AId: TCurveInstanceId): boolean;

        { Moves a pick, KEEPING its handle - the whole point of issuing one.
          The instance keeps everything the last fit found about its shape, and
          is re-seeded at the new position (see ReseedOnRestore, which
          TFitTask.RestoreCurveValues reads).

          False when no instance is seeded at AFrom, which is an ordinary
          outcome: a pick may be moved before anything was ever built from it. }
        function TakeSeedFrom(AFrom, ATo: double): boolean;
        { Drops the instance seeded at ASeed. False when there is none. }
        function RemoveSeed(ASeed: double): boolean;
        { Drops an instance by handle - what a module's builder does when the
          markup that placed a curve is deleted. }
        function RemoveId(const AId: TCurveInstanceId): boolean;

        { Records that an optimiser produced values for these instances, and for
          no others. Called where a fit COMPLETES, never where a model is merely
          rebuilt. }
        procedure MarkFitted(const AIds: array of TCurveInstanceId);
        function IsFitted(const AId: TCurveInstanceId): boolean;
        { Whether any instance at all carries optimiser results. }
        function AnyFitted: boolean;
        { Whether the instance seeded at ASeed does. }
        function FittedAtSeed(ASeed: double): boolean;

        { Whether the instance's position must be taken from its pick rather
          than from the values stored for it, because that pick has moved since
          those values were found. Cleared by ClearReseed once honoured. }
        function NeedsReseed(const AId: TCurveInstanceId): boolean;
        procedure ClearReseed(const AId: TCurveInstanceId);
    end;

{ What deleting ONE instance has to take with it, which depends entirely on what
  put it there.

  A DELETION THAT DOES NOT REACH WHAT PLACED THE CURVE UNDOES ITSELF. Every model
  edit rebuilds the instances from what placed them - the picks, or a module's
  markup - so removing the curve and its handle and nothing else deletes it for
  exactly as long as it takes the next rebuild to run. That is the shape this
  answers, once, where it can be asked without an engine, a module and a rebuild:

    * a pick placed it, so the pick goes too;
    * nothing placed it - the positionless formula curve, one per fit interval -
      so only the handle goes and the rebuild decides what the model holds;
    * a module placed it from its own markup, and only that module can say which
      mark, so the framework has to ask rather than act. }
function RemovalOf(const AEntry: TCurveIdentity): TCurveRemoval;

implementation

//  Two picks are the same pick when they name the same profile sample. The x
//  values are copied between point sets rather than recomputed, so an exact
//  comparison would very nearly do; a tolerance is used because "very nearly"
//  is not a property anything enforces, and the cost of being wrong here is a
//  silently orphaned fit.
const
    SEED_EPSILON = 1e-9;

function SameSeed(A, B: double): boolean;
begin
    Result := Abs(A - B) <= SEED_EPSILON;
end;

destructor TCurveIdentityRegistry.Destroy;
begin
    SetLength(FItems, 0);
    inherited;
end;

function RemovalOf(const AEntry: TCurveIdentity): TCurveRemoval;
begin
    //  THE MODULE FIRST. Such an instance has a seed - the position it was built
    //  at - and no pick, so reading it as pick-placed would delete a pick that
    //  belongs to some other curve or to nothing at all, and would leave the
    //  markup to put the pattern straight back.
    if AEntry.PlacedByModule then
        Result := crMarkupThatPlacedIt
    else if AEntry.Positionless then
        Result := crIdentityOnly
    else
        Result := crPickAndIdentity;
end;

procedure TCurveIdentityRegistry.Clear;
begin
    SetLength(FItems, 0);
end;

function TCurveIdentityRegistry.Count: longint;
begin
    Result := Length(FItems);
end;

function TCurveIdentityRegistry.Item(APosition: longint): TCurveIdentity;
begin
    if (APosition < 0) or (APosition > High(FItems)) then
        raise Exception.CreateFmt(
            'There is no curve identity at position %d.', [APosition]);
    Result := FItems[APosition];
end;

function TCurveIdentityRegistry.IndexOfId(
    const AId: TCurveInstanceId): longint;
var
    i: longint;
begin
    Result := -1;
    //  A handle that was never issued matches nothing. Without this an
    //  all-zero id would match an all-zero id and every unissued instance
    //  would look like the same one.
    if not IsCurveInstanceId(AId) then
        Exit;
    for i := 0 to High(FItems) do
        if SameCurveInstanceId(FItems[i].Id, AId) then
        begin
            Result := i;
            Exit;
        end;
end;

function TCurveIdentityRegistry.IndexOfSeed(ASeed: double): longint;
var
    i: longint;
begin
    Result := -1;
    for i := 0 to High(FItems) do
        if (not FItems[i].Positionless) and SameSeed(FItems[i].Seed, ASeed) then
        begin
            Result := i;
            Exit;
        end;
end;

function TCurveIdentityRegistry.IndexOfSlot(ASlot: longint): longint;
var
    i: longint;
begin
    Result := -1;
    for i := 0 to High(FItems) do
        if FItems[i].Positionless and (FItems[i].Slot = ASlot) then
        begin
            Result := i;
            Exit;
        end;
end;

procedure TCurveIdentityRegistry.RemoveAt(AIndex: longint);
var
    i: longint;
begin
    for i := AIndex to High(FItems) - 1 do
        FItems[i] := FItems[i + 1];
    SetLength(FItems, Length(FItems) - 1);
end;

function TCurveIdentityRegistry.Append(const AId: TCurveInstanceId;
    ASeed: double; APositionless: boolean; ASlot: longint): TCurveInstanceId;
var
    Entry: TCurveIdentity;
begin
    //  DEFAULT FIRST, and it is not belt and braces. A local record is not
    //  zero-initialised, so a field added to TCurveIdentity later and not
    //  assigned below carries whatever was on the stack - which is exactly what
    //  PlacedByModule did the day it was added: every entry looked as though a
    //  module had placed it, so nothing was ever pruned, and the test that
    //  asserts a deleted pick takes its instance with it was the only thing
    //  that noticed.
    Entry := Default(TCurveIdentity);
    Entry.Id     := AId;
    Entry.Seed   := ASeed;
    Entry.Positionless := APositionless;
    Entry.Slot   := ASlot;
    Entry.Fitted := False;
    Entry.ReseedOnRestore := False;
    Entry.PlacedByModule := False;

    SetLength(FItems, Length(FItems) + 1);
    FItems[High(FItems)] := Entry;
    Result := AId;
end;

function TCurveIdentityRegistry.IssueForSeed(ASeed: double): TCurveInstanceId;
var
    Index: longint;
begin
    Index := IndexOfSeed(ASeed);
    if Index <> -1 then
    begin
        //  The pick already has an instance. Handing back the same handle is
        //  what makes this callable from a rebuild as well as from a new pick.
        Result := FItems[Index].Id;
        Exit;
    end;
    Result := Append(NewCurveInstanceId, ASeed, False, -1);
end;

function TCurveIdentityRegistry.IssueForSlot(ASlot: longint): TCurveInstanceId;
var
    Index: longint;
begin
    Index := IndexOfSlot(ASlot);
    if Index <> -1 then
    begin
        //  STABLE ACROSS REBUILDS, which is the whole reason this is keyed on
        //  the interval rather than issued fresh. A positionless curve is built
        //  again on every edit; a new handle each time would orphan its values
        //  every time, silently.
        Result := FItems[Index].Id;
        Exit;
    end;
    Result := Append(NewCurveInstanceId, 0, True, ASlot);
end;

procedure TCurveIdentityRegistry.Adopt(const AId: TCurveInstanceId;
    ASeed: double; APlacedByModule: boolean);
var
    Index: longint;
begin
    if not IsCurveInstanceId(AId) then
        raise Exception.Create(
            'A curve identity was offered that was never issued.');

    Index := IndexOfId(AId);
    if Index <> -1 then
    begin
        //  Same instance, rebuilt - possibly somewhere else, because the markup
        //  that owns this identity can be dragged. The handle is the point: it
        //  survives the move, which is what lets the values found for this
        //  instance still be its own afterwards.
        FItems[Index].Seed := ASeed;
        FItems[Index].Positionless := False;
        //  RE-STATED RATHER THAN MERGED: who placed this instance is a fact
        //  about the caller doing the adopting, and the same handle is offered
        //  by a project restore (a pick) and then by a rebuild (a module).
        FItems[Index].PlacedByModule := APlacedByModule;
        Exit;
    end;

    Append(AId, ASeed, False, -1);
    FItems[High(FItems)].PlacedByModule := APlacedByModule;
end;

procedure TCurveIdentityRegistry.KeepOnlySeeds(const ASeeds: array of double);
var
    i, j:  longint;
    Found: boolean;
    Stale: array of double;
begin
    //  Collected first, then removed: removing while enumerating would shift
    //  the entries under the walk.
    SetLength(Stale, 0);
    for i := 0 to High(FItems) do
    begin
        //  AN INSTANCE NO PICK PLACES IS NOT KEYED ON ONE, so no pick can make
        //  it stale. Two kinds: the positionless formula curve, which goes when
        //  its fit interval does, and a pack's own instance, which goes when
        //  the markup that placed it does (RemoveId).
        if FItems[i].Positionless or FItems[i].PlacedByModule then
            Continue;

        Found := False;
        for j := 0 to High(ASeeds) do
            if SameSeed(ASeeds[j], FItems[i].Seed) then
            begin
                Found := True;
                Break;
            end;

        if not Found then
        begin
            SetLength(Stale, Length(Stale) + 1);
            Stale[High(Stale)] := FItems[i].Seed;
        end;
    end;

    for i := 0 to High(Stale) do
        RemoveSeed(Stale[i]);
end;

function TCurveIdentityRegistry.IdForSeed(ASeed: double): TCurveInstanceId;
var
    Index: longint;
begin
    Index := IndexOfSeed(ASeed);
    if Index = -1 then
        Result := NoCurveInstanceId
    else
        Result := FItems[Index].Id;
end;

function TCurveIdentityRegistry.IdForSlot(ASlot: longint): TCurveInstanceId;
var
    Index: longint;
begin
    Index := IndexOfSlot(ASlot);
    if Index = -1 then
        Result := NoCurveInstanceId
    else
        Result := FItems[Index].Id;
end;

function TCurveIdentityRegistry.Has(const AId: TCurveInstanceId): boolean;
begin
    Result := IndexOfId(AId) <> -1;
end;

function TCurveIdentityRegistry.TakeSeedFrom(AFrom, ATo: double): boolean;
var
    Index: longint;
begin
    Index := IndexOfSeed(AFrom);
    Result := Index <> -1;
    if not Result then
        Exit;

    FItems[Index].Seed := ATo;
    //  The shape the last fit found is still this instance's; where it SITS is
    //  not, because the user has just said otherwise. Only a fitted instance
    //  needs saying so - one that was never fitted has nothing to re-seed from,
    //  and would be built at its new pick anyway.
    if FItems[Index].Fitted then
        FItems[Index].ReseedOnRestore := True;
end;

function TCurveIdentityRegistry.RemoveSeed(ASeed: double): boolean;
var
    Index: longint;
begin
    Index := IndexOfSeed(ASeed);
    Result := Index <> -1;
    if Result then
        RemoveAt(Index);
end;

function TCurveIdentityRegistry.RemoveId(
    const AId: TCurveInstanceId): boolean;
var
    Index: longint;
begin
    Index := IndexOfId(AId);
    Result := Index <> -1;
    if Result then
        RemoveAt(Index);
end;

procedure TCurveIdentityRegistry.MarkFitted(
    const AIds: array of TCurveInstanceId);
var
    i, Index: longint;
begin
    //  "and for no others": a fit REPLACES what is known to carry optimiser
    //  results. An instance the last fit did not produce values for is not
    //  fitted any more, whatever an earlier round found.
    for i := 0 to High(FItems) do
        FItems[i].Fitted := False;

    for i := 0 to High(AIds) do
    begin
        Index := IndexOfId(AIds[i]);
        //  An id the registry does not hold is ignored rather than raised on:
        //  the fit reports the instances it produced, and one whose pick was
        //  deleted while the fit ran is gone from the model by the user's own
        //  action.
        if Index <> -1 then
            FItems[Index].Fitted := True;
    end;
end;

function TCurveIdentityRegistry.IsFitted(
    const AId: TCurveInstanceId): boolean;
var
    Index: longint;
begin
    Index := IndexOfId(AId);
    Result := (Index <> -1) and FItems[Index].Fitted;
end;

function TCurveIdentityRegistry.AnyFitted: boolean;
var
    i: longint;
begin
    Result := False;
    for i := 0 to High(FItems) do
        if FItems[i].Fitted then
        begin
            Result := True;
            Exit;
        end;
end;

function TCurveIdentityRegistry.FittedAtSeed(ASeed: double): boolean;
var
    Index: longint;
begin
    Index := IndexOfSeed(ASeed);
    Result := (Index <> -1) and FItems[Index].Fitted;
end;

function TCurveIdentityRegistry.NeedsReseed(
    const AId: TCurveInstanceId): boolean;
var
    Index: longint;
begin
    Index := IndexOfId(AId);
    Result := (Index <> -1) and FItems[Index].ReseedOnRestore;
end;

procedure TCurveIdentityRegistry.ClearReseed(const AId: TCurveInstanceId);
var
    Index: longint;
begin
    Index := IndexOfId(AId);
    if Index <> -1 then
        FItems[Index].ReseedOnRestore := False;
end;

end.
