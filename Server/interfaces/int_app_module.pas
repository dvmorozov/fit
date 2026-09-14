// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(What a module contributes to a fitting problem, and how it is reached.)

THE PROBLEM THIS SOLVES. IFitService is the contract between the client and the
engine, and it had grown five verbs belonging to one analysis pack -
four verbs named after one pack's markup, plus a pair for
adding picks to that pack's own point set. Worse than the count: the interface
unit had to USE that pack's wire records to declare them, so the framework's
central contract could not compile without the pack.

WHAT REPLACES THEM: one generic channel plus per-problem state.

  - a module owns state per PROBLEM (a markup, a selection, an analysis), which
    the framework creates and destroys with the problem and slices per fit
    interval, without knowing what is in it;

  - a module answers named RESOURCES over one pair of verbs, carrying JSON it
    defines. That costs nothing in fidelity: the wave payloads were already raw
    JSON bodies on the wire, so what is lost is only the typed record on an
    interface that had no business naming it.

WHAT IS DELIBERATELY NOT GENERIC. A module's picked point set is its own
interface rather than "a point set in a dictionary", because the existing
add-a-point helper treats a repeated x as an edit-then-delete - correct for a
flat set, fatal for one whose items may share endpoints, which is what nesting
means.
}
unit int_app_module;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils;

type
    { Curve handles, in their wire form. Declared here because it is what
      IModuleSession answers with, and this contract names nothing the engine or
      any module defines. }
    TInstanceHandles = array of string;

    { Where a module's picks are collected.

      One per named point set a module contributes; the framework routes
      AddPointToSet by that name and knows nothing else about it. }
    IModulePointSink = interface
        ['{2B7E9A14-6C3D-4F58-9E20-7A1D5C8B3F41}']
        { The name used on the wire and by the client's selection mode, e.g.
          'wave-bounds'. }
        function SetName: string;
        { What to call this mode in a log line or a hint, e.g. 'wave bounds'. }
        function DisplayName: string;
        procedure AddPoint(XValue, YValue: double);
        procedure ReplacePoint(PrevX, PrevY, NewX, NewY: double);
        procedure Clear;
    end;

    { The module's state for ONE fit interval, handed to the task that fits it.

      Refcounted deliberately: the previous equivalent was a raw object the task
      took ownership of and freed, which is a lifetime rule stated in a comment
      and nowhere enforced. }
    IModuleTaskState = interface
        ['{8F3C2D71-4A96-4E05-B7C8-1D6E9A4B2057}']
        function Kind: string;
        { The object behind the interface, so the module that made this state can
          read it back.

          Explicit because the application compiles with CORBA-style interfaces,
          which carry no cast to TObject: without this the module would have to
          publish its state class just to recover its own data, and the framework
          would end up holding a type it has no business naming. }
        function AsObject: TObject;
    end;

    { A module's state for one problem.

      Created with the problem, cleared when a new profile is loaded, sliced per
      fit interval, and asked - never told - whether the problem is ready to
      fit. }
    IModuleSession = interface
        ['{5D1A7E62-93B4-4C08-AF35-6E2B8D0C4917}']
        { Which module this belongs to, e.g. 'sample'. }
        function Kind: string;

        { The module's own picked point set, or nil when it collects none. }
        function PointSink: IModulePointSink;

        { Answers a named resource, or returns False so the caller can try the
          next module and then report that nothing owns it. The reply is the
          resource itself - a JSON document the module defines. }
        function TryGet(const AResource: string; out AJson: string): boolean;
        function TryPost(const AResource, APayload: string;
            out AJson: string): boolean;

        { Everything is forgotten: a new profile was loaded. }
        procedure Reset;

        { True when this module has enough marked for a fit to be worth
          starting. The framework's own preconditions (an interval, a curve
          position) are joined with this by OR: a module's markup is an
          alternative way to describe a model, not an addition to the usual one. }
        function ContributesFitReadiness: boolean;

        { Raises with a message the user can act on when a fit must not proceed -
          the selected curve type is one this module places, and nothing has been
          marked. Silence means "no objection". }
        procedure CheckFitAllowed;

        { Raises when this particular fit interval cannot be fitted with what
          the module holds - the user marked intervals the module has nothing
          in. The framework owns the intervals and does the iterating; the
          module owns the judgement and the wording, which is why this is not a
          boolean. }
        procedure CheckIntervalAllowed(ALoX, AHiX: double);

        { The module's state restricted to one fit interval, or nil when it has
          nothing in that stretch. }
        function SliceForInterval(ALoX, AHiX: double): IModuleTaskState;

        { Drops whatever in this module's markup placed the instance AInstanceId
          (in its wire form), and answers whether this module placed it.

          ARemoved names EVERY instance that went - the one asked for and any
          the module had to take with it. It is not a courtesy: the framework
          reports a list of curves that a rebuild refreshes only while something
          still describes the model, so an instance whose markup has gone and
          whose curve nobody removed goes on being drawn. A nested pattern
          deleted with its parent was exactly that - the parent vanished, the
          child stayed on the chart hanging from nothing.

          WHY THE FRAMEWORK CANNOT JUST DELETE IT. An instance a module placed is
          rebuilt from that module's markup on every model edit, so removing the
          curve and its identity and nothing else deletes it for as long as it
          takes the next rebuild to put it back - a Delete that visibly does
          nothing. Only the module knows which mark produced which instance, and
          what else has to go with it: a pattern nested in the one being removed
          has no leg left to hang from.

          False means "not mine", so the framework can try the next module and
          then refuse in words rather than pretending the deletion happened. }
        function TryRemoveInstance(const AInstanceId: string;
            out ARemoved: TInstanceHandles): boolean;
    end;

    { What a module declares about one of its resources, so the framework can
      apply the policy the resource needs without knowing what it does. }
    TModuleResource = record
        { '<module>/<resource>', e.g. 'sample/detect'. }
        Name: string;
        { The Python sidecar must be running before this resource is reached. }
        NeedsPythonSidecar: boolean;
        { May take as long as a fit, so a client must not use its ordinary reply
          timeout. Getting this wrong is invisible until real data is slow
          enough to hit it. }
        LongRunning: boolean;
    end;

    TModuleResourceArray = array of TModuleResource;

    { A module: its name, the resources it answers, and how to make its
      per-problem state. }
    IAppModule = interface
        ['{0C4F8B39-72A5-4D16-8E93-5B1C7A26D480}']
        function Name: string;
        function Resources: TModuleResourceArray;
        { Makes this module's state for one problem. AHost is the problem
          (a TFitService), passed as TObject so this contract - which the
          framework compiles - names nothing a module defines and the framework
          needs no knowledge of any module to declare it. The module casts it,
          which is sound in the only direction that matters: a module may depend
          on the framework, never the reverse. }
        function CreateSession(AHost: TObject): IModuleSession;
    end;

implementation

end.
