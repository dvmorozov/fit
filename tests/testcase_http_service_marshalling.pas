// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(What the client makes of the compute server's replies, and what it sends.)

Every action the desktop takes goes through http_fit_service: read a setting,
push a profile, select a curve type, ask a module a question. Roughly seven
hundred lines of it are marshalling decisions - what a missing field means, when a
reply is unreadable, whether a refusal is the server talking or the connection
failing - and none of it was reachable by a test, because each transport call site
built its own HTTP client inline. The unit sat at 2 %.

THttpFitService.Fetch and .Send are the seam that came out of that; see
tests/mocks/mock_http_transport. Nothing here opens a socket, so these are unit
tests: the real marshalling runs against a canned reply.
}
unit testcase_http_service_marshalling;

{$MODE Delphi}

interface

uses
    Classes, SysUtils, fpcunit, testregistry,
    MyExceptions, mock_http_transport, points_set, title_points_set,
    fit_statistics,
    //  For a real curve-type id: TCurveTypeId is a GUID (declared in
    //  named_points_set), not a display name.
    named_points_set, gauss_points_set, pseudo_voigt_points_set,
    //  For reading the model back: the curve list, its parameter containers and
    //  the instance handle that ties a row to a curve.
    curve_list, mscr_specimen_list, persistent_curve_parameters,
    special_curve_parameter, curve_instance_id, self_copied_component,
    int_fit_service;

type
    THttpServiceMarshallingTest = class(TTestCase)
    private
        FSvc: TMockHttpService;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        //  Reading settings
        procedure ASettingIsReadFromTheReply;
        procedure AMissingSettingFallsBackRatherThanFailing;
        procedure SeveralSettingsComeFromTheOneDocument;

        //  Writing settings
        procedure WritingASettingSendsIt;

        //  Points
        procedure APointSetIsBuiltFromTheReply;
        procedure AnEmptyPointSetIsNotAnError;
        procedure AReplyWithNoPointsAnswersNil;
        procedure PushingAProfileSendsItsPoints;
        procedure ThePushedProfileStaysTheCallers;

        //  What the server says versus what the connection does
        procedure AnUnreadableReplyIsReported;
        procedure ARejectionByTheServerReachesTheUserAsItsOwnMessage;
        procedure ARejectionIsNotRelabelledAsATransportFailure;

        //  THE HANDLES, both ways. A pick and the handle of the curve it
        //  stands for are one fact and must travel together; these are the
        //  client's half of that.
        procedure PushingPicksWithHandlesSendsThemBesideTheCoordinates;
        procedure PushingPicksWithNoHandlesSendsNoIdsFieldAtAll;
        procedure AProfilePushNeverCarriesHandles;
        procedure TheHandlesComeBackWithThePicksInOneRequest;
        procedure TheWholeModelIsWrittenBackInOneCall;
        procedure ThatCallCarriesWhetherAnOptimiserProducedTheValues;
        procedure WhetherACurveWasFittedIsReadFromTheCurvesReply;
        procedure ACurveTheReplyDoesNotHaveIsNotReportedAsFitted;
        procedure WritingValuesForACurveTheModelHasLostIsRefused;
        procedure AReplyCarryingNoModuleStatesIsNotAFailure;
        procedure AReplyCarryingNoHandlesLeavesThePicksUnnamed;

        //  Modules
        procedure AModuleResourceIsFetchedByName;
        procedure EveryModulesProjectStateArrivesInOneRequest;

        //  The URL
        procedure ATrailingSlashOnTheBaseUrlIsIgnored;
        procedure TheServerUrlIsReportedBack;

        //  The rest of the surface, swept rather than sampled: each of these
        //  verbs is a few lines of marshalling, and the mistake they are prone to
        //  is reading or writing a NEIGHBOUR's field or route - which no
        //  one-verb-at-a-time test can catch.
        procedure EverySettingIsReadFromItsOwnField;
        procedure EverySettingIsWrittenUnderItsOwnName;
        procedure ABooleanSettingSurvivesBothWays;
        procedure TheServerUrlCanBeRepointed;
        procedure TheCurveTypeRoundTrips;
        procedure EveryPointSetIsReadFromItsOwnRoute;
        procedure ThePointSetTitleIsCarried;
        procedure EveryPointSetIsWrittenToItsOwnRoute;
        procedure OnlyTwoOfTheFourSettersTakeOwnership;
        procedure AddingAPointNamesItsSetAndItsCoordinates;
        procedure EachPickKindReachesItsOwnSet;
        procedure ReplacingAPointSendsBothTheOldAndTheNew;
        procedure AnActionIsPostedByName;
        procedure ClearingTheSpecialCurveIsSent;
        procedure AModulesOwnSetIsAddressedByItsName;
        procedure AndAPointInItCanBeMoved;
        procedure AModulePostCarriesItsPayload;

        //  State, progress, the model and the verb table - the rest of the
        //  surface, swept for the same reason as the settings above.
        procedure TheServerStateIsRead;
        procedure AnAsyncOperationIsReportedAsRunning;
        procedure TheProgressStringsAreReadFromTheirOwnFields;
        procedure GoodnessOfFitStatisticsAreRead;
        procedure TheCurveCountIsRead;
        procedure ACurveIsAddressedByItsHandle;
        procedure AnUnknownHandleIsNotFound;
        //  Removing one curve: what the client sends, and what it refuses to
        //  send.
        procedure DeletingACurveAddressesItByHandle;
        procedure DeletingACurveTheModelHasNotGotSendsNothing;
        procedure TheSpecialCurveExpressionIsSent;
        procedure EveryActionIsPostedUnderItsOwnName;
        procedure SubtractBackgroundCarriesItsAutoFlag;
        procedure TheDerivedProfilesAreReadFromTheirOwnRoutes;

        //  The model read back as objects - the two largest routines in the unit,
        //  and the ones carrying real decisions rather than field lookups.
        procedure TheCurvesAreBuiltFromTheirPoints;
        procedure ACurveWithNoHandleIsRefusedNotSkipped;
        procedure CurveAttributesCarryEveryParameter;
        procedure AParameterKindDecidesHowItsValueIsRead;
        procedure TheInstanceHandleReachesTheAttributes;
        procedure TheSpecialCurveParametersAreRead;
        procedure AnEmptyModelReadsAsNoCurves;
    end;

implementation

const
    BASE = 'http://127.0.0.1:8787';
    { Two fixed handles, so a test can assert which curve it got back. Spelled out
      rather than generated: a generated pair would make the expected value depend
      on the run. }
    ID1 = '{11111111-1111-1111-1111-111111111111}';
    ID2 = '{22222222-2222-2222-2222-222222222222}';

procedure THttpServiceMarshallingTest.SetUp;
begin
    FSvc := TMockHttpService.Create(BASE);
end;

procedure THttpServiceMarshallingTest.TearDown;
begin
    FreeAndNil(FSvc);
end;

{ ---- settings -------------------------------------------------------------- }

procedure THttpServiceMarshallingTest.ASettingIsReadFromTheReply;
begin
    FSvc.Reply('settings', '{"ok":true,"maxRFactor":0.125}');
    AssertEquals(0.125, FSvc.GetMaxRFactor, 1e-12);
end;

procedure THttpServiceMarshallingTest.AMissingSettingFallsBackRatherThanFailing;
begin
    //  A server one version behind does not carry every field. Falling back keeps
    //  the client usable against it; raising would make an older server look
    //  broken rather than merely older.
    FSvc.Reply('settings', '{"ok":true}');
    AssertEquals('absent maxRFactor reads as the default',
        0.0, FSvc.GetMaxRFactor, 1e-12);
end;

procedure THttpServiceMarshallingTest.SeveralSettingsComeFromTheOneDocument;
begin
    //  Each getter fetches /settings and picks its field out, so a document
    //  carrying several must not have them interfere.
    FSvc.Reply('settings',
        '{"ok":true,"maxRFactor":0.5,"backFactor":0.25,"curveThresh":0.75}');
    AssertEquals('max', 0.5, FSvc.GetMaxRFactor, 1e-12);
    AssertEquals('back', 0.25, FSvc.GetBackFactor, 1e-12);
    AssertEquals('thresh', 0.75, FSvc.GetCurveThresh, 1e-12);
end;

procedure THttpServiceMarshallingTest.WritingASettingSendsIt;
begin
    FSvc.SetMaxRFactor(0.375);
    //  The FIELD and the VALUE both have to be in the body: a setter that sent an
    //  empty document, or the right number under the wrong name, would look
    //  identical from the outside.
    AssertTrue('the field was named: ' + FSvc.Log.AsText,
        Pos('maxRFactor', FSvc.Log.AsText) > 0);
    //  Matched as '3.75' rather than '0.375' because fpjson writes a double in
    //  exponential form - 3.7500000000000000E-001. Worth knowing before anyone
    //  compares two of these documents as strings and finds they differ while
    //  meaning the same thing.
    AssertTrue('and the value with it: ' + FSvc.Log.AsText,
        Pos('3.75', FSvc.Log.AsText) > 0);
end;

{ ---- points ---------------------------------------------------------------- }

procedure THttpServiceMarshallingTest.APointSetIsBuiltFromTheReply;
var
    PS: TTitlePointsSet;
begin
    FSvc.Reply('profile',
        '{"title":"exp","x":[1,2,3],"y":[10,20,30]}');
    PS := FSvc.GetProfilePointsSet;
    try
        AssertTrue('a set was built', Assigned(PS));
        AssertEquals('every point', 3, PS.PointsCount);
        AssertEquals('x', 2.0, PS.PointXCoord[1], 1e-12);
        AssertEquals('y', 30.0, PS.PointYCoord[2], 1e-12);
    finally
        PS.Free;
    end;
end;

procedure THttpServiceMarshallingTest.AnEmptyPointSetIsNotAnError;
var
    PS: TTitlePointsSet;
begin
    //  Nothing loaded yet is an ordinary state, and it must be distinguishable
    //  from a broken reply - which is the next test.
    FSvc.Reply('profile', '{"title":"","x":[],"y":[]}');
    PS := FSvc.GetProfilePointsSet;
    try
        AssertTrue('a set was still built', Assigned(PS));
        AssertEquals('with no points in it', 0, PS.PointsCount);
    finally
        PS.Free;
    end;
end;

procedure THttpServiceMarshallingTest.AReplyWithNoPointsAnswersNil;
var
    PS: TTitlePointsSet;
begin
    //  nil, not an empty set: a reply that carries no x/y at all is a reply this
    //  client cannot read, and answering with an empty profile would present it
    //  as "the server has nothing" - which is a different thing entirely.
    FSvc.Reply('profile', '{"ok":true}');
    PS := FSvc.GetProfilePointsSet;
    try
        AssertTrue('nothing was built', not Assigned(PS));
    finally
        PS.Free;
    end;
end;

procedure THttpServiceMarshallingTest.PushingAProfileSendsItsPoints;
var
    PS: TTitlePointsSet;
begin
    PS := TTitlePointsSet.Create(nil);
    try
        PS.AddNewPoint(1.5, 10.5);
        PS.AddNewPoint(2.5, 20.5);
        FSvc.SetProfilePointsSet(PS);
        //  MANTISSA DIGITS, because fpjson writes every double in exponential
        //  form: 20.5 goes out as 2.0500000000000000E+001, so a test looking for
        //  the literal '20.5' fails while the value is perfectly correct.
        AssertTrue('the x values went out: ' + FSvc.Log.AsText,
            Pos('1.5', FSvc.Log.AsText) > 0);
        AssertTrue('and the y values with them: ' + FSvc.Log.AsText,
            Pos('2.05', FSvc.Log.AsText) > 0);
        AssertTrue('both x and y arrays were named',
            (Pos('"x"', FSvc.Log.AsText) > 0) and
            (Pos('"y"', FSvc.Log.AsText) > 0));
    finally
        PS.Free;
    end;
end;

procedure THttpServiceMarshallingTest.ThePushedProfileStaysTheCallers;
var
    PS: TTitlePointsSet;
begin
    //  The service encodes and sends; it does not take ownership. A service that
    //  freed the caller's set would leave the form holding a dangling profile -
    //  and the symptom would be a crash on the next repaint, not here.
    PS := TTitlePointsSet.Create(nil);
    try
        PS.AddNewPoint(1, 2);
        FSvc.SetProfilePointsSet(PS);
        AssertEquals('still usable afterwards', 1, PS.PointsCount);
        AssertEquals('and unchanged', 2.0, PS.PointYCoord[0], 1e-12);
    finally
        PS.Free;
    end;
end;

{ ---- the server talking, versus the connection failing --------------------- }

procedure THttpServiceMarshallingTest.AnUnreadableReplyIsReported;
var
    Raised: boolean;
begin
    //  An HTML error page, a truncated body, a proxy notice - anything that is not
    //  a JSON object. Reported as a fault rather than silently treated as empty,
    //  because "the server said nothing" and "I could not read the server" lead
    //  the user to different places.
    FSvc.Reply('settings', '<html>502 Bad Gateway</html>');
    Raised := False;
    try
        FSvc.GetMaxRFactor;
    except
        on E: Exception do
            Raised := True;
    end;
    AssertTrue('an unreadable reply is a fault', Raised);
end;

procedure THttpServiceMarshallingTest.ARejectionByTheServerReachesTheUserAsItsOwnMessage;
var
    Message_: string;
begin
    //  ok:false is the server declining, with a reason it wrote for the user. That
    //  reason must survive to the surface; replacing it with a generic failure is
    //  how a careful refusal became "something went wrong".
    FSvc.Reply('settings',
        '{"ok":false,"error":"the interval has no points in it"}');
    Message_ := '';
    try
        FSvc.GetMaxRFactor;
    except
        on E: EUserException do
            Message_ := E.Message;
    end;
    AssertEquals('the server''s own words',
        'the interval has no points in it', Message_);
end;

procedure THttpServiceMarshallingTest.ARejectionIsNotRelabelledAsATransportFailure;
var
    Kind: string;
begin
    //  THE DISTINCTION THE TRANSPORT SEAM HAS TO PRESERVE. A refusal raised from
    //  below must pass through as EUserException - a message for the user - and
    //  not be caught and re-reported as a connection problem, which would send
    //  the user to check their network over a model they can fix.
    FSvc.FailNextWith('no interval is selected');
    Kind := '';
    try
        FSvc.GetMaxRFactor;
    except
        on E: EUserException do Kind := 'user';
        on E: Exception do Kind := 'transport';
    end;
    AssertEquals('reported as the server talking', 'user', Kind);
end;

{ ---- modules --------------------------------------------------------------- }

procedure THttpServiceMarshallingTest.AModuleResourceIsFetchedByName;
var
    Json: string;
begin
    //  The reply is the module's own document, passed through unread: the
    //  framework must not parse a vocabulary it does not own.
    FSvc.Reply('detect', '{"ok":true,"pivots":[1,2,3]}');
    Json := FSvc.ModuleGet('sample/detect');
    AssertTrue('the resource was asked for by name: ' + FSvc.Log.AsText,
        Pos('sample/detect', FSvc.Log.AsText) > 0);
    AssertTrue('and its document came back: ' + Json, Pos('pivots', Json) > 0);
end;

{ ---- the URL --------------------------------------------------------------- }

procedure THttpServiceMarshallingTest.ATrailingSlashOnTheBaseUrlIsIgnored;
var
    Svc: TMockHttpService;
begin
    //  A URL typed with a trailing slash is the same server. Without this the
    //  client builds '//problems' and every call fails against a correct address.
    Svc := TMockHttpService.Create(BASE + '/');
    try
        Svc.Reply('settings', '{"ok":true,"maxRFactor":0.25}');
        AssertEquals('the call still lands', 0.25, Svc.GetMaxRFactor, 1e-12);
        AssertTrue('and the URL has no double slash: ' + Svc.Log.AsText,
            Pos('//problems', Svc.Log.AsText) = 0);
    finally
        Svc.Free;
    end;
end;

procedure THttpServiceMarshallingTest.TheServerUrlIsReportedBack;
begin
    //  The form shows this, so it has to be the address actually in use rather
    //  than whatever was typed.
    AssertEquals(BASE, FSvc.GetServerUrl);
end;

{ How many points a set holds, and frees it. The point-set getters all return a
  fresh object, so a sweep over several of them would otherwise leak one each. }
function CountOf(APoints: TTitlePointsSet): longint;
begin
    Result := 0;
    if not Assigned(APoints) then
        Exit;
    try
        Result := APoints.PointsCount;
    finally
        APoints.Free;
    end;
end;

{ ---- the rest of the settings surface -------------------------------------- }

procedure THttpServiceMarshallingTest.EverySettingIsReadFromItsOwnField;
begin
    //  A SWEEP, deliberately. Each of these getters is three lines - fetch
    //  /settings, pick one field, fall back - and the mistake they are prone to is
    //  reading a neighbour's field, which no single-getter test can catch. Reading
    //  them all from one document with distinct values does.
    FSvc.Reply('settings',
        '{"ok":true,"maxRFactor":0.11,"backFactor":0.22,"curveThresh":0.33,' +
        '"waveLength":1.44,"backgroundVariation":true,"minimizerKind":2,' +
        '"lossKind":3,"weighting":"poisson","curveScaling":true}');
    AssertEquals('maxRFactor', 0.11, FSvc.GetMaxRFactor, 1e-12);
    AssertEquals('backFactor', 0.22, FSvc.GetBackFactor, 1e-12);
    AssertEquals('curveThresh', 0.33, FSvc.GetCurveThresh, 1e-12);
    AssertEquals('waveLength', 1.44, FSvc.GetWaveLength, 1e-12);
    AssertTrue('backgroundVariation', FSvc.GetBackgroundVariationEnabled);
    AssertEquals('minimizerKind', 2, FSvc.GetMinimizerKind);
    AssertEquals('lossKind', 3, FSvc.GetLossKind);
    AssertEquals('weighting', 'poisson', FSvc.GetWeighting);
    AssertTrue('curveScaling', FSvc.GetCurveScalingEnabled);
end;

procedure THttpServiceMarshallingTest.EverySettingIsWrittenUnderItsOwnName;
var
    Sent: string;
begin
    //  The mirror of the sweep above, and the same class of mistake: a setter
    //  writing the right value under a neighbour's name would round-trip through
    //  a server that echoes and fail only against the real one.
    FSvc.SetBackFactor(0.5);
    FSvc.SetCurveThresh(0.25);
    FSvc.SetWaveLength(1.79);
    FSvc.SetBackgroundVariationEnabled(True);
    FSvc.SetMinimizerKind(1);
    FSvc.SetLossKind(4);
    FSvc.SetWeighting('none');
    FSvc.SetCurveScalingEnabled(False);
    Sent := FSvc.Log.AsText;
    AssertTrue('backFactor: ' + Sent, Pos('backFactor', Sent) > 0);
    AssertTrue('curveThresh', Pos('curveThresh', Sent) > 0);
    AssertTrue('waveLength', Pos('waveLength', Sent) > 0);
    AssertTrue('backgroundVariation', Pos('backgroundVariation', Sent) > 0);
    AssertTrue('minimizerKind', Pos('minimizerKind', Sent) > 0);
    AssertTrue('lossKind', Pos('lossKind', Sent) > 0);
    AssertTrue('weighting', Pos('weighting', Sent) > 0);
    AssertTrue('curveScaling', Pos('curveScaling', Sent) > 0);
end;

procedure THttpServiceMarshallingTest.ABooleanSettingSurvivesBothWays;
begin
    //  False is the value a "did the field arrive at all?" bug returns either way,
    //  so both have to be checked or the getter looks right while always saying no.
    FSvc.Reply('settings', '{"ok":true,"curveScaling":true}');
    AssertTrue('true reads as true', FSvc.GetCurveScalingEnabled);
    FSvc.Reply('settings', '{"ok":true,"curveScaling":false}');
    AssertFalse('and false as false', FSvc.GetCurveScalingEnabled);
end;

procedure THttpServiceMarshallingTest.TheServerUrlCanBeRepointed;
begin
    //  The user changes this in Fit -> Compute Server..., and the next call has to
    //  go to the new address - which means the problem id from the old server must
    //  not be reused, or the client asks the new server about a problem it has
    //  never heard of.
    FSvc.SetServerUrl('http://elsewhere:9999');
    AssertEquals('reported back', 'http://elsewhere:9999', FSvc.GetServerUrl);
    FSvc.Reply('settings', '{"ok":true,"maxRFactor":0.9}');
    FSvc.GetMaxRFactor;
    AssertTrue('and the call went there: ' + FSvc.Log.AsText,
        Pos('elsewhere:9999', FSvc.Log.AsText) > 0);
end;

{ ---- the curve type -------------------------------------------------------- }

procedure THttpServiceMarshallingTest.TheCurveTypeRoundTrips;
var
    Wanted: TCurveTypeId;
begin
    //  A GUID on the wire, not a display name - which is the point: the name is
    //  what the menu shows and it changes, the id is what the server fits with.
    //  The field is 'curveType'; a getter reading any other spelling would answer
    //  the null GUID and the server would fit with its own default instead, which
    //  is the failure testcase_curve_type_selection was written for.
    Wanted := TPseudoVoigtPointsSet.GetCurveTypeId;
    FSvc.Reply('settings',
        '{"ok":true,"curveType":"' + GUIDToString(Wanted) + '"}');
    AssertTrue('read back as the same id',
        IsEqualGUID(Wanted, FSvc.GetCurveType));

    FSvc.SetCurveType(TGaussPointsSet.GetCurveTypeId);
    AssertTrue('and the id was sent: ' + FSvc.Log.AsText,
        Pos(Copy(GUIDToString(TGaussPointsSet.GetCurveTypeId), 2, 8),
            FSvc.Log.AsText) > 0);
end;

{ ---- the other point sets -------------------------------------------------- }

procedure THttpServiceMarshallingTest.EveryPointSetIsReadFromItsOwnRoute;
begin
    //  Each of these is GetPoints against a different path, and the failure they
    //  are prone to is asking the wrong route - which returns a plausible set of
    //  points, so nothing looks wrong. Distinct point counts per route catch it.
    FSvc.Reply('profile', '{"title":"p","x":[1],"y":[1]}');
    FSvc.Reply('background', '{"title":"b","x":[1,2],"y":[1,2]}');
    FSvc.Reply('positions', '{"title":"c","x":[1,2,3],"y":[1,2,3]}');
    FSvc.Reply('rfactor-bounds', '{"title":"r","x":[1,2,3,4],"y":[1,2,3,4]}');

    AssertEquals('profile', 1, CountOf(FSvc.GetProfilePointsSet));
    AssertEquals('background', 2, CountOf(FSvc.GetBackgroundPoints));
    AssertEquals('positions', 3, CountOf(FSvc.GetCurvePositions));
    AssertEquals('rfactor bounds', 4, CountOf(FSvc.GetRFactorBounds));
end;

procedure THttpServiceMarshallingTest.ThePointSetTitleIsCarried;
var
    PS: TTitlePointsSet;
begin
    //  The title names the series in the legend, so losing it leaves the chart
    //  with an unlabelled curve rather than an error.
    FSvc.Reply('profile', '{"title":"experiment 1","x":[1],"y":[1]}');
    PS := FSvc.GetProfilePointsSet;
    try
        AssertEquals('experiment 1', PS.FTitle);
    finally
        PS.Free;
    end;
end;

function NewPoint: TTitlePointsSet;
begin
    Result := TTitlePointsSet.Create(nil);
    Result.AddNewPoint(1, 1);
end;

procedure THttpServiceMarshallingTest.EveryPointSetIsWrittenToItsOwnRoute;
var
    Borrowed: TTitlePointsSet;
    Sent: string;
begin
    //  A SEPARATE OBJECT FOR EACH OWNING SETTER. SetCurvePositions and
    //  SetRFactorBounds free their argument; the profile and background setters do
    //  not. Reusing one set across all four dereferences freed memory on the
    //  fourth call - which is what the next test is about.
    Borrowed := NewPoint;
    try
        FSvc.SetProfilePointsSet(Borrowed);
        FSvc.SetBackgroundPointsSet(Borrowed);
    finally
        Borrowed.Free;
    end;
    FSvc.SetCurvePositions(NewPoint);
    FSvc.SetRFactorBounds(NewPoint);

    Sent := FSvc.Log.AsText;
    AssertTrue('profile: ' + Sent, Pos('/profile', Sent) > 0);
    AssertTrue('background', Pos('/background', Sent) > 0);
    AssertTrue('positions', Pos('/positions', Sent) > 0);
    AssertTrue('rfactor bounds', Pos('rfactor-bounds', Sent) > 0);
end;

procedure THttpServiceMarshallingTest.OnlyTwoOfTheFourSettersTakeOwnership;
var
    Kept: TTitlePointsSet;
begin
    //  AN ASYMMETRY THAT HAS ALREADY COST A BUG, and it is worth a test of its own
    //  because neither half is guessable from the name.
    //
    //  The PROFILE and BACKGROUND setters borrow: TFitClient goes on plotting the
    //  profile after handing it over - it is one of the viewer's series - and frees
    //  the background itself. Freeing them here left the chart drawing a dangling
    //  pointer, typically into whatever was allocated next.
    //
    //  POSITIONS and RFACTOR BOUNDS take ownership, matching the engine's own
    //  setters: the caller builds a set for the call and hands it over.
    Kept := NewPoint;
    try
        FSvc.SetProfilePointsSet(Kept);
        AssertEquals('the profile set is still the caller''s', 1, Kept.PointsCount);
        FSvc.SetBackgroundPointsSet(Kept);
        AssertEquals('and so is the background set', 1, Kept.PointsCount);
    finally
        Kept.Free;
    end;

    //  Handed over and NOT freed here: doing so would be a double free, which is
    //  the mirror-image mistake and just as silent.
    FSvc.SetCurvePositions(NewPoint);
    FSvc.SetRFactorBounds(NewPoint);
    AssertTrue('both handovers were sent',
        Pos('/positions', FSvc.Log.AsText) > 0);
end;

{ ---- adding and replacing single points ------------------------------------ }

procedure THttpServiceMarshallingTest.AddingAPointNamesItsSetAndItsCoordinates;
var
    Sent: string;
begin
    //  This is what a chart click becomes. The coordinates matter as much as the
    //  route: a pick sent to the right set at the wrong place moves a curve
    //  somewhere the user did not click, and nothing reports it.
    FSvc.AddPointToProfile(1.25, 2.5);
    Sent := FSvc.Log.AsText;
    AssertTrue('the set: ' + Sent, Pos('profile', Sent) > 0);
    AssertTrue('the x', Pos('1.25', Sent) > 0);
    AssertTrue('the y', Pos('2.5', Sent) > 0);
end;

procedure THttpServiceMarshallingTest.EachPickKindReachesItsOwnSet;
var
    Sent: string;
begin
    //  A dispatch table in all but name, and the bug it is prone to is two kinds
    //  sharing a branch - which sends a background pick to the profile.
    FSvc.AddPointToBackground(1, 1);
    FSvc.AddPointToRFactorBounds(2, 2);
    FSvc.AddPointToCurvePositions(3, 3);
    Sent := FSvc.Log.AsText;
    AssertTrue('background: ' + Sent, Pos('/background', Sent) > 0);
    AssertTrue('rfactor bounds', Pos('rfactor-bounds', Sent) > 0);
    AssertTrue('positions', Pos('/positions', Sent) > 0);
end;

procedure THttpServiceMarshallingTest.AModulesOwnSetIsAddressedByItsName;
var
    Sent: string;
begin
    //  A MODULE'S SET IS NOT ONE OF THE FOUR. It has its own verb rather than
    //  being folded into the shared add-a-point helper, because that helper
    //  treats a repeated x as an edit-then-delete - right for a flat set, fatal
    //  for one whose items may share endpoints. So the name has to reach the
    //  route intact.
    FSvc.AddPointToSet('outline-anchors', 4, 5);
    Sent := FSvc.Log.AsText;
    AssertTrue('the module set is named: ' + Sent,
        Pos('outline-anchors', Sent) > 0);
    //  And it must not be routed as one of the built-in four.
    AssertTrue('not the profile', Pos('/points/profile', Sent) = 0);
end;

procedure THttpServiceMarshallingTest.AndAPointInItCanBeMoved;
var
    Sent: string;
begin
    //  The same for a drag inside a module's set: the old point and the new one,
    //  under the module's own name.
    FSvc.ReplacePointInSet('outline-anchors', 1, 2, 3, 4);
    Sent := FSvc.Log.AsText;
    AssertTrue('the module set is named: ' + Sent,
        Pos('outline-anchors', Sent) > 0);
    AssertTrue('the old point is sent', Pos('1', Sent) > 0);
    AssertTrue('and the new one', Pos('3', Sent) > 0);
end;

procedure THttpServiceMarshallingTest.AModulePostCarriesItsPayload;
var
    Sent: string;
begin
    //  A module's own request, with the JSON the module defined. Sent with the
    //  LONG timeout, because what a module asks for may be a computation rather
    //  than a lookup - a short one would abandon a running analysis and report a
    //  transport failure for a server that was working correctly.
    FSvc.Reply('modules', '{"ok":true}');
    FSvc.ModulePost('decompose', '{"from":1,"to":9}');
    Sent := FSvc.Log.AsText;
    AssertTrue('the resource is named: ' + Sent, Pos('decompose', Sent) > 0);
    AssertTrue('and the payload went with it', Pos('"from"', Sent) > 0);
end;

procedure THttpServiceMarshallingTest.ReplacingAPointSendsBothTheOldAndTheNew;
var
    Sent: string;
begin
    //  A drag: the server has to know which point moved, not just where it landed.
    //  Sending only the new position would add a point rather than move one.
    //  Distinctive values, not 1 and 3: a single digit occurs in the URL's
    //  problem id and in the timeout, so an assertion on '1' would pass whatever
    //  the body said.
    FSvc.ReplacePointInProfile(11.5, 22.5, 33.5, 44.5);
    Sent := FSvc.Log.AsText;
    AssertTrue('the point that moved: ' + Sent, Pos('11.5', Sent) > 0);
    AssertTrue('and where it went', Pos('33.5', Sent) > 0);
end;

{ ---- actions and the special curve ----------------------------------------- }

procedure THttpServiceMarshallingTest.AnActionIsPostedByName;
begin
    FSvc.Reply('smooth-profile', '{"ok":true}');
    FSvc.SmoothProfile;
    AssertTrue('the action was named: ' + FSvc.Log.AsText,
        Pos('smooth-profile', FSvc.Log.AsText) > 0);
end;

procedure THttpServiceMarshallingTest.ClearingTheSpecialCurveIsSent;
begin
    //  Asserted on the ROUTE, not merely that a request happened: "something was
    //  sent" would pass for a call to any endpoint at all.
    FSvc.ClearSpecialCurve;
    AssertTrue('the special-params route was used: ' + FSvc.Log.AsText,
        Pos('special-params', FSvc.Log.AsText) > 0);
end;

{ ---- state, progress and statistics ---------------------------------------- }

procedure THttpServiceMarshallingTest.TheServerStateIsRead;
begin
    //  The form polls this to decide what is enabled. A state read as the wrong
    //  value leaves menu items greyed out with nothing explaining why.
    FSvc.Reply('state', '{"ok":true,"state":2,"busy":false}');
    AssertEquals(2, Ord(FSvc.GetState));
end;

procedure THttpServiceMarshallingTest.AnAsyncOperationIsReportedAsRunning;
begin
    //  ITS OWN ROUTE, /async, and the field is 'busy' - the method is named for
    //  the caller's question and the field for the server's answer. This is also
    //  the route rest_polling classifies as a heartbeat, so it is fetched
    //  constantly and must stay cheap.
    FSvc.Reply('async', '{"ok":true,"busy":true}');
    AssertTrue('running', FSvc.AsyncOper);
    FSvc.Reply('async', '{"ok":true,"busy":false}');
    AssertFalse('and not running', FSvc.AsyncOper);
end;

procedure THttpServiceMarshallingTest.TheProgressStringsAreReadFromTheirOwnFields;
begin
    //  Four strings the status bar shows side by side, all from one document, all
    //  read by three-line accessors. Distinct values catch a getter reading its
    //  neighbour's field - which would show the wrong number where the user is
    //  watching a fit converge.
    //  All four come from /stats as TOP-LEVEL fields, beside the nested
    //  'statistics' object the next test reads - one route, two shapes.
    FSvc.Reply('stats',
        '{"ok":true,"calcTime":"1.5 s","rFactor":"0.11",' +
        '"absRFactor":"0.22","sqrRFactor":"0.33"}');
    AssertEquals('calc time', '1.5 s', FSvc.GetCalcTimeStr);
    AssertEquals('r factor', '0.11', FSvc.GetRFactorStr);
    AssertEquals('absolute', '0.22', FSvc.GetAbsRFactorStr);
    AssertEquals('squared', '0.33', FSvc.GetSqrRFactorStr);
end;

procedure THttpServiceMarshallingTest.GoodnessOfFitStatisticsAreRead;
var
    S: TFitStatistics;
begin
    //  NESTED under 'statistics': a reply without that object leaves the record
    //  invalid rather than zero-filled, which is the difference between "not
    //  fitted yet" and "fitted perfectly".
    FSvc.Reply('stats',
        '{"ok":true,"statistics":{"valid":true,"dataPoints":20,"params":10,' +
        '"degreesOfFreedom":10,"chiSquare":12.5,"reducedChiSquare":1.25,' +
        '"rSquared":0.99,"aic":3.5,"bic":4.5}}');
    S := FSvc.GetStatistics;
    AssertTrue('valid', S.Valid);
    AssertEquals('data points', 20, S.DataPoints);
    AssertEquals('varying parameters', 10, S.Params);
    AssertEquals('degrees of freedom', 10, S.DegreesOfFreedom);
    AssertEquals('chi square', 12.5, S.ChiSquare, 1e-9);
    AssertEquals('reduced', 1.25, S.ReducedChiSquare, 1e-9);
    AssertEquals('r squared', 0.99, S.RSquared, 1e-9);
    AssertEquals('aic', 3.5, S.AIC, 1e-9);
    AssertEquals('bic', 4.5, S.BIC, 1e-9);

    //  And the absent-object case, which is what an unfitted problem returns.
    FSvc.Reply('stats', '{"ok":true}');
    S := FSvc.GetStatistics;
    AssertFalse('no statistics means not valid', S.Valid);
end;

{ ---- the model: curves, handles and parameters ----------------------------- }

procedure THttpServiceMarshallingTest.TheCurveCountIsRead;
begin
    FSvc.Reply('curves',
        '{"ok":true,"curves":[{"id":"{11111111-1111-1111-1111-111111111111}"},' +
        '{"id":"{22222222-2222-2222-2222-222222222222}"}]}');
    AssertEquals(2, FSvc.GetCurveCount);
end;

procedure THttpServiceMarshallingTest.ACurveIsAddressedByItsHandle;
begin
    //  Handles, not indices: a fit renumbers the model, so an index taken before
    //  one and used after it addresses a different curve. That is the bug the
    //  handle exists to make impossible, and it is invisible from outside - the
    //  wrong curve simply gets the edit.
    FSvc.Reply('curves',
        '{"ok":true,"curves":[{"id":"{11111111-1111-1111-1111-111111111111}"},' +
        '{"id":"{22222222-2222-2222-2222-222222222222}"}]}');
    AssertEquals('the first handle',
        '{11111111-1111-1111-1111-111111111111}', FSvc.GetCurveInstanceId(0));
    AssertEquals('the second',
        '{22222222-2222-2222-2222-222222222222}', FSvc.GetCurveInstanceId(1));
    AssertEquals('and back again by handle', 1,
        FSvc.IndexOfCurveInstance('{22222222-2222-2222-2222-222222222222}'));
end;

procedure THttpServiceMarshallingTest.AnUnknownHandleIsNotFound;
begin
    //  A negative index rather than a raise: the curve may legitimately have gone
    //  since the caller took the handle, and that is not a fault.
    FSvc.Reply('curves',
        '{"ok":true,"curves":[{"id":"{11111111-1111-1111-1111-111111111111}"}]}');
    AssertTrue('not found',
        FSvc.IndexOfCurveInstance('{99999999-9999-9999-9999-999999999999}') < 0);
end;

procedure THttpServiceMarshallingTest.DeletingACurveAddressesItByHandle;
begin
    FSvc.Reply('curves',
        '{"ok":true,"curves":[{"id":"{11111111-1111-1111-1111-111111111111}"},' +
        '{"id":"{22222222-2222-2222-2222-222222222222}"}]}');
    FSvc.DeleteCurve(1);
    //  BY HANDLE, not by the index the caller held: the model's order is
    //  derived - it follows the intervals and the picks inside them - so an
    //  index that survived an edit names a different curve, and deleting the
    //  wrong one is the worst outcome available here.
    AssertTrue('the member route, carrying the handle: ' + FSvc.LastUrl,
        Pos('/points/positions/', FSvc.LastUrl) > 0);
    AssertTrue('and it is the second curve''s handle: ' + FSvc.LastUrl,
        Pos('22222222', FSvc.LastUrl) > 0);
end;

procedure THttpServiceMarshallingTest.DeletingACurveTheModelHasNotGotSendsNothing;
var
    Before: string;
    Refused: boolean;
begin
    FSvc.Reply('curves',
        '{"ok":true,"curves":[{"id":"{11111111-1111-1111-1111-111111111111}"}]}');
    //  Force the curves to be read, so LastUrl holds something to compare
    //  against.
    FSvc.GetCurveInstanceId(0);
    Before := FSvc.LastUrl;

    Refused := False;
    try
        FSvc.DeleteCurve(7);
    except
        on E: EUserException do
            Refused := True;
    end;
    //  REFUSED BEFORE SENDING. An index that resolves to no handle would
    //  otherwise become a request naming whatever sits at that position now.
    AssertTrue('refused', Refused);
    AssertEquals('and nothing was sent', Before, FSvc.LastUrl);
end;

{ ---- the special (user-defined) curve -------------------------------------- }

procedure THttpServiceMarshallingTest.TheSpecialCurveExpressionIsSent;
var
    Sent: string;
begin
    FSvc.SetSpecialCurveParameters('a*exp(-x*x)', nil);
    Sent := FSvc.Log.AsText;
    AssertTrue('the route: ' + Sent, Pos('special-params', Sent) > 0);
    AssertTrue('and the expression', Pos('exp(-x*x)', Sent) > 0);
end;

{ ---- the actions ----------------------------------------------------------- }

procedure THttpServiceMarshallingTest.EveryActionIsPostedUnderItsOwnName;
var
    Sent: string;
begin
    //  A sweep over the verb table. Each is a one-line RunAction, and the mistake
    //  is a copy-paste that leaves two verbs pointing at one name - so a menu item
    //  silently does something else. Named individually so a new verb has to be
    //  added here too.
    FSvc.SmoothProfile;
    FSvc.SubtractBackground(True);
    FSvc.DoAllAutomatically;
    FSvc.MinimizeNumberOfCurves;
    FSvc.ComputeCurveBounds;
    FSvc.ComputeBackgroundPoints;
    FSvc.ComputeCurvePositions;
    FSvc.SelectAllPointsAsCurvePositions;
    FSvc.StopAsyncOper;
    Sent := FSvc.Log.AsText;
    AssertTrue('smooth: ' + Sent, Pos('smooth-profile', Sent) > 0);
    AssertTrue('subtract background', Pos('subtract-background', Sent) > 0);
    AssertTrue('do all', Pos('do-all-automatically', Sent) > 0);
    AssertTrue('minimize curve count', Pos('minimize-number-of-curves', Sent) > 0);
    AssertTrue('curve bounds', Pos('compute-curve-bounds', Sent) > 0);
    AssertTrue('background points', Pos('compute-background-points', Sent) > 0);
    AssertTrue('curve positions', Pos('compute-curve-positions', Sent) > 0);
    AssertTrue('select all', Pos('select-all-points-as-curve-positions', Sent) > 0);
    AssertTrue('stop', Pos('stop', Sent) > 0);
end;

procedure THttpServiceMarshallingTest.SubtractBackgroundCarriesItsAutoFlag;
var
    Sent: string;
begin
    //  The only action with a body, and the flag decides whether the user's picked
    //  background is used or one is computed. Sending the wrong one silently
    //  discards what the user marked.
    //  The body is written by hand rather than by fpjson, so it is compact -
    //  no spaces around the colon. Matched as it is actually sent.
    FSvc.SubtractBackground(True);
    Sent := FSvc.Log.AsText;
    AssertTrue('auto true was sent: ' + Sent, Pos('"auto":true', Sent) > 0);

    FSvc.Log.Clear;
    FSvc.SubtractBackground(False);
    Sent := FSvc.Log.AsText;
    AssertTrue('auto false was sent: ' + Sent, Pos('"auto":false', Sent) > 0);
end;

{ ---- the remaining derived point sets -------------------------------------- }

procedure THttpServiceMarshallingTest.TheDerivedProfilesAreReadFromTheirOwnRoutes;
begin
    //  What the engine MADE of the picks, as opposed to the picks themselves - the
    //  calc-profile/profile distinction. Reading one for the other shows the user
    //  their input where the result belongs, or the reverse.
    FSvc.Reply('calc-profile', '{"title":"c","x":[1],"y":[1]}');
    FSvc.Reply('delta-profile', '{"title":"d","x":[1,2],"y":[1,2]}');
    FSvc.Reply('calc-positions', '{"title":"p","x":[1,2,3],"y":[1,2,3]}');
    FSvc.Reply('selected-interval', '{"title":"i","x":[1,2,3,4],"y":[1,2,3,4]}');
    AssertEquals('calculated profile', 1, CountOf(FSvc.GetCalcProfilePointsSet));
    AssertEquals('delta profile', 2, CountOf(FSvc.GetDeltaProfilePointsSet));
    AssertEquals('resulted positions', 3, CountOf(FSvc.GetResultedCurvePositions));
    AssertEquals('selected interval', 4, CountOf(FSvc.GetSelectedProfileInterval));
end;

{ ---- the model read back as objects ---------------------------------------- }

procedure THttpServiceMarshallingTest.TheCurvesAreBuiltFromTheirPoints;
var
    Curves: TSelfCopiedCompList;
begin
    //  ONE read of the model, then one request per curve BY HANDLE. Asking by
    //  index needed the count and the points to agree about the order across
    //  several requests; a handle does not care what the order is.
    FSvc.Reply('curves',
        '{"ok":true,"curves":[{"id":"' + ID1 + '"},{"id":"' + ID2 + '"}]}');
    FSvc.Reply('points', '{"title":"Gauss","x":[1,2],"y":[10,20]}');
    Curves := FSvc.GetCurves;
    try
        AssertEquals('one series per curve', 2, Curves.Count);
        AssertEquals('with its points',
            2, TNamedPointsSet(Curves.Items[0]).PointsCount);
        AssertEquals('and its type name',
            'Gauss', TNamedPointsSet(Curves.Items[0]).FTitle);
    finally
        Curves.Free;
    end;
end;

procedure THttpServiceMarshallingTest.ACurveWithNoHandleIsRefusedNotSkipped;
var
    Curves: TSelfCopiedCompList;
    Raised: boolean;
begin
    //  REPORTED, not skipped. A curve with no handle cannot be addressed, so
    //  dropping it quietly would leave the chart short of curves with nothing to
    //  say why - the model would look wrong rather than broken. The message names
    //  an older server, because that is the likely cause.
    FSvc.Reply('curves', '{"ok":true,"curves":[{"id":""}]}');
    Raised := False;
    Curves := nil;
    try
        Curves := FSvc.GetCurves;
    except
        on E: EUserException do
            Raised := True;
    end;
    Curves.Free;
    AssertTrue('refused with a message for the user', Raised);
end;

procedure THttpServiceMarshallingTest.CurveAttributesCarryEveryParameter;
var
    L: TMSCRCurveList;
    CP: Curve_parameters;
begin
    FSvc.Reply('curves',
        '{"ok":true,"curves":[{"id":"' + ID1 + '","params":[' +
        '{"name":"A","type":1,"value":2.5,"error":0.25},' +
        '{"name":"sigma","type":1,"value":1.5,"error":-1}]}]}');
    L := FSvc.GetCurveAttributes;
    try
        AssertEquals('one curve', 1, L.Count);
        CP := Curve_parameters(L.Items[0]);
        AssertEquals('both parameters', 2, CP.Params.Count);
        AssertEquals('the first by name', 2.5,
            CP.FindByName('A').Value, 1e-12);
        AssertEquals('its uncertainty', 0.25,
            CP.FindByName('A').Error, 1e-12);
        //  A NEGATIVE ERROR MEANS "none estimated", and it has to survive as such:
        //  read as 0 it would claim the fit was certain about that parameter.
        AssertTrue('and an absent one stays negative',
            CP.FindByName('sigma').Error < 0);
    finally
        L.Free;
    end;
end;

procedure THttpServiceMarshallingTest.AParameterKindDecidesHowItsValueIsRead;
var
    L: TMSCRCurveList;
    CP: Curve_parameters;
begin
    //  DECLARED, not guessed: 'kind' says what 'value' is, so a label that happens
    //  to read as a number cannot be mistaken for one. Without it a text parameter
    //  holding "3" would arrive as the quantity 3 and the fit would vary it.
    FSvc.Reply('curves',
        '{"ok":true,"curves":[{"id":"' + ID1 + '","params":[' +
        '{"name":"label","type":1,"kind":"text","value":"3"},' +
        '{"name":"A","type":1,"value":3}]}]}');
    L := FSvc.GetCurveAttributes;
    try
        CP := Curve_parameters(L.Items[0]);
        AssertFalse('the text one is not numeric',
            CP.FindByName('label').IsNumeric);
        AssertTrue('the other one is', CP.FindByName('A').IsNumeric);
    finally
        L.Free;
    end;
end;

procedure THttpServiceMarshallingTest.TheInstanceHandleReachesTheAttributes;
var
    L: TMSCRCurveList;
begin
    //  Carried so the parameters grid and the chart are talking about the same
    //  curve. Lost, a click on a row would edit whichever curve happened to be at
    //  that index.
    FSvc.Reply('curves',
        '{"ok":true,"curves":[{"id":"' + ID1 + '","params":[]}]}');
    L := FSvc.GetCurveAttributes;
    try
        //  BRACELESS on the wire, while GUIDToString writes braces - and
        //  TryStrToCurveInstanceId accepts either, which is why a braced id in the
        //  reply above parses. Asserted in the wire form because that is the one
        //  both processes exchange.
        AssertEquals('the handle came with it',
            Copy(ID1, 2, Length(ID1) - 2),
            CurveInstanceIdToWire(Curve_parameters(L.Items[0]).FInstanceId));
    finally
        L.Free;
    end;
end;

procedure THttpServiceMarshallingTest.TheSpecialCurveParametersAreRead;
var
    CP: Curve_parameters;
begin
    FSvc.Reply('special-params',
        '{"ok":true,"params":[{"name":"a","value":1.5,"type":1},' +
        '{"name":"b","value":2.5,"type":1}]}');
    CP := FSvc.GetSpecialCurveParameters;
    try
        AssertEquals('both', 2, CP.Params.Count);
        AssertEquals('a', 1.5, CP.FindByName('a').Value, 1e-12);
        AssertEquals('b', 2.5, CP.FindByName('b').Value, 1e-12);
    finally
        CP.Free;
    end;
end;

procedure THttpServiceMarshallingTest.AnEmptyModelReadsAsNoCurves;
var
    L: TMSCRCurveList;
    Curves: TSelfCopiedCompList;
begin
    //  Nothing built yet is the state the program starts in, so it must not read
    //  as a failure - and it must not read as one curve either.
    FSvc.Reply('curves', '{"ok":true,"curves":[]}');
    L := FSvc.GetCurveAttributes;
    try
        AssertEquals('no attributes', 0, L.Count);
    finally
        L.Free;
    end;
    Curves := FSvc.GetCurves;
    try
        AssertEquals('and no series', 0, Curves.Count);
    finally
        Curves.Free;
    end;
end;

{ ---- the handles a pick carries, over the wire ---------------------------- }

procedure THttpServiceMarshallingTest.PushingPicksWithHandlesSendsThemBesideTheCoordinates;
var
    PS: TTitlePointsSet;
    Ids: TCurveInstanceIdList;
begin
    //  WITHOUT THIS the client can restore where the picks were and not which
    //  curve each one stands for, so every saved value orphans on reopening -
    //  a fit that quietly starts over.
    PS := TTitlePointsSet.Create(nil);
    PS.AddNewPoint(6, 20);
    PS.AddNewPoint(14, 20);
    SetLength(Ids, 2);
    Ids[0] := '0a0a0a0a-1111-2222-3333-444444444444';
    Ids[1] := '0b0b0b0b-1111-2222-3333-444444444444';
    //  Takes ownership, as the engine's own setter does.
    FSvc.SetCurvePositions(PS, Ids);
    AssertTrue('the field went out: ' + FSvc.LastBody,
        Pos('"ids"', FSvc.LastBody) > 0);
    AssertTrue('with the first handle', Pos(Ids[0], FSvc.LastBody) > 0);
    AssertTrue('and the second', Pos(Ids[1], FSvc.LastBody) > 0);
    AssertTrue('to the picks route', Pos('/positions', FSvc.LastUrl) > 0);
end;

procedure THttpServiceMarshallingTest.PushingPicksWithNoHandlesSendsNoIdsFieldAtAll;
var
    PS: TTitlePointsSet;
begin
    //  ADDITIVE. An ordinary interactive edit carries none, and its message has
    //  to be what it always was - a profile of a hundred thousand points must
    //  not grow an empty array per push because the picks gained a field.
    PS := TTitlePointsSet.Create(nil);
    PS.AddNewPoint(6, 20);
    FSvc.SetCurvePositions(PS);
    AssertEquals('no ids field: ' + FSvc.LastBody, 0,
        Pos('"ids"', FSvc.LastBody));
end;

procedure THttpServiceMarshallingTest.AProfilePushNeverCarriesHandles;
var
    PS: TTitlePointsSet;
begin
    //  A curve's identity is issued to the pick it is seeded from, so a pick
    //  can be named and a profile sample cannot. The server refuses ids on this
    //  route by name; the client must never put the question.
    PS := TTitlePointsSet.Create(nil);
    try
        PS.AddNewPoint(1.5, 10.5);
        FSvc.SetProfilePointsSet(PS);
        AssertEquals('none: ' + FSvc.LastBody, 0, Pos('"ids"', FSvc.LastBody));
    finally
        PS.Free;
    end;
end;

procedure THttpServiceMarshallingTest.TheHandlesComeBackWithThePicksInOneRequest;
var
    Ids: TCurveInstanceIdList;
begin
    //  IN ONE REQUEST, because that is the reply that carries them. Reading the
    //  picks and their handles separately would be two requests that an edit in
    //  between could make disagree.
    FSvc.Reply('positions',
        '{"ok":true,"title":"p","x":[6,14],"y":[20,20],' +
        '"ids":["0a0a0a0a-1111-2222-3333-444444444444",' +
        '"0b0b0b0b-1111-2222-3333-444444444444"]}');
    Ids := FSvc.GetCurvePositionIds;
    AssertEquals('one per pick', 2, Length(Ids));
    AssertEquals('', '0a0a0a0a-1111-2222-3333-444444444444', Ids[0]);
    AssertEquals('', '0b0b0b0b-1111-2222-3333-444444444444', Ids[1]);
end;

procedure THttpServiceMarshallingTest.TheWholeModelIsWrittenBackInOneCall;
var
    Entries: TCurveValuesList;
begin
    //  ONE REQUEST FOR THE MODEL. The per-parameter route rebuilds the whole
    //  model on every write, so restoring a ten-curve fit through it would be
    //  fifty requests and fifty rebuilds.
    FSvc.Reply('curves',
        '{"ok":true,"curves":[{"id":"0a0a0a0a-1111-2222-3333-444444444444",' +
        '"fitted":false,"params":[{"name":"sigma","value":2.5,"error":-1}]}]}');
    SetLength(Entries, 1);
    Entries[0].CurveIndex := 0;
    Entries[0].Fitted := True;
    SetLength(Entries[0].Params, 1);
    Entries[0].Params[0].Name := 'sigma';
    Entries[0].Params[0].Value := 0.37;
    Entries[0].Params[0].Error := 0.004;
    FSvc.SetCurveValues(Entries);

    AssertTrue('addressed by handle, not by index: ' + FSvc.LastBody,
        Pos('0a0a0a0a-1111-2222-3333-444444444444', FSvc.LastBody) > 0);
    AssertTrue('the parameter is named', Pos('"sigma"', FSvc.LastBody) > 0);
    AssertTrue('and its value went out at full precision: ' + FSvc.LastBody,
        Pos('3.7', FSvc.LastBody) > 0);
    AssertTrue('to the curves collection', Pos('/curves', FSvc.LastUrl) > 0);
end;

procedure THttpServiceMarshallingTest.ThatCallCarriesWhetherAnOptimiserProducedTheValues;
var
    Entries: TCurveValuesList;
begin
    //  THE ONE THING THE PER-PARAMETER ROUTE CANNOT SAY. It cannot be derived
    //  from the values - every instance has some from the moment it is placed -
    //  and without it a restored seed is indistinguishable from a restored fit.
    FSvc.Reply('curves',
        '{"ok":true,"curves":[{"id":"0a0a0a0a-1111-2222-3333-444444444444",' +
        '"fitted":false,"params":[]}]}');
    SetLength(Entries, 1);
    Entries[0].CurveIndex := 0;
    Entries[0].Fitted := True;
    FSvc.SetCurveValues(Entries);
    AssertTrue('the flag crossed: ' + FSvc.LastBody,
        Pos('"fitted"', FSvc.LastBody) > 0);
    AssertTrue('as true', Pos('true', FSvc.LastBody) > 0);
end;

procedure THttpServiceMarshallingTest.WhetherACurveWasFittedIsReadFromTheCurvesReply;
begin
    //  From the SAME reply the handles and the parameters come from, so one
    //  request answers for the whole model rather than one per curve.
    FSvc.Reply('curves',
        '{"ok":true,"curves":[' +
        '{"id":"0a0a0a0a-1111-2222-3333-444444444444","fitted":true,"params":[]},' +
        '{"id":"0b0b0b0b-1111-2222-3333-444444444444","fitted":false,"params":[]}]}');
    AssertTrue('the fitted one', FSvc.IsCurveFitted(0));
    AssertFalse('and the one that was only placed', FSvc.IsCurveFitted(1));
end;

procedure THttpServiceMarshallingTest.ACurveTheReplyDoesNotHaveIsNotReportedAsFitted;
begin
    //  FALSE, not an exception and not True. An index past the end means the
    //  model moved on under the caller, and the safe reading of "I cannot
    //  tell" is the one that does not claim a fit happened - claiming one
    //  refuses edits to protect work that may not exist.
    FSvc.Reply('curves', '{"ok":true,"curves":[]}');
    AssertFalse('nothing there', FSvc.IsCurveFitted(0));
    AssertFalse('nor below zero', FSvc.IsCurveFitted(-1));
    FSvc.Reply('curves', '{"ok":true}');
    AssertFalse('nor when the reply carries no curves at all',
        FSvc.IsCurveFitted(0));
end;

procedure THttpServiceMarshallingTest.WritingValuesForACurveTheModelHasLostIsRefused;
var
    Entries: TCurveValuesList;
    Raised: boolean;
begin
    //  The index is resolved to a handle before anything is sent, so a model
    //  that moved on under the caller is a refusal here rather than a request
    //  addressed at whatever now sits in that position - the fallthrough both
    //  curve routes used to have.
    FSvc.Reply('curves', '{"ok":true,"curves":[]}');
    SetLength(Entries, 1);
    Entries[0].CurveIndex := 3;
    Raised := False;
    try
        FSvc.SetCurveValues(Entries);
    except
        on E: EUserException do
            Raised := True;
    end;
    AssertTrue('refused', Raised);
end;

procedure THttpServiceMarshallingTest.AReplyCarryingNoModuleStatesIsNotAFailure;
begin
    //  The published framework's own case: no module keeps anything. An empty
    //  answer and a malformed one both mean "nothing to restore", and neither
    //  may stop a save.
    FSvc.Reply('module-states', '{"ok":true,"states":[]}');
    AssertEquals('none', 0, Length(FSvc.GetModuleProjectStates));
    FSvc.Reply('module-states', '{"ok":true}');
    AssertEquals('nor when the field is absent', 0,
        Length(FSvc.GetModuleProjectStates));
end;

procedure THttpServiceMarshallingTest.AReplyCarryingNoHandlesLeavesThePicksUnnamed;
begin
    //  Every server that predates the field answers like this, and it has to
    //  mean "no handles" rather than a decode failure - the picks are still
    //  perfectly good.
    FSvc.Reply('positions', '{"ok":true,"x":[6,14],"y":[20,20]}');
    AssertEquals('no handles', 0, Length(FSvc.GetCurvePositionIds));
end;

procedure THttpServiceMarshallingTest.EveryModulesProjectStateArrivesInOneRequest;
var
    States: TModuleStateArray;
begin
    //  Collected server-side, over the registry the problem's sessions were
    //  made from - so the client asks once and names no module.
    FSvc.Reply('module-states',
        '{"ok":true,"states":[{"module":"sample","content":"{\"marks\":[1]}"}]}');
    States := FSvc.GetModuleProjectStates;
    AssertEquals('one module kept something', 1, Length(States));
    AssertEquals('', 'sample', States[0].Module);
    AssertEquals('and its document came through as text', '{"marks":[1]}',
        States[0].Content);
end;


initialization
    //  A unit test: the real marshalling over a canned reply, no socket.
    RegisterTest('unit', THttpServiceMarshallingTest);
end.
