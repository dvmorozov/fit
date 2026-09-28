// SPDX-License-Identifier: GPL-3.0-or-later
{ Tests for the seam through which an explanation gets drawn.

  WHY THESE EXIST. The HTML component the desktop program draws explanations
  with pulls in the printing package, and that package does not compile against
  the headless widget set this suite is built with - while this suite links the
  main form. So the form names only this seam, and the program registers the
  component behind it. The seam has one job, and the failure worth catching is
  the quiet one: a form built with no view registered must get NO view - which
  it checks for and says so - rather than a fault, and a registered factory must
  be the one that builds the view, handed the owner and parent the form gave. }
unit testcase_explain_view;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Controls, fpcunit, testregistry, int_explain_view;

type
    TExplainViewTest = class(TTestCase)
    protected
        procedure TearDown; override;
    published
        procedure WithNoFactoryNoViewIsCreated;
        procedure WithNoFactoryNoneIsSaidToBeRegistered;
        procedure ARegisteredFactoryBuildsTheView;
        procedure ItIsHandedTheOwnerAndParentTheFormGave;
        procedure RegisteringAgainReplacesTheFactory;
        procedure TheSameExplanationShownAgainIsNotRedrawn;
        procedure ADifferentExplanationIsDrawn;
        procedure TheControlAndTheLinkHandlerAreTheViewsOwn;
        procedure ASizeReachesTheViewThatDrawsAtIt;
        procedure ASizeIsNotAPageAndDoesNotResendOne;
    end;

implementation

type
    TRecordingView = class(TObject, IExplainView)
    public
        Tag_: string;
        Drawn: longint;
        LastHtml: string;
        LinkSet: boolean;
        Zoom: longint;
        function Control: TWinControl;
        procedure ShowHtml(const AHtml: string);
        procedure SetOnLink(AHandler: TExplainLinkEvent);
        procedure SetZoom(APercent: longint);
    end;

    { A link handler is a method, so the test needs something to own one. }
    TLinkTarget = class
    public
        procedure Link(const AHref: string);
    end;

var
    GView1, GView2: TRecordingView;
    GLinkTarget: TLinkTarget;
    GOwnerSeen: TComponent;
    GParentSeen: TWinControl;

procedure TLinkTarget.Link(const AHref: string);
begin
end;

function TRecordingView.Control: TWinControl;
begin
    Result := nil;
end;

procedure TRecordingView.ShowHtml(const AHtml: string);
begin
    Inc(Drawn);
    LastHtml := AHtml;
end;

procedure TRecordingView.SetOnLink(AHandler: TExplainLinkEvent);
begin
    LinkSet := Assigned(AHandler);
end;

procedure TRecordingView.SetZoom(APercent: longint);
begin
    Zoom := APercent;
end;

function FirstFactory(AOwner: TComponent; AParent: TWinControl): IExplainView;
begin
    GOwnerSeen := AOwner;
    GParentSeen := AParent;
    Result := GView1;
end;

function SecondFactory(AOwner: TComponent; AParent: TWinControl): IExplainView;
begin
    Result := GView2;
end;

procedure TExplainViewTest.TearDown;
begin
    //  The registry is process-wide; nil is how the program says "no view", and
    //  leaving a test's factory behind would hand the next test its view.
    RegisterExplainViewFactory(nil);
    GOwnerSeen := nil;
    GParentSeen := nil;
end;

procedure TExplainViewTest.WithNoFactoryNoViewIsCreated;
begin
    RegisterExplainViewFactory(nil);
    AssertTrue(CreateExplainView(nil, nil) = nil);
end;

procedure TExplainViewTest.WithNoFactoryNoneIsSaidToBeRegistered;
begin
    RegisterExplainViewFactory(nil);
    AssertFalse(ExplainViewFactoryRegistered);
end;

procedure TExplainViewTest.ARegisteredFactoryBuildsTheView;
var
    View: IExplainView;
begin
    RegisterExplainViewFactory(@FirstFactory);
    AssertTrue(ExplainViewFactoryRegistered);
    View := CreateExplainView(nil, nil);
    AssertTrue('a view', Assigned(View));
    GView1.Drawn := 0;
    View.ShowHtml('<p>a</p>');
    AssertEquals('the factory''s view is the one drawn on', 1, GView1.Drawn);
end;

procedure TExplainViewTest.ItIsHandedTheOwnerAndParentTheFormGave;
var
    Owner: TComponent;
begin
    Owner := TComponent.Create(nil);
    try
        RegisterExplainViewFactory(@FirstFactory);
        CreateExplainView(Owner, nil);
        AssertTrue(GOwnerSeen = Owner);
        AssertTrue(GParentSeen = nil);
    finally
        Owner.Free;
    end;
end;

procedure TExplainViewTest.RegisteringAgainReplacesTheFactory;
var
    View: IExplainView;
begin
    //  Asked by what it draws on, not by object identity: the view handed back
    //  is wrapped so it redraws only on a change.
    RegisterExplainViewFactory(@FirstFactory);
    RegisterExplainViewFactory(@SecondFactory);
    GView1.Drawn := 0;
    GView2.Drawn := 0;
    View := CreateExplainView(nil, nil);
    View.ShowHtml('<p>later</p>');
    AssertEquals('the later factory''s view is drawn on', 1, GView2.Drawn);
    AssertEquals('and not the earlier one''s', 0, GView1.Drawn);
end;


procedure TExplainViewTest.TheSameExplanationShownAgainIsNotRedrawn;
var
    View: IExplainView;
begin
    //  THE CRASH THIS PREVENTS: every panel refill asked the pane to draw the
    //  explanation it already showed, the HTML component replaced its document
    //  each time, and a refill during a scroll-bar drag freed the control the
    //  drag was being delivered to.
    RegisterExplainViewFactory(@FirstFactory);
    GView1.Drawn := 0;
    View := CreateExplainView(nil, nil);
    View.ShowHtml('<p>same</p>');
    View.ShowHtml('<p>same</p>');
    View.ShowHtml('<p>same</p>');
    AssertEquals('drawn once', 1, GView1.Drawn);
end;

procedure TExplainViewTest.ADifferentExplanationIsDrawn;
var
    View: IExplainView;
begin
    RegisterExplainViewFactory(@FirstFactory);
    GView1.Drawn := 0;
    View := CreateExplainView(nil, nil);
    View.ShowHtml('<p>one</p>');
    View.ShowHtml('<p>two</p>');
    View.ShowHtml('<p>one</p>');
    AssertEquals('every change is drawn', 3, GView1.Drawn);
    AssertEquals('<p>one</p>', GView1.LastHtml);
end;

procedure TExplainViewTest.TheControlAndTheLinkHandlerAreTheViewsOwn;
var
    View: IExplainView;
begin
    RegisterExplainViewFactory(@FirstFactory);
    GView1.LinkSet := False;
    View := CreateExplainView(nil, nil);
    AssertTrue('the view''s own control', View.Control = GView1.Control);
    View.SetOnLink(@GLinkTarget.Link);
    AssertTrue('the handler reaches the view', GView1.LinkSet);
end;

procedure TExplainViewTest.ASizeReachesTheViewThatDrawsAtIt;
var
    View: IExplainView;
begin
    RegisterExplainViewFactory(@FirstFactory);
    GView1.Zoom := 0;
    View := CreateExplainView(nil, nil);
    View.SetZoom(140);
    AssertEquals('the size arrives whole', 140, GView1.Zoom);
end;

procedure TExplainViewTest.ASizeIsNotAPageAndDoesNotResendOne;
var
    View: IExplainView;
begin
    //  The wrapper exists to stop the same explanation being drawn twice, and
    //  a change of size must not defeat that: the view below redraws what it
    //  is holding, so re-sending the page here would put the scroll bar back
    //  under exactly the replacement this seam was built to avoid.
    RegisterExplainViewFactory(@FirstFactory);
    GView1.Drawn := 0;
    View := CreateExplainView(nil, nil);
    View.ShowHtml('<p>same</p>');
    View.SetZoom(200);
    View.ShowHtml('<p>same</p>');
    AssertEquals('drawn once', 1, GView1.Drawn);
    AssertEquals('and resized once', 200, GView1.Zoom);
end;

initialization
    GView1 := TRecordingView.Create;
    GView2 := TRecordingView.Create;
    GLinkTarget := TLinkTarget.Create;
    RegisterTest('unit', TExplainViewTest);

finalization
    GView1.Free;
    GView2.Free;
    GLinkTarget.Free;
end.
