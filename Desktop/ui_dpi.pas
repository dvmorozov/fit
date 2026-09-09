// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Decides what pixel density the user interface is laid out for.)

@author(Dmitry Morozov dvmorozov@hotmail.com,
LinkedIn https://ru.linkedin.com/pub/dmitry-morozov/59/90a/794,
Facebook https://www.facebook.com/profile.php?id=100004082021870)
}
unit ui_dpi;

{$mode objfpc}{$H+}

interface

uses
    Forms,
    //  The decisions - parsing a scale factor, reading a resource database,
    //  what counts as a usable ppi, which source wins - live there, where they
    //  can be tested without a display. This unit is what talks to the widget
    //  set, the environment and gdk. DesignPPI is re-exported so that callers
    //  need not know the split.
    ui_scaling;

const
    { The ppi every pixel literal in this application is quoted at. Scale96ToForm
      and Scale96ToFont convert from it; so does everything in this unit. }
    DesignPPI = ui_scaling.DesignPPI;

{ Works out what ppi the interface should be laid out for and tells the LCL.

  ON A QT OR WINDOWS BUILD THIS IS ALMOST ALWAYS A NO-OP, and should be: those
  widget sets know the display's ppi, and the only thing that overrides them is
  an explicit /DPI. The guesswork exists for GTK2, which is blind to a scaled
  desktop - see the Linux notes in docs/contributing/building.md. It stays in
  place because a machine with no Qt widget set installed still builds on GTK2.

  CALL IT ONCE, AFTER Application.Initialize AND BEFORE THE FIRST FORM IS
  CREATED. Initialize is what asks the widget set for the screen, so there is
  nothing to correct before it; and every form reads the answer while it is
  being constructed, so a form built before this call is built at the wrong
  size and stays there. }
procedure ApplyUiPixelsPerInch;

{ An explicit ppi that overrides everything this unit would otherwise work out;
  0 (the default) means "work it out". Set it before ApplyUiPixelsPerInch - the
  command line is read before Initialize, which is early enough. }
procedure SetUiPixelsPerInchOverride(APPI: integer);

{ Writes what a form actually ended up scaled to.

  Not decoration. Whether the interface scaled is otherwise only answerable by
  looking at it, and "it still looks small" cannot tell a form that never scaled
  from a display that is genuinely 96 ppi from a binary that was never rebuilt.
  Three numbers in the log settle it: the ppi the form was designed at, the ppi
  it scaled itself to, and the size its font came out. }
procedure LogFormScaling(AForm: TCustomForm);

{ Walks every form that exists and logs each caption too wide for the control
  holding it.

  A DIAGNOSTIC, not a fix. Most of this application's dialogs are modal and
  reachable only through a menu, so "does the text still fit?" could otherwise
  only be answered by opening each one by hand on a display of each density -
  which is how a clipped caption survives for years. Measuring is cheap and
  needs no window to be shown. Run it with /CHECK_UI and read the log - which
  also asks the window whether its surfaces agree (ui_selfcheck). }
procedure ReportClippedCaptions;

implementation

uses
    SysUtils, Controls, ComCtrls, StdCtrls, ExtCtrls, Graphics, log
{$IFDEF LCLGtk2}
    //  StrUtils is here rather than above because PosEx is used only by the
    //  GTK2 resource-database reader; on any other widget set it is dead weight
    //  and the compiler says so.
    , StrUtils, glib2, gdk2
{$ENDIF}
    ;

var
    FOverridePPI: integer = 0;

procedure SetUiPixelsPerInchOverride(APPI: integer);
begin
    FOverridePPI := APPI;
end;

{$IFDEF LCLGtk2}
{ A named environment variable holding a toolkit scale factor, as a ppi. Reading
  the variable is this unit's business; what the text means is ui_scaling's. }
function PPIFromScaleEnv(const AName: string): integer;
begin
    Result := PPIFromScaleFactorText(GetEnvironmentVariable(AName));
end;

{ Reads Xft.dpi out of the X resource database.

  THIS IS THE ONE THAT MATTERS ON A SCALED LINUX DESKTOP, and the reason this
  unit exists at all. GTK2 has no idea the desktop is scaled: it predates the
  whole notion, and what Plasma and GNOME publish through XSETTINGS is
  Gdk/WindowScalingFactor and Gdk/UnscaledDPI - the GTK3 scheme. GTK2
  understands neither, only Xft/DPI, which those desktops no longer send. So
  gdk_screen_get_resolution returns -1, the LCL falls back to dividing the pixel
  size of the screen by its millimetre size, and on a 3840-pixel 1016 mm display
  that is exactly 96. The application is then told, with complete confidence,
  that a 200% desktop is a 100% one.

  Xft.dpi is the cross-toolkit answer and it is correct: the same desktop that
  withholds the XSETTINGS key still writes "Xft.dpi: 192" into the root window's
  RESOURCE_MANAGER property, because that is what Qt and Xft read. This reads
  the same property, through gdk, so it needs nothing the gtk2 widget set has
  not already linked. }
function XftDpiFromResourceManager(out APPI: integer): boolean;
const
    MaxBytes = 64 * 1024;
var
    ResourceManager, StringType, ActualType: TGdkAtom;
    ActualFormat, ActualLength: gint;
    Data: PGuchar;
    Database: string;
begin
    Result := False;
    APPI := 0;

    //  only_if_exists: if nothing on this display has ever set a resource
    //  database there is no atom to intern and nothing to read.
    ResourceManager := gdk_atom_intern('RESOURCE_MANAGER', True);
    StringType := gdk_atom_intern('STRING', True);
    if (ResourceManager = 0) or (StringType = 0) then
        Exit;

    Data := nil;
    if not gdk_property_get(gdk_get_default_root_window, ResourceManager,
        StringType, 0, MaxBytes, 0, @ActualType, @ActualFormat,
        @ActualLength, @Data) then
        Exit;
    if Data = nil then
        Exit;
    try
        SetString(Database, PAnsiChar(Data), ActualLength);
    finally
        g_free(Data);
    end;

    //  Reading the property is this unit's business; what the database says is
    //  ui_scaling's, and is tested there.
    Result := XftDpiFromDatabase(Database, APPI);
end;
{$ENDIF}

{ What the interface should be laid out for, and where the answer came from.
  ASource is filled in whether or not the answer differs from the widget set's,
  because "we asked and the display really is 96" and "we never found out" look
  identical in a window and must not look identical in the log. }
function DetectUiPixelsPerInch(out ASource: TPPISource): integer;
{$IFDEF LCLGtk2}
var
    Xft: integer;
{$ENDIF}
begin
    if FOverridePPI > 0 then
    begin
        ASource := psOverride;
        Exit(FOverridePPI);
    end;

    Result := PPIFromText(GetEnvironmentVariable('FIT_UI_DPI'));
    if Result > 0 then
    begin
        ASource := psEnvironment;
        Exit;
    end;

{$IFDEF LCLGtk2}
    //  EVERYTHING BELOW IS FOR GTK2 ONLY, and guessing behind a widget set that
    //  is not blind would be worse than not guessing at all. Qt already applies
    //  QT_SCALE_FACTOR itself and already knows the display's ppi; reading the
    //  same signals a second time here and handing the product back to the LCL
    //  is how an application ends up scaled twice. Where the widget set can
    //  answer, its answer is the answer.
    //
    //  What a Qt or GTK3 application on this desktop would be scaling by. Set by
    //  the session on some desktops and by the user on others; when it is there
    //  it is authoritative, because it is the number the rest of the desktop is
    //  already using.
    Result := PPIFromScaleEnv('GDK_SCALE');
    if Result > 0 then
    begin
        ASource := psGdkScale;
        Exit;
    end;
    Result := PPIFromScaleEnv('QT_SCALE_FACTOR');
    if Result > 0 then
    begin
        ASource := psQtScaleFactor;
        Exit;
    end;

    if XftDpiFromResourceManager(Xft) then
    begin
        ASource := psXftResource;
        Exit(Xft);
    end;
{$ENDIF}

    ASource := psWidgetSet;
    Result := Screen.PixelsPerInch;
end;

procedure LogFormScaling(AForm: TCustomForm);
begin
    if AForm = nil then
        Exit;
    WriteLog(Format(
        '%s: designed at %d ppi, laid out at %d ppi (monitor says %d), ' +
        '%dx%d, font height %d',
        [AForm.Name, AForm.DesignTimePPI, AForm.PixelsPerInch,
         AForm.Monitor.PixelsPerInch, AForm.Width, AForm.Height,
         AForm.Font.Height]), Notification);
end;

procedure ReportClippedCaptions;
var
    Measure: TBitmap;
    Reported: integer;

    procedure CheckForm(AForm: TCustomForm);
    var
        j, Needed: integer;
        C: TControl;
    begin
        for j := 0 to AForm.ComponentCount - 1 do
        begin
            if not (AForm.Components[j] is TControl) then
                Continue;
            C := TControl(AForm.Components[j]);
            //  An autosizing control cannot be too narrow for its own text, and
            //  an empty or zero-width one is not a layout, it is a control being
            //  hidden (the edit balloons are set to 0x0 deliberately).
            if C.AutoSize or (C.Caption = '') or (C.Width <= 0) then
                Continue;
            //  A tool button's caption is its hint and its action's name, not
            //  something drawn, unless the bar is showing captions. Sixteen of
            //  them were the first thing this check reported, and every one was
            //  an icon that fits perfectly well.
            if (C is TToolButton) and (C.Parent is TToolBar) and
                not TToolBar(C.Parent).ShowCaptions then
                Continue;
            //  AND NEITHER IS A CONTROL THAT DRAWS NO CAPTION AT ALL. A list
            //  box and a splitter have no text of their own, but
            //  TControl.Caption falls back to the component's NAME - so a
            //  control created in code and never given a caption is measured
            //  against a string it will never show, and reports as clipped. The
            //  same reasoning as the tool button above: this check exists to
            //  find text the user cannot read, and there is none here.
            if (C is TCustomListBox) or (C is TSplitter) or
                (C is TScrollBox) or (C is TCustomTreeView) or
                //  A colour swatch is a rectangle. It appeared here the moment
                //  the swatches were given names, which is the fallback working
                //  exactly as described above.
                (C is TShape) then
                Continue;
            Measure.Canvas.Font.Assign(C.Font);
            Needed := Measure.Canvas.TextWidth(C.Caption);
            //  A button draws its caption inside a border and some padding, so
            //  equal widths are already clipped; a plain label is not. One
            //  tolerance for both, deliberately loose - this is here to find
            //  the ones that are wrong, not to police the last pixel.
            if not CaptionFits(Needed, C.Width, C.Scale96ToFont(4)) then
            begin
                WriteLog(Format(
                    'layout: %s.%s (%s) is %d px wide at left %d and its ' +
                    'caption needs %d: "%s"  [form %dx%d, client %dx%d, ' +
                    'designed at %d ppi]',
                    [AForm.Name, C.Name, C.ClassName, C.Width, C.Left, Needed,
                     C.Caption, AForm.Width, AForm.Height, AForm.ClientWidth,
                     AForm.ClientHeight, AForm.DesignTimePPI]), Warning);
                Inc(Reported);
            end;
        end;
    end;

var
    i: integer;
begin
    Reported := 0;
    Measure := TBitmap.Create;
    try
        Measure.SetSize(1, 1);
        for i := 0 to Screen.CustomFormCount - 1 do
        begin
            //  A form that has never been shown has not resolved its anchors,
            //  and its children still carry whatever the streaming left them -
            //  the first run of this check reported a label 20 pixels wide that
            //  is really 448. HandleNeeded builds the widget without mapping it,
            //  which is what makes the numbers the ones the user would see.
            Screen.CustomForms[i].HandleNeeded;
            CheckForm(Screen.CustomForms[i]);
        end;
    finally
        Measure.Free;
    end;
    WriteLog(Format('layout: checked %d forms, %d caption(s) do not fit',
        [Screen.CustomFormCount, Reported]), Notification);
end;

procedure ApplyUiPixelsPerInch;
var
    Detected, FromWidgetSet: integer;
    Source: TPPISource;
begin
    FromWidgetSet := Screen.PixelsPerInch;
    Detected := DetectUiPixelsPerInch(Source);

    case ScalingAction(Detected, FromWidgetSet) of
        saRefuse:
        begin
            WriteLog(Format(
                'UI scaling: ignoring %d ppi from %s - outside %d..%d; ' +
                'keeping the %d ppi the widget set reports',
                [Detected, PPISourceName(Source), MinPPI, MaxPPI,
                 FromWidgetSet]), Warning);
            Exit;
        end;
        saAlreadyRight:
        begin
            WriteLog(Format('UI scaling: %d ppi (%s)',
                [Detected, PPISourceName(Source)]), Notification);
            Exit;
        end;
    end;

    //  ScreenInfo is where the widget set left its answer and where the LCL
    //  reads it from - Screen.PixelsPerInch, Monitor.PixelsPerInch and every
    //  form's own scaling all come from here. Correcting it in place, rather
    //  than scaling each form afterwards, is what makes one assignment reach
    //  the whole application: forms created after this line scale themselves
    //  from their design ppi to this one, with no per-form call to forget.
    ScreenInfo.PixelsPerInchX := Detected;
    ScreenInfo.PixelsPerInchY := Detected;
    Screen.UpdateScreen;

    WriteLog(Format(
        'UI scaling: %d ppi from %s, overriding the %d ppi the widget set ' +
        'reports. Pass /DPI=<ppi> to set it by hand.',
        [Detected, PPISourceName(Source), FromWidgetSet]), Notification);
end;

end.
