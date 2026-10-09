// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(What every width of a peak shares: it is positive, and it is no wider
than the fit interval the peak is fitted in.)

THE CAP COMES FROM THE DATA'S EXTENT, not from the quantity, which is why it is
told to the parameter (LimitToWindow) rather than written into the class - the
same way the position parameter reads its window off the samples either side of
its seed. The curve passes its window on when it is given one
(TCurvePointsSet.SetWindow), so every width of every curve type built on this
class is capped without a list of types anywhere.

WHY A CAP AT ALL. A width wider than the stretch of data it is fitted to is not a
peak any more: it is a hump under the whole interval, and it trades against
whatever else lies under the peaks. On a real pattern a two-branch pseudo-Voigt
in the interval 96.2 .. 98.2 reached sigma = 7.07, and the quadratic background
went to -919 to cancel its hump. The fit was good and meant nothing.

WHY THE FULL WIDTH AT HALF MAXIMUM, decided with the user. A width
parameter's meaning differs by type - a standard deviation for a Gaussian, a
full width at half maximum for a pseudo-Voigt, a half width for a Voigt's
Lorentzian - so one number for all of them would hold a Gaussian 2.35 times
looser than a pseudo-Voigt. The cap is on the PEAK instead: no wider at half
maximum than its fit interval. The window carries how many units of x one unit
of the parameter spans there (TCurveWindow.FullWidthPerUnit, answered by the
curve's type), and the cap is the interval's extent divided by it. The
interval itself and not a fraction of it, because it is the one bound that
needs no convention to state, and loose enough never to touch a peak the
interval was drawn around.

A curve whose widths COMBINE - a Voigt's Gaussian and Lorentzian - answers each
width's conversion from what the other leaves room for at its current value
(voigt_points_set), so the pair is held and not only each alone.

UNBOUNDED UNTIL THERE IS A WINDOW, which is every curve the client rebuilds from
the wire: it has values and no samples, and capping it at anything would be a
guess. A window of no extent (a single sample) caps nothing either: zero would
sit below the floor that keeps a width from dividing by zero.

Explained to the user in the guide's "Limits on parameter values" topic
(Desktop/guide_model.pas).
}
unit width_curve_parameter;

{$IF NOT DEFINED(FPC)}
{$DEFINE _WINDOWS}
{$ELSEIF DEFINED(WINDOWS)}
{$DEFINE _WINDOWS}
{$ENDIF}

interface

uses
    Classes, log, Math, SimpMath, special_curve_parameter, SysUtils;

const
    { The full width at half maximum of a Gaussian, in its standard
      deviations: 2 sqrt(2 ln 2). }
    FULL_WIDTH_PER_STANDARD_DEVIATION: double = 2.3548200450309493;

type
    { A width: folds its sign away, floors at TINY because it divides, and is
      capped at the extent of the curve's window once it has one. }
    TWidthCurveParameter = class(TSpecialCurveParameter)
    protected
        { The largest width allowed; +Infinity until a window gives one. }
        FWindowCap: double;
        procedure SetValue(AValue: double); override;

    public
        { The cap for a window of AExtent, of which one unit of a width spans
          AFullWidthPerUnit; +Infinity for no extent. Shared with the
          difference of two widths (delta_sigma_curve_parameter). }
        class function CapFor(const AExtent, AFullWidthPerUnit: double): double;
        constructor Create;
        procedure CopyTo(const Dest: TSpecialCurveParameter); override;
        { Caps the width at the window's extent, and brings a width already
          held inside it. }
        procedure LimitToWindow(const AWindow: TCurveWindow); override;
        function GetMinValue: double; override;
        function GetMaxValue: double; override;
    end;

implementation

constructor TWidthCurveParameter.Create;
begin
    //  Before inherited: the base constructor calls InitValue, and a descendant
    //  may assign through SetValue there, which reads the cap.
    FWindowCap := Infinity;
    inherited;
end;

procedure TWidthCurveParameter.CopyTo(const Dest: TSpecialCurveParameter);
begin
    inherited;
    //  The copy is what a formula backend is handed its bounds from.
    if Dest is TWidthCurveParameter then
        TWidthCurveParameter(Dest).FWindowCap := FWindowCap;
end;

class function TWidthCurveParameter.CapFor(
    const AExtent, AFullWidthPerUnit: double): double;
begin
    //  No extent, or a conversion that is not a positive number - a shape the
    //  type cannot measure - caps nothing rather than everything.
    if (AExtent > TINY) and (AFullWidthPerUnit > 0) then
        Result := AExtent / AFullWidthPerUnit
    else
        Result := Infinity;
end;

procedure TWidthCurveParameter.LimitToWindow(const AWindow: TCurveWindow);
begin
    FWindowCap := CapFor(Abs(AWindow.LastX - AWindow.FirstX),
        AWindow.FullWidthPerUnit);
    //  A width restored from an earlier fit, or typed in before the interval
    //  was narrowed, is held when the window arrives.
    if Value > FWindowCap then
        SetValue(Value);
end;

procedure TWidthCurveParameter.SetValue(AValue: double);
begin
    FValue := Abs(AValue);
    if FValue = 0 then
        FValue := TINY;
    if FValue > FWindowCap then
        FValue := FWindowCap;
    WriteValueToLog(AValue);
end;

function TWidthCurveParameter.GetMinValue: double;
begin
    Result := TINY;
end;

function TWidthCurveParameter.GetMaxValue: double;
begin
    Result := FWindowCap;
end;

end.
