// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(The two dialogs that define a user's own curve type, as answers.)

DEFINING A CURVE TYPE TAKES TWO WINDOWS: one asks for a name and a formula, the
other asks which of the parsed parameters plays which role - position, amplitude,
width. The sequence between them is not linear. A formula that will not parse
sends the user back to the first window; rejecting the roles sends them back too,
and the draft type has to be removed on the way; cancelling either abandons the
whole thing.

ASKED FOR AN ANSWER, NOT FOR A MODAL RESULT. The dialogs used to be consulted by
comparing `ShowModal` against the widget set's `mrOk` and `mrRetry`, which put
the sequence in a routine that could only run with two real windows on a real
screen - so none of the ways through it had a test, including the one that
deletes a draft. Mapping a modal result onto one of three answers is the
adapters' work, because the adapters are the part that knows about windows.

THREE ANSWERS, NOT A BOOLEAN, AND THE THIRD IS THE POINT. "Cancelled" and "start
again" are the two ways of saying no, and they are opposites: one leaves the
program as it was, the other keeps the user in the flow with a draft to clean up.
Collapsed into a boolean they were distinguished by which of two labels a `goto`
jumped to.
}
unit int_user_curve_dialogs;

{$mode objfpc}{$H+}

interface

uses
    app_settings;

type
    { What the user did with a dialog. }
    TDialogAnswer = (
        { Confirmed it; carry on. }
        daAccepted,
        { Rejected what it shows and wants to go back a step, NOT to leave. }
        daStartAgain,
        { Abandoned the whole thing. }
        daCancelled);

    { The first window: a name and a formula. }
    IUserCurveFormulaDlg = interface
        { Shows it and answers what the user did. Only two of the three answers
          can come from this one - there is no step to go back to. }
        function Ask: TDialogAnswer;
        function GetExpression: string;
        function GetName: string;
    end;

    { The second window: which parameter plays which role.

      THE CURVE TYPE IS PASSED IN rather than assigned to a field beforehand.
      The dialog it wraps does hold a field, and setting it from the caller is
      how it used to be done - which made "show the dialog" two statements that
      had to stay together, one of them reaching into another unit's public
      field. }
    IUserCurveRolesDlg = interface
        function Ask(ACurveType: Curve_type): TDialogAnswer;
    end;

implementation

end.
