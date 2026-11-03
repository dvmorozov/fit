// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Where the user's formula type finds the windows that define a
formula: one registration, made by the client that has them.)

THE MODULE NAMES NO WINDOW (module-architecture.md): its UI is data, so it
survives a replacement of the thin client. The two windows that define a
formula - its name and expression, then its parameters' roles - and the
parser that reports a bad expression in a message box are LCL forms, and are
part of the Free and Pro client application (Desktop/ScientificForms), not of
this module (decided with the user, 2026-10-17). The client links them only
where it links this module (module.json "forms", called through the client's
own composition unit, client_modules), and they register themselves here.

NOTHING REGISTERED IS A BUILD WITH NO SUCH WINDOWS - the compute server, the
test suites, a client of the module alone - and the formula type then defines
nothing rather than failing (TConfigurableUserPointsSet).
}
unit user_curve_dialog_registry;

{$mode objfpc}{$H+}

interface

uses
    int_user_curve_dialogs, int_expression_parser;

{ Registers the windows that define a formula. The last registration wins:
  there is one client. }
procedure RegisterUserCurveDialogs(AFormula: IUserCurveFormulaDlg;
    ARoles: IUserCurveRolesDlg; AParser: IExpressionParser);
{ Whether a client registered them. }
function UserCurveDialogsRegistered: boolean;
function UserCurveFormulaDialog: IUserCurveFormulaDlg;
function UserCurveRolesDialog: IUserCurveRolesDlg;
function UserCurveExpressionParser: IExpressionParser;

implementation

var
    Formula: IUserCurveFormulaDlg;
    Roles: IUserCurveRolesDlg;
    Parser: IExpressionParser;

procedure RegisterUserCurveDialogs(AFormula: IUserCurveFormulaDlg;
    ARoles: IUserCurveRolesDlg; AParser: IExpressionParser);
begin
    Formula := AFormula;
    Roles := ARoles;
    Parser := AParser;
end;

function UserCurveDialogsRegistered: boolean;
begin
    Result := Assigned(Formula) and Assigned(Roles) and Assigned(Parser);
end;

function UserCurveFormulaDialog: IUserCurveFormulaDlg;
begin
    Result := Formula;
end;

function UserCurveRolesDialog: IUserCurveRolesDlg;
begin
    Result := Roles;
end;

function UserCurveExpressionParser: IExpressionParser;
begin
    Result := Parser;
end;

end.
