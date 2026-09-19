// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(Doubles for the five collaborators of the user-curve definition flow.)

TWO DIALOGS, A PARSER, A FACTORY AND A STORAGE. Each is scripted with the answers
it should give, in order, and each records what it was asked - because the rules
worth checking are about ORDER and about what happens to a draft that is never
kept, and neither shows in a return value.

-SIcorba, so these are plain objects exposing AsObject, and the fixture holds
both an interface and an object reference: interfaces here carry no refcount, and
a class that looked like TInterfacedObject would read as a lifetime guarantee
that does not exist. See tests/mocks/mock_support.pas.

THE SCRIPTS RUN OUT DELIBERATELY. A dialog asked more times than it was scripted
for answers "cancelled" and records the overrun, so a flow that loops forever
fails as a wrong answer rather than by hanging the suite.
}
unit mock_user_curve_dialogs;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, app_settings, persistent_curve_parameters,
    int_curve_type_parameters_factory, int_curve_type_storage,
    int_expression_parser, int_user_curve_dialogs, mock_support;

type
    TAnswerScript = array of TDialogAnswer;

    { The formula dialog. }
    TMockFormulaDlg = class(TObject, IUserCurveFormulaDlg)
    private
        FAnswers: TAnswerScript;
        FAsked: longint;
        FOverruns: longint;
        FName: string;
        FExpression: string;
        FTexts: TStringList;
        FLog: TCallLog;
        function TextOfThisShowing(out AName, AExpression: string): boolean;
    public
        constructor Create(const AAnswers: TAnswerScript);
        destructor Destroy; override;
        function AsObject: TObject;

        function Ask: TDialogAnswer;
        function GetExpression: string;
        function GetName: string;

        { What the dialog reports the user typed, when every showing says the
          same thing. }
        property Name_: string read FName write FName;
        property Expression: string read FExpression write FExpression;
        { DIFFERENT TEXT PER SHOWING, which is what the real dialog does: it
          keeps its text box, so a user who is sent back corrects what is there.
          One call per showing, in order; a showing with none scripted answers
          the properties above. Without this, "the corrected attempt is defined
          from the corrected formula" cannot be asked at all - and a flow that
          read the text once and reused it would define the second attempt from
          the formula that failed, storing a type the user never wrote. }
        procedure Types(const AName, AExpression: string);
        property Asked: longint read FAsked;
        property Overruns: longint read FOverruns;
        property Log: TCallLog read FLog;
    end;

    { The roles dialog. }
    TMockRolesDlg = class(TObject, IUserCurveRolesDlg)
    private
        FAnswers: TAnswerScript;
        FAsked: longint;
        FOverruns: longint;
        FLastCurveType: Curve_type;
        FSawNil: boolean;
        FLog: TCallLog;
    public
        constructor Create(const AAnswers: TAnswerScript);
        destructor Destroy; override;
        function AsObject: TObject;

        function Ask(ACurveType: Curve_type): TDialogAnswer;

        { The type it was last shown. Recorded because being shown the WRONG
          one - a stale draft from a previous attempt - is the failure this
          dialog cannot report itself. }
        property LastCurveType: Curve_type read FLastCurveType;
        { True if it was ever shown nothing at all. }
        property SawNil: boolean read FSawNil;
        property Asked: longint read FAsked;
        property Overruns: longint read FOverruns;
        property Log: TCallLog read FLog;
    end;

    { The parser. Answers a real parameter set, or nil for a formula it refuses. }
    TMockExpressionParser = class(TObject, IExpressionParser)
    private
        FRefuse: TStringList;
        FSeen: TStringList;
        FOwned: TList;
    public
        constructor Create;
        destructor Destroy; override;
        function AsObject: TObject;

        function ParseExpression(Expression: string): Curve_parameters;

        { Refuses this formula, answering nil as the real parser does for one it
          cannot read. }
        procedure Refuse(const AExpression: string);
        { Every formula it was handed, in order. }
        property Seen: TStringList read FSeen;
    end;

    { The factory. Makes real Curve_type objects - they are cheap, and a double
      would have to answer questions about identity that the tests ask. }
    TMockCurveTypeFactory = class(TObject, ICurveTypeParametersFactory)
    private
        FMade: TList;
        FLog: TCallLog;
    public
        constructor Create;
        destructor Destroy; override;
        function AsObject: TObject;

        function CreateUserCurveType(Name: string; Expression: string;
            Parameters: Curve_parameters): Curve_type;

        { How many types the flow asked for. One per formula that parsed, so a
          count above the number of accepted formulas means a draft was made
          twice. }
        function MadeCount: longint;
        function Made(AIndex: longint): Curve_type;
        property Log: TCallLog read FLog;
    end;

    { The storage. Keeps a list, so "was the draft removed" is a question about
      what it holds rather than about which methods were called - though it
      records those too, because the ORDER of a delete against a later add is
      the rule. }
    TMockCurveTypeStorage = class(TObject, ICurveTypeStorage)
    private
        FHeld: TList;
        FLog: TCallLog;
    public
        constructor Create;
        destructor Destroy; override;
        function AsObject: TObject;

        procedure AddCurveType(CurveType: Curve_type);
        procedure UpdateCurveType(CurveType: Curve_type);
        procedure DeleteCurveType(CurveType: Curve_type);

        { What it still holds. }
        function HeldCount: longint;
        function Holds(ACurveType: Curve_type): boolean;
        property Log: TCallLog read FLog;
    end;

{ An answer script, for readability at the call sites. }
function Answers(A: TDialogAnswer): TAnswerScript; overload;
function Answers(A, B: TDialogAnswer): TAnswerScript; overload;
function Answers(A, B, C: TDialogAnswer): TAnswerScript; overload;

implementation

function Answers(A: TDialogAnswer): TAnswerScript;
begin
    SetLength(Result, 1);
    Result[0] := A;
end;

function Answers(A, B: TDialogAnswer): TAnswerScript;
begin
    SetLength(Result, 2);
    Result[0] := A;
    Result[1] := B;
end;

function Answers(A, B, C: TDialogAnswer): TAnswerScript;
begin
    SetLength(Result, 3);
    Result[0] := A;
    Result[1] := B;
    Result[2] := C;
end;

{ ------------------------------ the formula dialog -------------------------- }

constructor TMockFormulaDlg.Create(const AAnswers: TAnswerScript);
begin
    inherited Create;
    FAnswers := AAnswers;
    FLog := TCallLog.Create;
    FTexts := TStringList.Create;
    FName := 'my curve';
    FExpression := 'A*exp(-x*x)';
end;

destructor TMockFormulaDlg.Destroy;
begin
    FTexts.Free;
    FLog.Free;
    inherited Destroy;
end;

procedure TMockFormulaDlg.Types(const AName, AExpression: string);
begin
    //  '|' as the separator, which no name or formula in these tests contains.
    FTexts.Add(AName + '|' + AExpression);
end;

function TMockFormulaDlg.AsObject: TObject;
begin
    Result := Self;
end;

function TMockFormulaDlg.Ask: TDialogAnswer;
begin
    FLog.Note('Ask');
    if FAsked <= High(FAnswers) then
        Result := FAnswers[FAsked]
    else
    begin
        //  Off the end of the script: answer the one that ends the flow, and
        //  record it. A flow that keeps asking then fails on the count rather
        //  than running until the suite is killed.
        Inc(FOverruns);
        Result := daCancelled;
    end;
    Inc(FAsked);
end;

{ The text of the showing just completed - FAsked has already been advanced by
  Ask, so the current showing is FAsked - 1. }
function TMockFormulaDlg.TextOfThisShowing(out AName,
    AExpression: string): boolean;
var
    i, p: longint;
    Line: string;
begin
    AName := FName;
    AExpression := FExpression;
    Result := False;
    i := FAsked - 1;
    if (i < 0) or (i >= FTexts.Count) then
        Exit;
    Line := FTexts[i];
    p := Pos('|', Line);
    if p = 0 then
        Exit;
    AName := Copy(Line, 1, p - 1);
    AExpression := Copy(Line, p + 1, MaxInt);
    Result := True;
end;

function TMockFormulaDlg.GetExpression: string;
var
    N: string;
begin
    TextOfThisShowing(N, Result);
    FLog.Note('GetExpression', Result);
end;

function TMockFormulaDlg.GetName: string;
var
    E: string;
begin
    TextOfThisShowing(Result, E);
    FLog.Note('GetName', Result);
end;

{ ------------------------------- the roles dialog --------------------------- }

constructor TMockRolesDlg.Create(const AAnswers: TAnswerScript);
begin
    inherited Create;
    FAnswers := AAnswers;
    FLog := TCallLog.Create;
end;

destructor TMockRolesDlg.Destroy;
begin
    FLog.Free;
    inherited Destroy;
end;

function TMockRolesDlg.AsObject: TObject;
begin
    Result := Self;
end;

function TMockRolesDlg.Ask(ACurveType: Curve_type): TDialogAnswer;
begin
    FLastCurveType := ACurveType;
    if not Assigned(ACurveType) then
        FSawNil := True;
    if Assigned(ACurveType) then
        FLog.Note('Ask', ACurveType.Name)
    else
        FLog.Note('Ask', '<nothing>');
    if FAsked <= High(FAnswers) then
        Result := FAnswers[FAsked]
    else
    begin
        Inc(FOverruns);
        Result := daCancelled;
    end;
    Inc(FAsked);
end;

{ --------------------------------- the parser ------------------------------- }

constructor TMockExpressionParser.Create;
begin
    inherited Create;
    FRefuse := TStringList.Create;
    FSeen := TStringList.Create;
    FOwned := TList.Create;
end;

destructor TMockExpressionParser.Destroy;
var
    i: longint;
begin
    //  The parameter sets it handed out. The real parser's go on to a curve
    //  type, which is owned by whatever stored it; nothing in these tests
    //  stores one for keeps, so the mock keeps them and frees them here rather
    //  than leaving the leak gate to find them.
    for i := 0 to FOwned.Count - 1 do
        TObject(FOwned[i]).Free;
    FOwned.Free;
    FSeen.Free;
    FRefuse.Free;
    inherited Destroy;
end;

function TMockExpressionParser.AsObject: TObject;
begin
    Result := Self;
end;

procedure TMockExpressionParser.Refuse(const AExpression: string);
begin
    FRefuse.Add(AExpression);
end;

function TMockExpressionParser.ParseExpression(
    Expression: string): Curve_parameters;
begin
    FSeen.Add(Expression);
    if FRefuse.IndexOf(Expression) >= 0 then
        Exit(nil);
    Result := Curve_parameters.Create(nil);
    FOwned.Add(Result);
end;

{ --------------------------------- the factory ------------------------------ }

constructor TMockCurveTypeFactory.Create;
begin
    inherited Create;
    FMade := TList.Create;
    FLog := TCallLog.Create;
end;

destructor TMockCurveTypeFactory.Destroy;
var
    i: longint;
begin
    for i := 0 to FMade.Count - 1 do
        TObject(FMade[i]).Free;
    FMade.Free;
    FLog.Free;
    inherited Destroy;
end;

function TMockCurveTypeFactory.AsObject: TObject;
begin
    Result := Self;
end;

function TMockCurveTypeFactory.CreateUserCurveType(Name: string;
    Expression: string; Parameters: Curve_parameters): Curve_type;
begin
    FLog.Note('CreateUserCurveType', Name + '|' + Expression);
    Result := Curve_type.Create(nil);
    Result.Name := Name;
    Result.Expression := Expression;
    //  NOT assigned: Curve_type.SetParameters takes a copy or takes ownership
    //  depending on the build, and these tests are about the sequence rather
    //  than about the parameter set. Leaving it alone keeps the ownership of
    //  what the parser handed out with the parser mock.
    FMade.Add(Result);
end;

function TMockCurveTypeFactory.MadeCount: longint;
begin
    Result := FMade.Count;
end;

function TMockCurveTypeFactory.Made(AIndex: longint): Curve_type;
begin
    Result := Curve_type(FMade[AIndex]);
end;

{ --------------------------------- the storage ------------------------------ }

constructor TMockCurveTypeStorage.Create;
begin
    inherited Create;
    FHeld := TList.Create;
    FLog := TCallLog.Create;
end;

destructor TMockCurveTypeStorage.Destroy;
begin
    //  Holds references only. The factory mock owns what it made, so a type
    //  still held here at the end is a reference, not a leak.
    FHeld.Free;
    FLog.Free;
    inherited Destroy;
end;

function TMockCurveTypeStorage.AsObject: TObject;
begin
    Result := Self;
end;

procedure TMockCurveTypeStorage.AddCurveType(CurveType: Curve_type);
begin
    FLog.Note('Add', CurveType.Name);
    FHeld.Add(CurveType);
end;

procedure TMockCurveTypeStorage.UpdateCurveType(CurveType: Curve_type);
begin
    FLog.Note('Update', CurveType.Name);
end;

procedure TMockCurveTypeStorage.DeleteCurveType(CurveType: Curve_type);
var
    i: longint;
begin
    FLog.Note('Delete', CurveType.Name);
    i := FHeld.IndexOf(CurveType);
    if i >= 0 then
        FHeld.Delete(i);
end;

function TMockCurveTypeStorage.HeldCount: longint;
begin
    Result := FHeld.Count;
end;

function TMockCurveTypeStorage.Holds(ACurveType: Curve_type): boolean;
begin
    Result := FHeld.IndexOf(ACurveType) >= 0;
end;

end.
