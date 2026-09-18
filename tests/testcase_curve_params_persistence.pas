// SPDX-License-Identifier: GPL-3.0-or-later
{ Regression test for the startup crash "Error reading Curve_type.Params:
  ... TPersistentCurveParameterContainer.Name: Assertion failed". The container
  now creates a concrete parameter in its constructor, so a stored curve type's
  parameters can be streamed back in. Reproduces the read path via TXMLConfig. }
unit testcase_curve_params_persistence;
{$mode objfpc}{$H+}
interface
uses Classes, SysUtils, fpcunit, testregistry, Laz_XMLCfg,
  app_settings,   //  provides Write/ReadComponentTo/FromXMLConfig (project-local)
  persistent_curve_parameters, persistent_curve_parameter_container,
  special_curve_parameter, calculated_curve_parameter, Variants,
  mscr_specimen_list, curve_list, Math;

type
  { The shared fixture. Not registered: it declares no test of its own. }
  TCurveParamsBaseTest = class(TTestCase)
  private
    procedure FindClass(Reader: TReader; const AClassName: string;
      var ComponentClass: TComponentClass);
  private
    { Curve attributes carrying the given calculated parameters. }
    function AttributesWith(const ANames: array of string;
      const AValues: array of double): Curve_parameters;
    function ParamsOf(const ANames: array of string;
      const ATypes: array of TParameterType): Curve_parameters;
  end;

  { THE XML ROUND TRIP, which writes a settings file - so it stays in the slow
    half. A file is an external dependency exactly as a socket is. }
  TCurveParamsPersistenceTest = class(TCurveParamsBaseTest)
  published
    procedure ParamsSurviveXmlRoundTrip;
    procedure GetCopyIsIndependent;
    procedure TypedValuesSurviveTheRealXmlRoundTrip;
  end;

  { WHAT THE PARAMETER TABLE'S COLUMNS MEAN, which touches no file at all: these
    build Curve_parameters in memory and ask curve_list what the columns are.
    They were in the slow half only because they share this file with the XML
    tests, and coverage is measured over the unit half - so the column rules read
    as untested while being the ones that put a wrong number under a plausible
    heading. }
  TCurveListColumnsTest = class(TCurveParamsBaseTest)
  published
    procedure ColumnsSkipTheArgumentWhenReportingAParameterKind;
    procedure ColumnsAreTheUnionOfEveryCurvesParameters;
    procedure EqualParameterCountsWithDifferentNamesDoNotAlias;
    procedure AHomogeneousModelIsUnchanged;
  end;

implementation

procedure TCurveParamsBaseTest.FindClass(Reader: TReader;
  const AClassName: string; var ComponentClass: TComponentClass);
begin
  if AClassName = Curve_parameters.ClassName then
    ComponentClass := Curve_parameters;
end;

procedure TCurveParamsPersistenceTest.ParamsSurviveXmlRoundTrip;
var
  FileName: string;
  Cfg: TXMLConfig;
  Saved, Loaded: Curve_parameters;
  Cont: TPersistentCurveParameterContainer;
begin
  FileName := GetTempFileName('', 'cparm') + '.xml';
  //  Curve_parameters.Create seeds one container/parameter.
  Saved := Curve_parameters.Create(nil);
  try
    Cfg := TXMLConfig.Create(FileName);
    try
      WriteComponentToXMLConfig(Cfg, 'Component', Saved);
      Cfg.Flush;
    finally
      Cfg.Free;
    end;
  finally
    Saved.Free;
  end;

  Loaded := Curve_parameters.Create(nil);
  try
    Cfg := TXMLConfig.Create(FileName);
    try
      //  Before the fix this raised an assertion while setting the container's
      //  Name property (its parameter object did not exist yet).
      ReadComponentFromXMLConfig(Cfg, 'Component', TComponent(Loaded),
        @FindClass, nil);
    finally
      Cfg.Free;
    end;
    AssertTrue('at least one parameter was read', Loaded.Count >= 1);
    Cont := TPersistentCurveParameterContainer(Loaded.Params.Items[0]);
    AssertTrue('read container has a concrete parameter',
      Assigned(Cont.Parameter));
  finally
    Loaded.Free;
    if FileExists(FileName) then
      DeleteFile(FileName);
  end;
end;

procedure TCurveParamsPersistenceTest.GetCopyIsIndependent;
var
  Original, Copy: Curve_parameters;
begin
  //  Selecting a user curve and creating one both rely on GetCopy producing a
  //  fully independent object: the copy is handed to a Curve_type / the service
  //  (which takes ownership) while the original keeps its own params. A shallow
  //  copy would share the inner collection and free it twice -> access
  //  violation. Verify the copy outlives the original and can be freed cleanly.
  Original := Curve_parameters.Create(nil);
  Copy := Curve_parameters(Original.GetCopy);
  try
    AssertTrue('copy has its own parameters', Copy.Count >= 1);
    Original.Free;
    Original := nil;
    //  Copy must still be valid after the original is gone.
    AssertTrue('copy still usable after original freed',
      Assigned(Copy.Parameters[0]));
  finally
    Original.Free;   //  nil-safe if already freed
    Copy.Free;       //  must not double-free the original's collection
  end;
end;

{ THE END-TO-END CHECK, through the real TXMLConfig path rather than the container
  in isolation.

  Every identity defect in this pack shared one shape: correct code that the
  production path never reached, with green tests throughout. A parameter value is
  only genuinely safe once it has been through the actual writer and reader - so a
  GUID, a label that looks like a number, and an ordinary quantity all go through
  it here, in one file, together. }
procedure TCurveParamsPersistenceTest.TypedValuesSurviveTheRealXmlRoundTrip;
var
  FileName: string;
  Cfg: TXMLConfig;
  Saved, Loaded: Curve_parameters;
  P: TSpecialCurveParameter;
  Cont: TPersistentCurveParameterContainer;

  procedure AddParam(const AName: string; const AValue: Variant);
  var
    Q: TSpecialCurveParameter;
    C: TPersistentCurveParameterContainer;
  begin
    Q := TCalculatedCurveParameter.Create;
    Q.Name := AName;
    Q.TypedValue := AValue;
    C := TPersistentCurveParameterContainer(Saved.Params.Add);
    C.Parameter := Q;
  end;

  function Find(AList: Curve_parameters; const AName: string):
    TSpecialCurveParameter;
  var i: longint;
  begin
    Result := nil;
    for i := 0 to AList.Params.Count - 1 do
    begin
      Cont := TPersistentCurveParameterContainer(AList.Params.Items[i]);
      if Assigned(Cont.Parameter) and (Cont.Parameter.Name = AName) then
      begin
        Result := Cont.Parameter;
        Exit;
      end;
    end;
  end;

begin
  FileName := GetTempFileName('', 'cparm') + '.xml';
  Saved := Curve_parameters.Create(nil);
  try
    Saved.Params.Clear;
    AddParam('waveId', '{D4FF48CA-FD58-4EF1-BC9E-7D316B4AE4EB}');
    AddParam('waveLabel', '3');        //  parses as a number - the hard case
    AddParam('amplitude', 12.5);

    Cfg := TXMLConfig.Create(FileName);
    try
      WriteComponentToXMLConfig(Cfg, 'Component', Saved);
      Cfg.Flush;
    finally
      Cfg.Free;
    end;
  finally
    Saved.Free;
  end;

  Loaded := Curve_parameters.Create(nil);
  try
    Cfg := TXMLConfig.Create(FileName);
    try
      ReadComponentFromXMLConfig(Cfg, 'Component', TComponent(Loaded),
        @FindClass, nil);
    finally
      Cfg.Free;
    end;

    P := Find(Loaded, 'waveId');
    AssertTrue('the identity parameter came back', Assigned(P));
    AssertFalse('and is still text', P.IsNumeric);
    AssertEquals('and intact', '{D4FF48CA-FD58-4EF1-BC9E-7D316B4AE4EB}',
      VarToStr(P.TypedValue));

    P := Find(Loaded, 'waveLabel');
    AssertTrue('the label came back', Assigned(P));
    AssertFalse('a label that looks numeric is STILL text after a real save',
      P.IsNumeric);
    AssertEquals('and is still "3"', '3', VarToStr(P.TypedValue));

    P := Find(Loaded, 'amplitude');
    AssertTrue('the quantity came back', Assigned(P));
    AssertTrue('and is still a number', P.IsNumeric);
    AssertEquals('unchanged', 12.5, P.Value, 1e-12);
  finally
    Loaded.Free;
    if FileExists(FileName) then
      DeleteFile(FileName);
  end;
end;

function TCurveParamsBaseTest.AttributesWith(
  const ANames: array of string; const AValues: array of double): Curve_parameters;
var
  i: integer;
  P: TCalculatedCurveParameter;
  Cont: TPersistentCurveParameterContainer;
begin
  Result := Curve_parameters.Create(nil);
  for i := 0 to High(ANames) do
  begin
    P := TCalculatedCurveParameter.Create;
    P.Name := ANames[i];
    P.Value := AValues[i];
    Cont := TPersistentCurveParameterContainer(Result.Params.Add);
    Cont.Parameter := P;
  end;
end;




{ One parameter set from parallel name/type lists. Extracted because the three
  cases below differ only in what the curves CONTAIN, and that difference is the
  whole point of each of them. }
function TCurveParamsBaseTest.ParamsOf(const ANames: array of string;
  const ATypes: array of TParameterType): Curve_parameters;
var
  i: longint;
  P: TCalculatedCurveParameter;
  Cont: TPersistentCurveParameterContainer;
begin
  Result := Curve_parameters.Create(nil);
  for i := 0 to High(ANames) do
  begin
    P := TCalculatedCurveParameter.Create;
    P.Name := ANames[i];
    P.Type_ := ATypes[i];
    Cont := TPersistentCurveParameterContainer(Result.Params.Add);
    Cont.Parameter := P;
  end;
end;

{ A MODEL MAY HOLD CURVES OF DIFFERENT TYPES, and the parameters table has to
  survive it. This is the defect a multi-type curve pack found the hard way: the
  table's columns were positional and sized from the FIRST curve, which silently
  assumed every curve carries the same parameters in the same order.

  Two curve types break that in two different ways, and the second is the nastier:

    * different COUNTS - a corrective pattern has 15 parameters where a
      motive one has 19 - fired an internal check and took the client down on the
      next repaint. Loud, and therefore the one that got reported.

    * the same count with different NAMES - a motive pattern's k5 is a diagonal's
      c5 - complained about nothing at all. The grid put one curve's c5 under the
      other's k5 heading: a wrong number under a plausible label, which is worse
      than a crash because nobody finds out.

  So the column identity is the parameter NAME, the set of columns is the union
  over every curve, and a curve that lacks one gets a blank cell. }
procedure TCurveListColumnsTest.ColumnsAreTheUnionOfEveryCurvesParameters;
var
  List: TCurveList;
  Names: TStringList;
  T: TParameterType;
begin
  List := TCurveList.Create;
  Names := TStringList.Create;
  try
    //  Motive: ...r4, k5. Corrective: fewer parameters, and its own names.
    List.Add(ParamsOf(['x', 'A', 'r4', 'k5'], [Argument, Variable, Variable, Variable]));
    List.Add(ParamsOf(['x', 'A', 'rB'], [Argument, Variable, Variable]));

    List.CollectColumnNames(Names);
    AssertEquals('every parameter of both curves gets a column, once',
      4, Names.Count);
    //  First-seen order, so the columns the user already knows keep their places
    //  when a second type joins.
    AssertEquals('A', Names[0]);
    AssertEquals('r4', Names[1]);
    AssertEquals('k5', Names[2]);
    AssertEquals('rB', Names[3]);

    //  Row 0 has the first three and not the fourth.
    AssertTrue('the motive curve has A', List.ColumnParameterType(0, 0, T));
    AssertTrue('and r4', List.ColumnParameterType(0, 1, T));
    AssertTrue('and k5', List.ColumnParameterType(0, 2, T));
    AssertFalse('but not the corrective pattern''s rB',
      List.ColumnParameterType(0, 3, T));

    //  Row 1 has A and rB only - and crucially NOT r4 or k5, which is what the
    //  positional version got wrong by reading whatever sat at that index.
    AssertTrue('the corrective curve has A', List.ColumnParameterType(1, 0, T));
    AssertFalse('and no r4', List.ColumnParameterType(1, 1, T));
    AssertFalse('and no k5', List.ColumnParameterType(1, 2, T));
    AssertTrue('but it does have rB', List.ColumnParameterType(1, 3, T));
  finally
    Names.Free;
    List.Free;
  end;
end;

{ The silent half, stated on its own because it needs no count mismatch to
  happen: two types with the SAME number of parameters and different names. The
  old grid was perfectly happy here and showed the wrong values. }
procedure TCurveListColumnsTest.EqualParameterCountsWithDifferentNamesDoNotAlias;
var
  List: TCurveList;
  Names: TStringList;
  T: TParameterType;
begin
  List := TCurveList.Create;
  Names := TStringList.Create;
  try
    List.Add(ParamsOf(['x', 'A', 'k5'], [Argument, Variable, Variable]));
    List.Add(ParamsOf(['x', 'A', 'c5'], [Argument, Variable, Calculated]));

    List.CollectColumnNames(Names);
    AssertEquals('the differing names are separate columns, not one',
      3, Names.Count);
    AssertEquals('A', Names[0]);
    AssertEquals('k5', Names[1]);
    AssertEquals('c5', Names[2]);

    AssertTrue('row 0 has k5', List.ColumnParameterType(0, 1, T));
    AssertTrue('and it is fitted there', T = Variable);
    AssertFalse('row 0 has no c5', List.ColumnParameterType(0, 2, T));

    AssertFalse('row 1 has no k5', List.ColumnParameterType(1, 1, T));
    AssertTrue('row 1 has c5', List.ColumnParameterType(1, 2, T));
    //  The KIND travels with the name too: c5 is computed for this type, and a
    //  column shared by name would have coloured it as row 0's fitted k5.
    AssertTrue('and it is computed there', T = Calculated);
  finally
    Names.Free;
    List.Free;
  end;
end;

{ The ordinary homogeneous model must be untouched by all of the above - one
  column per parameter, in order, for every row. }
procedure TCurveListColumnsTest.AHomogeneousModelIsUnchanged;
var
  List: TCurveList;
  Names: TStringList;
begin
  List := TCurveList.Create;
  Names := TStringList.Create;
  try
    List.Add(ParamsOf(['x', 'A', 'sigma', 'x0'],
      [Argument, Variable, Variable, InvariablePosition]));
    List.Add(ParamsOf(['x', 'A', 'sigma', 'x0'],
      [Argument, Variable, Variable, InvariablePosition]));

    List.CollectColumnNames(Names);
    AssertEquals('two identical curves contribute one set of columns',
      3, Names.Count);
    AssertEquals('A', Names[0]);
    AssertEquals('sigma', Names[1]);
    AssertEquals('x0', Names[2]);
  finally
    Names.Free;
    List.Free;
  end;
end;

{ The parameters table shows one column per parameter EXCEPT the argument, and
  that skip is applied in three places now - the column options, the row
  contents, and the colouring that says what kind of parameter a column holds.
  A fourth reader that counted columns its own way would tint the wrong one, and
  the table would look right to anyone not checking the arithmetic. }
procedure TCurveListColumnsTest.ColumnsSkipTheArgumentWhenReportingAParameterKind;
var
  List: TCurveList;
  Params: Curve_parameters;
  T: TParameterType;

  procedure AddParam(const AName: string; AType: TParameterType);
  var
    P: TCalculatedCurveParameter;
    Cont: TPersistentCurveParameterContainer;
  begin
    P := TCalculatedCurveParameter.Create;
    P.Name := AName;
    P.Type_ := AType;
    Cont := TPersistentCurveParameterContainer(Params.Params.Add);
    Cont.Parameter := P;
  end;

begin
  List := TCurveList.Create;
  try
    Params := Curve_parameters.Create(nil);
    //  The argument sits FIRST, which is the arrangement that catches an
    //  off-by-one: skip it and every column after shifts.
    AddParam('x', Argument);
    AddParam('A', Variable);
    AddParam('x0', InvariablePosition);
    AddParam('y0', Calculated);
    List.Add(Params);

    AssertTrue('column 0 is the first parameter after the argument',
      List.ColumnParameterType(0, 0, T));
    AssertTrue('and it is the fitted one', T = Variable);

    AssertTrue('column 1', List.ColumnParameterType(0, 1, T));
    AssertTrue('is the pinned position', T = InvariablePosition);

    AssertTrue('column 2', List.ColumnParameterType(0, 2, T));
    AssertTrue('is the computed one', T = Calculated);

    AssertFalse('there is no column for the argument itself',
      List.ColumnParameterType(0, 3, T));
    AssertFalse('nor for a row that does not exist',
      List.ColumnParameterType(1, 0, T));
  finally
    List.Free;
  end;
end;

initialization
  //  INTEGRATION: writes and reads an XML settings file.
  RegisterTest('integration', TCurveParamsPersistenceTest);
  //  UNIT: the column rules, in memory, no file.
  RegisterTest('unit', TCurveListColumnsTest);
end.
