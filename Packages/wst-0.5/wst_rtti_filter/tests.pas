unit tests;

{$mode objfpc}{$H+}
//{$DEFINE DBG_DISPLAY}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, TypInfo,
  
  rtti_filters;

type

  { TClass_A }

  TClass_A = class(TPersistent)
  private
    FIntProp: Integer;
    FStrProp: string;
    FWideStrProp: widestring;
  published
    property IntProp : Integer read FIntProp write FIntProp;
    property StrProp : string read FStrProp write FStrProp;
    property WideStrProp : widestring read FWideStrProp write FWideStrProp;
  end;
  TClass_AClass = class of TClass_A;
  
  { TRttiExpIntegerNodeItem_Test }

  TRttiExpIntegerNodeItem_Test = class(TTestCase)
  published
    procedure Create_Test();
    procedure Evaluate_Equal();
    procedure Evaluate_Lesser();
    procedure Evaluate_Greater();
  end; 

  { TRttiExpAnsiStringNodeItem_Test }

  TRttiExpAnsiStringNodeItem_Test = class(TTestCase)
  published
    procedure Create_Test();
    procedure Evaluate_EqualCaseSensitive();
    procedure Evaluate_EqualCaseInsensitive();
  end;

  { TRttiExpwWideStringNodeItem_Test }

  TRttiExpwWideStringNodeItem_Test = class(TTestCase)
  published
    procedure Create_Test();
    procedure Evaluate_EqualCaseSensitive();
    procedure Evaluate_EqualCaseInsensitive();
  end;

  { TRttiExpNode_Test }

  TRttiExpNode_Test = class(TTestCase)
  published
    procedure Left_True();
    procedure LeftTrue_Or_RightFalse();
    procedure LeftTrue_Or_RightTrue();
    procedure LeftTrue_And_RightFalse();
    procedure LeftTrue_And_RightTrue();
    
    procedure Left_False();
    procedure LeftFalse_Or_RightFalse();
    procedure LeftFalse_Or_RightTrue();
    procedure LeftFalse_And_RightFalse();
  end;

  { TRttiFilterCreator_Test }

  TRttiFilterCreator_Test = class(TTestCase)
  published
    procedure Creation();
    procedure AddContion();
    procedure BeginEnd_Group();
  end;

  { TRttiParser_Test }

  TRttiParser_Test = class(TTestCase)
  published
    procedure SimpleOperators();
    procedure BeginEnd_Group();
  end;

implementation

procedure TRttiExpIntegerNodeItem_Test.Create_Test();
var
  x : TRttiExpIntegerNodeItem;
begin
  x := nil;
  try
    try
      x := TRttiExpIntegerNodeItem.Create(GetPropInfo(TClass_A,'StrProp'),nfoEqual,10);
      Check(False);
    except
      on e : EAssertionFailedError do
        raise;
      on e : ERttiFilterException do begin
        // nothing!
      end;
    end;
  finally
    x.Free();
  end;
end;

procedure TRttiExpIntegerNodeItem_Test.Evaluate_Equal();
const VAL_1 : Integer = 1210;
var
  x : TRttiExpIntegerNodeItem;
  t : TClass_A;
begin
  x := nil;
  t := TClass_A.Create();
  try
    x := TRttiExpIntegerNodeItem.Create(GetPropInfo(t,'IntProp'),nfoEqual,VAL_1);

    t.IntProp := 0;
    Check( x.Evaluate(t) = False ,'False');

    t.IntProp := VAL_1;
    Check( x.Evaluate(t) = True, 'True' );
  finally
    x.Free();
    t.Free();
  end;
end;

procedure TRttiExpIntegerNodeItem_Test.Evaluate_Lesser();
const VAL_1 : Integer = 1210;
var
  x : TRttiExpIntegerNodeItem;
  t : TClass_A;
begin
  x := nil;
  t := TClass_A.Create();
  try
    x := TRttiExpIntegerNodeItem.Create(GetPropInfo(t,'IntProp'),nfoLesser,VAL_1);

    t.IntProp := 0;
    Check( x.Evaluate(t) = True ,'True');

    t.IntProp := VAL_1 + 1;
    Check( x.Evaluate(t) = False, 'False' );
  finally
    x.Free();
    t.Free();
  end;
end;

procedure TRttiExpIntegerNodeItem_Test.Evaluate_Greater();
const VAL_1 : Integer = 1210;
var
  x : TRttiExpIntegerNodeItem;
  t : TClass_A;
begin
  x := nil;
  t := TClass_A.Create();
  try
    x := TRttiExpIntegerNodeItem.Create(GetPropInfo(t,'IntProp'),nfoGreater,VAL_1);

    t.IntProp := 0;
    Check( x.Evaluate(t) = False, 'False' );

    t.IntProp := VAL_1 + 1;
    Check( x.Evaluate(t) = True ,'True');
  finally
    x.Free();
    t.Free();
  end;
end;


{ TRttiExpNode_Test }

procedure TRttiExpNode_Test.Left_True();
const VAL_1 : Integer = 1210;
var
  x : TRttiExpNode;
  t : TClass_A;
begin
  x := nil;
  t := TClass_A.Create();
  try
    x := TRttiExpNode.Create();
    CheckNull(x.Left);
    CheckNull(x.Right);
    Check(x.Connector = fcNone);
    
    x.Left := TRttiExpIntegerNodeItem.Create(GetPropInfo(t,'IntProp'),nfoEqual,VAL_1);

    t.IntProp := 0;
    Check( x.Evaluate(t) = False, 'False' );

    t.IntProp := VAL_1;
    Check( x.Evaluate(t) = True ,'True');
  finally
    x.Free();
    t.Free();
  end;
end;

procedure TRttiExpNode_Test.LeftTrue_Or_RightFalse();
const VAL_1 : Integer = 1210;
var
  x : TRttiExpNode;
  t : TClass_A;
begin
  x := nil;
  t := TClass_A.Create();
  try
    x := TRttiExpNode.Create();
    CheckNull(x.Left);
    CheckNull(x.Right);
    Check(x.Connector = fcNone);

    x.Left := TRttiExpIntegerNodeItem.Create(GetPropInfo(t,'IntProp'),nfoLesser,VAL_1);
    x.Connector := fcOr;
    x.Right := TRttiExpIntegerNodeItem.Create(GetPropInfo(t,'IntProp'),nfoGreater,VAL_1);

    t.IntProp := 0;
    Check( x.Evaluate(t) = True ,'True');
  finally
    x.Free();
    t.Free();
  end;
end;

procedure TRttiExpNode_Test.LeftTrue_Or_RightTrue();
const VAL_1 : Integer = 1210;
var
  x : TRttiExpNode;
  t : TClass_A;
begin
  x := nil;
  t := TClass_A.Create();
  try
    x := TRttiExpNode.Create();
    CheckNull(x.Left);
    CheckNull(x.Right);
    Check(x.Connector = fcNone);

    x.Left := TRttiExpIntegerNodeItem.Create(GetPropInfo(t,'IntProp'),nfoLesser,VAL_1);
    x.Connector := fcOr;
    x.Right := TRttiExpIntegerNodeItem.Create(GetPropInfo(t,'IntProp'),nfoLesser,VAL_1);

    t.IntProp := 0;
    Check( x.Evaluate(t) = True ,'True');
  finally
    x.Free();
    t.Free();
  end;
end;

procedure TRttiExpNode_Test.LeftTrue_And_RightFalse();
const VAL_1 : Integer = 1210;
var
  x : TRttiExpNode;
  t : TClass_A;
begin
  x := nil;
  t := TClass_A.Create();
  try
    x := TRttiExpNode.Create();
    CheckNull(x.Left);
    CheckNull(x.Right);
    Check(x.Connector = fcNone);

    x.Left := TRttiExpIntegerNodeItem.Create(GetPropInfo(t,'IntProp'),nfoLesser,VAL_1);
    x.Connector := fcAnd;
    x.Right := TRttiExpIntegerNodeItem.Create(GetPropInfo(t,'IntProp'),nfoGreater,VAL_1);

    t.IntProp := 0;
    Check( x.Evaluate(t) = False ,'False');
  finally
    x.Free();
    t.Free();
  end;
end;

procedure TRttiExpNode_Test.LeftTrue_And_RightTrue();
const VAL_1 : Integer = 1210;
var
  x : TRttiExpNode;
  t : TClass_A;
begin
  x := nil;
  t := TClass_A.Create();
  try
    x := TRttiExpNode.Create();
    CheckNull(x.Left);
    CheckNull(x.Right);
    Check(x.Connector = fcNone);

    x.Left := TRttiExpIntegerNodeItem.Create(GetPropInfo(t,'IntProp'),nfoLesser,VAL_1);
    x.Connector := fcAnd;
    x.Right := TRttiExpIntegerNodeItem.Create(GetPropInfo(t,'IntProp'),nfoLesser,VAL_1);

    t.IntProp := 0;
    Check( x.Evaluate(t) = True ,'True');
  finally
    x.Free();
    t.Free();
  end;
end;

procedure TRttiExpNode_Test.Left_False();
const VAL_1 : Integer = 1210;
var
  x : TRttiExpNode;
  t : TClass_A;
begin
  x := nil;
  t := TClass_A.Create();
  try
    x := TRttiExpNode.Create();
    CheckNull(x.Left);
    CheckNull(x.Right);
    Check(x.Connector = fcNone);

    x.Left := TRttiExpIntegerNodeItem.Create(GetPropInfo(t,'IntProp'),nfoGreater,VAL_1);

    t.IntProp := 0;
    Check( x.Evaluate(t) = False, 'False' );
  finally
    x.Free();
    t.Free();
  end;
end;

procedure TRttiExpNode_Test.LeftFalse_Or_RightFalse();
const VAL_1 : Integer = 1210;
var
  x : TRttiExpNode;
  t : TClass_A;
begin
  x := nil;
  t := TClass_A.Create();
  try
    x := TRttiExpNode.Create();
    CheckNull(x.Left);
    CheckNull(x.Right);
    Check(x.Connector = fcNone);

    x.Left := TRttiExpIntegerNodeItem.Create(GetPropInfo(t,'IntProp'),nfoEqual,VAL_1);
    x.Connector := fcOr;
    x.Right := TRttiExpIntegerNodeItem.Create(GetPropInfo(t,'IntProp'),nfoGreater,VAL_1);

    t.IntProp := 0;
    Check( x.Evaluate(t) = False ,'False');
  finally
    x.Free();
    t.Free();
  end;
end;

procedure TRttiExpNode_Test.LeftFalse_Or_RightTrue();
const VAL_1 : Integer = 1210;
var
  x : TRttiExpNode;
  t : TClass_A;
begin
  x := nil;
  t := TClass_A.Create();
  try
    x := TRttiExpNode.Create();
    CheckNull(x.Left);
    CheckNull(x.Right);
    Check(x.Connector = fcNone);

    x.Left := TRttiExpIntegerNodeItem.Create(GetPropInfo(t,'IntProp'),nfoEqual,VAL_1);
    x.Connector := fcOr;
    x.Right := TRttiExpIntegerNodeItem.Create(GetPropInfo(t,'IntProp'),nfoEqual,0);

    t.IntProp := 0;
    Check( x.Evaluate(t) = True ,'True');
  finally
    x.Free();
    t.Free();
  end;
end;

procedure TRttiExpNode_Test.LeftFalse_And_RightFalse();
const VAL_1 : Integer = 1210;
var
  x : TRttiExpNode;
  t : TClass_A;
begin
  x := nil;
  t := TClass_A.Create();
  try
    x := TRttiExpNode.Create();
    CheckNull(x.Left);
    CheckNull(x.Right);
    Check(x.Connector = fcNone);

    x.Left := TRttiExpIntegerNodeItem.Create(GetPropInfo(t,'IntProp'),nfoLesser,-VAL_1);
    x.Connector := fcAnd;
    x.Right := TRttiExpIntegerNodeItem.Create(GetPropInfo(t,'IntProp'),nfoGreater,VAL_1);

    t.IntProp := 0;
    Check( x.Evaluate(t) = False ,'False');
  finally
    x.Free();
    t.Free();
  end;
end;

{ TRttiFilterCreator_Test }

procedure TRttiFilterCreator_Test.Creation();
var
  x : TRttiFilterCreator;
begin
  x := TRttiFilterCreator.Create(TClass_A);
  try
    CheckNull(x.Root,'Root <> nil');
    Check(( x.TargetClass = TClass_A ), 'TargetClass');
  finally
    x.Free();
  end;
end;

procedure Display(const AMsg : string);
begin
  {$IFDEF DBG_DISPLAY}
  Write(AMsg);
  {$ENDIF}
end;

type TPrintProc = procedure(const AMsg : string);
procedure PrintTree(
        ATree : TRttiExpNodeItem;
        APrintProc : TPrintProc;
  const AIndent : Integer
);
begin
  if Assigned(ATree) then begin
    if ATree.InheritsFrom(TRttiExpNode) then begin
      APrintProc(StringOfChar('-',AIndent));
        APrintProc(GetEnumName(TypeInfo(TFilterConnector),Ord(TRttiExpNode(ATree).Connector)) + #10#13);
      PrintTree(TRttiExpNode(ATree).Left,APrintProc,AIndent+2);
      PrintTree(TRttiExpNode(ATree).Right,APrintProc,AIndent+2);
    end else if ATree.InheritsFrom(TRttiExpConcreteNodeItem) then begin
      APrintProc(StringOfChar('-',AIndent));
        if ATree.InheritsFrom(TRttiExpNumericNodeItem) then begin
          APrintProc(TRttiExpConcreteNodeItem(ATree).PropInfo^.Name + ' ' + GetEnumName(TypeInfo(TNumericFilterOperator),Ord(TRttiExpNumericNodeItem(ATree).Operation)) );
        end else if ATree.InheritsFrom(TRttiExpStringNodeItem) then begin
          APrintProc(TRttiExpConcreteNodeItem(ATree).PropInfo^.Name + ' ' + GetEnumName(TypeInfo(TStringFilterOperator),Ord(TRttiExpStringNodeItem(ATree).Operation)) )
        end;
      if ATree.InheritsFrom(TRttiExpIntegerNodeItem) then
        APrintProc(' ' + IntToStr(TRttiExpIntegerNodeItem(ATree).ComparedValue))
      else if ATree.InheritsFrom(TRttiExpAnsiStringNodeItem) then
        APrintProc(' ' + QuotedStr(TRttiExpAnsiStringNodeItem(ATree).ComparedValue));
      APrintProc(#10#13);
    end;
  end;
end;

procedure CompareTree(ATreeA,ATreeB : TRttiExpNodeItem);
begin
  if ( ( ATreeA = nil ) and ( ATreeB <> nil ) ) or
     ( ( ATreeB = nil ) and ( ATreeA <> nil ) )
  then begin
    raise Exception.Create('not equal');
  end;
  if ( ATreeA <> nil ) then begin
    if ATreeA.ClassType <> ATreeB.ClassType then
      raise Exception.Create('Class not equal');
    if ATreeA.InheritsFrom(TRttiExpNode) then begin
      if TRttiExpNode(ATreeA).Connector <>
         TRttiExpNode(ATreeB).Connector
      then
        raise Exception.Create('TRttiExpNode not equal');
      CompareTree(TRttiExpNode(ATreeA).Left,TRttiExpNode(ATreeB).Left);
      CompareTree(TRttiExpNode(ATreeA).Right,TRttiExpNode(ATreeB).Right);
    end else if ATreeA.InheritsFrom(TRttiExpConcreteNodeItem) then begin
      if ATreeA.InheritsFrom(TRttiExpIntegerNodeItem) then begin
        if TRttiExpIntegerNodeItem(ATreeA).Operation <>
           TRttiExpIntegerNodeItem(ATreeB).Operation
        then
          raise Exception.Create('Operation not equal');

        if TRttiExpIntegerNodeItem(ATreeA).ComparedValue <>
           TRttiExpIntegerNodeItem(ATreeB).ComparedValue
        then
          raise Exception.Create('Value not equal');
      end else if ATreeA.InheritsFrom(TRttiExpStringNodeItem) then begin
        if TRttiExpStringNodeItem(ATreeA).Operation <>
           TRttiExpStringNodeItem(ATreeB).Operation
        then
          raise Exception.Create('Operation not equal');

        if ATreeA.InheritsFrom(TRttiExpAnsiStringNodeItem) then begin
          if TRttiExpAnsiStringNodeItem(ATreeA).ComparedValue <>
             TRttiExpAnsiStringNodeItem(ATreeB).ComparedValue
          then
            raise Exception.Create('Value not equal');
        end else if ATreeA.InheritsFrom(TRttiExpWideStringNodeItem) then begin
          if TRttiExpWideStringNodeItem(ATreeA).ComparedValue <>
             TRttiExpWideStringNodeItem(ATreeB).ComparedValue
          then
            raise Exception.Create('Value not equal');
        end
      end;

    end;
  end;
end;

procedure TRttiFilterCreator_Test.AddContion();
const VAL_1 : Integer = 1210; VAL_2 : Integer = 1076; VAL_3 : Integer = 176;
      VAL_4 : Integer = -176;
var
  x : TRttiFilterCreator;
  xin : TRttiExpIntegerNodeItem;
  xn : TRttiExpNode;
begin
  x := TRttiFilterCreator.Create(TClass_A);
  try
    x.AddCondition('IntProp',nfoGreater,VAL_1,fcOr);
      CheckNotNull(x.Root,'Root');
      CheckNotNull(x.Root.Left,'Root.Left');
      CheckIs(x.Root.Left,TRttiExpIntegerNodeItem,'Root.Left');
      xin := x.Root.Left as TRttiExpIntegerNodeItem;
      CheckEquals(VAL_1,xin.ComparedValue);
      Check( ( xin.Operation = nfoGreater ), 'Operation');
      CheckNull(x.Root.Right,'Root.Right');
      Check( ( x.Root.Connector = fcOr ), 'Root.Connector');
    
    x.AddCondition('IntProp',nfoLesser,VAL_2,fcAnd);
      CheckNotNull(x.Root.Left,'Root.Left');
      CheckIs(x.Root.Left,TRttiExpIntegerNodeItem,'Root.Left');
      xin := x.Root.Left as TRttiExpIntegerNodeItem;
      CheckEquals(VAL_1,xin.ComparedValue);
      Check( ( xin.Operation = nfoGreater ), 'Operation');
      CheckNotNull(x.Root.Right,'Root.Right');
      Check( ( x.Root.Connector = fcAnd ), 'Root.Connector');
      CheckIs(x.Root.Right,TRttiExpIntegerNodeItem,'Root.Right');
      xin := x.Root.Right as TRttiExpIntegerNodeItem;
      CheckEquals(VAL_2,xin.ComparedValue);
      Check( ( xin.Operation = nfoLesser ), 'Operation');

    x.AddCondition('IntProp',nfoEqual,VAL_3,fcOr);
      CheckNotNull(x.Root.Left,'Root.Left');
      CheckIs(x.Root.Left,TRttiExpIntegerNodeItem,'Root.Left');
      xin := x.Root.Left as TRttiExpIntegerNodeItem;
      CheckEquals(VAL_1,xin.ComparedValue);
      Check( ( xin.Operation = nfoGreater ), 'Operation');
      CheckNotNull(x.Root.Right,'Root.Right');
      Check( ( x.Root.Connector = fcAnd ), 'Root.Connector');
      CheckIs(x.Root.Right,TRttiExpNode,'Root.Right');
      xn := x.Root.Right as TRttiExpNode;
        CheckNotNull(xn.Left,'Root.Right.Left');
        CheckIs(xn.Left,TRttiExpIntegerNodeItem);
        xin := xn.Left as TRttiExpIntegerNodeItem;
        CheckEquals(VAL_2,xin.ComparedValue);
        Check( ( xin.Operation = nfoLesser ), 'Operation');

        CheckIs(xn.Right,TRttiExpIntegerNodeItem,'xn.Right');
        xin := xn.Right as TRttiExpIntegerNodeItem;
        CheckEquals(VAL_3,xin.ComparedValue);
        Check( ( xin.Operation = nfoEqual ), 'Operation');
        
    x.AddCondition('IntProp',nfoEqual,VAL_4,fcAnd);
    PrintTree(x.Root,@Display,2);
  finally
    x.Free();
  end;
end;

procedure TRttiFilterCreator_Test.BeginEnd_Group();
const VAL_1 : Integer = 1210; VAL_2 : Integer = 1076; VAL_3 : Integer = 176;
      VAL_4 : Integer = -176;
var
  x : TRttiFilterCreator;
  xin : TRttiExpIntegerNodeItem;
  xn : TRttiExpNode;
begin
  x := TRttiFilterCreator.Create(TClass_A);
  try
    x.AddCondition('IntProp',nfoGreater,VAL_1,fcOr);
      CheckNotNull(x.Root,'Root');
      CheckNotNull(x.Root.Left,'Root.Left');
      CheckIs(x.Root.Left,TRttiExpIntegerNodeItem,'Root.Left');
      xin := x.Root.Left as TRttiExpIntegerNodeItem;
      CheckEquals(VAL_1,xin.ComparedValue);
      Check( ( xin.Operation = nfoGreater ), 'Operation');
      CheckNull(x.Root.Right,'Root.Right');
      Check( ( x.Root.Connector = fcOr ), 'Root.Connector');

    x.BeginGroup(fcOr);
      CheckNotNull(x.Root.Left,'Root.Left');
      CheckIs(x.Root.Left,TRttiExpIntegerNodeItem,'Root.Left');
      xin := x.Root.Left as TRttiExpIntegerNodeItem;
      CheckEquals(VAL_1,xin.ComparedValue);
      Check( ( xin.Operation = nfoGreater ), 'Operation');

      CheckNotNull(x.Root.Right,'Root.Right');
      Check( ( x.Root.Connector = fcOr ), 'Root.Connector');
      CheckIs(x.Root.Right,TRttiExpNode,'Root.Right');
      xn := x.Root.Right as TRttiExpNode;
      CheckNull(xn.Left);
      CheckNull(xn.Right);

      x.AddCondition('IntProp',nfoLesser,VAL_2,fcAnd);
        CheckNotNull(x.Root.Left,'Root.Left');
        CheckIs(x.Root.Left,TRttiExpIntegerNodeItem,'Root.Left');
        xin := x.Root.Left as TRttiExpIntegerNodeItem;
        CheckEquals(VAL_1,xin.ComparedValue);
        Check( ( xin.Operation = nfoGreater ), 'Operation');

        CheckNotNull(x.Root.Right,'Root.Right');
        Check( ( x.Root.Connector = fcOr ), 'Root.Connector');
        CheckIs(x.Root.Right,TRttiExpNode,'Root.Right');
        xn := x.Root.Right as TRttiExpNode;
        CheckNotNull(xn.Left,'xn.Left');
        CheckNull(xn.Right,'xn.Right');
        Check( ( xn.Connector = fcAnd ), 'xn.Connector');
        CheckIs(xn.Left,TRttiExpIntegerNodeItem,'xn.Left');
        xin := xn.Left as TRttiExpIntegerNodeItem;
        CheckEquals(VAL_2,xin.ComparedValue);
        Check( ( xin.Operation = nfoLesser ), 'Operation');

      x.AddCondition('IntProp',nfoEqual,VAL_3,fcAnd);
        CheckNotNull(x.Root.Left,'Root.Left');
        CheckIs(x.Root.Left,TRttiExpIntegerNodeItem,'Root.Left');
        xin := x.Root.Left as TRttiExpIntegerNodeItem;
        CheckEquals(VAL_1,xin.ComparedValue);
        Check( ( xin.Operation = nfoGreater ), 'Operation');

        CheckNotNull(x.Root.Right,'Root.Right');
        Check( ( x.Root.Connector = fcOr ), 'Root.Connector');
        CheckIs(x.Root.Right,TRttiExpNode,'Root.Right');
        xn := x.Root.Right as TRttiExpNode;
        CheckNotNull(xn.Left,'xn.Left');
        CheckNotNull(xn.Right,'xn.Right');
        Check( ( xn.Connector = fcAnd ), 'xn.Connector');
        CheckIs(xn.Left,TRttiExpIntegerNodeItem,'xn.Left');
        xin := xn.Left as TRttiExpIntegerNodeItem;
        CheckEquals(VAL_2,xin.ComparedValue);
        Check( ( xin.Operation = nfoLesser ), 'Operation');

        CheckIs(xn.Right,TRttiExpIntegerNodeItem,'xn.Right');
        xin := xn.Right as TRttiExpIntegerNodeItem;
        CheckEquals(VAL_3,xin.ComparedValue);
        Check( ( xin.Operation = nfoEqual ), 'Operation');

    x.EndGroup();
    x.AddCondition('IntProp',nfoEqual,VAL_4,fcOr);
    PrintTree(x.Root,@Display,2);
    
    Display(#10#13);
    Display(#10#13);
    x.Clear(clrFreeObjects);
    x.BeginGroup(fcAnd);
      x.AddCondition('IntProp',nfoLesser,VAL_1,fcAnd);
      x.BeginGroup(fcOr);
        x.AddCondition('IntProp',nfoEqual,VAL_2,fcAnd);
        x.AddCondition('IntProp',nfoEqual,VAL_3,fcAnd);
      x.EndGroup();
      x.AddCondition('IntProp',nfoEqual,VAL_2,fcOr);
    x.EndGroup();
    x.AddCondition('IntProp',nfoGreater,VAL_4,fcAnd);
    PrintTree(x.Root,@Display,2);
  finally
    x.Free();
  end;
end;


{ TRttiExpAnsiStringNodeItem_Test }

procedure TRttiExpAnsiStringNodeItem_Test.Create_Test();
var
  x : TRttiExpAnsiStringNodeItem;
begin
  x := nil;
  try
    try
      x := TRttiExpAnsiStringNodeItem.Create(GetPropInfo(TClass_A,'IntProp'),sfoEqualCaseInsensitive,'Azerty');
      Check(False);
    except
      on e : EAssertionFailedError do
        raise;
      on e : ERttiFilterException do begin
        // nothing!
      end;
    end;
  finally
    x.Free();
  end;
end;

procedure TRttiExpAnsiStringNodeItem_Test.Evaluate_EqualCaseSensitive();
const VAL_1 = 'AzertY';
var
  x : TRttiExpAnsiStringNodeItem;
  t : TClass_A;
begin
  x := nil;
  t := TClass_A.Create();
  try
    x := TRttiExpAnsiStringNodeItem.Create(GetPropInfo(t,'StrProp'),sfoEqualCaseSensitive,VAL_1);

    t.StrProp := 'aaadddd';
    Check( x.Evaluate(t) = False ,'False');

    t.StrProp := UpperCase(VAL_1);
    Check( x.Evaluate(t) = False ,'False');

    t.StrProp := LowerCase(VAL_1);
    Check( x.Evaluate(t) = False ,'False');
    
    t.StrProp := VAL_1;
    Check( x.Evaluate(t) = True, 'True' );
  finally
    x.Free();
    t.Free();
  end;
end;

procedure TRttiExpAnsiStringNodeItem_Test.Evaluate_EqualCaseInsensitive();
const VAL_1 = 'AzertY';
var
  x : TRttiExpAnsiStringNodeItem;
  t : TClass_A;
begin
  x := nil;
  t := TClass_A.Create();
  try
    x := TRttiExpAnsiStringNodeItem.Create(GetPropInfo(t,'StrProp'),sfoEqualCaseInsensitive,VAL_1);

    t.StrProp := 'aaadddd';
    Check( x.Evaluate(t) = False ,'False');

    t.StrProp := UpperCase(VAL_1);
    Check( x.Evaluate(t) = True ,'True');

    t.StrProp := LowerCase(VAL_1);
    Check( x.Evaluate(t) = True ,'True');

    t.StrProp := VAL_1;
    Check( x.Evaluate(t) = True, 'True' );
  finally
    x.Free();
    t.Free();
  end;
end;

{ TRttiExpwWideStringNodeItem_Test }

procedure TRttiExpwWideStringNodeItem_Test.Create_Test();
var
  x : TRttiExpWideStringNodeItem;
begin
  x := nil;
  try
    try
      x := TRttiExpWideStringNodeItem.Create(GetPropInfo(TClass_A,'IntProp'),sfoEqualCaseInsensitive,'Azerty');
      Check(False);
    except
      on e : EAssertionFailedError do
        raise;
      on e : ERttiFilterException do begin
        // nothing!
      end;
    end;
  finally
    x.Free();
  end;
end;

procedure TRttiExpwWideStringNodeItem_Test.Evaluate_EqualCaseSensitive();
const VAL_1 = 'AzertY';
var
  x : TRttiExpWideStringNodeItem;
  t : TClass_A;
begin
  x := nil;
  t := TClass_A.Create();
  try
    x := TRttiExpWideStringNodeItem.Create(GetPropInfo(t,'WideStrProp'),sfoEqualCaseSensitive,VAL_1);

    t.WideStrProp := 'aaadddd';
    Check( x.Evaluate(t) = False ,'False');

    t.WideStrProp := UpperCase(VAL_1);
    Check( x.Evaluate(t) = False ,'False');

    t.WideStrProp := LowerCase(VAL_1);
    Check( x.Evaluate(t) = False ,'False');

    t.WideStrProp := VAL_1;
    Check( x.Evaluate(t) = True, 'True' );
  finally
    x.Free();
    t.Free();
  end;
end;

procedure TRttiExpwWideStringNodeItem_Test.Evaluate_EqualCaseInsensitive();
const VAL_1 = 'AzertY';
var
  x : TRttiExpWideStringNodeItem;
  t : TClass_A;
begin
  x := nil;
  t := TClass_A.Create();
  try
    x := TRttiExpWideStringNodeItem.Create(GetPropInfo(t,'WideStrProp'),sfoEqualCaseInsensitive,VAL_1);

    t.WideStrProp := 'aaadddd';
    Check( x.Evaluate(t) = False ,'False');

    t.WideStrProp := UpperCase(VAL_1);
    Check( x.Evaluate(t) = True ,'True');

    t.WideStrProp := LowerCase(VAL_1);
    Check( x.Evaluate(t) = True ,'True');

    t.WideStrProp := VAL_1;
    Check( x.Evaluate(t) = True, 'True' );
  finally
    x.Free();
    t.Free();
  end;
end;


{ TRttiParser_Test }

procedure TRttiParser_Test.SimpleOperators();
const VAL_1 = 'inoussa';
var
  x : TRttiFilterCreator;
  sN : TRttiExpAnsiStringNodeItem;
begin
  x := TRttiFilterCreator.Create(TClass_A);
  try
    ParseFilter(Format('StrProp = %s',[QuotedStr(VAL_1)]),x);
    CheckNotNull(x.Root,'Root <> nil');
    CheckIs(x.Root.Left,TRttiExpAnsiStringNodeItem);
    sN := x.Root.Left as TRttiExpAnsiStringNodeItem;
    CheckEquals('StrProp',sN.PropInfo^.Name);
    CheckEquals(VAL_1,sN.ComparedValue);
  finally
    x.Free();
  end;
end;

procedure TRttiParser_Test.BeginEnd_Group();
const VAL_1 : Integer = 1210; VAL_2 : Integer = 1076; VAL_3 : Integer = 176;
      VAL_4 : Integer = -176;
      VAL_S = 'inoussa';
var
  x, y : TRttiFilterCreator;
  xin : TRttiExpIntegerNodeItem;
  xn : TRttiExpNode;
  sfltr : string;
begin
  y := nil;
  x := TRttiFilterCreator.Create(TClass_A);
  try
    sfltr := Format('IntProp > %d or ( IntProp < %d and StrProp = %s ) or IntProp = %d',[VAL_1,VAL_2,QuotedStr(VAL_S),VAL_4]);
    ParseFilter(sfltr,x);
    PrintTree(x.Root,@Display,2);
    y := TRttiFilterCreator.Create(TClass_A);
    y.AddCondition('IntProp',nfoGreater,VAL_1,fcOr);
    y.BeginGroup(fcOr);
      y.AddCondition('IntProp',nfoLesser,VAL_2,fcAnd);
      y.AddCondition('StrProp',sfoEqualCaseInsensitive,VAL_S,fcAnd);
    y.EndGroup();
    y.AddCondition('IntProp',nfoEqual,VAL_4,fcOr);

    CompareTree(x.Root,y.Root);
  finally
    x.Free();
  end;
end;


Initialization
  RegisterTests(
    [ TRttiExpIntegerNodeItem_Test,
      TRttiExpAnsiStringNodeItem_Test,
      TRttiExpwWideStringNodeItem_Test,
      TRttiExpNode_Test,TRttiFilterCreator_Test,

      TRttiParser_Test
    ]
  );
  
end.
