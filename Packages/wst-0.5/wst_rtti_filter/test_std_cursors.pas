{$INCLUDE wst_global.inc}
unit test_std_cursors;

interface

uses
  Classes, SysUtils, Contnrs,
  fpcunit, testutils, testregistry,
  cursor_intf, std_cursors, rtti_filters;

{$INCLUDE wst.inc}
{$INCLUDE wst_delphi.inc}  
  
type

  { TClass_A }

  TClass_A = class(TPersistent)
  private
    FIntProp: Integer;
  public
    constructor Create(AIntProp : Integer);
  published
    property IntProp : Integer read FIntProp;
  end;

  { TClass_B }

  TClass_B = class(TClass_A)
  private
    FIntProp_A: Integer;
    FIntProp_B: Integer;
  public
    constructor Create(AIntProp,AIntProp_A,AIntProp_B : Integer);
  published
    property IntProp_A : Integer read FIntProp_A;
    property IntProp_B : Integer read FIntProp_B;
  end;

  { TObjectListCursor_Test }

  TObjectListCursor_Test = class(TTestCase)
  published
    procedure All();
  end;

  { TObjectListFilterableCursor_Test }

  TObjectListFilterableCursor_Test = class(TTestCase)
  published
    procedure All();
  end;

implementation

{ TClass_A }

constructor TClass_A.Create(AIntProp: Integer);
begin
  FIntProp := AIntProp;
end;

{ TObjectListCursor_Test }

procedure TObjectListCursor_Test.All();
const O_COUNT = 100;
var
  x : IObjectCursor;
  ls : TObjectList;
  c, i : Integer;
begin
  ls := TObjectList.Create(True);
  try
    x := TObjectListCursor.Create(ls);
    x.Reset();
    CheckEquals(False,x.MoveNext());
    x.Reset();
    CheckEquals(False,x.MoveNext());
    CheckEquals(False,x.MoveNext());
    try
      x.GetCurrent();
      Check(False);
    except
      on e : ECursorException do begin
        // GOOD
      end;
    end;
    
    ls.Add(TClass_A.Create(0));
    x.Reset();
    CheckEquals(True,x.MoveNext());
    CheckSame(ls[0],x.GetCurrent());
    CheckEquals(False,x.MoveNext());
    try
      x.GetCurrent();
      Check(False);
    except
      on e : ECursorException do begin
        // GOOD
      end;
    end;
    x.Reset();
    CheckEquals(True,x.MoveNext());
    CheckSame(ls[0],x.GetCurrent());
    CheckEquals(False,x.MoveNext());

    ls.Clear();
    for i := 0 to Pred(O_COUNT) do
      ls.Add(TClass_A.Create(i));
    x.Reset();
    for i := 0 to Pred(O_COUNT) do begin
      CheckEquals(True,x.MoveNext());
      CheckSame(ls[i],x.GetCurrent());
    end;
    CheckEquals(False,x.MoveNext());
    x.Reset();
    for i := 0 to Pred(O_COUNT) do begin
      CheckEquals(True,x.MoveNext());
      CheckSame(ls[i],x.GetCurrent());
    end;
  finally
    ls.Free();
  end;
end;


{ TClass_B }

constructor TClass_B.Create(AIntProp, AIntProp_A, AIntProp_B: Integer);
begin
  inherited Create(AIntProp);
  FIntProp_A := AIntProp_A;
  FIntProp_B := AIntProp_B;
end;

{ TObjectListFilterableCursor_Test }

procedure TObjectListFilterableCursor_Test.All();
const O_COUNT = 100;
var
  x : IFilterableObjectCursor;
  ls : TObjectList;
  c, i : Integer;
  f : IObjectFilter;
  fcr : TRttiFilterCreator;
begin
  fcr := nil;
  ls := TObjectList.Create(True);
  try
    x := TObjectListFilterableCursor.Create(ls);
    CheckNull(x.GetFilter());
    x.Reset();
    CheckEquals(False,x.MoveNext());
    x.Reset();
    CheckEquals(False,x.MoveNext());
    CheckEquals(False,x.MoveNext());
    try
      x.GetCurrent();
      Check(False);
    except
      on e : ECursorException do begin
        // GOOD
      end;
    end;

    ls.Add(TClass_A.Create(0));
    x.Reset();
    CheckEquals(True,x.MoveNext());
    CheckSame(ls[0],x.GetCurrent());
    CheckEquals(False,x.MoveNext());
    try
      x.GetCurrent();
      Check(False);
    except
      on e : ECursorException do begin
        // GOOD
      end;
    end;
    x.Reset();
    CheckEquals(True,x.MoveNext());
    CheckSame(ls[0],x.GetCurrent());
    CheckEquals(False,x.MoveNext());

    ls.Clear();
    for i := 0 to Pred(O_COUNT) do
      ls.Add(TClass_A.Create(i));
    x.Reset();
    for i := 0 to Pred(O_COUNT) do begin
      CheckEquals(True,x.MoveNext());
      CheckSame(ls[i],x.GetCurrent());
    end;
    CheckEquals(False,x.MoveNext());
    x.Reset();
    for i := 0 to Pred(O_COUNT) do begin
      CheckEquals(True,x.MoveNext());
      CheckSame(ls[i],x.GetCurrent());
    end;
    
    ls.Clear();
    for i := 0 to Pred(O_COUNT) do
      ls.Add(TClass_B.Create(i,( i mod 10 ), ( i mod ( ( i + 1 ) * 2 ) ) ));

    fcr := TRttiFilterCreator.Create(TClass_B);
    fcr.AddCondition('IntProp',nfoEqual,-1,fcOr);//
    f := TRttiObjectFilter.Create(fcr.Root,clrFreeObjects) as IObjectFilter;
    x.SetFilter(f);
    Check(x.GetFilter()=f);
    x.SetFilter(nil);
    CheckNull(x.GetFilter());
    x.SetFilter(f);
    Check(x.GetFilter()=f);
    x.Reset();
    CheckEquals(False,x.MoveNext());
    
    fcr.AddCondition('IntProp',nfoGreater,-1,fcOr);
    x.Reset();
    CheckEquals(True,x.MoveNext());

    x.Reset();
    for i := 0 to Pred(O_COUNT) do begin
      CheckEquals(True,x.MoveNext());
      CheckSame(ls[i],x.GetCurrent());
    end;

  finally
    ls.Free();
    fcr.Free();
  end;
end;

Initialization
  RegisterTests([TObjectListCursor_Test,TObjectListFilterableCursor_Test]);

end.
