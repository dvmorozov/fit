unit test_parserdef;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, parserdefs, fpcunit, testregistry;
  
type

  { TTest_TClassTypeDefinition }

  TTest_TClassTypeDefinition = class(TTestCase)
  published
    procedure IsDescendantOf();
    procedure SetParent();
  end;

implementation

{ TTest_TClassTypeDefinition }

procedure TTest_TClassTypeDefinition.IsDescendantOf();
var
  a, b , c, d : TClassTypeDefinition;
begin
  a := nil;
  b := nil;
  c := nil;
  d := nil;
  try
    a := TClassTypeDefinition.Create('a');

    b := TClassTypeDefinition.Create('b');
    b.SetParent(a);

    c := TClassTypeDefinition.Create('c');
    c.SetParent(b);
    
    d := TClassTypeDefinition.Create('d');
    
    AssertTrue('b IsDescendantOf a',b.IsDescendantOf(a));
    AssertTrue('c IsDescendantOf b',c.IsDescendantOf(b));
    AssertTrue('c IsDescendantOf a',c.IsDescendantOf(a));
    
    AssertFalse('b IsDescendantOf c',b.IsDescendantOf(c));
    AssertFalse('a IsDescendantOf b',a.IsDescendantOf(b));
    AssertFalse('a IsDescendantOf c',a.IsDescendantOf(c));

    AssertFalse('d IsDescendantOf a',d.IsDescendantOf(a));

  finally
    FreeAndNil(d);
    FreeAndNil(c);
    FreeAndNil(b);
    FreeAndNil(a);
  end;
end;

procedure TTest_TClassTypeDefinition.SetParent();
var
  a, b , c, d : TClassTypeDefinition;
  excp : Boolean;
begin
  a := nil;
  b := nil;
  c := nil;
  d := nil;
  try
    a := TClassTypeDefinition.Create('a');

    b := TClassTypeDefinition.Create('b');
    b.SetParent(a);

    c := TClassTypeDefinition.Create('c');
    c.SetParent(b);

    d := TClassTypeDefinition.Create('d');

    excp := False;;
    try
      c.SetParent(c);
    except
      excp := True;
    end;
    if not excp then begin
      Fail('c.SetParent(c);');
    end;
    
    AssertSame('a.Parent = nil',nil,a.Parent);
    AssertSame('b.Parent = a',a,b.Parent);
    AssertSame('c.Parent = b',b,c.Parent);
    AssertSame('d.Parent = nil',nil,d.Parent);

  finally
    FreeAndNil(d);
    FreeAndNil(c);
    FreeAndNil(b);
    FreeAndNil(a);
  end;

end;

initialization
  RegisterTest(TTest_TClassTypeDefinition);
  
end.

