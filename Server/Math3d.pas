{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definitions of 3D primitive functions.)

@author(Dmitry Morozov dvmorozov@hotmail.com, 
LinkedIn https://ru.linkedin.com/pub/dmitry-morozov/59/90a/794, 
Facebook https://www.facebook.com/profile.php?id=100004082021870)
}
unit Math3d;

{$MODE Delphi}

interface

uses SimpMath;

type
  TMatrix = array[1..4, 1..4] of Double;
  T4Vector = array[1..4] of Double;
  T3Vector = TDoubleVector3;

procedure StandardVectTrans(var Vector: T3Vector);
procedure RotationX(Angle: Double);
procedure RotationX2(Angle: Double);
procedure RotationY(Angle: Double);
procedure RotationY2(Angle: Double);
procedure RotationZ(Angle: Double);
procedure Dilatation(A, B, C: Double);
procedure ReflectionXY;
procedure ReflectionYZ;
procedure ReflectionZX;
procedure Translation(A, B, C: Double);
procedure Transition(Al, Bt, Gm: Double);
procedure Pro(A, B, C: Double);

procedure GetZerosMatrix(var Matr: TMatrix);
{ Returns unitary matrix. }
procedure GetUnitMatrix(var Matr: TMatrix);
{ Returns inverse matrix. }
procedure GetReverseMatrix(var Matr: TMatrix);
{ Returns transformation matrix. }
procedure GetMatrixTransition(Al, Bt, Gm: Double; var Matr: TMatrix);
{ Returns translation matrix. }
procedure GetMatrixTrans(A, B, C: Double; var Matr: TMatrix);
{ Returns dilatation matrix. }
procedure GetMatrixDilat(A, B, C: Double; var Matr: TMatrix);
{ Returns rotation matrix around X-axis. The angle is given in radians. }
procedure GetMatrixRotX(Angle: Double; var Matr: TMatrix);
{ Returns rotation matrix around Y-axis. The angle is given in radians. }
procedure GetMatrixRotY(Angle: Double; var Matr: TMatrix);
{ Returns rotation matrix around Z-axis. The angle is given in radians. }
procedure GetMatrixRotZ(Angle: Double; var Matr: TMatrix);

procedure MulVectMatr(Matr: TMatrix; var Vector: T3Vector);
{ Returns product of matrices. Result is placed into Matr parameter.
  As result container any of the original variables can be given. }
procedure Mul3DMatrix(var A, B: TMatrix; var Matr: TMatrix);

function Rad(Angle: Double): Double;

var TempMatr2, TempRotMatr: TMatrix;
    RotXMatr, RotYMatr, RotZMatr, DilatMatr, TempMatr: TMatrix;

implementation

function Rad(Angle: Double): Double; 
begin
  Rad := (Angle / 180) * Pi
end;

procedure GetZerosMatrix(var Matr: TMatrix);
var i, j: Integer;
begin
  for i := 1 to 4 do
    for j := 1 to 4 do Matr[i, j] := 0;
end;

procedure GetUnitMatrix(var Matr: TMatrix);
var i, j: Integer;
begin
  for i := 1 to 4 do
    for j := 1 to 4 do
      if i = j then Matr[i, j] := 1 else Matr[i, j] := 0;
end;

procedure GetReverseMatrix(var Matr: TMatrix);
var i: Integer;
begin
    GetZerosMatrix(Matr);
    for i := 1 to 3 do Matr[i, i] := -1;
    Matr[4, 4] := 1;
end;

procedure Mov3in4Vector(Vect1: T3Vector; var Vect2: T4Vector);
var i: Integer;
begin
  for i := 1 to 3 do Vect2[i] := Vect1[i];
  Vect2[4] := 1
end;

procedure Mov4in3Vector(Vect1: T4Vector; var Vect2: T3Vector);
var i: Integer;
begin
  for i := 1 to 3 do Vect2[i] := Vect1[i] * Vect1[4];
end;

procedure GetMatrixRotX(Angle: Double; var Matr: TMatrix);
begin
  GetZerosMatrix(Matr);
  Matr[1, 1] := 1;
  Matr[2, 2] := Cos(Angle);
  Matr[2, 3] := Sin(Angle);
  Matr[3, 2] := (-1) * Sin(Angle);
  Matr[3, 3] := Cos(Angle);
  Matr[4, 4] := 1;
end;

procedure GetMatrixRotY(Angle: Double; var Matr: TMatrix);
begin
  GetZerosMatrix(Matr);
  Matr[1, 1] := Cos(Angle);
  Matr[1, 3] := (-1) * Sin(Angle);
  Matr[2, 2] := 1;
  Matr[3, 1] := Sin(Angle);
  Matr[3, 3] := Cos(Angle);
  Matr[4, 4] := 1;
end;

procedure GetMatrixRotZ(Angle: Double; var Matr: TMatrix);
begin
  GetZerosMatrix(Matr);
  Matr[1, 1] := Cos(Angle);
  Matr[1, 2] := Sin(Angle);
  Matr[2, 1] := (-1) * Sin(Angle);
  Matr[2, 2] := Cos(Angle);
  Matr[3, 3] := 1;
  Matr[4, 4] := 1;
end;

procedure GetMatrixRefXY(var Matr: TMatrix);
begin
  GetZerosMatrix(Matr);
  Matr[1, 1] := 1;
  Matr[2, 2] := 1;
  Matr[3, 3] := -1;
  Matr[4, 4] := 1;
end;

procedure GetMatrixRefYZ(var Matr: TMatrix);
begin
  GetZerosMatrix(Matr);
  Matr[1, 1] := -1;
  Matr[2, 2] := 1;
  Matr[3, 3] := 1;
  Matr[4, 4] := 1;
end;

procedure GetMatrixRefZX(var Matr: TMatrix);
begin
  GetZerosMatrix(Matr);
  Matr[1, 1] := 1;
  Matr[2, 2] := -1;
  Matr[3, 3] := 1;
  Matr[4, 4] := 1;
end;

procedure GetMatrixDilat(A, B, C: Double; var Matr: TMatrix);
begin
  GetZerosMatrix(Matr);
  Matr[1, 1] := A;
  Matr[2, 2] := B;
  Matr[3, 3] := C;
  Matr[4, 4] := 1;
end;

procedure GetMatrixTrans(A, B, C: Double; var Matr: TMatrix);
begin
  GetZerosMatrix(Matr);
  Matr[1, 1] := 1;
  Matr[2, 2] := 1;
  Matr[3, 3] := 1;
  Matr[4, 1] := A;
  Matr[4, 2] := B;
  Matr[4, 3] := C;
  Matr[4, 4] := 1;
end;

procedure GetMatrixTransition(Al, Bt, Gm: Double; var Matr: TMatrix);
begin
  GetZerosMatrix(Matr);
(*  Matr[1, 1] := 1;
  Matr[1, 2] := Cos(Gm);
  Matr[1, 3] := Cos(Bt);
  Matr[2, 1] := 0;
  Matr[2, 2] := Sin(Gm);
  Matr[2, 3] := Cos(Al) * Sin(Bt);
  Matr[3, 1] := 0;
  Matr[3, 2] := 0;
  Matr[3, 3] := Sin(Al) * Sin(Bt);
  Matr[4, 4] := 1; *)
  Matr[1, 1] := 1;
  Matr[2, 1] := Cos(Gm);
  Matr[2, 2] := Sin(Gm);
  Matr[2, 3] := 0;
  Matr[3, 1] := Cos(Bt);
  Matr[3, 2] := Cos(Al) * Sin(Bt);
  Matr[3, 3] := Sin(Al) * Sin(Bt);
  Matr[4, 4] := 1;
 (*
   1          Cos(Gamma)              Cos(Beta)
   0          Sin(Gamma)    Cos(Alpha)Sin(Beta)
   0          0             Sin(Alpha)Sin(Beta)
 *)
end;

procedure GetMatrixPro(A, B, C: Double; var Matr: TMatrix);
begin
  GetZerosMatrix(Matr);
  Matr[1, 1] := 1;
  Matr[2, 2] := 1;
  Matr[3, 3] := 1;
  Matr[1, 4] := A;
  Matr[2, 4] := B;
  Matr[3, 4] := C;
  Matr[4, 4] := 1;
end;

{$hints off}
procedure Mul3DMatrix(var A, B: TMatrix; var Matr: TMatrix);
var i, k, l: Integer;
    TempMatr: TMatrix;
begin
  GetZerosMatrix(TempMatr);
  for i := 1 to 4 do
    for k := 1 to 4 do
      for l := 1 to 4 do TempMatr[i, k] := TempMatr[i, k] + A[i, l] * B[l, k];
  Matr := TempMatr;
end;
{$hints on}

procedure Add3DMatrix(var A, B: TMatrix; var Matr: TMatrix);
var i, k: Integer;
begin
  for i := 1 to 4 do
    for k := 1 to 4 do Matr[i, k] := A[i, k] + B[i, k];
end;

{$hints off}
procedure MulVectMatr(Matr: TMatrix; var Vector: T3Vector);
var Vect1, Vect2: T4Vector;
    i, k: Integer;
begin
    Mov3in4Vector(Vector, Vect1);
    for i := 1 to 4 do Vect2[i] := 0;
    for i := 1 to 4 do
        for k := 1 to 4 do Vect2[i] := Vect2[i] + Vect1[k] * Matr[k, i];
    Mov4in3Vector(Vect2, Vector);
end;
{$hints on}

procedure RotationX(Angle: Double);
var TempMatr3: TMatrix;
    i, j: Longint;
begin
  for i := 1 to 4 do
    for j := 1 to 4 do TempMatr3[i, j] := TempMatr2[i, j];
  GetMatrixRotX(Angle, RotXMatr);
  Mul3DMatrix(RotXMatr, TempMatr3, TempMatr2);
end;

{$hints off}
procedure RotationX2(Angle: Double);
var TempMatr3, TempRotXMatr: TMatrix;
    i, j: Longint;
begin
  for i := 1 to 4 do
    for j := 1 to 4 do TempMatr3[i, j] := TempRotMatr[i, j];
  GetMatrixRotX(Angle, TempRotXMatr);
  Mul3DMatrix(TempMatr3, TempRotXMatr, TempRotMatr);
end;
{$hints on}

procedure RotationY(Angle: Double);
var TempMatr3: TMatrix;
    i, j: Longint;
begin
  for i := 1 to 4 do
    for j := 1 to 4 do TempMatr3[i, j] := TempMatr2[i, j];
  GetMatrixRotY(Angle, RotYMatr);
  Mul3DMatrix(RotYMatr, TempMatr3, TempMatr2);
end;

{$hints off}
procedure RotationY2(Angle: Double);
var TempMatr3, TempRotYMatr: TMatrix;
    i, j: Longint;
begin
  for i := 1 to 4 do
    for j := 1 to 4 do TempMatr3[i, j] := TempRotMatr[i, j];
  GetMatrixRotY(Angle, TempRotYMatr);
  Mul3DMatrix(TempMatr3, TempRotYMatr, TempRotMatr);
end;
{$hints on}

procedure RotationZ(Angle: Double);
var TempMatr3: TMatrix;
    i, j: Longint;
begin
  for i := 1 to 4 do
    for j := 1 to 4 do TempMatr3[i, j] := TempMatr2[i, j];
  GetMatrixRotZ(Angle, RotZMatr);
  Mul3DMatrix(RotZMatr, TempMatr3, TempMatr2);
end;

procedure Dilatation(A, B, C: Double);
begin
  GetMatrixDilat(A, B, C, DilatMatr);
end;

procedure ReflectionXY;
begin
  GetMatrixRefXY(TempMatr);
end;

procedure ReflectionYZ;
begin
  GetMatrixRefYZ(TempMatr);
end;

procedure ReflectionZX;
begin
  GetMatrixRefZX(TempMatr);
end;

procedure Translation(A, B, C: Double);
begin
  GetMatrixTrans(A, B, C, TempMatr);
end;

procedure Transition(Al, Bt, Gm: Double);
begin
  GetMatrixTransition(Al, Bt, Gm, TempMatr);
end;

procedure Pro(A, B, C: Double);
begin
  GetMatrixPro(A, B, C, TempMatr);
end;

procedure StandardVectTrans(var Vector: T3Vector);
begin
  MulVectMatr(TempMatr, Vector);
  MulVectMatr(TempMatr2, Vector);
  MulVectMatr(TempRotMatr, Vector);
  MulVectMatr(DilatMatr, Vector);
end;

initialization
  GetUnitMatrix(RotXMatr);
  GetUnitMatrix(RotYMatr);
  GetUnitMatrix(RotZMatr);
  GetUnitMatrix(DilatMatr);
  GetUnitMatrix(TempMatr2);
  GetUnitMatrix(TempMatr);
  GetUnitMatrix(TempRotMatr);
end.


