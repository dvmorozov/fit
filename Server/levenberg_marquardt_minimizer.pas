// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Levenberg-Marquardt, through the adapter every minimiser here is
used through.)

THE SIMPLEX'S MINIMISER, RUNNING ANOTHER ALGORITHM. Everything the host sees -
the events of int_minimizer.TMinimizer, the walk over its parameters, Stop - is
TDownhillSimplexMinimizer's; this only puts fitminimizers'
TLevenbergMarquardtServer in its place and hands the algorithm the residuals
through OnGetResiduals. For a fit with an exact answer, where the simplex stalls
short (fitminimizers, LevenbergMarquardtAlgorithm).

Copyright (C) Dmitry Morozov
}
unit levenberg_marquardt_minimizer;

interface

uses Classes, Types, DownhillSimplexServer, LevenbergMarquardtServer,
    downhill_simplex_minimizer, checks;

type
    TLevenbergMarquardtMinimizer = class(TDownhillSimplexMinimizer,
        IOptimizedResiduals)
    private
        FResiduals: TDoubleDynArray;
    protected
        function NewServer: TDownhillSimplexServer; override;
    public
        {IOptimizedResiduals}
        function GetResidualsNumber: longint;
        function GetResidual(index: longint): double;
    end;

implementation

function TLevenbergMarquardtMinimizer.NewServer: TDownhillSimplexServer;
var
    Lm: TLevenbergMarquardtServer;
begin
    Lm := TLevenbergMarquardtServer.Create(nil);
    Lm.OptimizedResiduals := Self;
    Result := Lm;
end;

function TLevenbergMarquardtMinimizer.GetResidualsNumber: longint;
begin
    //  Read once per point: the algorithm asks for the count right after the
    //  point is evaluated, then for each residual.
    CheckThat(Assigned(OnGetResiduals),
        'Levenberg-Marquardt needs the residuals: OnGetResiduals is not set');
    FResiduals := OnGetResiduals();
    Result := Length(FResiduals);
end;

function TLevenbergMarquardtMinimizer.GetResidual(index: longint): double;
begin
    Result := FResiduals[index];
end;

end.
