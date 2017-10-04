{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definitions of auxiliary classes used to control algorithm execution.)

@author(Dmitry Morozov dvmorozov@hotmail.com, 
LinkedIn https://ru.linkedin.com/pub/dmitry-morozov/59/90a/794, 
Facebook https://www.facebook.com/profile.php?id=100004082021870)
}

unit AlgorithmContainer;

{$MODE Delphi}

interface

uses
    Classes, Runner, Algorithm, Tools;

type
	{ Class defines abstract methods to control any type of algorithms. } 
    TAlgorithmContainer = class(TComponent)
    protected
        Algorithm: TAlgorithm;
		{Method creates appropriate environment for executing algorithm,
		create algorithm object and start execution.}
        procedure Running; virtual; abstract;
		{Method is called after finishing execution of algorithm. 
		Can be used to do post processing and displaying results.}
        procedure RunningFinished; virtual; abstract;
        {Descendants override this method to create algorithm object of appropriate type.}
        procedure CreateAlgorithm; virtual; abstract;
        {Method destroys algorithm object.}
        procedure DestroyAlgorithm; virtual; abstract;

    public
		{Method implements actions necessary to abort execution of algorithm.}
        procedure StopAlgorithm; virtual; abstract;
        procedure Run; virtual;
    end;

	{Class allows executing algorithm in separate thread.}
    TRunningAlgorithmContainer = class(TAlgorithmContainer)
    protected
		{Object implementing separate thread.
		DestroyAlgorithm must be called after destroying the object.}
        Runner: TRunner;
    public
        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;
        procedure Suspend; virtual;
        procedure Resume; virtual;
        
        procedure Run; override;
    end;

procedure Register;

implementation

procedure Register;
begin
end;

constructor TRunningAlgorithmContainer.Create(AOwner: TComponent);
begin
    inherited Create(nil);
    Runner := TRunner.Create(nil);
    Runner.OnRunningProcedure := Running;
    Runner.OnEndRunningProcedure := RunningFinished;
end;

destructor TRunningAlgorithmContainer.Destroy;
begin
    UtilizeObject(Runner);
    inherited;
end;

procedure TRunningAlgorithmContainer.Run;
begin
    Runner.Run;
end;

procedure TRunningAlgorithmContainer.Suspend;
begin
    Runner.Suspend;
end;

procedure TRunningAlgorithmContainer.Resume;
begin
    Runner.Resume;
end;

procedure TAlgorithmContainer.Run;
begin
    Running;
    RunningFinished;
end;

initialization
    RegisterClass(TAlgorithmContainer);
    RegisterClass(TRunningAlgorithmContainer);
end.


