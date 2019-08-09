{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definitions of thread containers.)

@author(Dmitry Morozov dvmorozov@hotmail.com, 
LinkedIn https://ru.linkedin.com/pub/dmitry-morozov/59/90a/794, 
Facebook https://www.facebook.com/profile.php?id=100004082021870)
}
unit runner_thread;

{$MODE Delphi}

interface

uses Classes, Tools;

type
	TRunningProcedure = procedure of object;
	TEndRunningProcedure = procedure of object;

	TRunningThread = class(TThread)
	{ If process was terminated by means of object destruction then termination procedure is not called. }
	public
		RunningProcedure: TRunningProcedure;
		EndRunningProcedure: TEndRunningProcedure;
		procedure Execute; override;
	end;

	{ Class-container for TRunningThread. }
	TRunner = class(TComponent)
	protected
		FOnRunningProcedure: TRunningProcedure;
		FOnEndRunningProcedure: TEndRunningProcedure;
		RunningThread: TRunningThread;

	public
		constructor Create(AOwner: TComponent); override;
		destructor Destroy; override;
		procedure Run;
		procedure Suspend;
		procedure Resume;
		procedure Synchronize(AProcedure: TThreadMethod);

	published
		property OnRunningProcedure: TRunningProcedure
				read FOnRunningProcedure		write FOnRunningProcedure;
		property OnEndRunningProcedure: TEndRunningProcedure
				read FOnEndRunningProcedure		write FOnEndRunningProcedure;
	end;

procedure Register;

implementation

procedure Register;
begin
	RegisterComponents('Common',[TRunner]);
	(*???
	RegisterPropertyEditor(TypeInfo(TRunningProcedure),TRunner,'OnRunningProcedure',TMethodProperty);
	RegisterPropertyEditor(TypeInfo(TEndRunningProcedure),TRunner,'OnEndRunningProcedure',TMethodProperty);
	*)
end;

procedure TRunningThread.Execute;
begin
	if Assigned(RunningProcedure) then RunningProcedure;
	if (not Terminated) and Assigned(EndRunningProcedure) then EndRunningProcedure;
end;

constructor TRunner.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);
	RunningThread := TRunningThread.Create(True);
end;

destructor TRunner.Destroy;
begin
	UtilizeObject(RunningThread);
	inherited Destroy;
end;

{$warnings off}
procedure TRunner.Run;
begin
	RunningThread.RunningProcedure := OnRunningProcedure;
	RunningThread.EndRunningProcedure := OnEndRunningProcedure;
	RunningThread.Resume;
end;

procedure TRunner.Suspend;
begin
	RunningThread.Suspend;
end;

procedure TRunner.Resume;
begin
	RunningThread.Resume;
end;
{$warnings on}

procedure TRunner.Synchronize(AProcedure: TThreadMethod);
begin
	RunningThread.Synchronize(AProcedure);
end;

end.


