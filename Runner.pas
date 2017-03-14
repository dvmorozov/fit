//      dvoynoy kosoy chertoy kommentiruyutsya zamechaniya, sohranyaemye vo
//      vseh versiyah ishodnika; figurnymi skobkami kommentiruyutsya zamechaniya,
//      sohranyaemye tol'ko v versii ishodnika dlya besplatnogo rasprostraneniya
{------------------------------------------------------------------------------}
{       Copyright (C) 1999-2007 D.Morozov (dvmorozov@mail.ru)                  }
{------------------------------------------------------------------------------}
unit Runner;

{$MODE Delphi}

interface

uses Classes, Tools;

type
    TRunningProcedure = procedure of object;
    TEndRunningProcedure = procedure of object;

    TRunningThread = class(TThread)
        //  !!! esli protsess byl prekraschen s pomosch'yu unichtozheniya
        //  komponenta, to protsedura zaversheniya ne vyzyvaetsya !!!
    public
        RunningProcedure: TRunningProcedure;
        EndRunningProcedure: TEndRunningProcedure;
        procedure Execute; override;
    end;
    
    TRunner = class(TComponent)
        //  komponent - konteyner dlya TRunningThread
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
                read FOnRunningProcedure        write FOnRunningProcedure;
        property OnEndRunningProcedure: TEndRunningProcedure
                read FOnEndRunningProcedure     write FOnEndRunningProcedure;
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

procedure TRunner.Synchronize(AProcedure: TThreadMethod);
begin
    RunningThread.Synchronize(AProcedure);
end;

end.


