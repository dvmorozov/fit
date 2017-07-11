//      dvoynoy kosoy chertoy kommentiruyutsya zamechaniya, sohranyaemye vo
//      vseh versiyah ishodnika; figurnymi skobkami kommentiruyutsya zamechaniya,
//      sohranyaemye tol'ko v versii ishodnika dlya besplatnogo rasprostraneniya
{------------------------------------------------------------------------------}
{       Copyright (C) 1999-2007 D.Morozov (dvmorozov@mail.ru)                  }
{------------------------------------------------------------------------------}
unit AlgorithmContainer;

{$MODE Delphi}

interface

uses
    Classes, Runner, Algorithm, Tools;

type
    TAlgorithmContainer = class(TComponent)
    protected
        Algorithm: TAlgorithm;
        //  vypolnyaet nastroyku okruzheniya dlya vyzova algoritma,
        //  sozdaet ob'ekt algoritma i zapuskaet metod ego realizatsii
        procedure Running; virtual; abstract;
        //  vyzyvaetsya posle zaversheniya raboty algoritma -
        //  mozhet vydavat' kakoy-nibud' signal
        procedure RunningFinished; virtual; abstract;
        //  sozdaet trebuemyy algoritm
        procedure CreateAlgorithm; virtual; abstract;
        //  unichtozhaet komponent - algoritm;
        procedure DestroyAlgorithm; virtual; abstract;

    public
        //  ustanavlivaet flagi, neobhodimye dlya prekrascheniya raboty algoritma
        procedure StopAlgorithm; virtual; abstract;
        procedure Run; virtual;
    end;

    //  relizuet zapusk algoritma v otdel'nom potoke
    //  !!! DestroyAlgorithm dolzhen vyzyvat'sya
    //  obyazatel'no posle unichtozheniya Runner'a !!!
    TRunningAlgorithmContainer = class(TAlgorithmContainer)
    protected
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


