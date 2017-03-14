unit MSCRDataClasses;

{$MODE Delphi}

interface

uses Classes, ComponentList, Grids, SysUtils, NumericGrid, SimpMath,
     Tools, TableComp, DataClasses, DataLoader;

const
    //  konstanty rezhima otobrazheniya
    XCM_2T    = 0; 
    XCM_T     = 1; 
    XCM_SINTL = 2; 
    
const
    StartPosName: string = 'Start Pos.';
    FinishPosName: string = 'Finish Pos.';

type
    //  pozvolyaet vvodit'/vyvodit' ugly v razlichnyh predstavleniyah;
    //  pri zagruzke dannyh iz Grid'a proveryaet korrektnost' i
    //  dobavlyaet v spisok tol'ko te dannye, dlya kot. sootvetstvuyuschie
    //  stroki sovershenno korrektny
    TMSCRSpecimenList = class(TSpecimenList)
    protected
        function RecalcParamValue(P: TSpecialCurveParameter): Double; override;
        procedure ReverseCalcParamValue(
            P: TSpecialCurveParameter; NewValue: Double); override;
        
    public
        Lambda: Double;   //  dlina volny pri kotoroy byla snyata neytronogramma
        ViewMode: LongInt;//  predpolagaetsya,  chto dannye zadany v formate 2 * Theta
      
        function GetCopy: TObject; override;
        procedure CopyParameters(const Dest: TObject); override;
    end; 
    
    //  spisok parametrov krivyh, sohranyaemyy v XML-potoke
    Parameters_list = class(TComponent)
    private
        FParameters: TMSCRSpecimenList;

    public
        constructor Create(Owner: TComponent); override;
        destructor Destroy; override;

    published
        property Parameters: TMSCRSpecimenList
            read FParameters write FParameters;
    end;

implementation

function TMSCRSpecimenList.RecalcParamValue(
    P: TSpecialCurveParameter): Double;
begin
    if (P.Type_ = InvariablePosition) or (P.Type_ = VariablePosition) or
       (P.Name = StartPosName) or (P.Name = FinishPosName) then
    begin
        case ViewMode of
        XCM_2T: begin
            //  schitaetsya, chto iznachal'no koordinaty zadany v 2*Theta
            Result := P.Value;
            end;

        XCM_T : begin
            //  pereschet v Theta
            Result := P.Value / 2;
            end;

        XCM_SINTL : begin
            Assert(Lambda <> 0);
            //  pereschet v Sin(Theta)/Lambda
            Result := Sin((P.Value * pi) / (2 * 180)) / Lambda;
            end;
        end;{case ViewMode of...}
    end
    else Result := P.Value;
end;

procedure TMSCRSpecimenList.ReverseCalcParamValue(
    P: TSpecialCurveParameter; NewValue: Double);
begin
    if (P.Type_ = InvariablePosition) or (P.Type_ = VariablePosition) or
       (P.Name = StartPosName) or (P.Name = FinishPosName) then
    begin
        case ViewMode of
            XCM_T : P.Value := NewValue * 2;
            XCM_2T : P.Value := NewValue;
            XCM_SINTL : begin
                Assert(Lambda <> 0);
                P.Value := 2 * (180 / pi) * ArcSin(NewValue * Lambda);
                end;
        end; {case ViewMode of...}
    end
    else P.Value := NewValue;
end;

function TMSCRSpecimenList.GetCopy: TObject;
begin
    Result := TMSCRSpecimenList.Create(nil);
    CopyParameters(Result);
end;

procedure TMSCRSpecimenList.CopyParameters(const Dest: TObject);
begin
    inherited;
    TMSCRSpecimenList(Dest).Lambda := Lambda;
    TMSCRSpecimenList(Dest).ViewMode := ViewMode;
end;

{ Parameters_list }

constructor Parameters_list.Create(Owner: TComponent);
begin
    inherited Create(Owner);
    FParameters := TMSCRSpecimenList.Create(nil);
end;

destructor Parameters_list.Destroy;
begin
    FParameters.Free;
    inherited Destroy;
end;

initialization
    RegisterClass(TMSCRSpecimenList);
    DecimalSeparator := '.';
end.


