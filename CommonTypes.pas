unit CommonTypes;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils;

type
    TCurveType = (
        Gaussian,
        Lorentzian,
        Special,
        PseudoVoigtian,
        AsymPseudoVoigtian,
        TwoBranchesPseudoVoigtian
        );
    //  sostoyaniya servera
    TFitServerState = (
        //  !!! posledovatel'nost' sostoyaniy oboznachena tsiframi !!!
        ProfileWaiting,         //  ozhidanie zagruzki dannyh profilya          (1)
        BackNotRemoved,         //  fon esche ne otsechen posle posledney       (2)
                                //  zagruzki dannyh; !!! sostoyanie ne
                                //  d. menyat'sya pri zagruzke tochek fona !!!
        AsyncOperation,         //  vypolnyaetsya dlitel'naya operatsiya        (4)
        //  eti sostoyaniaya dolzhny ispol'zovat'sya tol'ko dlya
        //  informirovaniya pol'zovatelya - podgonka dolzhna byt'
        //  razreshena v lubom sluchae
        ReadyForAutoFit,        //  fon uzhe otsechen (gotovnost' k podgonke    (3)
                                //  krivyh v avtomaticheskom rezhime)
        ReadyForFit             //  gotovnost' k podgonke pri zadannyh          (3)
                                //  pol'zovatelem ogranicheniyah
        );

implementation

end.

