{ Ce fichier a été automatiquement créé par Lazarus. Ne pas l'éditer !
Cette source est seulement employée pour compiler et installer le paquet.
 }

unit wst_design; 

interface

uses
  wstimportdlg, wst_register, uwsttypelibraryedit, uabout, LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit ('wst_register', @wst_register.Register ); 
end; 

initialization
  RegisterPackage ('wst_design', @Register ); 
end.
