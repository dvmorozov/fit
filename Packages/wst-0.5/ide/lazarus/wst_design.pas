{ Ce fichier a �t� automatiquement cr�� par Lazarus. Ne pas l'�diter !
Cette source est seulement employ�e pour compiler et installer le paquet.
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
