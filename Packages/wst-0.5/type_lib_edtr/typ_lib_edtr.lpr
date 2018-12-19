program typ_lib_edtr;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms
  , uwsttypelibraryedit, view_helper, uabout, ufEnumedit,
  edit_helper, ufclassedit, wsdl_generator, ufpropedit, uinterfaceedit, udm,
  pascal_parser_intf, PasTree, PParser, uprocedit, common_gui_utils, uargedit,
  umoduleedit, ubindingedit, ufrmsaveoption, ufarrayedit, generator,
  uftypealiasedit;

begin
  Application.Initialize;
  Application.CreateForm(TfWstTypeLibraryEdit, fWstTypeLibraryEdit);
  Application.Run;
end.

