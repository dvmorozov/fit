program cgifit_linux;

{$mode objfpc}{$H+}

uses
  cgiModules, Unit2, SysUtils, data, background, background_more,
project_files_empty,
  pattern_more, specimen_parameters_file, specimen_intervals,
gen_spec_int_progress, start, error,
  Main, CommonTypes, specimen_positions, DataLoader, MSCRDataClasses,
  fitting, fitting_process, gen_spec_pos_progress, gen_back_progress,
  fitting_progress, registration_free, Settings, projects_empty,
file_results_empty, pattern, projects, specimen_parameters, project_files,
  fit_server_proxy, GeneralHashFunctions, file_results, TA_LINUX;
  
var MSec: Comp;
    FileName: string;
    SavedOutput: Text;
begin
  //uzhe est' v initialization cgiModules
  //Application:=TModuledCGIApplication.Create(nil);
  Application.Initialize;
  Application.Title:='cgifit';
  // bez modulya rabotat' ne budet - smotri ishodniki
  Application.CreateForm(TCGIDatamodule2, CGIDatamodule2);
  // mozhno tol'ko tak - konstruktor svoistv normal'no ne rabotaet
  CGIDatamodule2.OnCGIRequest := @CGIDatamodule2.CGIDatamodule2CGIRequest;
  // sozdaetsya vremennyi fail dlya pereklyucheniya standartnogo vyvoda,
  // chtoby udalit' deystvie DebugLn
{$IFDEF WINDOWS}
  MSec := TimeStampToMSecs(DateTimeToTimeStamp(Now));
  FileName := IntToStr(Int64(MSec));
{$ELSE}
  FileName := '/dev/null';
{$ENDIF}
  SavedOutput := Output;
  AssignFile(Output, FileName);
  ReWrite(Output);

  Application.Run;
  
  CloseFile(Output);
  Output := SavedOutput;
  DeleteFile(FileName);
  //konfliktuet s finalization cgiModules
  //Application.Free;
end.

