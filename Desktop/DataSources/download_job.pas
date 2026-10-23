// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Fetching a file without the window stopping while it happens.)

WHY IT IS NOT DONE WHERE IT IS ASKED FOR. A download takes as long as somebody
else's service takes - seconds, or the whole timeout when the service has
decided not to answer. Run in the window's own thread, that is a frozen
application: nothing repaints, the Cancel button cannot be pressed, and the
desktop eventually offers to kill the program. Which is also why the transfer's
own Cancel was useless before this: there was no moment at which anyone could
press it.

WHAT IS AND IS NOT HERE. The job runs the wizard's download and REMEMBERS what
happened - the message, and whether it was a cancellation rather than a fault.
It draws nothing and decides nothing else: what the window shows while it waits
is the window's business, and what the download means is the wizard's.

NOTHING CROSSES A THREAD BOUNDARY UNGUARDED. The job touches the wizard, which
touches no LCL; the window reads the job's fields only after it has finished.
Progress arrives through a callback the window hands over, and the window
marshals it - see TDataSourceWindow.
}
unit download_job;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, data_source, data_source_wizard, int_web_client;

type
    { WHICH STEP OF THE WIZARD THE JOB TAKES. Every step that can ask a
      service runs here, not only the download: a search of two repositories
      asks one of them once per record it lists, and run on the window's
      thread that froze the window for seconds with nothing to press
      (2026-10-21). }
    TWizardJobAction = (
        wjDownload, //  fetch the chosen file and read it for the preview
        wjAdvance,  //  Next: search, open a record, take the one file
        wjEnter,    //  choosing a source: a source that asks nothing lists
        wjBack      //  Back out of a folder: its parent is asked again
        );

    TDownloadJob = class(TThread)
    private
        FWizard: TDataSourceWizard;
        FAction: TWizardJobAction;
        FError: string;
        FCancelled: boolean;
        FFinished: boolean;
    protected
        { Runs the download and keeps whatever it raised. NOTHING is re-raised:
          an exception leaving a thread's Execute takes the process with it,
          and every failure here is one the user has to be told about rather
          than one the program cannot continue past. }
        procedure Execute; override;
    public
        constructor Create(AWizard: TDataSourceWizard;
            AAction: TWizardJobAction = wjDownload);
        property Action: TWizardJobAction read FAction;
        { What went wrong, or '' when the file was fetched and read. }
        property Error: string read FError;
        { Whether the user stopped it - which is not a failure, and is said in
          a line rather than put in a box. }
        property Cancelled: boolean read FCancelled;
        { Whether the job has run at all. Read by the window's wait, so that a
          job that never started cannot be mistaken for one that finished. }
        property Finished: boolean read FFinished;
    end;

implementation

uses
    web_client;

constructor TDownloadJob.Create(AWizard: TDataSourceWizard;
    AAction: TWizardJobAction);
begin
    //  SUSPENDED, and started by the caller once it has drawn whatever it
    //  shows while waiting: a thread that starts inside its own constructor
    //  can finish before the caller holds the reference to it.
    inherited Create(True);
    FreeOnTerminate := False;
    FWizard := AWizard;
    FAction := AAction;
end;

procedure TDownloadJob.Execute;
begin
    try
        case FAction of
            wjAdvance: FWizard.Advance;
            wjEnter: FWizard.EnterChosenSource;
            wjBack: FWizard.GoBack;
            else
                FWizard.Download;
        end;
    except
        on E: EWebCancelled do
        begin
            FCancelled := True;
            FError := E.Message;
        end;
        on E: Exception do
            FError := E.Message;
    end;
    //  LAST, and deliberately: the window waits on this, so it must not be
    //  true until everything the window will read afterwards has been written.
    FFinished := True;
end;

end.
