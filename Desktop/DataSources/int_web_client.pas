// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Fetching something over the web, as the rest of the client sees it.)

WHY THE INTERFACE IS SEPARATE FROM THE IMPLEMENTATION, exactly as IDataLoader is
from TDataLoader: everything that finds data - each source, the wizard, the
import - is written against this, and a test hands it a table of canned replies
instead of a socket. A source is then testable with no network, which is what
makes "no test touches the network" a rule rather than a hope.

WHAT A FAILURE IS. Every refusal comes back as EWebError carrying a sentence a
user can act on: what was asked for, what happened, and what to do instead.
Falling through to plausible-looking wrong behaviour - an empty file, a zero-point
profile - is worse than stopping.
}
unit int_web_client;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils;

type
    { Anything that goes wrong between asking for a URL and holding its bytes. }
    EWebError = class(Exception);

    { How far a download has got, and the chance to stop it. ATotal is 0 when
      the server did not say how big the answer is, which is ordinary. }
    TWebProgress = procedure(ABytes, ATotal: int64) of object;

    IWebClient = interface
        { The body at AUrl as text - a catalogue's JSON or HTML. }
        function GetText(const AUrl: string): string;
        { The bytes at AUrl into ADest, answering the file name the server
          suggested, or '' when it suggested none. }
        function Download(const AUrl: string; ADest: TStream): string;
        { Called as bytes arrive. One handler, set by whoever is waiting. }
        procedure SetProgress(AProgress: TWebProgress);
        { Stops the transfer in progress at the next chunk. Safe to call from
          another thread: it sets a flag and nothing else. }
        procedure Cancel;
    end;

implementation

end.
