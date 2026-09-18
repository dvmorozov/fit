// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(The user guide's chapter on projects and files: the File menu, the
formats Fit reads and writes, the command line and the log.)

WHY IT IS ONE CHAPTER. Everything here is where the user's work meets the disk:
a project saved and opened, a data file imported, a table exported, a log read
after something went wrong. A user asking "where did my work go" or "why did my
file not load" is asking a question this chapter answers, and the answer is
spread over a dozen units - project_workflow, fit_project_file and the
fit_project_* codecs, recent_project and startup_sequence, the data loaders,
table_export, command_line_switches, log and client_log.

THE SILENT BEHAVIOUR IS SAID OUT LOUD. The two-column reader skips a line it cannot read,
None of that is visible from
the menu, and each is what a user would otherwise find out by losing something.
When the code changes, the sentence here that describes it has to change with it.
}
unit guide_projects;

{$mode objfpc}{$H+}

interface

uses
    SysUtils, explanation;

const
    ProjectsNamespace = 'projects-and-files';
    ProjectsTopic = 'projects-and-files/projects';
    NewProjectTopic = 'projects-and-files/new-project';
    OpenProjectTopic = 'projects-and-files/open-project';
    OpenRecentTopic = 'projects-and-files/open-recent';
    SaveProjectTopic = 'projects-and-files/save-project';
    SaveProjectAsTopic = 'projects-and-files/save-project-as';
    ProjectContentsTopic = 'projects-and-files/project-contents';
    ProjectFormatTopic = 'projects-and-files/project-format';
    SourceFileTopic = 'projects-and-files/source-file';
    UnsavedChangesTopic = 'projects-and-files/unsaved-changes';
    ImportProfileTopic = 'projects-and-files/import-profile';
    DatFilesTopic = 'projects-and-files/dat-files';
    CsvFilesTopic = 'projects-and-files/csv-files';
    ReloadProfileTopic = 'projects-and-files/reload-profile';
    ExportCurveParametersTopic = 'projects-and-files/export-curve-parameters';
    ExportSummaryTableTopic = 'projects-and-files/export-summary-table';
    QuitTopic = 'projects-and-files/quit';
    SampleDataTopic = 'projects-and-files/sample-data';
    CommandLineTopic = 'projects-and-files/command-line';
    LogFilesTopic = 'projects-and-files/log-files';
    RememberedSettingsTopic = 'projects-and-files/remembered-settings';

{ Every topic of the chapter, in reading order. }
function ProjectsExplanations: TExplanations;

implementation

uses
    static_explanations;   // AppendExplanation

const
    //  A paragraph that starts with U+2022 and a space is a bullet.
    Bullet = #$E2#$80#$A2 + ' ';

    //  Topics of the first chapter, written out rather than taken from its unit:
    //  a chapter does not use another chapter's unit.
    FirstFitTopic = 'getting-started/first-fit';
    TwoProgramsTopic = 'getting-started/two-programs';

function Projects: TExplanation;
begin
    Result := NewExplanation(ProjectsTopic, 'Projects',
        'A project is your whole piece of work in one file, so that you can ' +
        'close Fit and later carry on fitting from where you stopped.',
        esModelChoice);
    AddParagraph(Result, 'A project holds the data, everything you marked on it, ' +
        'the model, the values the last fit found and how the window was set up. ' +
        'Opening it again does not merely redraw what you had: the fitted values ' +
        'stay attached to the curves they belong to, so the next fit continues ' +
        'from them.');
    AddParagraph(Result, 'Project files end in .fitproj. The File menu holds the ' +
        'commands for them:');
    AddParagraph(Result, Bullet + 'File > New Project (Ctrl+N) starts empty.');
    AddParagraph(Result, Bullet + 'File > Open Project (Ctrl+O) opens a saved ' +
        'project, and File > Open Recent offers the last few again.');
    AddParagraph(Result, Bullet + 'File > Save Project (Ctrl+S) and File > Save ' +
        'Project As (Ctrl+Shift+S) write the project.');
    AddParagraph(Result, Bullet + 'File > Import Profile (F2) and File > Reload ' +
        'Profile bring data from a file into the project.');
    AddParagraph(Result, Bullet + 'File > Export > Curve Parameters and File > ' +
        'Export > Summary Table write a table of results as text.');
    AddParagraph(Result, 'Saving and exporting are different things. A saved ' +
        'project can be opened again and is how you keep your work. An export is ' +
        'a one-way copy of a table for a spreadsheet or a script: Fit cannot ' +
        'open it again, and exporting does not count as saving.');
    AddParagraph(Result, 'The project open when you last closed Fit opens by ' +
        'itself at the next start.');
    AddLimitation(Result, 'On macOS the shortcuts use the Command key instead of ' +
        'Ctrl.');
    AddRelated(Result, ProjectContentsTopic);
    AddRelated(Result, SaveProjectTopic);
    AddRelated(Result, ImportProfileTopic);
    AddRelated(Result, UnsavedChangesTopic);
end;

function NewProject: TExplanation;
begin
    Result := NewExplanation(NewProjectTopic, 'New Project',
        'File > New Project closes what is open and leaves an empty window, ' +
        'ready for a profile to be imported.', esModelChoice);
    AddParagraph(Result, 'The profile, the background points, the fit intervals, ' +
        'the curve positions and the model are all removed from the window, and ' +
        'the title bar shows the program''s name again. The new project has no ' +
        'file yet: the first File > Save Project asks for a name.');
    AddParagraph(Result, 'It also means no project will open by itself at the ' +
        'next start, until you open or save one.');
    AddParagraph(Result, 'The shortcut is Ctrl+N. The command is always offered.');
    AddParagraph(Result, 'With unsaved work in the window, it first asks ' +
        'whether to save the project, as closing the window does: Yes saves and ' +
        'then clears, No clears without saving, and Cancel - or a save that does ' +
        'not happen - leaves everything as it was.');
    AddRelated(Result, SaveProjectTopic);
    AddRelated(Result, ImportProfileTopic);
end;

function OpenProject: TExplanation;
begin
    Result := NewExplanation(OpenProjectTopic, 'Open Project',
        'File > Open Project asks for a .fitproj file and restores the work it ' +
        'holds.', esModelChoice);
    AddParagraph(Result, 'The dialog shows project files (*.fitproj), and can be ' +
        'switched to all files for a project that was renamed. Opening restores ' +
        'the profile, the background points, the fit intervals, the curve ' +
        'positions, the settings and the fitted values, and then the working ' +
        'context: the argument axis, the picking mode, the table in front and the ' +
        'selected curve. The title bar shows the project''s file name.');
    AddParagraph(Result, 'The R-factor is not recalculated on opening. Until you ' +
        'fit again it reads as not calculated; the value the project recorded ' +
        'stays in the file.');
    AddParagraph(Result, 'If the file cannot be used, a message says why and ' +
        'the work already in the window is left as it was. Typical reasons: there ' +
        'is no file at that path, the file is not a Fit project, or it needs a ' +
        'newer version of Fit - the message then names the format version it ' +
        'needs and the one this build reads.');
    AddParagraph(Result, 'If the project names a curve type this build does not ' +
        'have, for example one from an analysis pack you do not have, the project ' +
        'still opens, with the curve type already selected here.');
    AddParagraph(Result, 'The shortcut is Ctrl+O. The command is always offered. ' +
        'A project can also be opened from the command line with /PROJECT, and ' +
        'on Windows by double-clicking a .fitproj file when the installer was ' +
        'allowed to associate the extension with Fit.');
    AddParagraph(Result, 'With unsaved work in the window, it first asks ' +
        'whether to save it, before the file dialog opens - the same question ' +
        'and the same answers as File > New Project. File > Open Recent asks it ' +
        'too.');
    AddRelated(Result, OpenRecentTopic);
    AddRelated(Result, ProjectContentsTopic);
    AddRelated(Result, SourceFileTopic);
    AddRelated(Result, CommandLineTopic);
end;

function OpenRecent: TExplanation;
begin
    Result := NewExplanation(OpenRecentTopic, 'Open Recent',
        'File > Open Recent lists the projects you opened or saved most ' +
        'recently, newest first, and opens one with a click.', esModelChoice);
    AddParagraph(Result, 'Each line is the whole path of a project, because two ' +
        'projects in different folders often have the same name. At most eight ' +
        'are kept. Opening or saving a project already in the list moves it to ' +
        'the top instead of listing it twice; two spellings of one path that ' +
        'differ only in capital letters count as the same project.');
    AddParagraph(Result, 'With no project remembered, the submenu shows one ' +
        'greyed line, (none), so that you can see the list is empty rather than ' +
        'missing.');
    AddParagraph(Result, 'The project at the top - the one open when Fit was ' +
        'last closed - opens by itself at the next start. If it has been moved ' +
        'or deleted in the meantime, Fit starts with nothing open, writes a ' +
        'warning to the log, and drops that project from the list.');
    AddParagraph(Result, 'The list is kept in Fit''s settings file and written ' +
        'when the window closes.');
    AddLimitation(Result, 'A project deleted while Fit is running stays in the ' +
        'list until the next start; clicking it reports that there is no project ' +
        'at that path.');
    AddRelated(Result, OpenProjectTopic);
    AddRelated(Result, RememberedSettingsTopic);
end;

function SaveProject: TExplanation;
begin
    Result := NewExplanation(SaveProjectTopic, 'Save Project',
        'File > Save Project writes the project to its file, and asks for a ' +
        'name the first time.', esModelChoice);
    AddParagraph(Result, 'A project that has never been saved has no file yet, ' +
        'so the first save behaves exactly like File > Save Project As. After ' +
        'that, File > Save Project writes to the same file without asking.');
    AddParagraph(Result, 'When the save succeeds, the title bar shows the ' +
        'project''s file name, the project becomes the one that opens at the next ' +
        'start, and it moves to the top of File > Open Recent. If it fails - no ' +
        'such folder, no permission, a full disk - a message says why, the ' +
        'project still counts as unsaved, and the file already there is left ' +
        'exactly as it was: the new file is written beside it and takes its ' +
        'name only once it is complete.');
    AddParagraph(Result, 'The shortcut is Ctrl+S. The command is greyed out ' +
        'until a profile has been imported or a project opened: there is nothing ' +
        'to save before that.');
    AddRelated(Result, SaveProjectAsTopic);
    AddRelated(Result, ProjectContentsTopic);
    AddRelated(Result, UnsavedChangesTopic);
end;

function SaveProjectAs: TExplanation;
begin
    Result := NewExplanation(SaveProjectAsTopic, 'Save Project As',
        'File > Save Project As always asks for a file name and saves the ' +
        'project there.', esModelChoice);
    AddParagraph(Result, 'Use it to keep a copy under another name, for example ' +
        'before trying a different model. From then on the window works on the ' +
        'new file: File > Save Project writes there.');
    AddParagraph(Result, 'If you type a name with no extension, .fitproj is ' +
        'added. A name you give with your own extension is kept as you typed it.');
    AddParagraph(Result, 'If a file of that name already exists, Fit asks ' +
        'whether to replace it, naming the file. The question is asked after the ' +
        'extension has been added, so it is about the file that would really be ' +
        'written. Answering No writes nothing and changes nothing.');
    AddParagraph(Result, 'The shortcut is Ctrl+Shift+S. The command is greyed ' +
        'out until a profile has been imported or a project opened.');
    AddRelated(Result, SaveProjectTopic);
    AddRelated(Result, ProjectFormatTopic);
end;

function ProjectContents: TExplanation;
begin
    Result := NewExplanation(ProjectContentsTopic, 'What a project file holds',
        'A project holds the data and everything needed to continue fitting it, ' +
        'but nothing that can be recalculated from those.', esModelChoice);
    AddParagraph(Result, 'Stored in the file:');
    AddParagraph(Result, Bullet + 'The profile as the program holds it at the ' +
        'time of saving, that is, after any background subtraction or smoothing.');
    AddParagraph(Result, Bullet + 'Your background points, fit intervals and ' +
        'curve positions, and which curve each position belongs to.');
    AddParagraph(Result, Bullet + 'The settings of the fit: the curve type, the ' +
        'wavelength, the maximum acceptable difference, the background ' +
        'fraction, the minimizer, the loss function, the weighting, and whether ' +
        'background variation and curve scaling are on.');
    AddParagraph(Result, Bullet + 'For every curve, its fitted parameter values ' +
        'with their errors, and whether a fit produced them.');
    AddParagraph(Result, Bullet + 'The R-factor and the statistics of the fit ' +
        'that was saved.');
    AddParagraph(Result, Bullet + 'The formula and parameters of a user-defined ' +
        'curve, if one is in use.');
    AddParagraph(Result, Bullet + 'The stretch of the profile you selected, if ' +
        'any, and where the data came from.');
    AddParagraph(Result, Bullet + 'The working context: the argument axis ' +
        '(including a custom axis you defined), the picking mode, the table in ' +
        'front and the selected curve.');
    AddParagraph(Result, Bullet + 'What any analysis module keeps about the ' +
        'model, in a part of its own.');
    AddParagraph(Result, Bullet + 'When the project was created and last saved, ' +
        'in UTC, and which version of Fit wrote it.');
    AddParagraph(Result, 'Not stored: the calculated profile, the difference, ' +
        'the curves'' own points and where the fitted curves sit. They are ' +
        'recalculated from what is stored, so a project cannot disagree with its ' +
        'own model. Also not stored: the window''s size and position, and the ' +
        'settings that belong to your installation rather than to the work, such ' +
        'as the compute server''s address or Animation Mode.');
    AddParagraph(Result, 'The original data file is not needed to open a ' +
        'project: the profile is inside it. The file''s path is recorded, but only ' +
        'so that you can tell where the data came from.');
    AddLimitation(Result, 'A parameter that is not a number - a label or a ' +
        'handle some curve types keep - is not stored. It is rebuilt from what ' +
        'the module keeps when the model is rebuilt.');
    AddRelated(Result, ProjectFormatTopic);
    AddRelated(Result, SourceFileTopic);
    AddRelated(Result, RememberedSettingsTopic);
end;

function ProjectFormat: TExplanation;
begin
    Result := NewExplanation(ProjectFormatTopic, 'The .fitproj file format',
        'A .fitproj file is a ZIP archive of JSON parts, which any archive tool ' +
        'can open.', esModelChoice);
    AddParagraph(Result, 'It is the same arrangement that .xlsx and OpenDocument ' +
        'files use. If something goes wrong you can unpack a project and read ' +
        'what it holds. The parts are:');
    AddParagraph(Result, Bullet + 'manifest.json - which version of the format ' +
        'and of Fit wrote the file, the oldest format version a reader must ' +
        'understand to open it, and when it was created and saved.');
    AddParagraph(Result, Bullet + 'problem.json - the profile, the background ' +
        'points, the fit intervals, the curve positions, the settings, the ' +
        'selected stretch, a user-defined curve and where the data came from.');
    AddParagraph(Result, Bullet + 'results.json - the fitted values of every ' +
        'curve, the R-factor and the fit statistics.');
    AddParagraph(Result, Bullet + 'ui.json - the working context of the window.');
    AddParagraph(Result, Bullet + 'modules/<module>.json - one part for each ' +
        'analysis module that keeps something.');
    AddParagraph(Result, 'Old and new versions of Fit get along. When an older ' +
        'version opens a project written by a newer one and saves it, the parts ' +
        'and the entries it does not understand are written back unchanged, so ' +
        'passing a file between versions does not quietly lose work. A project ' +
        'that truly needs a newer reader is refused with a message saying so, ' +
        'rather than opened half understood.');
    AddParagraph(Result, 'Numbers are written at full precision, so a value comes ' +
        'back exactly as it was saved.');
    AddLimitation(Result, 'Editing the parts by hand is possible but unchecked: ' +
        'a value that does not make sense may be refused, or taken as it is, ' +
        'when the project is opened.');
    AddReference(Result, 'PKWARE Inc., APPNOTE.TXT - .ZIP File Format ' +
        'Specification', '', 'https://pkware.cachefly.net/webdocs/casestudies/' +
        'APPNOTE.TXT');
    AddReference(Result, 'T. Bray (ed.), The JavaScript Object Notation (JSON) ' +
        'Data Interchange Format, RFC 8259', '',
        'https://www.rfc-editor.org/rfc/rfc8259');
    AddRelated(Result, ProjectContentsTopic);
end;

function SourceFile: TExplanation;
begin
    Result := NewExplanation(SourceFileTopic, 'Where the data came from',
        'A project remembers which data file it was imported from, and notes in ' +
        'the log when that file no longer says the same thing.', esModelChoice);
    AddParagraph(Result, 'When a profile is imported, Fit records the file''s ' +
        'path, its size, a fingerprint of its contents (an MD5 hash) and the kind ' +
        'of file it was. All of it is saved with the project.');
    AddParagraph(Result, 'When the project is opened again, Fit reads the file at ' +
        'that path, if it is still there, and compares its contents with the ' +
        'fingerprint. If they differ, a warning goes to the log naming the file. ' +
        'Nothing in the project changes: it still holds the data it was fitted ' +
        'to. The notice is there so that a result which no longer matches its ' +
        'input can be noticed rather than puzzled over months later.');
    AddParagraph(Result, 'The contents are compared rather than the file''s date, ' +
        'because copying or restoring a file changes its date without changing ' +
        'the data, and an edit can leave the date as it was.');
    AddLimitation(Result, 'When the file is gone, cannot be read, or the project ' +
        'was saved by a version that recorded no fingerprint, nothing is said: ' +
        'Fit cannot tell, and does not guess that the data changed.');
    AddLimitation(Result, 'The fingerprint guards against accidents - a ' +
        're-export, an edited column, a truncated copy - not against someone ' +
        'deliberately forging a file.');
    AddRelated(Result, LogFilesTopic);
    AddRelated(Result, ReloadProfileTopic);
end;

function UnsavedChanges: TExplanation;
begin
    Result := NewExplanation(UnsavedChangesTopic, 'Unsaved changes',
        'Closing Fit, starting a new project or opening another with unsaved ' +
        'work asks whether to save the project first, and a save that does not ' +
        'happen stops the command.', esModelChoice);
    AddParagraph(Result, 'Whether you close the window, use File > Quit, or ' +
        'replace the project with File > New Project, File > Open Project or ' +
        'File > Open Recent, Fit asks "Project has been modified. Save?" when ' +
        'there is unsaved work, and does not ask when there is none.');
    AddParagraph(Result, Bullet + 'Yes saves, exactly as File > Save Project ' +
        'does - asking for a name if the project has none - and then closes. If ' +
        'you cancel that name dialog, or the save fails, the window stays open, ' +
        'because closing then would lose the work you asked to keep.');
    AddParagraph(Result, Bullet + 'No closes without saving.');
    AddParagraph(Result, Bullet + 'Cancel, or closing the question, keeps the ' +
        'window open.');
    AddParagraph(Result, 'Work counts as unsaved when anything a save would ' +
        'write has changed since the project was last opened, saved or started ' +
        'from an imported file: the points picked for the background, the fit ' +
        'intervals or the curve positions, the model, a fit result, a setting. ' +
        'Changing the axis, the table in front or the selected curve does not ' +
        'count, and neither does exporting a table.');
    AddParagraph(Result, 'File > Import Profile and File > Reload Profile ask ' +
        'a question of their own, about the model they discard.');
    AddRelated(Result, SaveProjectTopic);
    AddRelated(Result, QuitTopic);
end;

function ImportProfile: TExplanation;
begin
    Result := NewExplanation(ImportProfileTopic, 'Import Profile',
        'File > Import Profile reads a data file and makes it the profile of the ' +
        'current project.', esModelChoice);
    AddParagraph(Result, 'The dialog, Open Profile Data, starts in the folder ' +
        'Fit is installed in. It offers the formats this build can read: All ' +
        'supported, then one entry per format, then All files. The file''s ' +
        'extension decides how it is read, whatever its capitals:');
    AddParagraph(Result, Bullet + '.dat - a two-column text file, argument and ' +
        'value (Diffraction profile in the dialog).');
    AddParagraph(Result, Bullet + '.csv - price data with date, open, high, ' +
        'low and close columns (Price data, OHLC in the dialog).');
    AddParagraph(Result, 'A file with any other extension is refused with a ' +
        'message naming the extension no reader handles. An analysis module can ' +
        'add formats of its own; the dialog then lists them too.');
    AddParagraph(Result, 'Importing starts the model again: the curves, the ' +
        'picks and the fitted parameters describe the data being replaced. So ' +
        'when the project has curves or unsaved work, Fit first asks, naming the ' +
        'file, whether to replace the data and discard the current model. With ' +
        'nothing to lose it does not ask.');
    AddParagraph(Result, 'Afterwards the profile is on the chart and in the Data ' +
        'tab, and the title bar shows the data file''s name. The project keeps ' +
        'its own file: File > Save Project saves the new data into it. The data ' +
        'file''s path, size and fingerprint are recorded for the project.');
    AddParagraph(Result, 'The shortcut is F2, and the first toolbar button does ' +
        'the same. The command is always offered.');
    AddLimitation(Result, 'There is no choice of columns in the dialog: a .dat ' +
        'file is always read as its first two numbers per line, and a .csv file ' +
        'as described under price data files.');
    AddRelated(Result, DatFilesTopic);
    AddRelated(Result, CsvFilesTopic);
    AddRelated(Result, ReloadProfileTopic);
    AddRelated(Result, SampleDataTopic);
    AddRelated(Result, SourceFileTopic);
end;

function DatFiles: TExplanation;
begin
    Result := NewExplanation(DatFilesTopic, 'Two-column data files (.dat)',
        'A .dat file is plain text with the argument and the measured value on ' +
        'each line, and Fit reads the first two numbers of every line.',
        esModelChoice);
    AddParagraph(Result, 'Each line should hold two numbers: first the argument ' +
        '(for a diffraction pattern, the angle), then the value (the intensity). ' +
        'Anything that is not part of a number separates numbers, so spaces, ' +
        'tabs and semicolons all work. For example:');
    AddParagraph(Result, '116.0000    773.00');
    AddParagraph(Result, 'A number may have a sign (-1.5, +2) and an exponent ' +
        '(1.5e3, 2E-2). Its decimal mark may be a full stop or a comma. A comma ' +
        'is read as a decimal mark only on a line that writes no full stop, and ' +
        'only while that still leaves two numbers: 1,5;2,5 is one and a half and ' +
        'two and a half, while 12,40 and 12.5,40.0 are both pairs.');
    AddParagraph(Result, 'What the reader does without saying so:');
    AddParagraph(Result, Bullet + 'A line with fewer than two numbers - a ' +
        'heading, a comment, a blank line - is skipped, as is a line holding a ' +
        'malformed number such as 1.2.3.');
    AddParagraph(Result, Bullet + 'Everything after the second number on a line ' +
        'is ignored, so a third column (an error, say) is dropped.');
    AddParagraph(Result, Bullet + 'A line whose argument is already in the file ' +
        'is skipped; the first one wins.');
    AddParagraph(Result, Bullet + 'A minus sign written straight after a digit ' +
        'separates two numbers: 10-20 is the pair 10 and 20.');
    AddLimitation(Result, 'A comment line that happens to contain two numbers ' +
        '(# run 3 of 5) is read as a data point. Start such lines with no ' +
        'numbers, or remove them, and compare the Data tab with the file.');
    AddRelated(Result, ImportProfileTopic);
    AddRelated(Result, SampleDataTopic);
end;

function CsvFiles: TExplanation;
begin
    Result := NewExplanation(CsvFilesTopic, 'Price data files (.csv)',
        'A .csv file is read as a price series: one row per period, and the ' +
        'closing price becomes the value.', esModelChoice);
    AddParagraph(Result, 'The separator - comma, semicolon or tab - is worked ' +
        'out from the first non-empty line. A semicolon wins a tie with commas, ' +
        'because files separated by semicolons usually write decimals with a ' +
        'comma.');
    AddParagraph(Result, 'A header row is recognised by its column names, in any ' +
        'capitals: Date (or Time, Timestamp, Datetime, observation_date), Open, ' +
        'High, Low and Close. An Adj Close column is preferred to Close when both ' +
        'are there. A header of exactly two columns, a date column first and ' +
        'one other, takes the other column as the value. A file with no ' +
        'recognisable header must ' +
        'have at least five columns, taken as date, open, high, low, close.');
    AddParagraph(Result, 'The value of each point is the closing price, and the ' +
        'argument is the row number, counting from 0, so that weekends and ' +
        'holidays leave no gaps - unless Data > Price Data chooses another ' +
        'column or the date as the argument.');
    AddParagraph(Result, 'A row that has no usable value - a blank, a missing ' +
        'observation written as a single full stop, a short last row - is ' +
        'skipped, and the log says how many were. A file with no usable row at ' +
        'all is refused.');
    AddLimitation(Result, 'Only this price layout is read. A general ' +
        'comma-separated table of measurements is not; save it as a two-column ' +
        '.dat file instead.');
    AddRelated(Result, ImportProfileTopic);
    AddRelated(Result, SampleDataTopic);
end;

function ReloadProfile: TExplanation;
begin
    Result := NewExplanation(ReloadProfileTopic, 'Reload Profile',
        'File > Reload Profile reads the data file the open project came from ' +
        'again, for when it has changed on disk.', esModelChoice);
    AddParagraph(Result, 'It re-reads the file the project records as its ' +
        'source - the one imported last, or for a project opened from disk the ' +
        'one it was built from - in the same way it was read the first time, ' +
        'and replaces the profile with what it finds.');
    AddParagraph(Result, 'Like importing, it starts the model again, so when the ' +
        'project has curves or unsaved work Fit first asks whether to go on.');
    AddParagraph(Result, 'When the project names no data file, or the file is ' +
        'no longer where it was, Fit says so and reads nothing; use File > ' +
        'Import Profile to choose the file.');
    AddParagraph(Result, 'The command is greyed out until a profile has been ' +
        'imported or a project opened. It has no shortcut.');
    AddRelated(Result, ImportProfileTopic);
    AddRelated(Result, SourceFileTopic);
end;

function ExportCurveParameters: TExplanation;
begin
    Result := NewExplanation(ExportCurveParametersTopic,
        'Export Curve Parameters',
        'File > Export > Curve Parameters writes the Curve Attributes table to ' +
        'a tab-separated text file.', esModelChoice);
    AddParagraph(Result, 'What is written is the table''s content, headings ' +
        'included: one line per curve, the cells separated by tabs, with no tab ' +
        'after the last cell. A spreadsheet or a script reads it directly. It ' +
        'does not matter which table is in front when you use the command.');
    AddParagraph(Result, 'The numbers are written with every digit, not as ' +
        'the table rounds them, and with a full stop as the decimal mark ' +
        'whatever the computer''s language settings. Where the fit estimated an ' +
        'uncertainty, it gets a column of its own beside its value, headed with ' +
        'the parameter''s name and "error".');
    AddParagraph(Result, 'The dialog, Select File to Save Data, offers text ' +
        'files. A name typed with no extension gets .txt; one typed with an ' +
        'extension keeps it. If the file exists, Fit asks whether to overwrite ' +
        'it: Yes overwrites, No lets you choose another name, and Cancel writes ' +
        'nothing. An empty name asks whether to choose again.');
    AddParagraph(Result, 'The command is greyed out while the Curve Attributes ' +
        'table is empty.');
    AddLimitation(Result, 'Fit cannot open an exported file again, and ' +
        'exporting does not count as saving the project.');
    AddRelated(Result, ExportSummaryTableTopic);
    AddRelated(Result, SaveProjectTopic);
end;

function ExportSummaryTable: TExplanation;
begin
    Result := NewExplanation(ExportSummaryTableTopic, 'Export Summary Table',
        'File > Export > Summary Table writes the Summary table - data, model ' +
        'and difference point by point - to a tab-separated text file.',
        esModelChoice);
    AddParagraph(Result, 'The Summary table has, for every point inside every ' +
        'fit interval, the argument, the measured value, the model''s value, the ' +
        'difference and each curve''s contribution, with a heading row for each ' +
        'interval. A curve that covers only part of an interval leaves the rest ' +
        'of its column blank.');
    AddParagraph(Result, 'The file is written exactly as for File > Export > ' +
        'Curve Parameters: headings included, every digit of each number, a ' +
        'full stop as the decimal mark, cells separated by tabs, .txt added to ' +
        'a name with no extension, and the same questions about an existing ' +
        'file or an empty name.');
    AddParagraph(Result, 'The command is greyed out while the Summary table is ' +
        'empty.');
    AddLimitation(Result, 'Fit cannot open the file again.');
    AddRelated(Result, ExportCurveParametersTopic);
end;

function Quit: TExplanation;
begin
    Result := NewExplanation(QuitTopic, 'Quit',
        'File > Quit closes Fit, asking first whether to save unsaved work.',
        esModelChoice);
    AddParagraph(Result, 'It does the same as closing the window. If the project ' +
        'has unsaved changes you are asked whether to save them, and the window ' +
        'stays open if you cancel or the save does not happen.');
    AddParagraph(Result, 'On closing, Fit writes its settings file: the project ' +
        'that was open, the recent projects and your other preferences.');
    AddParagraph(Result, 'The compute server is not stopped: it stays running so ' +
        'that the next start is immediate and other windows keep their engine.');
    AddParagraph(Result, 'The shortcut is Ctrl+Q, on macOS Command+Q.');
    AddRelated(Result, UnsavedChangesTopic);
    AddRelated(Result, RememberedSettingsTopic);
    AddRelated(Result, TwoProgramsTopic);
end;

function SampleData: TExplanation;
begin
    Result := NewExplanation(SampleDataTopic, 'Sample data',
        'Fit comes with a few data files to try it on, in a folder called Data.',
        esModelChoice);
    AddParagraph(Result, Bullet + '1.dat - a whole diffraction pattern, 1692 ' +
        'points with arguments from 3.0 to 172.1 in steps of 0.1.');
    AddParagraph(Result, Bullet + '2.dat to 9.dat - short stretches of 20 to 70 ' +
        'points, each around one or a few peaks. 2.dat, with two overlapping ' +
        'peaks between 116 and 121, is a good first fit.');
    AddParagraph(Result, Bullet + 'sample-ohlc.csv - a daily price series with ' +
        'Date, Open, High, Low, Close and Volume columns.');
    AddParagraph(Result, Bullet + 'test_data.zip - the nine .dat files packed in ' +
        'one archive.');
    AddParagraph(Result, 'Where the folder is: on Windows, beside Fit.exe in the ' +
        'folder Fit was installed to, which is where File > Import Profile ' +
        'starts; on Linux, /usr/share/fit/Data (the package''s own name in place ' +
        'of fit for another package); on macOS, in the Resources folder inside ' +
        'the application bundle.');
    AddRelated(Result, ImportProfileTopic);
    AddRelated(Result, FirstFitTopic);
end;

function CommandLine: TExplanation;
begin
    Result := NewExplanation(CommandLineTopic, 'Command-line options',
        'Fit''s window accepts a few switches that say what to open at start-up ' +
        'and how much to log.', esModelChoice);
    AddParagraph(Result, 'A switch starts with / (or \), is written in capitals ' +
        'as below, and takes its value after an equals sign, for example ' +
        '/PROJECT=C:\work\sample.fitproj. Put quotes around a path with spaces. ' +
        'A relative path that is not found is tried again relative to the ' +
        'folder Fit is installed in.');
    AddParagraph(Result, Bullet + '/PROJECT=file opens that project. It wins ' +
        'over everything else. If the file is not there, Fit starts with nothing ' +
        'open and says so in the log; it never opens a different project ' +
        'instead.');
    AddParagraph(Result, Bullet + '/INFILE=file starts a new project with that ' +
        'data file imported. It wins over the project remembered from last time.');
    AddParagraph(Result, Bullet + 'With neither, the project open when Fit was ' +
        'last closed is opened again.');
    AddParagraph(Result, Bullet + '/LOG_LEVEL=level sets how much goes to the ' +
        'log: fatal, warning, notification, debug or trace. The default is ' +
        'debug, which already records almost everything, so the switch mostly ' +
        'makes the log quieter; trace adds the most repetitive lines as well. An ' +
        'unknown level is reported in the log and the default kept.');
    AddParagraph(Result, Bullet + '/WRITE_PARAMS_LOG writes to the log every ' +
        'value assigned to a parameter of a user-defined curve.');
    AddParagraph(Result, Bullet + '/DPI=number sets the screen resolution the ' +
        'window is scaled for, in pixels per inch, for a display that does not ' +
        'report it correctly.');
    AddParagraph(Result, Bullet + '/CHECK_UI is for the program''s developers: ' +
        'the window checks its own layout and behaviour, runs a fit, writes the ' +
        'results to the log and closes.');
    AddParagraph(Result, 'The window ignores an argument that is not a switch, ' +
        'such as a bare file name. The installed launchers turn a bare file name ' +
        'into /PROJECT for a .fitproj file and /INFILE for any other file, which ' +
        'is how opening a project from the desktop works.');
    AddParagraph(Result, 'The compute server, fit_server, has options of its ' +
        'own: --host and --port for where it listens (127.0.0.1 and 8787 by ' +
        'default), --log-level, and --verbose to echo its log to the console.');
    AddLimitation(Result, 'A misspelt switch is ignored without a message, and ' +
        'switch names are matched in capitals only.');
    AddRelated(Result, LogFilesTopic);
    AddRelated(Result, OpenRecentTopic);
    AddRelated(Result, TwoProgramsTopic);
end;

function LogFiles: TExplanation;
begin
    Result := NewExplanation(LogFilesTopic, 'Log files',
        'The window and the compute server each keep a log file, which is where ' +
        'to look when something went wrong.', esModelChoice);
    AddParagraph(Result, 'Both files are in Fit''s folder for the current user: ' +
        'on Windows %APPDATA%\Fit, on Linux and macOS a folder called Fit in ' +
        'your home folder. The window writes fit_client.log and the server ' +
        'writes fit_server_log.txt - when the server runs on this computer under ' +
        'your account, which is how the launchers start it.');
    AddParagraph(Result, 'By default the logs record what you did, what the ' +
        'window asked the server and how long each call took, warnings and ' +
        'errors with the program location where they happened. Some notices go ' +
        'only to the log, among them a remembered project that could not be ' +
        'found at start-up, a data file that has changed since a project was ' +
        'saved, and rows skipped while reading a price file.');
    AddParagraph(Result, 'A log that reaches 32 MB is renamed with .1 added to ' +
        'its name, replacing the previous .1 file, and a new one is started. So ' +
        'the two most recent files of each log are kept, and nothing older.');
    AddParagraph(Result, 'The launchers on Linux and macOS also keep what the ' +
        'server prints to its console: on Linux in ' +
        '~/.local/share/<package>/server.log, on macOS in ' +
        '~/Library/Logs/<application>/server.log.');
    AddParagraph(Result, 'When you report a problem, attach both log files: ' +
        'each tells one side of the story, and a call can be matched on both ' +
        'sides by its time.');
    AddRelated(Result, CommandLineTopic);
    AddRelated(Result, RememberedSettingsTopic);
end;

function RememberedSettings: TExplanation;
begin
    Result := NewExplanation(RememberedSettingsTopic,
        'What Fit remembers between sessions',
        'Some choices belong to you rather than to a project, and Fit keeps them ' +
        'in a settings file of its own.', esModelChoice);
    AddParagraph(Result, 'The file is config.xml, in the same folder as the log ' +
        'files, and it is written when the window closes. It holds:');
    AddParagraph(Result, Bullet + 'the project that was open, and the list ' +
        'behind File > Open Recent;');
    AddParagraph(Result, Bullet + 'the compute server''s address;');
    AddParagraph(Result, Bullet + 'the curve type, the minimizer, the loss ' +
        'function and the weighting last chosen;');
    AddParagraph(Result, Bullet + 'the argument axis, whether you chose it ' +
        'yourself, and a custom axis you defined;');
    AddParagraph(Result, Bullet + 'whether Animation Mode is on.');
    AddParagraph(Result, 'A project carries its own copy of the fit settings, so ' +
        'opening a project sets them as they were when it was saved.');
    AddLimitation(Result, 'If Fit ends without closing its window normally, ' +
        'changes since the start of the session are not written to the file.');
    AddRelated(Result, ProjectContentsTopic);
    AddRelated(Result, LogFilesTopic);
end;

function ProjectsExplanations: TExplanations;
begin
    Result := nil;
    AppendExplanation(Result, Projects);
    AppendExplanation(Result, NewProject);
    AppendExplanation(Result, OpenProject);
    AppendExplanation(Result, OpenRecent);
    AppendExplanation(Result, SaveProject);
    AppendExplanation(Result, SaveProjectAs);
    AppendExplanation(Result, ProjectContents);
    AppendExplanation(Result, ProjectFormat);
    AppendExplanation(Result, SourceFile);
    AppendExplanation(Result, UnsavedChanges);
    AppendExplanation(Result, ImportProfile);
    AppendExplanation(Result, DatFiles);
    AppendExplanation(Result, CsvFiles);
    AppendExplanation(Result, ReloadProfile);
    AppendExplanation(Result, ExportCurveParameters);
    AppendExplanation(Result, ExportSummaryTable);
    AppendExplanation(Result, Quit);
    AppendExplanation(Result, SampleData);
    AppendExplanation(Result, CommandLine);
    AppendExplanation(Result, LogFiles);
    AppendExplanation(Result, RememberedSettings);
end;

end.
