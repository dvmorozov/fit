#!/usr/bin/env pwsh
# SPDX-License-Identifier: GPL-3.0-or-later
<#
.SYNOPSIS
    Builds, tests and packages Fit from source.

.DESCRIPTION
    This is the whole build. There is no hidden step: what runs here is what the
    project's own CI runs and what its maintainer runs before publishing, so if
    something here does not work for you, it is a bug rather than a missing
    instruction.

    Requirements and the alternative route through the Lazarus IDE are documented
    in docs/user-guide/building-from-source.md.

.PARAMETER NoVersionBump
    Build the version the project file already carries. Without it, every build
    raises the build number first, which is what the Lazarus IDE does when it
    builds the same project - lazbuild does not, so a script-driven build would
    otherwise report the number the last IDE build left behind. Automated builds
    ($env:CI) never raise it: they reproduce a commit rather than make one.

.PARAMETER Task
    all       - toolchain check, build, test, package (the default)
    check     - report the toolchain and stop
    build     - the desktop client and the compute server
    test      - the test suite
    package   - an installer for this operating system (a portable archive too,
                on Linux)
    api-docs  - build and start the compute server, then open its own API
                documentation in this machine's browser

    The first five are PHASES OF THE BUILD, in the order `all` runs them. api-docs
    is not one, and it is a task all the same: what it opens is a page the server
    assembles from its own routing while it runs, so there is no file to point a
    reader at and nothing to hand them except a command that starts the server.

.PARAMETER Install
    Install what the package phase just produced, on this machine. Windows runs
    the setup.exe, macOS copies Fit.app into Applications, Linux installs the
    .deb or the .rpm.

    A PARAMETER rather than a task, for the reason the build's own task list is
    five long: a task there is a PHASE OF THE BUILD, and installing is not a
    phase of the build - it is what to do with what the build produced.
    -Task all -Install therefore means "build the whole thing and put it on this
    machine", which is the only thing anyone wanted another task for.

.PARAMETER Suite
    Which half of the test suite -Task test runs. Every test class declares which
    half it belongs to, and the criterion is dependencies rather than speed:

    unit        - tests that need nothing outside their own process. No compute
                  server is built and none is started, because a unit test does
                  not cross a process boundary. Seconds.
    integration - the rest: a compute server, HTTP, the optional Python sidecar,
                  data files on disk, and fits run to convergence. Minutes.
    all         - both, which is the default and what -Task all runs.

    Run the unit half while you are changing something and the whole suite before
    you believe a result. tests/README.md gives the rule and names the test that
    fails the build when a class is registered into neither half.

.PARAMETER TargetCpu
    Build for a processor other than this machine's. Only macOS is supported,
    and only because GitHub retired its Intel runners. Empty means build for the
    machine running this.

    UNTESTED since the assembler problem was fixed at the source. What blocked
    this was a user-declared Pascal label in TAGraph.pas, which Free Pascal
    emits as a global symbol and clang refuses inside a CFI block; the label is
    gone and a NATIVE x86_64-darwin build now works, debug info and all. What
    has NOT been checked is a real cross build - arm64 host, x86_64 target -
    because the only Mac to hand is an Intel one. The nasm workaround that used
    to stand in for this is removed; docs/contributing/building.md has the detail.

.EXAMPLE
    ./scripts/build-app.ps1
    ./scripts/build-app.ps1 -Task build
    ./scripts/build-app.ps1 -Task test -Suite unit
    ./scripts/build-app.ps1 -Task package -TargetCpu x86_64
    ./scripts/build-app.ps1 -Task api-docs
#>
param(
    [ValidateSet('all', 'check', 'build', 'test', 'package', 'api-docs')]
    [string] $Task = 'all',

    #  See .PARAMETER Suite above. A PARAMETER rather than a task of its own, and
    #  the distinction is worth keeping: a build task is a PHASE of it - check,
    #  build, test, package, each a stage of `all` - which is what lets the
    #  synopsis above say there is no hidden step. Running half of a phase is a
    #  narrowing of one, and narrowings are already parameters in this script:
    #  -TargetCpu narrows build and package, -PackageName narrows package. A
    #  'test-unit' task would never appear in `all` and would make the build's
    #  phases a menu of commands rather than a description of the build.
    [ValidateSet('all', 'unit', 'integration')]
    [string] $Suite = 'all',

    #  Package binaries other than the ones in this tree, under another name.
    #  A build that adds a module produces different binaries, and packaging them
    #  is the same job - so it is this script's job, rather than a second
    #  implementation somewhere else that drifts from this one.
    [string] $ClientBinary,
    [string] $ServerBinary,
    [string] $PackageName = 'fit',
    #  The name a person reads - installer title, Start menu, file names, the
    #  macOS bundle, the Linux menu entry - kept apart from $PackageName, which
    #  is the package identity and decides what an install upgrades.
    [string] $PackageLabel = 'Fit',

    #  See .PARAMETER TargetCpu above.
    [ValidateSet('', 'x86_64', 'aarch64')]
    [string] $TargetCpu = '',

    #  See .PARAMETER NoVersionBump above.
    [switch] $NoVersionBump,

    #  See .PARAMETER Install above.
    [switch] $Install,

    #  A module's own Data folder, shipped beside Fit's samples (Copy-PackageData).
    #  The private variant passes its module's; the public one has none.
    [string] $ModuleData = ''
)

$ErrorActionPreference = 'Stop'
#  The repository root, whether this is run from there or from scripts/.
$Root = Split-Path -Parent $PSScriptRoot
Set-Location $Root

function Write-Step($text) { Write-Host "==> $text" -ForegroundColor Cyan }

#  THE SAMPLES A PACKAGE SHIPS: Fit's own Data folder, and then a module's merged
#  into it. A module's samples belong to the module that reads them - the price
#  series went with the price reader into the module that reads it - so a package
#  that contains the module ships them too, in the one folder the samples
#  source looks in. One function for every package kind, so no kind can ship a
#  module without its samples.
function Copy-PackageData([string] $Dest, [string] $SourceData = 'Data',
                          [string] $ModuleData = '') {
    if (Test-Path -LiteralPath $SourceData) {
        Copy-Item -Recurse -LiteralPath $SourceData -Destination $Dest
    }
    if ($ModuleData -and (Test-Path -LiteralPath $ModuleData)) {
        New-Item -ItemType Directory -Force -Path $Dest | Out-Null
        Copy-Item -Recurse -Force -Path (Join-Path $ModuleData '*') -Destination $Dest
    }
}

function Remove-Tree([string] $Path) {
    #  Remove-Item -Recurse enumerates and deletes at the same time, and on a
    #  large tree intermittently fails with "Directory not empty" on a directory
    #  it has just emptied. .NET's recursive delete walks the tree properly; the
    #  retry covers something else writing into it while it goes away.
    if (-not (Test-Path -LiteralPath $Path)) { return }
    $full = (Resolve-Path -LiteralPath $Path).Path
    foreach ($attempt in 1..3) {
        try { [System.IO.Directory]::Delete($full, $true); return }
        catch [System.IO.IOException] {
            if ($attempt -eq 3) {
                throw "Could not remove $full after $attempt attempts: $($_.Exception.Message)"
            }
            Start-Sleep -Milliseconds (200 * $attempt)
        }
    }
}

function Resolve-Lazbuild {
    #  lazbuild is rarely on PATH after an ordinary install: the Windows installer
    #  adds a Start Menu entry, and the macOS cask puts it inside /Applications.
    #  Relying on PATH therefore fails for most people who followed the
    #  instructions - and failed silently, which was worse: PowerShell ran
    #  something that was not lazbuild, the "build" finished in milliseconds
    #  having compiled nothing, and only the tests noticed.
    if ($script:Lazbuild) { return $script:Lazbuild }
    if ($env:LAZBUILD) {
        if (-not (Test-Path $env:LAZBUILD)) { throw "LAZBUILD is set to '$env:LAZBUILD', which does not exist." }
        $script:Lazbuild = (Resolve-Path $env:LAZBUILD).Path
        return $script:Lazbuild
    }

    $cmd = Get-Command lazbuild -ErrorAction SilentlyContinue
    if ($cmd -and $cmd.Source -and (Test-Path $cmd.Source)) {
        $script:Lazbuild = $cmd.Source
        return $script:Lazbuild
    }

    #  The places the official installers actually use.
    $candidates = @(
        '/Applications/Lazarus/lazbuild',
        '/usr/local/bin/lazbuild', '/usr/bin/lazbuild', '/opt/lazarus/lazbuild',
        'C:\lazarus\lazbuild.exe', "$env:ProgramFiles\Lazarus\lazbuild.exe",
        "${env:ProgramFiles(x86)}\Lazarus\lazbuild.exe"
    )
    foreach ($c in $candidates) {
        if ($c -and (Test-Path $c)) { $script:Lazbuild = (Resolve-Path $c).Path; return $script:Lazbuild }
    }

    throw @"
lazbuild was not found.

It is part of Lazarus, but the installers do not always put it on PATH:
  Linux    apt install lazarus         (usually /usr/bin/lazbuild)
  macOS    scripts/install-lazarus-macos.sh  (/Applications/lazarus/lazbuild)
  Windows  choco install lazarus       (C:\lazarus\lazbuild.exe)

Either add its directory to PATH, or point at it directly:
  `$env:LAZBUILD = '/path/to/lazbuild'
"@
}

#  WHICH WIDGET SET THE LINUX CLIENT IS BUILT AGAINST, and why it is not GTK2.
#
#  GTK2 cannot see that a desktop is scaled. It predates the idea: Plasma and
#  GNOME publish Gdk/WindowScalingFactor through XSETTINGS, which is the GTK3
#  scheme, and GTK2 understands only the Xft/DPI key those desktops no longer
#  send. Where GTK2 fails to recover the number from the X resource database the
#  LCL falls back to dividing the pixel size of the screen by its millimetre
#  size, gets 96, and lays the whole application out at 100% on a 200% desktop.
#  Nothing reports an error.
#
#  Two more things GTK2 cannot do at all, whatever the application does: one DPI
#  per session (so a second monitor at a different scale is always wrong), and
#  no live rescale (there is no WM_DPICHANGED equivalent, so changing the scale
#  or moving the window does nothing until Fit is restarted).
#
#  Qt knows all three. qt6 first because Plasma 6 is Qt6 and the theming matches;
#  qt5 as the fallback for distributions that package only that. Both are
#  distribution packages: lcl-qt6 with libqt6pas6, or lcl-qt5 with libqt5pas1.
#
#  If neither is installed the build still runs, on GTK2, but it SAYS SO: a
#  scaled desktop rendering at 100% is exactly the failure this replaces, and it
#  must not come back silently. See docs/contributing/no-silent-degradation.md.
$script:WidgetSet = $null

function Get-LazarusRoot {
    #  The Lazarus installation directory, from lazbuild itself. On Linux the
    #  binary on PATH is usually a symlink into it (/usr/bin/lazbuild ->
    #  ../lib64/lazarus/lazbuild), so the link has to be followed or the
    #  interface units are looked for next to /usr/bin.
    $lb = Resolve-Lazbuild
    if (-not $lb) { return '' }
    if (-not $IsWindows) {
        $real = (& readlink -f $lb 2>$null)
        if ($real) { $lb = $real }
    }
    return (Split-Path $lb -Parent)
}

function Test-LclInterface([string] $Name) {
    #  An LCL widget set is present when its compiled interface units are:
    #  <lazarus>/lcl/units/<target>/<name>/. Asking lazbuild to try it and
    #  watching it fail costs a full compile to answer the same question.
    $root = Get-LazarusRoot
    if (-not $root) { return $false }
    $units = Join-Path $root 'lcl/units'
    if (-not (Test-Path $units)) { return $false }
    foreach ($target in (Get-ChildItem $units -Directory -ErrorAction SilentlyContinue)) {
        if (Test-Path (Join-Path $target.FullName $Name)) { return $true }
    }
    return $false
}

#  THE WIDGET SET IS ALWAYS NAMED, on every platform. Left unnamed, lazbuild takes
#  the project file's LCLWidgetType - and an IDE session leaves those macros in
#  Desktop/Fit.lpi (qt5, at the time of writing). The Windows client was then linked
#  against Qt5, which no Windows machine has and no installer ships, and it failed
#  to start with STATUS_DLL_NOT_FOUND. The platform's own set is the one every
#  package is built and tested for: win32 on Windows, cocoa on macOS, and on Linux
#  Qt when it is installed (it follows a scaled desktop) and GTK2 otherwise.
#  Carried identically in tools/build-lib/build.ps1 (DF10); a test compares the two.
function Select-ClientWidgetSet([string] $TargetOS, [string[]] $InstalledQt) {
    switch ($TargetOS) {
        'windows' { return 'win32' }
        'macos'   { return 'cocoa' }
    }
    foreach ($ws in 'qt6', 'qt5') {
        if ($InstalledQt -contains $ws) { return $ws }
    }
    return 'gtk2'
}

function Get-ClientWidgetSet {
    if ($null -ne $script:WidgetSet) { return $script:WidgetSet }
    #  The TARGET's platform: the one cross build here produces a macOS binary.
    $target = if (Test-CrossBuild) { 'macos' }
              elseif ($IsWindows) { 'windows' }
              elseif ($IsMacOS) { 'macos' }
              else { 'linux' }
    [string[]] $qt = @()
    if ($target -eq 'linux') {
        $qt = @('qt6', 'qt5' | Where-Object { Test-LclInterface $_ })
    }
    $script:WidgetSet = Select-ClientWidgetSet $target $qt
    if ($script:WidgetSet -eq 'gtk2') {
        Write-Host ('    WIDGET SET: gtk2 - neither the qt6 nor the qt5 LCL interface is ' +
                    'installed.') -ForegroundColor Yellow
        Write-Host ('                GTK2 cannot see a scaled desktop, so on one this build ' +
                    'will lay itself out at 100%.') -ForegroundColor Yellow
        Write-Host ('                Install: lcl-qt6 and libqt6pas6 (or lcl-qt5 and ' +
                    'libqt5pas1).') -ForegroundColor Yellow
    }
    return $script:WidgetSet
}

function Get-LinuxRuntimeDependency([string] $Packaging) {
    #  The shared library the built binary actually needs, which follows the
    #  widget set it was built against and not a guess. A .deb that names gtk2
    #  while the binary links libQt6Pas installs cleanly and then does not start.
    $names = switch (Get-ClientWidgetSet) {
        'qt6'   { @{ deb = 'libqt6pas6';   rpm = 'qt6pas' } }
        'qt5'   { @{ deb = 'libqt5pas1';   rpm = 'qt5pas' } }
        default { @{ deb = 'libgtk2.0-0';  rpm = 'gtk2'    } }
    }
    return $names[$Packaging]
}

function Resolve-Fpc {
    #  Same problem as lazbuild, and for the same reason: the compiler ships
    #  INSIDE the Lazarus installation and none of the installers put it on PATH
    #  on Windows (C:\lazarus\fpc\3.2.2\bin\x86_64-win64\fpc.exe). Demanding it
    #  on PATH failed the toolchain check on a machine where Lazarus was
    #  installed exactly as the instructions say - so the check reported a
    #  missing compiler that was in fact right there, next to the lazbuild it had
    #  just found.
    if ($script:Fpc) { return $script:Fpc }
    if ($env:FPC) {
        if (-not (Test-Path $env:FPC)) { throw "FPC is set to '$env:FPC', which does not exist." }
        $script:Fpc = (Resolve-Path $env:FPC).Path
        return $script:Fpc
    }

    $cmd = Get-Command fpc -ErrorAction SilentlyContinue
    if ($cmd -and $cmd.Source -and (Test-Path $cmd.Source)) {
        $script:Fpc = $cmd.Source
        return $script:Fpc
    }

    #  Beside the lazbuild already located: whichever Lazarus this build uses,
    #  that is the compiler it will use, so looking there cannot pick a stray
    #  second installation.
    $exe = if ($IsWindows) { 'fpc.exe' } else { 'fpc' }
    $lazDir = Split-Path -Parent (Resolve-Lazbuild)
    $found = Get-ChildItem -Path (Join-Path $lazDir 'fpc') -Filter $exe -Recurse -File -ErrorAction SilentlyContinue |
             Sort-Object FullName -Descending | Select-Object -First 1
    if ($found) { $script:Fpc = $found.FullName; return $script:Fpc }

    throw @"
fpc was not found.

It is the Free Pascal compiler and comes with Lazarus, but the installers do not
always put it on PATH - on Windows it lives under the Lazarus directory, e.g.
  C:\lazarus\fpc\3.2.2\bin\x86_64-win64\fpc.exe

Either add that directory to PATH, or point at it directly:
  `$env:FPC = '/path/to/fpc'
"@
}

function Test-CrossBuild { return [bool] $TargetCpu }

function Get-TargetArgs {
    #  What turns a native lazbuild into a cross build. Nothing else differs:
    #  same project files, same packages, same compiler driver.
    if (-not (Test-CrossBuild)) { return @() }
    return @("--cpu=$TargetCpu", '--os=darwin')
}

function Get-TargetArchPattern {
    #  `file` names the same processor differently on different systems, and the
    #  check below is worthless if it matches nothing anywhere.
    switch ($TargetCpu) {
        'x86_64'  { return 'x86[_-]64' }
        'aarch64' { return 'arm64|aarch64' }
        default   { return [regex]::Escape($TargetCpu) }
    }
}

function Assert-CrossCompiler {
    #  The Free Pascal package for macOS is a universal build and carries the
    #  compiler for BOTH processors, which is the only reason cross-building the
    #  Intel archive on an Apple Silicon runner is possible at all. Checked
    #  rather than assumed: without it lazbuild fails much later, inside a unit,
    #  with a message that does not mention the missing compiler.
    if (-not (Test-CrossBuild)) { return }
    $fpc = Resolve-Fpc
    #  Compiled, not asked. `-iV` answers from the driver and says nothing about
    #  whether the RUNTIME LIBRARY for the target is installed - and a missing
    #  RTL is the failure this is here to catch, because it surfaces otherwise as
    #  a unit not found halfway through the client build.
    $work = Join-Path ([System.IO.Path]::GetTempPath()) ("fit-cross-" + [guid]::NewGuid().ToString('N'))
    New-Item -ItemType Directory -Force -Path $work | Out-Null
    try {
        $src = Join-Path $work 'probe.pas'
        Set-Content -Path $src -Value "begin end." -Encoding ascii
        $probe = (& $fpc "-P$TargetCpu" -Tdarwin "-FE$work" $src 2>&1) -join "`n"
        $built = $LASTEXITCODE -eq 0
    }
    finally { Remove-Tree $work }
    if (-not $built) {
        throw @"
This Free Pascal cannot build for $TargetCpu-darwin, so the cross build cannot run.

$probe

The universal package installed by scripts/install-lazarus-macos.sh
(fpc-3.2.2.intelarm64-macosx.dmg) carries the compiler and the runtime library
for both processors; an fpc installed some other way may carry only the one it
runs on.
"@
    }
    Write-Host "    cross-compiling for $TargetCpu-darwin"
}

#  Lazarus packages the projects require BY NAME. Two live in sibling repositories
#  checked out beside this one; the third is bundled here.
$LazPackages = @(
    '../fitminimizers/package/FitMinimizers.lpk',
    '../fitgrids/package/FitGrids.lpk',
    'Packages/TAGraph/Package/ta.lpk'
)

function Register-LazarusPackages {
    #  lazbuild resolves these through its PACKAGE LINKS, not through a project's
    #  search paths, so on a machine where they were never registered the first
    #  build fails with "package not found" and nothing says which package. Making
    #  a link is idempotent and takes milliseconds, so it happens on every build
    #  rather than in a setup step someone can skip.
    foreach ($p in $LazPackages) {
        $full = Join-Path $Root $p
        if (-not (Test-Path $full)) {
            throw @"
Lazarus package not found: $full

Fit needs two sibling repositories checked out NEXT TO this one:

    git clone https://github.com/dvmorozov/fitgrids
    git clone https://github.com/dvmorozov/fitminimizers

so that the three directories sit side by side.
"@
        }
        #  A first run prints "CopySecondaryConfigFile ..." while succeeding, and a
        #  visitor's machine is always a first run - so treating ANY output as
        #  failure fails for everyone the script exists to serve. Judge by the
        #  exit code and by whether the output reads like an error; a link that
        #  genuinely did not happen surfaces two steps later as "package not
        #  found", which says so plainly.
        $out = (& (Resolve-Lazbuild) --add-package-link $full 2>&1) -join "`n"
        if ($LASTEXITCODE -ne 0 -or $out -match '(?i)(error|invalid|unknown|cannot|unable|failed)') {
            throw "Registering $p with lazbuild failed ($LASTEXITCODE): $out"
        }
    }
}

function Invoke-Check {
    Write-Step 'Checking the toolchain'
    $lb = Resolve-Lazbuild
    $fpc = Resolve-Fpc
    Write-Host "    lazbuild $((& $lb --version) -join ' ')  [$lb]"
    Write-Host "    Free Pascal $((& $fpc -iV) -join ' ')  [$fpc]"
    Assert-CrossCompiler
    Register-LazarusPackages
    Write-Host '    Lazarus packages registered: FitMinimizers, FitGrids, ta'
}

#  macOS: THE APPLICATION HAS TO BE A BUNDLE, EVEN IN DEVELOPMENT.
#
#  lazbuild writes a bare Mach-O, and macOS registers a bare executable as a
#  BACKGROUND application. Measured, not assumed - `lsappinfo` reports
#  type="BackgroundOnly" bundleID=[ NULL ] for it, and type="Foreground" for the
#  SAME binary reached through a bundle. A background application gets no screen
#  menu bar, which on this platform is where the LCL puts TMainMenu - so the
#  complete menu in Desktop/Forms/form_main.lfm is nowhere at all - and no Dock
#  tile, so a minimised window is hard to get back.
#
#  A SHELL AROUND THE BINARY, not a copy of it: the inner file is a relative
#  symlink, so it cannot be a build behind. A hard link would go stale silently,
#  because lazbuild writes a NEW file rather than rewriting the old one.
#  Idempotent and cheap enough to run on every build and every launch, so a tree
#  built before this existed gets its shell without being rebuilt.
function New-MacRunShell([string] $Exe, [string] $IconSource, [string] $Label = 'Fit', [string] $Name = 'fit') {
    if (-not $IsMacOS) { return $Exe }
    $dir = Split-Path -Parent $Exe
    #  Named like the installed bundle of the product being built.
    $app = Join-Path $dir "$Label.app"
    $contents = Join-Path $app 'Contents'
    $macos = Join-Path $contents 'MacOS'
    $res = Join-Path $contents 'Resources'
    foreach ($d in $macos, $res) { New-Item -ItemType Directory -Force -Path $d | Out-Null }

    #  The Lazarus IDE's own "Create Application Bundle" leaves one of these
    #  beside the binary, carrying com.company.Fit-x86_64-darwin and version 0.1.
    #  It is not what ships and it is not a spare: it starts the client without a
    #  compute server, and Get-ClientBinary's recursive glob can return the
    #  symlink inside it instead of the binary. Retired here, where the real one
    #  is made, rather than documented as a trap.
    Get-ChildItem -Path $dir -Filter '*.app' -Directory -ErrorAction SilentlyContinue |
        Where-Object { $_.Name -ne "$Label.app" } |
        ForEach-Object { Remove-Item -Recurse -Force -LiteralPath $_.FullName }

    #  Named `Fit`, the same as inside the shipped bundle, so development and the
    #  installed application have one shape - and so that Get-ClientBinary's
    #  `Fit-*` glob cannot match it.
    $inner = Join-Path $macos 'Fit'
    $target = '../../../' + (Split-Path -Leaf $Exe)
    $link = Get-Item -LiteralPath $inner -Force -ErrorAction SilentlyContinue
    if ($link -and $link.Target -ne $target) { Remove-Item -Force -LiteralPath $inner; $link = $null }
    if (-not $link) { New-Item -ItemType SymbolicLink -Path $inner -Target $target | Out-Null }

    #  CFBundleName is what the menu bar and "Quit Fit" read; without it they
    #  would say Fit-x86_64-darwin. The identifier is deliberately NOT the shipped
    #  io.github.dvmorozov.fit: LaunchServices, `open -b` and the Dock must never
    #  confuse a development build with an installed /Applications/Fit.app.
    $plist = @"
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
    <key>CFBundleName</key><string>$Label</string>
    <key>CFBundleDisplayName</key><string>$Label</string>
    <key>CFBundleExecutable</key><string>Fit</string>
    <key>CFBundleIdentifier</key><string>io.github.dvmorozov.$Name.dev</string>
    <key>CFBundleIconFile</key><string>Fit</string>
    <key>CFBundlePackageType</key><string>APPL</string>
    <key>LSMinimumSystemVersion</key><string>11.0</string>
    <key>NSHighResolutionCapable</key><true/>
</dict>
</plist>
"@
    [System.IO.File]::WriteAllText((Join-Path $contents 'Info.plist'),
                                   ($plist -replace "`r`n", "`n"),
                                   (New-Object System.Text.UTF8Encoding $false))
    [System.IO.File]::WriteAllText((Join-Path $contents 'PkgInfo'), "APPL????",
                                   (New-Object System.Text.UTF8Encoding $false))

    #  The Dock tile is the point of the exercise, so it gets the real icon when
    #  the tools are there - and a generic tile, which is still a tile, when they
    #  are not. Nothing here may FAIL a build over an icon.
    $icns = Join-Path $res 'Fit.icns'
    if ($IconSource -and (Test-Path -LiteralPath $IconSource) -and -not (Test-Path -LiteralPath $icns) -and
        (Get-Command sips -ErrorAction SilentlyContinue) -and (Get-Command iconutil -ErrorAction SilentlyContinue)) {
        $work = Join-Path ([System.IO.Path]::GetTempPath()) "fit-dev-iconset-$PID"
        $set = Join-Path $work 'Fit.iconset'
        New-Item -ItemType Directory -Force -Path $set | Out-Null
        try {
            foreach ($s in 16, 32, 128, 256, 512) {
                & sips -z $s $s $IconSource --out (Join-Path $set "icon_${s}x${s}.png") | Out-Null
                & sips -z ($s * 2) ($s * 2) $IconSource --out (Join-Path $set "icon_${s}x${s}@2x.png") | Out-Null
            }
            & iconutil -c icns $set -o $icns | Out-Null
        }
        catch { }
        finally { Remove-Item -Recurse -Force -LiteralPath $work -ErrorAction SilentlyContinue }
    }
    return $inner
}

#  A BUILD LEAVES ITS PROJECT FILE AS IT FOUND IT. lazbuild 3.0, handed
#  --widgetset, rewrites the project it builds and appends one more
#  LCLWidgetType macro every time - a tracked file dirtied by every build with a
#  change nobody made (twelve copies had been committed in Desktop/Fit.lpi). So
#  the file's bytes are put back afterwards, whether the build succeeded or not,
#  and a file the build left alone is not written. The build number is raised
#  before this runs (Step-AppBuildNumber), so it is part of what is kept.
#  RESOLVED FIRST: .NET resolves a relative path against the process's
#  directory, which Set-Location does not move.
#  Carried identically in both builders (DF10); a test compares the two.
function Invoke-KeepingProjectFile([string] $Project, [scriptblock] $Build) {
    $full = (Resolve-Path -LiteralPath $Project).ProviderPath
    $before = [System.IO.File]::ReadAllBytes($full)
    try { & $Build }
    finally {
        $after = [System.IO.File]::ReadAllBytes($full)
        if (-not [System.Collections.StructuralComparisons]::StructuralEqualityComparer.Equals($before, $after)) {
            [System.IO.File]::WriteAllBytes($full, $before)
        }
    }
}

function Build-Client {
    Write-Step 'Building the desktop client'
    $ws = Get-ClientWidgetSet
    #  [string[]], NOT `$wsArgs = if ($ws) { @("--widgetset=$ws") } else { @() }`.
    #  An if-statement unrolls its output, so a ONE-element array comes out as a
    #  plain string - and splatting a string spreads it one CHARACTER per
    #  argument. lazbuild was handed "-", "-", "w", "i", ... and answered
    #  "Invalid option at position 1: -". The empty case worked by accident, which
    #  is why this survived until a machine actually had Qt installed.
    [string[]] $wsArgs = @()
    if ($ws) { $wsArgs = @("--widgetset=$ws") }
    if ($ws) { Write-Host "    widget set: $ws" }
    Invoke-KeepingProjectFile Desktop/Fit.lpi { & (Resolve-Lazbuild) @(Get-TargetArgs) @wsArgs Desktop/Fit.lpi }
    if ($LASTEXITCODE -ne 0) { throw "Client build failed ($LASTEXITCODE)" }
    #  A build that produced nothing is a failure, whatever the exit code said.
    if (-not (Get-ClientBinary)) { throw 'The client build produced no binary.' }
    #  macOS only: leave something double-clickable beside the binary. See
    #  New-MacRunShell - a bare Mach-O has no menu bar and no Dock tile.
    if ($IsMacOS) {
        $shell = New-MacRunShell (Get-ClientBinary) (Join-Path $script:Root 'Desktop/Fit.png')
        Write-Host "    app bundle: $(Split-Path -Parent (Split-Path -Parent (Split-Path -Parent $shell)))"
    }
}

function Build-Server {
    Write-Step 'Building the compute server'
    #  Built from its own directory: lazbuild resolves relative search paths
    #  against the working directory. Stale units are cleared first, because a
    #  leftover .ppu resolves happily against a source file that has since moved.
    Push-Location Worker
    try {
        if (Test-Path lib) { Remove-Tree lib }
        & (Resolve-Lazbuild) @(Get-TargetArgs) --widgetset=nogui fit_server.lpi
        if ($LASTEXITCODE -ne 0) { throw "Server build failed ($LASTEXITCODE)" }
    }
    finally { Pop-Location }
}

function Build-Launcher {
    #  WHAT THE START MENU POINTS AT, on Windows only. The client has no fitting
    #  engine, so a shortcut to Fit.exe gives a window that cannot fit anything
    #  until the user knows to start a second program. Linux packages answer that
    #  with a shell wrapper and the macOS bundle with a stub inside it; Windows
    #  has no shell to write one in that does not flash a console window on every
    #  start, so the wrapper is a small GUI binary.
    #
    #  fpc, not lazbuild: it links no LCL and no Lazarus package, so there is
    #  nothing for a project file to describe - and a fourth .lpi would join the
    #  cross-build option juggling below for no reason.
    if (-not $IsWindows) { return }
    Write-Step 'Building the launcher'
    $out = Join-Path $Root 'Worker/o'
    $units = Join-Path $Root 'Worker/lib/launcher'
    foreach ($d in $out, $units) { New-Item -ItemType Directory -Force -Path $d | Out-Null }
    #  -WG is the Windows GUI subsystem: no console window is created for it, and
    #  none for the server it starts either.
    #  Common as well as Worker: the launcher waits for the server it starts to say
    #  it is listening, and that channel (readiness_channel) is shared with the
    #  server and the sidecar's owner.
    & (Resolve-Fpc) -Mobjfpc -Sh -WG "-FE$out" "-FU$units" "-Fu$(Join-Path $Root 'Worker')" `
        "-Fu$(Join-Path $Root 'Common')" (Join-Path $Root 'Worker/fit_launcher.lpr')
    if ($LASTEXITCODE -ne 0) { throw "Launcher build failed ($LASTEXITCODE)" }
    if (-not (Get-LauncherBinary)) { throw 'The launcher build produced no binary.' }
}

#  NO ASSEMBLER WORKAROUND HERE ANY MORE. A cross build used to append
#  `-Anasmdarwin -g-` to every <CustomOptions> and put the project files back
#  afterwards, because clang refused a label Free Pascal emitted inside a CFI
#  block. That label was a user-declared Pascal `label` in TAGraph.pas - our own
#  source - and it is gone, so this target assembles through clang with its debug
#  info intact, like every other one. See docs/contributing/building.md.

function Invoke-Build {
    Step-AppBuildNumber
    Register-LazarusPackages
    Build-Client
    Build-Server
    Build-Launcher
}

function Invoke-Test {
    #  ONE SUITE WITH TWO HALVES, not two suites. -Suite selects a half through the
    #  runner's own --suite option, so there is no second test program to keep in
    #  step with this one. Every test class registers itself into 'unit' or
    #  'integration', and tests/testcase_suite_split.pas fails the suite when one
    #  registers into neither - an unclassified test disappears from --suite=unit
    #  without failing anything, which is the one direction of this mistake that is
    #  invisible.
    #
    #  A cross-built suite cannot run here - it is built for another processor -
    #  and saying so is the point: the alternative is a green "tested" that tested
    #  nothing. The native job of the same build runs the same suite.
    if (Test-CrossBuild) {
        throw ("The test suite cannot run against a $TargetCpu build on this machine. " +
               'Run it natively, or ask for -Task build and -Task package.')
    }
    Write-Step $(if ($Suite -eq 'all') { 'Running the test suite' }
                 else { "Running the $Suite half of the test suite" })
    $lb = Resolve-Lazbuild

    #  REGISTERED HERE TOO, not only in -Task check. `-Task test` on its own is a
    #  documented entry point, and lazbuild resolves FitMinimizers, FitGrids and ta
    #  through its package LINKS rather than through the project's search paths - so
    #  without this the suite failed with "package not found" for anyone who asked
    #  for the tests without asking for a build first. Making a link is idempotent
    #  and takes milliseconds, which is why -Task all doing it twice costs nothing.
    Register-LazarusPackages

    #  A TYPED ARRAY BUILT IN TWO STATEMENTS, never `$sel = if (...) { @(...) }`.
    #  An if-statement unrolls its output, so a one-element array comes out as a
    #  plain string, and splatting a string spreads it one CHARACTER per argument.
    #  That exact expression broke the widget-set argument once already - see
    #  Build-Client, where the same comment stands over the same shape.
    [string[]] $selector = @('--all')
    if ($Suite -ne 'all') { $selector = @("--suite=$Suite") }

    #  ONLY A UNIT RUN SKIPS THE COMPUTE SERVER, and that follows from the rule
    #  rather than from thrift: a unit test crosses no process boundary, so there
    #  is nothing for a server to answer. Building one anyway spends a whole
    #  lazbuild project on a binary the run will never open.
    $needsServer = ($Suite -ne 'unit')

    #  tests/build-full.sh is the canonical recipe, and on a machine with sh it is
    #  what runs - one description of how the suite is built, not two. Windows
    #  often has no sh at all, though (GitHub's runner does, a developer's machine
    #  may not), so the same three steps are repeated here rather than telling a
    #  Windows user to install a shell to run tests.
    if (Get-Command sh -ErrorAction SilentlyContinue) {
        #  Handed the resolved path: the script cannot repeat the search, and PATH
        #  is exactly what is unreliable here.
        $env:LAZBUILD = $lb
        #  Captured AND shown: the run's own output is what a reader needs, and the
        #  count at the end of it is what Confirm-TestsRan reads.
        #
        #  SHOWN AS IT ARRIVES, not after the run. Capturing first and printing
        #  afterwards means a test that never returns prints nothing at all - two
        #  Windows releases hung for half an hour each on an empty log.
        & sh tests/build-full.sh @selector --format=plain 2>&1 |
            Tee-Object -Variable shown | ForEach-Object { Write-Host $_ }
        if ($LASTEXITCODE -ne 0) { throw "Tests failed ($LASTEXITCODE)" }
        Confirm-TestsRan $shown
        return
    }

    Push-Location (Join-Path $Root 'tests')
    try {
        if (Test-Path lib) { Remove-Tree lib }
        & $lb --widgetset=nogui fit_tests.lpi
        if ($LASTEXITCODE -ne 0) { throw "Building the test suite failed ($LASTEXITCODE)" }
    }
    finally { Pop-Location }

    if ($needsServer) { Build-Server }

    $bin = Join-Path $Root ('tests/fit_tests' + $(if ($IsWindows) { '.exe' } else { '' }))
    if (-not (Test-Path $bin)) { throw "The test binary was not produced: $bin" }
    Push-Location (Join-Path $Root 'tests')
    try {
        #  Streamed for the same reason as the sh branch above.
        & $bin @selector --format=plain 2>&1 |
            Tee-Object -Variable shown | ForEach-Object { Write-Host $_ }
        $code = $LASTEXITCODE
    }
    finally { Pop-Location }
    if ($code -ne 0) { throw "Tests failed ($code)" }
    Confirm-TestsRan $shown
}

function Confirm-TestsRan($Output) {
    #  A SUITE NAME THAT NO LONGER EXISTS RUNS NOTHING AND EXITS 0. The runner
    #  treats --suite=<unknown> as "no tests matched", which is not an error to it,
    #  so a rename on the Pascal side would turn this task into a green no-op -
    #  the exact shape of failure this project has been bitten by before: a passing
    #  build over a path nothing took. testcase_suite_split.pas guards the split
    #  itself, but it can only do that if it ran.
    $line = $Output | Select-String -Pattern '^Number of run tests:\s*(\d+)' | Select-Object -First 1
    if (-not $line) {
        throw ('The test runner did not report how many tests it ran, so there is no ' +
               'evidence the suite executed. Treating that as a failure rather than a pass.')
    }
    $count = [int] $line.Matches[0].Groups[1].Value
    if ($count -eq 0) {
        throw ("The runner matched no tests for -Suite $Suite and exited successfully. " +
               'That means the suite name no longer exists, not that everything passed.')
    }
    Write-Host "    $count tests ran." -ForegroundColor DarkGray
}

function Get-ClientBinary {
    #  An explicit path wins: that is how a build carrying a module gets packaged
    #  by this script rather than by a second copy of it.
    if ($ClientBinary) {
        if (-not (Test-Path $ClientBinary)) { throw "No such client binary: $ClientBinary" }
        return (Resolve-Path $ClientBinary).Path
    }
    #  ONE SEARCH FOR EVERY PLATFORM, because the project names its output the
    #  same way on all of them: Fit.lpi builds to
    #  o/$(TargetCPU)-$(TargetOS)/Fit-$(TargetCPU)-$(TargetOS). The Windows case
    #  used to name a path of its own and named the wrong one - Desktop/o/Fit/ is
    #  where the UNITS go, and the executable is a directory up under its full
    #  target name - so a Windows build that had just succeeded was reported as
    #  having produced no binary.
    #  Each target gets its own o/<cpu>-<os>/ directory, so a cross build does
    #  not overwrite the native one - and picking "the newest" would then pick
    #  whichever ran last rather than the one asked for.
    $pattern = if ($IsWindows) { 'Fit-*.exe' } elseif (Test-CrossBuild) { "Fit-$TargetCpu-*" } else { 'Fit-*' }
    $b = Get-ChildItem -Recurse -Path (Join-Path $script:Root 'Desktop/o') -Filter $pattern -File -ErrorAction SilentlyContinue |
         Where-Object { $IsWindows -or $_.Name -notmatch '\.' } |
         Sort-Object LastWriteTime -Descending | Select-Object -First 1
    if ($b) { return $b.FullName }
    return $null
}

function Get-ServerBinary {
    if ($ServerBinary) {
        if (-not (Test-Path $ServerBinary)) { throw "No such server binary: $ServerBinary" }
        return (Resolve-Path $ServerBinary).Path
    }
    $p = Join-Path $script:Root $(if ($IsWindows) { 'Worker/o/fit_server.exe' } else { 'Worker/o/fit_server' })
    if (Test-Path $p) { return (Resolve-Path $p).Path }
    return $null
}

function Get-LauncherBinary {
    #  Windows only, and $null everywhere else - the other two platforms launch
    #  through a shell wrapper that packaging writes rather than a binary.
    if (-not $IsWindows) { return $null }
    $p = Join-Path $script:Root 'Worker/o/fit_launcher.exe'
    if (Test-Path $p) { return (Resolve-Path $p).Path }
    return $null
}

function Get-AppVersion {
    #  From the project's own version info, so a package version can never
    #  disagree with what the application reports about itself.
    [xml] $lpi = Get-Content (Join-Path $script:Root 'Desktop/Fit.lpi')
    $vi = $lpi.CONFIG.ProjectOptions.VersionInfo
    function nr($node) { if ($node -and $node.Value) { return $node.Value } else { return '0' } }
    return "$(nr $vi.MajorVersionNr).$(nr $vi.MinorVersionNr).$(nr $vi.RevisionNr).$(nr $vi.BuildNr)"
}

function Set-AppVersion([string] $Version) {
    #  Edited as text inside the <VersionInfo> block rather than through the XML
    #  parser: saving an XmlDocument reformats the whole .lpi, turning a
    #  one-number change into a diff nobody can review - and Lazarus rewrites the
    #  file itself, so that churn would come back on the next IDE build.
    $path = Join-Path $script:Root 'Desktop/Fit.lpi'
    $n = $Version.Split('.')
    $text = Get-Content -LiteralPath $path -Raw
    $m = [regex]::Match($text, '(?s)<VersionInfo>.*?</VersionInfo>')
    if (-not $m.Success) { throw "No <VersionInfo> block in $path" }

    $block = $m.Value
    $fields = [ordered] @{
        MajorVersionNr = $n[0]; MinorVersionNr = $n[1]
        RevisionNr     = $n[2]; BuildNr        = $n[3]
    }
    foreach ($f in $fields.Keys) {
        $value = $fields[$f]
        if ($block -match "<$f Value=`"[0-9]+`"/>") {
            $block = $block -replace "<$f Value=`"[0-9]+`"/>", "<$f Value=`"$value`"/>"
        }
        else {
            #  Lazarus omits a field whose value is zero, so raising one from zero
            #  means adding the line rather than replacing it.
            $block = $block -replace '(?m)^([ \t]*)(<UseVersionInfo Value="True"/>)',
                                     ('$1$2' + "`n" + '$1' + "<$f Value=`"$value`"/>")
        }
    }
    Set-Content -LiteralPath $path -Value ($text.Remove($m.Index, $m.Length).Insert($m.Index, $block)) -NoNewline
}

function Step-AppBuildNumber {
    #  WHAT THE IDE DOES AND LAZBUILD DOES NOT. Desktop/Fit.lpi asks for
    #  AutoIncrementBuild, and the Lazarus IDE honours it on every build; lazbuild
    #  ignores it entirely. So a build driven from this script has to raise the
    #  number itself, or every binary between two releases reports the same four
    #  numbers and "which build is this?" has no answer - not in the window title,
    #  not in the About box, not in a bug report.
    #
    #  Raised BEFORE the compiler runs: the version is compiled into the binary
    #  from the .lpi, so a number written afterwards would describe the next build.
    #
    #  Once per run, whatever gets built. The client and the server are one
    #  application with one version.
    if ($script:BuildNumberRaised) { return }
    $script:BuildNumberRaised = $true

    if ($NoVersionBump) {
        Write-Host "    version stays at $(Get-AppVersion) (-NoVersionBump)"
        return
    }
    #  An automated build REPRODUCES a commit; it does not make one. A release
    #  workflow builds the tagged tree, and a number raised there would put
    #  1.2.0.1608 inside the artifacts of tag v1.2.0.1607.
    if ($env:CI) {
        Write-Host "    version stays at $(Get-AppVersion) (automated build)"
        return
    }

    $lpi = Get-Content -LiteralPath (Join-Path $script:Root 'Desktop/Fit.lpi') -Raw
    if ($lpi -notmatch '<AutoIncrementBuild Value="True"/>') {
        #  Said out loud rather than passed over: leaving the version alone is a
        #  legitimate choice, but a silent no-op is indistinguishable from a bump
        #  that failed.
        Write-Host "    Desktop/Fit.lpi has AutoIncrementBuild off - version stays at $(Get-AppVersion)" `
                   -ForegroundColor DarkYellow
        return
    }

    $p = @((Get-AppVersion).Split('.'))
    $next = '{0}.{1}.{2}.{3}' -f $p[0], $p[1], $p[2], ([int] $p[3] + 1)
    Set-AppVersion $next
    Write-Host "    version $next"
}

#  The launcher a package installs as /usr/bin/<package> - /usr/bin/fit, or
#  /usr/bin/fit-pro for a package built under another -PackageName. @NAME@ is
#  that package name, written in by New-LinuxInstallTree, so two products never
#  share a directory, a launcher or a log - and it is the client's own file
#  name, which is what the desktop identifies the window by (see the .desktop
#  entry below).
#
#  WHY A WRAPPER AND NOT THE BINARY ITSELF. Fit is a client and a compute server,
#  and the client has no engine: installed side by side, clicking the menu entry
#  gives a window that cannot fit anything until the user knows to start a second
#  program. That is not a package "installing the app". The wrapper starts the
#  server if nothing is answering, then runs the client - and leaves an already
#  running server alone, so it stays correct for anyone who runs their own, on
#  this machine or another.
#
#  THE SERVER OUTLIVES THE WINDOW, deliberately. Killing it on exit would be
#  tidier for a single window and wrong for two: the second client reuses the
#  first's server, and closing the first would take the engine away from the
#  second mid-fit. One idle server per session, reused by every later launch, is
#  the lesser cost - and it makes every start after the first immediate. Its log
#  is ~/.local/share/<package>/server.log.
$LauncherScript = @'
#!/bin/sh
#  Fit launcher: ensure a compute server is up, then start the desktop client.
set -e
PORT="${FIT_PORT:-8787}"
URL="http://127.0.0.1:$PORT/health"
LIB=/usr/lib/@NAME@

if ! curl -sf -m 2 "$URL" >/dev/null 2>&1; then
    mkdir -p "$HOME/.local/share/@NAME@"
    #  Returns once the server it starts has said it is listening - so the
    #  client's start-up probe never races it, and nothing here asks again and
    #  again. The server stays up after this returns.
    "$LIB/fit_server" --port "$PORT" --start-detached \
        >>"$HOME/.local/share/@NAME@/server.log" 2>&1 || true
fi

exec "$LIB/@NAME@" "$@"
'@

function New-LinuxInstallTree {
    # An FHS tree: the two binaries in /usr/lib/<package>, a launcher on the
    # PATH, a desktop entry, an icon, docs and the sample data - every path named
    # after the package, so Fit (fit) and Fit Pro (fit-pro) install side by side.
    #
    #  NOT NAMED $Root. PowerShell resolves a called function's free variables in
    #  the CALLER's scope, so a parameter by that name silently rebinds the
    #  script's repository root for Get-ClientBinary and Get-ServerBinary - which
    #  then looked for the binaries under the package tree, found none, and made
    #  packaging fail claiming they had never been built.
    param([string] $Dest, [string] $Name = 'fit', [string] $Label = 'Fit')
    Remove-Tree $Dest
    foreach ($d in 'usr/bin', "usr/lib/$Name", 'usr/share/applications',
                   'usr/share/icons/hicolor/256x256/apps', "usr/share/doc/$Name", "usr/share/$Name") {
        New-Item -ItemType Directory -Force -Path (Join-Path $Dest $d) | Out-Null
    }
    $client = Get-ClientBinary
    $server = Get-ServerBinary
    if (-not $client -or -not $server) { throw 'Both binaries must be built before packaging.' }
    $lib = Join-Path $Dest "usr/lib/$Name"
    #  THE CLIENT IS INSTALLED AS THE PACKAGE NAME, not as Fit. A Wayland
    #  compositor takes the window's app id from the executable's name, and on
    #  X11 its WM_CLASS is the same name; the desktop finds the menu entry - and
    #  with it the icon - by that. Named Fit, both products' windows said "Fit",
    #  which is neither fit.desktop nor fit-pro.desktop, and Plasma showed a
    #  generic icon in the application bar.
    Copy-Item $client (Join-Path $lib $Name)
    Copy-Item $server (Join-Path $lib 'fit_server')

    $launcher = Join-Path $Dest "usr/bin/$Name"
    #  LF endings and no trailing BOM: this is a shell script, and either would
    #  make it fail with an unreadable error about the interpreter.
    [System.IO.File]::WriteAllText($launcher, (($LauncherScript -replace "`r`n", "`n").Replace('@NAME@', $Name)),
                                   (New-Object System.Text.UTF8Encoding $false))

    #  Desktop/Fit.png is the application icon, generated beside Desktop/Fit.ico
    #  from one source image. A package with no icon is a defect, not a variant,
    #  so a missing file stops the build instead of shipping the blank default.
    $icon = Join-Path $Dest "usr/share/icons/hicolor/256x256/apps/$Name.png"
    $iconSrc = Join-Path $script:Root 'Desktop/Fit.png'
    if (-not (Test-Path $iconSrc)) { throw "The application icon $iconSrc is missing." }
    Copy-Item $iconSrc $icon

    $desktop = @(
        '[Desktop Entry]', 'Type=Application', "Name=$Label",
        'Comment=Interactive multi-peak curve fitting',
        "Exec=$Name %f", "Icon=$Name", 'Categories=Science;Education;',
        #  The window's app id and WM_CLASS are the client's file name, which
        #  is the package name - the same as this entry's. Stated anyway for
        #  X11 desktops that match on StartupWMClass alone.
        "StartupWMClass=$Name",
        'Keywords=curve;fitting;peak;spectroscopy;diffraction;', 'Terminal=false'
    ) -join "`n"
    [System.IO.File]::WriteAllText((Join-Path $Dest "usr/share/applications/$Name.desktop"),
                                   $desktop + "`n", (New-Object System.Text.UTF8Encoding $false))

    Copy-PackageData -Dest (Join-Path $Dest "usr/share/$Name/Data") -ModuleData $ModuleData
    foreach ($f in 'README.md', 'LICENSE', 'THIRD-PARTY.md') {
        if (Test-Path $f) { Copy-Item $f (Join-Path $Dest "usr/share/doc/$Name/") }
    }

    #  THE MODES ARE THE PACKAGE'S, NOT THE CHECKOUT'S. Copy-Item keeps each
    #  source file's mode, and a tree synced from Windows has every file at 0700
    #  - which installed the icon, the sample data and the docs readable by root
    #  alone, and left the desktop showing a generic icon for the window. Set
    #  last, once everything is in place: nothing executable, everything readable,
    #  directories searchable, then the launcher and the two binaries executable.
    & chmod -R 'a-x,u=rwX,go=rX' $Dest
    & chmod 755 $launcher (Join-Path $lib $Name) (Join-Path $lib 'fit_server')
}

function New-DebPackage([string] $OutDir, [string] $Version, [string] $Name = 'fit', [string] $Label = 'Fit') {
    if (-not (Get-Command dpkg-deb -ErrorAction SilentlyContinue)) {
        Write-Host '    .deb: dpkg-deb not found - skipped' -ForegroundColor DarkYellow; return
    }
    $tree = Join-Path $OutDir 'deb'
    New-LinuxInstallTree -Dest $tree -Name $Name -Label $Label
    New-Item -ItemType Directory -Force -Path (Join-Path $tree 'DEBIAN') | Out-Null
    #  curl is a dependency twice over: the launcher waits on the server with
    #  it, and every download from the internet goes through it
    #  (Common/curl_client.pas). No OpenSSL: nothing here loads it any more.
    $control = @(
        "Package: $Name", "Version: $Version", 'Architecture: amd64',
        'Maintainer: Dmitry Morozov <dvmorozov@hotmail.com>',
        'Section: science', 'Priority: optional',
        "Depends: $(Get-LinuxRuntimeDependency 'deb'), curl",
        'Description: Interactive multi-peak curve fitting',
        ' Models 1-D data - spectra, diffraction profiles, any measured curve - as',
        ' a sum of peak shapes, fitted to the data and compared by a number.'
    ) -join "`n"
    [System.IO.File]::WriteAllText((Join-Path $tree 'DEBIAN/control'), $control + "`n",
                                   (New-Object System.Text.UTF8Encoding $false))
    $deb = Join-Path $OutDir "${Name}_${Version}_amd64.deb"
    $builder = if (Get-Command fakeroot -ErrorAction SilentlyContinue) { 'fakeroot' } else { $null }
    if ($builder) { & fakeroot dpkg-deb --build $tree $deb } else { & dpkg-deb --build $tree $deb }
    if ($LASTEXITCODE -ne 0) { throw "dpkg-deb failed ($LASTEXITCODE)" }
    Write-Host "    $deb" -ForegroundColor Green
}

function Get-RpmSpec([string] $Name, [string] $Version, [string] $Dependency) {
    #  rpmbuild fails on any installed file the spec does not list, so %files must
    #  match New-LinuxInstallTree exactly - the launcher and BOTH binaries
    #  included, under the same package name.
    return @(
        "Name: $Name", "Version: $Version", 'Release: 1',
        'Summary: Interactive multi-peak curve fitting',
        'License: GPLv3+', 'BuildArch: x86_64',
        "Requires: $Dependency, curl",
        '%description',
        'Models 1-D data - spectra, diffraction profiles, any measured curve - as a',
        'sum of peak shapes, fitted to the data and compared by a number.',
        '%files',
        "/usr/bin/$Name",
        "/usr/lib/$Name",
        "/usr/share/applications/$Name.desktop",
        "/usr/share/icons/hicolor/256x256/apps/$Name.png",
        "/usr/share/$Name",
        "/usr/share/doc/$Name"
    ) -join "`n"
}

function New-RpmPackage([string] $OutDir, [string] $Version, [string] $Name = 'fit', [string] $Label = 'Fit') {
    if (-not (Get-Command rpmbuild -ErrorAction SilentlyContinue)) {
        Write-Host '    .rpm: rpmbuild not found - skipped' -ForegroundColor DarkYellow; return
    }
    $top   = Join-Path $OutDir 'rpmbuild'
    $broot = Join-Path $top "BUILDROOT/$Name"
    New-LinuxInstallTree -Dest $broot -Name $Name -Label $Label
    #  EMPTIED FIRST. rpmbuild's output directory outlives the run, and whatever
    #  an earlier build - of another product, or under an earlier name - left in
    #  it would otherwise be handed over again as if this run had made it.
    Remove-Tree (Join-Path $top 'RPMS')
    New-Item -ItemType Directory -Force -Path (Join-Path $top 'SPECS'), (Join-Path $top 'RPMS') | Out-Null
    $spec = Get-RpmSpec -Name $Name -Version $Version -Dependency (Get-LinuxRuntimeDependency 'rpm')
    $specFile = Join-Path $top "SPECS/$Name.spec"
    [System.IO.File]::WriteAllText($specFile, $spec + "`n", (New-Object System.Text.UTF8Encoding $false))
    & rpmbuild -bb --define "_topdir $top" --buildroot $broot $specFile
    if ($LASTEXITCODE -ne 0) { throw "rpmbuild failed ($LASTEXITCODE)" }
    Get-ChildItem -Recurse -Path (Join-Path $top 'RPMS') -Filter "$Name-$Version-*.rpm" | ForEach-Object {
        Copy-Item $_.FullName $OutDir
        Write-Host "    $(Join-Path $OutDir $_.Name)" -ForegroundColor Green
    }
}

#  The stub that IS the application as far as macOS is concerned - the bundle's
#  CFBundleExecutable. The same three rules as the Linux launcher above, and
#  deliberately the same numbers: the port, the health URL and the ten-second
#  budget are stated in Worker/launcher_rules.pas, written into both of these
#  scripts, and compared across all three by the packaging tests.
#
#  $0 is inside Contents/MacOS, where both binaries are: Gatekeeper expects
#  every Mach-O executable there, and one in Resources is the layout that fails
#  notarisation the day signing is turned on.
$MacLauncherScript = @'
#!/bin/sh
#  Fit launcher: ensure a compute server is up, then start the desktop client.
set -e
DIR="$(cd "$(dirname "$0")" && pwd)"
PORT="${FIT_PORT:-8787}"
URL="http://127.0.0.1:$PORT/health"

if ! curl -sf -m 2 "$URL" >/dev/null 2>&1; then
    mkdir -p "$HOME/Library/Logs/@LABEL@"
    #  Returns once the server it starts has said it is listening - so the
    #  client's start-up probe never races it, and nothing here asks again and
    #  again. The server stays up after this returns.
    "$DIR/fit_server" --port "$PORT" --start-detached \
        >>"$HOME/Library/Logs/@LABEL@/server.log" 2>&1 || true
fi

exec "$DIR/Fit" "$@"
'@

function New-MacIcns([string] $Dest) {
    #  AT PACKAGE TIME, from the icon the repository already generates, rather
    #  than as a fourth committed icon nobody remembers to regenerate.
    #
    #  Desktop/Fit.png is 256 px - it is the hicolor/256x256 icon - so the 512@2x
    #  slot is upscaled. Said out loud rather than shipped quietly: the honest fix
    #  is a 1024 px export from the artwork, which is a change to the icon
    #  pipeline and not to packaging.
    $src = Join-Path $script:Root 'Desktop/Fit.png'
    if (-not (Test-Path $src)) {
        throw "The application icon $src is missing. Generate it: ./scripts/build-app.ps1 -Task check tells you how, or see docs."
    }
    if (-not (Get-Command sips -ErrorAction SilentlyContinue) -or
        -not (Get-Command iconutil -ErrorAction SilentlyContinue)) {
        Write-Host '    .icns: sips/iconutil not found - the bundle gets the default icon' -ForegroundColor DarkYellow
        return
    }
    $work = Join-Path ([System.IO.Path]::GetTempPath()) "fit-iconset-$PID"
    $set = Join-Path $work 'Fit.iconset'
    New-Item -ItemType Directory -Force -Path $set | Out-Null
    try {
        foreach ($s in 16, 32, 128, 256, 512) {
            & sips -z $s $s $src --out (Join-Path $set "icon_${s}x${s}.png") | Out-Null
            & sips -z ($s * 2) ($s * 2) $src --out (Join-Path $set "icon_${s}x${s}@2x.png") | Out-Null
        }
        & iconutil -c icns $set -o $Dest
        if ($LASTEXITCODE -ne 0) { throw "iconutil failed ($LASTEXITCODE)" }
    }
    finally { Remove-Tree $work }
}

function New-MacAppBundle([string] $OutDir, [string] $Client, [string] $Server,
                          [string] $Version, [string] $Label, [string] $Name = 'fit') {
    #  There is no macOS build in CI - it could not be verified on the hardware
    #  available, and an unverified download is worse than none - so this is what
    #  a macOS user gets by building from source. It has to be a real bundle:
    #  Launchpad, the Dock and the Open With menu all address an .app and nothing
    #  else.
    $app = Join-Path $OutDir "$Label.app"
    Remove-Tree $app
    $contents = Join-Path $app 'Contents'
    $macos = Join-Path $contents 'MacOS'
    $res = Join-Path $contents 'Resources'
    foreach ($d in $macos, $res) { New-Item -ItemType Directory -Force -Path $d | Out-Null }

    #  THE THREE NAMES, ASSERTED DISTINCT UNDER CASE FOLDING BEFORE ANYTHING IS
    #  WRITTEN. On a case-insensitive volume - the macOS default - two names in
    #  one directory that differ only in case are ONE FILE. The launcher used to
    #  be called `fit` beside a client called `Fit`, which is exactly that: the
    #  stub overwrote the client it exists to start, and `exec "$DIR/Fit"` then
    #  re-exec'd the stub in an endless /bin/sh loop. -Task package produced an
    #  .app that could not start at all, on the volume almost every Mac has - and
    #  a test that only read this script's TEXT had nothing to say about it.
    $names = [ordered] @{ Client = 'Fit'; Launcher = 'fit_launcher'; Server = 'fit_server' }
    $folded = @($names.Values | ForEach-Object { $_.ToLowerInvariant() } | Select-Object -Unique)
    if ($folded.Count -ne $names.Count) {
        throw "Bundle file names collide under case folding: $($names.Values -join ', ')"
    }
    #  NOT $client/$server: PowerShell variable names are case-insensitive, so
    #  those would BE the $Client and $Server parameters - and the copy would have
    #  the destination as its source. The same trap as the file names above, one
    #  language up, and the packaging test caught it the same way.
    $clientPath = Join-Path $macos $names.Client
    $stubPath = Join-Path $macos $names.Launcher
    $serverPath = Join-Path $macos $names.Server

    Copy-Item $Client $clientPath
    Copy-Item $Server $serverPath
    #  LF endings and no BOM: this is a shell script, and either would make it
    #  fail with an unreadable error about the interpreter.
    #  @LABEL@ is the log directory: ~/Library/Logs/Fit or ~/Library/Logs/Fit Pro.
    [System.IO.File]::WriteAllText($stubPath, (($MacLauncherScript -replace "`r`n", "`n").Replace('@LABEL@', $Label)),
                                   (New-Object System.Text.UTF8Encoding $false))
    & chmod 755 $stubPath $clientPath $serverPath

    New-MacIcns (Join-Path $res 'Fit.icns')
    Copy-PackageData -Dest (Join-Path $res 'Data') -ModuleData $ModuleData
    foreach ($f in 'README.md', 'LICENSE', 'THIRD-PARTY.md') {
        if (Test-Path $f) { Copy-Item $f $res }
    }

    #  CFBundleShortVersionString is the three-part version people are shown;
    #  CFBundleVersion carries the build number, which is what tells two builds of
    #  one release apart. Both come from the project's own version info.
    $short = ($Version -split '\.')[0..2] -join '.'
    $plist = @"
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
    <key>CFBundleName</key><string>$Label</string>
    <key>CFBundleDisplayName</key><string>$Label</string>
    <!-- The stub, not the client: it starts the compute server first. -->
    <key>CFBundleExecutable</key><string>$($names.Launcher)</string>
    <!-- From the package name: two products must never share an identifier,
         or LaunchServices treats them as one application. -->
    <key>CFBundleIdentifier</key><string>io.github.dvmorozov.$Name</string>
    <key>CFBundleIconFile</key><string>Fit</string>
    <key>CFBundlePackageType</key><string>APPL</string>
    <key>CFBundleShortVersionString</key><string>$short</string>
    <key>CFBundleVersion</key><string>$Version</string>
    <key>LSMinimumSystemVersion</key><string>11.0</string>
    <key>LSApplicationCategoryType</key><string>public.app-category.education</string>
    <key>NSHighResolutionCapable</key><true/>
    <key>CFBundleDocumentTypes</key>
    <array>
        <dict>
            <key>CFBundleTypeName</key><string>$Label project</string>
            <key>CFBundleTypeRole</key><string>Editor</string>
            <key>CFBundleTypeExtensions</key><array><string>fitproj</string></array>
        </dict>
    </array>
</dict>
</plist>
"@
    [System.IO.File]::WriteAllText((Join-Path $contents 'Info.plist'),
                                   ($plist -replace "`r`n", "`n"),
                                   (New-Object System.Text.UTF8Encoding $false))

    #  Signing and notarisation are hooks: two environment variables turn them on
    #  and nothing here changes when they are unset. Nobody needs an Apple
    #  developer account to build this.
    if ($env:MACOS_SIGN_IDENTITY) {
        Write-Host '    signing the bundle' -ForegroundColor Cyan
        & codesign --deep --force --options runtime --sign $env:MACOS_SIGN_IDENTITY $app
        if ($LASTEXITCODE -ne 0) { throw "codesign failed ($LASTEXITCODE)" }
        if ($env:MACOS_NOTARY_PROFILE) {
            & xcrun notarytool submit --wait --keychain-profile $env:MACOS_NOTARY_PROFILE $app
            if ($LASTEXITCODE -ne 0) { throw "notarytool failed ($LASTEXITCODE)" }
            & xcrun stapler staple $app
        }
    }
    Write-Host "    $app" -ForegroundColor Green
    return $app
}

function New-MacDmg([string] $App, [string] $OutDir, [string] $Base, [string] $Label) {
    #  A BY-PRODUCT, one command wide. No CI builds a macOS download, so this is
    #  not a release artefact - it is the only way to hand the built application
    #  to another Mac to try.
    if (-not (Get-Command hdiutil -ErrorAction SilentlyContinue)) {
        Write-Host '    .dmg: hdiutil not found - skipped' -ForegroundColor DarkYellow
        return
    }
    $dmg = Join-Path $OutDir "$Base.dmg"
    if (Test-Path $dmg) { Remove-Item -Force $dmg }
    & hdiutil create -volname $Label -srcfolder $App -ov -format UDZO $dmg | Out-Null
    if ($LASTEXITCODE -ne 0) { throw "hdiutil failed ($LASTEXITCODE)" }
    Write-Host "    $dmg" -ForegroundColor Green
}

function Install-MacApp([string] $App, [string] $Label = 'Fit') {
    #  /Applications when it can be written without sudo - which it can, for an
    #  administrator, and most Macs have one user who is one - and the user's own
    #  Applications otherwise. Asking for a password to install something the
    #  user just built themselves is a step with nothing behind it.
    $dest = '/Applications'
    try { [System.IO.File]::WriteAllText("$dest/.fit-write-probe", ''); Remove-Item "$dest/.fit-write-probe" -Force }
    catch { $dest = Join-Path $HOME 'Applications' }
    New-Item -ItemType Directory -Force -Path $dest | Out-Null
    $target = Join-Path $dest (Split-Path -Leaf $App)
    Remove-Tree $target
    Copy-Item -Recurse $App $target
    #  A locally built bundle is not signed, and anything macOS considers
    #  quarantined and unsigned is reported to the user as DAMAGED - which is a
    #  lie about a program they compiled a minute ago.
    if (Get-Command xattr -ErrorAction SilentlyContinue) {
        & xattr -dr com.apple.quarantine $target 2>$null
    }
    Write-Host "    installed: $target" -ForegroundColor Green
    Write-Host "    it starts the compute server itself; its log is ~/Library/Logs/$Label/server.log"
}

function Install-WindowsSetup([string] $OutDir, [string] $Base) {
    $setup = Join-Path $OutDir "$Base.exe"
    if (-not (Test-Path $setup)) {
        Write-Host '    nothing to install: no setup.exe was built' -ForegroundColor DarkYellow
        return
    }
    #  Silent only where there is nobody to answer the wizard. An interactive run
    #  shows it, because the wizard is where the per-user/all-users choice and the
    #  file association are offered.
    $args = if ($env:CI) { @('/VERYSILENT', '/SUPPRESSMSGBOXES', '/NORESTART') } else { @() }
    & $setup @args | Out-Null
    if ($LASTEXITCODE -ne 0) { throw "The installer returned $LASTEXITCODE." }
    Write-Host '    installed; start it from the Start menu' -ForegroundColor Green
}

function Install-LinuxPackage([string] $OutDir, [string] $Version, [string] $Name) {
    $deb = Join-Path $OutDir "${Name}_${Version}_amd64.deb"
    if (Test-Path $deb) {
        Write-Host "==> Installing $deb (needs sudo)" -ForegroundColor Cyan
        & sudo dpkg -i $deb
        if ($LASTEXITCODE -ne 0) { throw 'dpkg failed. Try: sudo apt-get -f install' }
        Write-Host '    installed; run it with: fit' -ForegroundColor Green
        return
    }
    $rpm = Get-ChildItem -Path $OutDir -Filter "$Name-$Version-*.rpm" -ErrorAction SilentlyContinue |
           Sort-Object LastWriteTime | Select-Object -Last 1
    if ($rpm) {
        Write-Host "==> Installing $($rpm.FullName) (needs sudo)" -ForegroundColor Cyan
        & sudo rpm -Uvh $rpm.FullName
        if ($LASTEXITCODE -ne 0) { throw 'rpm failed.' }
        Write-Host '    installed; run it with: fit' -ForegroundColor Green
        return
    }
    Write-Host '    nothing to install: neither a .deb nor an .rpm was built' -ForegroundColor DarkYellow
}

function Resolve-Iscc {
    #  The Inno Setup compiler. Same problem as lazbuild and fpc: the installer
    #  does not put it on PATH, so looking there alone finds nothing on a machine
    #  where Inno Setup is installed exactly as its own installer left it.
    if ($script:Iscc) { return $script:Iscc }
    if ($env:ISCC) {
        if (-not (Test-Path $env:ISCC)) { throw "ISCC is set to '$env:ISCC', which does not exist." }
        $script:Iscc = (Resolve-Path $env:ISCC).Path
        return $script:Iscc
    }
    $cmd = Get-Command ISCC -ErrorAction SilentlyContinue
    if ($cmd -and $cmd.Source -and (Test-Path $cmd.Source)) {
        $script:Iscc = $cmd.Source
        return $script:Iscc
    }
    #  LOCALAPPDATA\Programs is where winget puts it when it installs for the
    #  current user, which is what the prerequisites task does - missing it made
    #  every package on such a machine skip the installer.
    $userPrograms = if ($env:LOCALAPPDATA) { Join-Path $env:LOCALAPPDATA 'Programs' } else { $null }
    foreach ($base in ${env:ProgramFiles(x86)}, $env:ProgramFiles, $userPrograms) {
        if (-not $base) { continue }
        $p = Join-Path $base 'Inno Setup 6/ISCC.exe'
        if (Test-Path $p) { $script:Iscc = (Resolve-Path $p).Path; return $script:Iscc }
    }
    return $null
}

function Get-AppId([string] $Name) {
    #  The upgrade key. FIXED for 'fit' and 'fit-pro' - a new one per build would
    #  install every release beside the last instead of over it - and derived
    #  from the name for any other variant, so a variant never installs over the
    #  application it was built from. The fit-pro key is the one Fit Pro was
    #  first installed with, under its earlier package name.
    if ($Name -eq 'fit') { return '8F3C6A1E-2D74-4B1E-9E0B-6A5C7D2F4B90' }
    if ($Name -eq 'fit-pro') { return '80F1F226-CC1B-AC82-D4A8-897F960D0AC6' }
    $md5 = [System.Security.Cryptography.MD5]::Create()
    $hash = $md5.ComputeHash([Text.Encoding]::UTF8.GetBytes("fit-setup:$Name"))
    return ([guid] $hash).ToString().ToUpperInvariant()
}

function New-WindowsInstaller([string] $Stage, [string] $OutDir, [string] $Version,
                              [string] $Base, [string] $Label, [string] $Name) {
    #  WHY AN INSTALLER AND NOT AN ARCHIVE. Fit is a client and a compute server,
    #  and the client has no fitting engine: an archive hands over two programs
    #  and no way to know that both have to run. What the installer adds is the
    #  shortcut - it points at fit_launcher.exe, which starts the server if
    #  nothing is answering - plus a place to uninstall from and an icon in the
    #  Start menu.
    $iscc = Resolve-Iscc
    if (-not $iscc) {
        #  Skipped with a word, exactly as .deb and .rpm are: a visitor without
        #  Inno Setup still gets a complete build. A release is protected by the
        #  workflow that checks the published assets instead.
        Write-Host '    setup.exe: ISCC (Inno Setup) not found - skipped' -ForegroundColor DarkYellow
        return
    }
    $template = Join-Path $script:Root 'scripts/windows/fit-setup.iss.in'
    if (-not (Test-Path $template)) { throw "The installer template $template is missing." }

    #  Signing is a hook, not a step: one environment variable holding an Inno
    #  SignTool command line turns it on, and nothing here changes when it is
    #  absent. No certificate is committed and none is needed to build.
    $signLine = if ($env:WINDOWS_SIGN_CMD) { 'SignTool=fitsign' } else { '' }

    #  The application's own icon, not a copy of it: a copy is what goes stale
    #  the day -Task set-icon replaces the original.
    $icon = Join-Path $script:Root 'Desktop/Fit.ico'
    if (-not (Test-Path $icon)) { throw "$icon is missing, so setup.exe would have no icon." }

    $iss = (Get-Content -Raw $template).
        Replace('@APPLABEL@', $Label).
        Replace('@APPID@', (Get-AppId $Name)).
        Replace('@VERSION@', $Version).
        Replace('@STAGE@', (Resolve-Path $Stage).Path).
        Replace('@OUTDIR@', (Resolve-Path $OutDir).Path).
        Replace('@OUTBASE@', $Base).
        Replace('@ICON@', (Resolve-Path $icon).Path).
        Replace('@SIGNTOOLLINE@', $signLine)
    $issFile = Join-Path $OutDir "$Base.iss"
    [System.IO.File]::WriteAllText($issFile, $iss, (New-Object System.Text.UTF8Encoding $false))

    $args = @()
    if ($env:WINDOWS_SIGN_CMD) { $args += "/Sfitsign=$env:WINDOWS_SIGN_CMD" }
    & $iscc @args $issFile
    if ($LASTEXITCODE -ne 0) { throw "ISCC failed ($LASTEXITCODE)" }
    Write-Host "    $(Join-Path $OutDir "$Base.exe")" -ForegroundColor Green
}

function Invoke-Package {
    Write-Step 'Packaging'
    $client = Get-ClientBinary
    $server = Get-ServerBinary
    if (-not $client) { throw 'The client binary is missing - run -Task build first.' }
    if (-not $server) { throw 'The server binary is missing - run -Task build first.' }
    $launcher = $null
    if ($IsWindows) {
        $launcher = Get-LauncherBinary
        if (-not $launcher) { throw 'The launcher binary is missing - run -Task build first.' }
    }

    $os   = if ($IsWindows) { 'windows' } elseif ($IsMacOS) { 'macos' } else { 'linux' }
    #  The architecture is READ, not assumed: macOS releases are built on both
    #  Apple Silicon and Intel runners, and a package that names the wrong one
    #  is worse than one that names none - it tells the person downloading it
    #  something false about the binary inside.
    $arch = if (Test-CrossBuild) {
        if ($TargetCpu -eq 'aarch64') { 'arm64' } else { $TargetCpu }
    }
    else {
        switch ([System.Runtime.InteropServices.RuntimeInformation]::OSArchitecture) {
            'X64'   { 'x86_64' }
            'Arm64' { 'arm64' }
            default { "$_".ToLowerInvariant() }
        }
    }
    #  A cross build has one way to go wrong quietly: the compute server writes
    #  to a single Worker/o/fit_server whatever it is built for, so packaging
    #  before rebuilding it would archive the previous target's binary under
    #  this target's name. Read the binaries back rather than trust the order.
    if (Test-CrossBuild) {
        foreach ($bin in @($client, $server)) {
            $desc = (& file -b $bin 2>&1) -join ' '
            if ($desc -notmatch (Get-TargetArchPattern)) {
                throw @"
$bin is not a $TargetCpu binary:
  $desc
Build for this target before packaging it:
  ./scripts/build-app.ps1 -Task build -TargetCpu $TargetCpu
"@
            }
        }
        Write-Host "    both binaries are $TargetCpu"
    }
    $label = $PackageLabel
    #  The label run together for file names: a space in one needs quoting in
    #  every shell and an escape in every URL.
    $name = "$($label -replace '\s', '')-$os-$arch"
    $dist = Join-Path $Root 'dist'
    $out  = Join-Path $dist $name
    $version = Get-AppVersion
    New-Item -ItemType Directory -Force -Path $dist | Out-Null

    if ($IsMacOS) {
        #  A BUNDLE, not a folder of files. macOS has one shape for an
        #  application and this is it: the launcher stub inside it starts the
        #  compute server, so the app works when it is double-clicked rather than
        #  when the user has read the instructions.
        $app = New-MacAppBundle -OutDir $dist -Client $client -Server $server -Version $version -Label $label -Name $PackageName
        New-MacDmg -App $app -OutDir $dist -Base "$name" -Label $label
        if ($Install) { Install-MacApp -App $app -Label $label }
        return
    }

    if (Test-Path $out) { Remove-Tree $out }
    New-Item -ItemType Directory -Force -Path $out | Out-Null

    Copy-Item $client (Join-Path $out ($(if ($IsWindows) { 'Fit.exe' } else { 'Fit' })))
    Copy-Item $server (Join-Path $out ($(if ($IsWindows) { 'fit_server.exe' } else { 'fit_server' })))
    if ($IsWindows) { Copy-Item $launcher (Join-Path $out 'fit_launcher.exe') }
    #  Sample data, so the application can be tried without hunting for a file to
    #  open.
    Copy-PackageData -Dest (Join-Path $out 'Data') -ModuleData $ModuleData
    foreach ($f in 'README.md', 'LICENSE', 'THIRD-PARTY.md') { Copy-Item $f $out }

    if ($IsWindows) {
        #  No portable archive beside it. Two Windows downloads that behave
        #  differently - one starting a server, one not - is the confusion this
        #  installer exists to end.
        New-WindowsInstaller -Stage $out -OutDir $dist -Version $version `
                             -Base "$name-setup" -Label $label -Name $PackageName
        if ($Install) { Install-WindowsSetup -OutDir $dist -Base "$name-setup" }
        return
    }

    $archive = Join-Path $dist "$name.tar.gz"
    if (Test-Path $archive) { Remove-Item -Force $archive }
    & tar -czf $archive -C $dist $name
    if ($LASTEXITCODE -ne 0) { throw "Creating $archive failed ($LASTEXITCODE)" }
    Write-Host "    $archive"

    #  Installable packages, on Linux, when the tools are there. Skipped with a
    #  word rather than failing: a portable archive is still a complete result.
    New-DebPackage -OutDir $dist -Version $version -Name $PackageName -Label $label
    New-RpmPackage -OutDir $dist -Version $version -Name $PackageName -Label $label
    if ($Install) { Install-LinuxPackage -OutDir $dist -Version $version -Name $PackageName }
}

function Open-InBrowser([string] $Url) {
    #  No Start-Process on Linux: it hands the URL to the desktop's default
    #  handler only when one is registered, and on a headless or minimal box it
    #  fails with a message about a missing file rather than about a browser.
    try {
        if ($IsWindows)   { Start-Process $Url | Out-Null }
        elseif ($IsMacOS) { & open $Url }
        else              { & xdg-open $Url 2>$null }
    }
    catch {
        Write-Host "    could not open a browser - go to $Url" -ForegroundColor Yellow
    }
}

function Test-ApiDocsReachable([string] $Url) {
    try {
        Invoke-WebRequest -Uri "$Url/health" -TimeoutSec 2 -UseBasicParsing | Out-Null
        return $true
    } catch { return $false }
}

function Invoke-ApiDocs {
    #  THE SERVER DESCRIBES ITSELF. Its /openapi.json is assembled by the running
    #  binary from its own routing and registries, so the only honest way to read
    #  the API of a build is to start that build and ask it - which is what this
    #  does, browser and all.
    Write-Step 'Starting the compute server and opening its API documentation'
    #  Built every time, not only when the binary is missing: the document is
    #  assembled by the binary that serves it, so an older one describes an API
    #  nobody is running - and reports it as "unknown endpoint" from a server
    #  that is otherwise working. lazbuild is incremental, so an unchanged tree
    #  costs a few seconds.
    Invoke-Check
    Build-Server
    $srv = Get-ServerBinary
    $url = 'http://127.0.0.1:8787'

    $proc = Start-Process -FilePath $srv -PassThru
    try {
        $ready = $false
        foreach ($i in 1..50) {
            if ($proc.HasExited) {
                throw "The compute server exited on start-up (code $($proc.ExitCode))."
            }
            if (Test-ApiDocsReachable $url) { $ready = $true; break }
            Start-Sleep -Milliseconds 200
        }
        #  Opening the browser first would be worse than waiting: Swagger UI
        #  fetches the document as the page loads, and against a server that has
        #  not bound yet it renders "Failed to load API definition" and stays
        #  that way until somebody reloads.
        if (-not $ready) { throw "The compute server did not answer at $url." }
        Write-Host "    API documentation at $url/docs"
        Write-Host "    The document itself is at $url/openapi.json"
        Open-InBrowser "$url/docs"
        Write-Host '    Serving until Ctrl+C.'
        Wait-Process -Id $proc.Id
    }
    finally {
        #  The server belongs to this command, so it goes when the command does.
        if ($proc -and -not $proc.HasExited) { $proc.Kill() }
    }
}

#  Checked before any task rather than inside the build: -Task package never
#  compiles anything, so a guard on the compile path let a cross-target package
#  run on Linux and archive the native binary under the target's name.
if ($TargetCpu -and -not $IsMacOS) {
    throw ("-TargetCpu is a macOS-only route (GitHub retired its Intel runners, so the " +
           "Intel archive is cross-built on Apple Silicon). This is " +
           "$([System.Runtime.InteropServices.RuntimeInformation]::OSDescription).")
}

switch ($Task) {
    'check'   { Invoke-Check }
    'build'   { Invoke-Check; Invoke-Build }
    'test'    { Invoke-Test }
    'package' { Invoke-Package }
    'api-docs' { Invoke-ApiDocs }
    'all'     {
        Invoke-Check
        Invoke-Build
        if (Test-CrossBuild) {
            Write-Host "    skipping the suite: a $TargetCpu build cannot run here (the native job runs it)" -ForegroundColor Yellow
        }
        else { Invoke-Test }
        Invoke-Package
    }
}
Write-Host 'Done.' -ForegroundColor Green
