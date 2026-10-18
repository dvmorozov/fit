#!/usr/bin/env pwsh
# SPDX-License-Identifier: GPL-3.0-or-later
<#
    Installs the Lazarus release on Windows from the Lazarus project's own
    installer, compiler included.

    WHY NOT CHOCOLATEY. Its newest Lazarus is 4.0, so `choco install lazarus`
    installs a release this project does not build with, and `choco upgrade`
    reports success and changes nothing. winget carries the release, and the
    prerequisites step uses it where it exists; this script is for everywhere
    else - CI included - and installs exactly the release asked for.

    The default must match $LazarusVersion in tools/build-lib/prerequisites.ps1;
    a test compares them.
#>
param(
    [string] $Version = '4.8',
    #  The compiler the release is built with - part of the installer's name.
    [string] $Fpc = '3.2.2',
    #  The installer's own default, and the first place the build looks.
    [string] $Dir = 'C:\lazarus'
)
$ErrorActionPreference = 'Stop'

$exe = "lazarus-$Version-fpc-$Fpc-win64.exe"
$url = "https://downloads.sourceforge.net/lazarus/Lazarus%20Windows%2064%20bits/Lazarus%20$Version/$exe"
$tmp = Join-Path ([IO.Path]::GetTempPath()) $exe

Write-Host "==> $url"
#  SourceForge serves a download PAGE to a browser-like agent and the file to a
#  command-line one, and closes connections mid-transfer often enough that one
#  attempt is not a reliable install step (see install-lazarus-macos.sh).
Invoke-WebRequest -Uri $url -OutFile $tmp -UserAgent 'Wget' `
                  -MaximumRetryCount 6 -RetryIntervalSec 10
try {
    Write-Host "==> Installing Lazarus $Version into $Dir"
    $p = Start-Process -FilePath $tmp -Wait -PassThru -ArgumentList @(
        '/VERYSILENT', '/SUPPRESSMSGBOXES', '/NORESTART', '/SP-', "/DIR=$Dir")
    if ($p.ExitCode -ne 0) { throw "The Lazarus installer failed (exit $($p.ExitCode))." }
}
finally { Remove-Item -LiteralPath $tmp -ErrorAction SilentlyContinue }

$lazbuild = Join-Path $Dir 'lazbuild.exe'
if (-not (Test-Path -LiteralPath $lazbuild)) {
    throw "The installer finished, but there is no lazbuild at $lazbuild."
}
Write-Host "==> lazbuild: $lazbuild"
