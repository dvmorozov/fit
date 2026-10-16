#!/usr/bin/env pwsh
# SPDX-License-Identifier: GPL-3.0-or-later
<#
.SYNOPSIS
    Writes latest.json, the update feed's manifest, beside a release's installers.

.DESCRIPTION
    The program reads this file from its update feed (Common/update_feed.pas):
    the newest version, its notes, and one installer per platform by EXACT file
    name with its SHA-256. It installs only a file whose digest matches, so the
    names and digests here are the ones of the files actually attached.

    The installer names are the ones the release publishes and the site links -
    <Product>-windows-setup.exe, <Product>-linux.deb, <Product>-linux.rpm,
    <Product>-macos.dmg - so the feed and the download table cannot disagree
    about what a file is called. Archives are not installers and are left out.

    PUBLISHED, and standalone rather than a task of build-app.ps1: a task there
    is a phase of the build, and writing a feed is not one. A module's own
    release writes its feed with this same script, under its product's name.

.EXAMPLE
    ./scripts/new-update-manifest.ps1 -AssetDir dist -Version v1.3.0 -NotesFile notes.md
#>
param(
    [Parameter(Mandatory)] [string] $AssetDir,
    [Parameter(Mandatory)] [string] $Version,
    [string] $NotesFile = '',
    [string] $Product = 'Fit',
    [string] $ReleaseDate = ((Get-Date).ToUniversalTime().ToString('yyyy-MM-dd'))
)

$ErrorActionPreference = 'Stop'

$names = @("$Product-windows-setup.exe", "$Product-linux.deb", "$Product-linux.rpm",
           "$Product-macos.dmg")
$assets = @()
foreach ($name in $names) {
    $path = Join-Path $AssetDir $name
    if (-not (Test-Path -LiteralPath $path)) { continue }
    $assets += [ordered] @{
        name   = $name
        url    = $name
        sha256 = (Get-FileHash -Algorithm SHA256 -LiteralPath $path).Hash.ToLowerInvariant()
    }
}
if ($assets.Count -eq 0) {
    throw "The release has no installer in $AssetDir to name in the update feed (looked for: $($names -join ', '))."
}

$notes = ''
if ($NotesFile -and (Test-Path -LiteralPath $NotesFile)) {
    $notes = Get-Content -Raw -LiteralPath $NotesFile
}

$manifest = [ordered] @{
    version     = $Version.TrimStart('v', 'V')
    releaseDate = $ReleaseDate
    notes       = $notes
    assets      = @($assets)
}
$out = Join-Path $AssetDir 'latest.json'
[System.IO.File]::WriteAllText($out, ($manifest | ConvertTo-Json -Depth 4),
    (New-Object System.Text.UTF8Encoding $false))
Write-Host "==> $out names $($assets.Count) installer(s) for $($manifest.version)"
