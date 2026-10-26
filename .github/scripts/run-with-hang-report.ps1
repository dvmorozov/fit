#!/usr/bin/env pwsh
<#
    Runs one command, and if it has not finished in -TimeoutSeconds, says what
    was still running before ending it.

    WHY IT EXISTS. A step's own `timeout-minutes` ends a hung test suite, but
    the runner kills the whole process tree first, so a step after it that
    lists the processes finds nothing: the run says "cancelled" and nothing
    else. A hang on Windows is the failure this matters for - 1.2.0.2039 to
    2042 never published because TLiveStopTest's timing tests starved on a
    4-core Windows runner - and on Windows nobody here can reproduce a
    runner's hang. So the command runs as a child of this script, which on a
    timeout prints the child's process tree (with each process's command line,
    CPU time and memory) and the busiest processes on the machine, then ends
    the tree and exits 124, the code `timeout` uses. The suite itself names
    each test as it starts when FIT_TEST_TRACE=1, so the log's last name is the
    test that hung.

    The step keeps a `timeout-minutes` above -TimeoutSeconds as a backstop for
    a hang in this script itself.

    Example (from fit/):
      ./.github/scripts/run-with-hang-report.ps1 -TimeoutSeconds 1200 `
          -Command './scripts/build-app.ps1 -Task test -Suite unit'
#>
param(
    [Parameter(Mandatory)] [int] $TimeoutSeconds,
    #  One string, run by a child pwsh: passing the command's own -Task and
    #  -Suite through this script's parameters would have this script's binder
    #  claim them.
    [Parameter(Mandatory)] [string] $Command
)
$ErrorActionPreference = 'Stop'

#  Every process descended from $RootId, by each process's parent.
function Get-ProcessTree([int] $RootId) {
    $all = @(Get-Process)
    $tree = [Collections.Generic.List[object]]::new()
    $ids = [Collections.Generic.HashSet[int]]::new()
    [void] $ids.Add($RootId)
    $grown = $true
    while ($grown) {
        $grown = $false
        foreach ($p in $all) {
            $parent = try { $p.Parent } catch { $null }
            if ($parent -and $ids.Contains($parent.Id) -and $ids.Add($p.Id)) {
                $tree.Add($p)
                $grown = $true
            }
        }
    }
    return $tree
}

function Format-Processes($Processes) {
    $Processes | ForEach-Object {
        $cpu = try { [math]::Round($_.TotalProcessorTime.TotalSeconds, 1) } catch { '?' }
        $cmd = try { $_.CommandLine } catch { '' }
        [pscustomobject] @{
            Id = $_.Id; Name = $_.ProcessName; CpuSeconds = $cpu
            MemoryMB = [math]::Round($_.WorkingSet64 / 1MB); CommandLine = $cmd
        }
    } | Format-Table -AutoSize -Wrap | Out-String -Width 250
}

$psi = [Diagnostics.ProcessStartInfo]::new((Get-Process -Id $PID).Path)
foreach ($a in '-NoProfile', '-NonInteractive', '-Command', $Command) { $psi.ArgumentList.Add($a) }
$psi.UseShellExecute = $false
$child = [Diagnostics.Process]::Start($psi)

if ($child.WaitForExit($TimeoutSeconds * 1000)) {
    #  The parameterless wait also waits for the output to drain.
    $child.WaitForExit()
    exit $child.ExitCode
}

Write-Host "::error::'$Command' did not finish in $TimeoutSeconds s; what was still running follows."
Write-Host '==> The command''s process tree'
Write-Host (Format-Processes (@($child) + @(Get-ProcessTree $child.Id)))
Write-Host '==> The busiest processes on this machine'
Write-Host (Format-Processes (Get-Process | Sort-Object { try { $_.TotalProcessorTime } catch { [timespan]::Zero } } -Descending |
    Select-Object -First 25))
try { $child.Kill($true) } catch { Write-Host "    could not end it: $_" }
exit 124
