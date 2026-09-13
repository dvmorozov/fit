#!/bin/sh
# Build + run the test suite via lazbuild with the nogui widgetset (LCL linked
# headless). Requires Lazarus and registered packages (FitMinimizers, FitGrids,
# ta) plus the sibling fitminimizers/fitgrids repos.
# A clean build avoids an intermittent unit-name/file-case lookup issue on Linux.
#
# WHICH HALF, and therefore whether a compute server is needed at all. A unit test
# crosses no process boundary - that is what makes it one - so --suite=unit has
# nothing to spawn, and building the server for it spends a whole lazbuild project
# on a binary the run will never open. No selector means both halves.
#
#   sh tests/build-full.sh                  # everything
#   sh tests/build-full.sh --suite=unit     # the fast half, no server built
#   sh tests/build-full.sh --suite=integration
set -e
cd "$(dirname "$0")"

SUITE=""
case "$1" in
    --suite=*) SUITE="$1"; shift ;;
    --all)     shift ;;
esac
SELECTOR="${SUITE:---all}"

#  $LAZBUILD if the caller resolved it - the installers do not reliably put
#  lazbuild on PATH, and a missing one used to fail here rather than where it
#  was actually needed.
LAZBUILD="${LAZBUILD:-lazbuild}"
command -v "$LAZBUILD" >/dev/null 2>&1 || {
    echo "lazbuild not found. Set LAZBUILD=/path/to/lazbuild, or add it to PATH." >&2
    exit 1
}
rm -rf lib
"$LAZBUILD" --widgetset=nogui fit_tests.lpi
# Build the compute server (links the engine, so nogui/LCL via lazbuild) so the
# server-process integration tests can spawn it. Clean first: same intermittent
# unit-name/file-case lookup issue on Linux as above.
#
# Skipped for a unit-only run - see the note at the top.
if [ "$SELECTOR" != "--suite=unit" ]; then
    ( cd ../Worker && rm -rf lib && "$LAZBUILD" --widgetset=nogui fit_server.lpi )
fi
#  RUN THE BINARY LAZBUILD JUST PRODUCED, which is not always the one
#  "./fit_tests" resolves to. tests/build.sh builds the LIGHT suite with plain
#  FPC to that exact name, and lazbuild writes fit_tests.exe beside it on
#  Windows - so under a POSIX shell there, "./fit_tests" found the light binary
#  and this script reported its 204 tests as the full run's. It failed nothing:
#  it just quietly stopped running the half that needs the LCL.
#
#  DECIDED BY PLATFORM, not by which file happens to exist. A Windows .exe can
#  easily be sitting in a Linux checkout - rsync to a test VM copies the whole
#  tree - and picking it there fails with "Exec format error", which is a worse
#  outcome than the one this replaced.
case "$(uname -s)" in
    MINGW*|MSYS*|CYGWIN*) TESTS=./fit_tests.exe ;;
    *)                    TESTS=./fit_tests ;;
esac
exec "$TESTS" "$SELECTOR" "${@:---format=plain}"
