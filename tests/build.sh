#!/bin/sh
# Build and run the plain-FPC test suite headlessly - the route for a machine that
# has FPC but not Lazarus. Reproduces the project's compiler settings (Delphi mode,
# CORBA interfaces, defines). Only needs FPC (fcl-fpcunit ships with it) and the
# sibling fitminimizers package.
#
# THIS IS NOT THE UNIT SUITE, and the difference has bitten before. Everything that
# pulls an LCL or server unit is absent from this binary, and that cuts BOTH ways:
#
#   * UNIT classes are missing - this binary runs 1549 of them where the
#     Lazarus-built one runs 1812 - because they reach the REST surface, the
#     curve-type registry or the user-defined curve, whose configuration dialog
#     names LCL Controls; and
#   * INTEGRATION classes are present - nine of them - because reading a fixture
#     from disk needs no LCL at all.
#
# So a green run here does not mean the unit half passed. That is
# ./scripts/build-app.ps1 -Task test -Suite unit, which selects the half by the
# suite each class registered itself into rather than by what happened to link.
#
#   sh tests/build.sh                   # everything in this binary
#   sh tests/build.sh --suite=unit      # its unit classes only
#   sh tests/build.sh --format=xml      # JUnit XML for CI
set -e
cd "$(dirname "$0")/.."   # repository root

SUITE=""
case "$1" in
    --suite=*) SUITE="$1"; shift ;;
    --all)     shift ;;
esac
SELECTOR="${SUITE:---all}"

FITMINIMIZERS="${FITMINIMIZERS:-../fitminimizers/package}"

#  NO -Sa, and that is now the whole point. This build used to need ASSERTIONS
#  ON because a dozen tests assert that a precondition is REFUSED, and Assert is
#  compiled out without it. Every one of those preconditions is a Common/checks.pas
#  call now, which is unconditional, so debug and release make the same checks and
#  no build flag can silently remove them. A stray Assert would be inert here and
#  in the shipped binary alike - and tools/build-tests/no_assert.tests.ps1 fails
#  the build before it can be relied on.
fpc -MDelphi -SIcorba -dFIT -dFITCLIENT -dFITSERVER \
  -Futests/no-modules -Futests/fitminimizers -FuDesktop -FuDesktop/DataLoaders -FuDesktop/ModelCurves -FuDesktop/ModelCurves/CurveParameters -FuDesktop/ModelCurves/UserPointsSet -FuServer -FuServer/interfaces -FuCommon -FuWorker -Futests/mocks \
  -Fu"$FITMINIMIZERS" \
  -Fitests -FEtests -otests/fit_tests tests/fit_tests.lpr

# Default to plain output; pass --format=xml for CI.
exec ./tests/fit_tests "$SELECTOR" "${@:---format=plain}"
