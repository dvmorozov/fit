#!/bin/sh
# SPDX-License-Identifier: GPL-3.0-or-later
#
#  Installs Lazarus and its Free Pascal compiler on macOS.
#
#  WHY NOT HOMEBREW. `brew install --cask lazarus` was the documented route and
#  no longer works: the cask was deprecated in January 2025 and DISABLED in
#  January 2026 ("no longer meets the criteria for acceptable casks"), and there
#  has never been a formula. CI failed on exactly that. What is left is what the
#  Lazarus download page itself offers - a compiler package and an IDE archive,
#  published per architecture - so that is what this installs.
#
#  ARCHITECTURE MATTERS HERE. Apple Silicon and Intel have separate IDE archives
#  under separate directories; the compiler package is one universal build used
#  by both. Installing the wrong IDE archive gives an lazbuild that cannot run.
#
#  Set LAZARUS_VERSION to install a different release.
set -eu

VERSION="${LAZARUS_VERSION:-4.4}"
FPC_DMG="fpc-3.2.2.intelarm64-macosx.dmg"

case "$(uname -m)" in
    arm64)
        DIR="Lazarus%20macOS%20aarch64"
        ZIP="lazarus-darwin-aarch64-$VERSION.zip"
        ;;
    x86_64)
        DIR="Lazarus%20macOS%20x86-64"
        ZIP="lazarus-darwin-x86_64-$VERSION.zip"
        ;;
    *)
        echo "No published Lazarus build for $(uname -m)." >&2
        exit 1
        ;;
esac

BASE="https://downloads.sourceforge.net/lazarus/$DIR/Lazarus%20$VERSION"
WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT

#  SourceForge closes connections mid-transfer often enough that a single
#  attempt is not a reliable install step - it is how the previous route failed
#  its last time too, after thirteen minutes.
fetch() {
    echo "==> $1"
    curl -fL --retry 6 --retry-all-errors --retry-delay 10 \
         --connect-timeout 30 -o "$2" "$1"
}

echo '==> Free Pascal compiler'
fetch "$BASE/$FPC_DMG" "$WORK/fpc.dmg"
MNT="$WORK/mnt"
mkdir -p "$MNT"
hdiutil attach -nobrowse -quiet -mountpoint "$MNT" "$WORK/fpc.dmg"
PKG="$(find "$MNT" -maxdepth 1 \( -name '*.mpkg' -o -name '*.pkg' \) | head -1)"
if [ -z "$PKG" ]; then
    hdiutil detach -quiet "$MNT"
    echo "The compiler image contained no installer package." >&2
    exit 1
fi
sudo installer -pkg "$PKG" -target /
hdiutil detach -quiet "$MNT"

echo '==> Lazarus IDE'
fetch "$BASE/$ZIP" "$WORK/lazarus.zip"
#  The archive's own top-level directory is "lazarus", so it unpacks straight
#  into /Applications - the location the build script already looks in.
sudo rm -rf /Applications/lazarus
sudo unzip -q "$WORK/lazarus.zip" -d /Applications
#  Downloaded archives carry a quarantine flag that makes Gatekeeper refuse the
#  binaries; the release notes say to clear it.
sudo xattr -cr /Applications/lazarus 2>/dev/null || true
#  Lazarus in portable mode MAINTAINS its config beside the binaries, so the
#  tree has to belong to whoever runs it - installed root-owned, lazbuild
#  cannot write the files it expects to write.
sudo chown -R "$(id -un)" /Applications/lazarus

if [ ! -x /Applications/lazarus/lazbuild ]; then
    echo "The archive unpacked without an lazbuild in /Applications/lazarus." >&2
    exit 1
fi

#  TELL LAZBUILD WHERE IT IS. The release is built with its Lazarus directory
#  compiled in as /Developer/lazarus, so straight out of the archive every
#  invocation fails with `Invalid Lazarus directory "/Developer/lazarus/":
#  directory not found` - before it does anything at all, including registering
#  a package. The archive ships a config/ directory beside the binaries, which
#  puts lazbuild in portable mode and makes that directory the one it reads.
CFG=/Applications/lazarus/config
mkdir -p "$CFG"

#  The compiler the package above installed, and the RTL sources that ship in
#  the archive - both named outright rather than left to be guessed, since a
#  wrong guess here surfaces much later as a missing unit.
FPC="$(command -v fpc || echo /usr/local/bin/fpc)"
FPCSRC=""
for d in /Applications/lazarus/fpcsrc /usr/local/share/fpcsrc; do
    if [ -d "$d" ]; then FPCSRC="$d"; break; fi
done

cat > "$CFG/environmentoptions.xml" <<XML
<?xml version="1.0" encoding="UTF-8"?>
<CONFIG>
  <EnvironmentOptions>
    <Version Value="110"/>
    <LazarusDirectory Value="/Applications/lazarus/"/>
    <CompilerFilename Value="$FPC"/>
    <FPCSourceDirectory Value="$FPCSRC"/>
  </EnvironmentOptions>
</CONFIG>
XML

#  PROVE IT TOOK. `lazbuild --version` is not proof - it prints the version
#  happily with a broken Lazarus directory, which is exactly how the toolchain
#  check passed and the very next call failed. Asking it to open a file that
#  does not exist does start it up properly, so the complaint we are fixing
#  would appear here if it were still true.
probe="$(/Applications/lazarus/lazbuild /nonexistent-probe.lpi 2>&1 || true)"
case "$probe" in
    *"Invalid Lazarus directory"*)
        echo "lazbuild still cannot find its Lazarus directory:" >&2
        echo "$probe" >&2
        exit 1
        ;;
esac

echo "==> lazbuild: /Applications/lazarus/lazbuild"
echo "    compiler:  $FPC"
echo "    fpc source: ${FPCSRC:-(none found - code tools will be limited)}"
echo "    Add /Applications/lazarus to PATH, or set LAZBUILD to that path."
