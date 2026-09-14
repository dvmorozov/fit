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
MNT="$WORK/mnt"
#  DETACH BEFORE DELETING. The image is attached inside $WORK, so a cleanup that
#  only removes the directory walks into a read-only file system and buries
#  whatever actually failed under a screenful of rm complaints - and leaves the
#  image attached besides.
trap 'hdiutil detach -quiet -force "$MNT" 2>/dev/null || true; rm -rf "$WORK"' EXIT

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
mkdir -p "$MNT"
hdiutil attach -nobrowse -quiet -mountpoint "$MNT" "$WORK/fpc.dmg"

#  INSTALL THE PACKAGES INSIDE THE WRAPPER, NOT THE WRAPPER. The image ships the
#  compiler as a BUNDLE-format .mpkg - a directory - and macOS 26 refuses that
#  format outright: `installer` answers "This package is incompatible with this
#  version of macOS" without ever reading the distribution inside. It is the
#  format, not the contents; the wrapper's own requirement is macOS 10.5 or
#  newer, deleting its installation-check changes nothing, and the flat .pkg it
#  carries installs on the same machine. That wrapper adds one choice line and a
#  Command Line Tools warning over a single flat package, so the packages under
#  its Contents/Packages are the whole installation.
PKGS="$(find "$MNT" -path '*.mpkg/Contents/Packages/*.pkg')"
if [ -z "$PKGS" ]; then
    PKGS="$(find "$MNT" -maxdepth 1 \( -name '*.pkg' -o -name '*.mpkg' \) | head -1)"
fi
if [ -z "$PKGS" ]; then
    echo "The compiler image contained no installer package." >&2
    exit 1
fi

#  The warning that wrapper would have raised, kept: the compiler shells out to
#  the system linker, which arrives with the Command Line Tools or with Xcode.
if ! xcrun --find ld >/dev/null 2>&1; then
    echo "warning: no linker found - run xcode-select --install" >&2
fi

OLD_IFS="$IFS"
IFS='
'
for pkg in $PKGS; do
    echo "    installing $(basename "$pkg")"
    sudo installer -pkg "$pkg" -target /
done
IFS="$OLD_IFS"
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

#  ON PATH FOR EVERY TERMINAL, not just the one that ran this. The archive
#  leaves lazbuild in /Applications/lazarus, a directory on nobody's PATH, and
#  every caller here - the build script, the prerequisites check - looks for it
#  by bare name. That is how a completed install still ended in "lazbuild still
#  not on PATH - install Lazarus, then re-run this step". A link in
#  /usr/local/bin is where the compiler package above put fpc, it is the first
#  entry in /etc/paths on every Mac, and lazbuild resolves its own directory
#  THROUGH the link - so the portable config beside the real binary is still
#  the one it reads, which is the whole reason this is a link and not a copy.
sudo mkdir -p /usr/local/bin
sudo ln -sf /Applications/lazarus/lazbuild /usr/local/bin/lazbuild

echo "==> lazbuild: /usr/local/bin/lazbuild -> /Applications/lazarus/lazbuild"
echo "    compiler:  $FPC"
echo "    fpc source: ${FPCSRC:-(not published for this release - code tools will be limited)}"
