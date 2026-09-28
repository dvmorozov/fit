#!/bin/sh
# SPDX-License-Identifier: GPL-3.0-or-later
#
#  Installs the Lazarus release, its Free Pascal and the Qt6 binding on Linux -
#  Debian and Ubuntu from the release's .deb packages, Fedora and openSUSE from
#  its .rpm packages.
#
#  WHY NOT THE DISTRIBUTION'S LAZARUS. It is older - Ubuntu 24.04 carries 3.0 -
#  and the chart is Lazarus's own TAChart, so the Lazarus version decides what
#  the chart can do. Every platform is brought to the one release named in
#  tools/build-lib/prerequisites.ps1 ($LazarusVersion), which the default below
#  must match (a test compares them).
#
#  THE DISTRIBUTION'S PACKAGES ARE PURGED FIRST. The release packages install
#  the same files - /usr/bin/fpc, /etc/fpc.cfg, the RTL - under different
#  package names, and declare conflicts with only some of Debian's names, so
#  leaving them in place ends in dpkg refusing to overwrite a file. Purged, not
#  removed: a removed package keeps its conffiles, /etc/fpc.cfg among them.
#
#  THE QT6 BINDING. The release packages carry the LCL's Qt6 interface as
#  source, and lazbuild compiles it on first use; what they do not carry is
#  libQt6Pas, the library the finished client loads. Ubuntu 24.04 packages none
#  at all, and the distributions' lcl-qt packages would bring their own Lazarus
#  back with them. Its maintainer publishes it per release, for .deb and .rpm.
#
#  Set LAZARUS_VERSION to install a different release; the compiler package
#  names below then may need FPC_DEB / FPC_RPM set to match its folder.
set -eu

VERSION="${LAZARUS_VERSION:-4.8}"
QT6PAS="${QT6PAS_VERSION:-6.2.10}"

case "$(uname -m)" in
    x86_64) ;;
    *)
        echo "The Lazarus project publishes Linux packages for x86_64 only, not $(uname -m)." >&2
        exit 1
        ;;
esac

if command -v apt-get >/dev/null 2>&1; then
    KIND=deb
    DIR="Lazarus%20Linux%20amd64%20DEB"
    FPC="${FPC_DEB:-3.2.2-210709}"
    FILES="fpc-laz_${FPC}_amd64.deb fpc-src_${FPC}_amd64.deb lazarus-project_${VERSION}.0-0_amd64.deb"
    QTFILES="libqt6pas6_${QT6PAS}-1_amd64.deb libqt6pas6-dev_${QT6PAS}-1_amd64.deb"
elif command -v dnf >/dev/null 2>&1 || command -v zypper >/dev/null 2>&1; then
    KIND=rpm
    DIR="Lazarus%20Linux%20x86_64%20RPM"
    FPC="${FPC_RPM:-3.2.2-241023}"
    FILES="fpc-laz-${FPC}.x86_64.rpm fpc-src-laz-${FPC}.x86_64.rpm lazarus-project-${VERSION}-0.x86_64.rpm"
    QTFILES="libqt6pas6-${QT6PAS}-1.x86_64.rpm libqt6pas6-devel-${QT6PAS}-1.x86_64.rpm"
else
    echo "Neither apt-get, dnf nor zypper found. Arch packages the current Lazarus itself: pacman -S lazarus qt6pas." >&2
    exit 1
fi

#  How to become root, the way tools/build-lib/prerequisites.ps1 does it
#  (Get-SudoPrefix): nothing as root, and -A when an askpass helper is set - the
#  VM tasks provide one, which is what lets a run with no terminal install.
if [ "$(id -u)" = 0 ]; then
    SUDO=""
elif [ -n "${SUDO_ASKPASS:-}" ] && [ -x "$SUDO_ASKPASS" ]; then
    SUDO="sudo -A"
else
    SUDO="sudo"
fi

BASE="https://downloads.sourceforge.net/lazarus/$DIR/Lazarus%20$VERSION"
QTBASE="https://github.com/davidbannon/libqt6pas/releases/download/v$QT6PAS"
WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT
#  The package manager reads the files as another user (apt as _apt).
chmod 755 "$WORK"

#  SourceForge closes connections mid-transfer often enough that a single
#  attempt is not a reliable install step (see install-lazarus-macos.sh).
fetch() {
    echo "==> $1"
    curl -fL --retry 6 --retry-all-errors --retry-delay 10 \
         --connect-timeout 30 -o "$2" "$1"
}

PATHS=""
for f in $FILES; do
    fetch "$BASE/$f" "$WORK/$f"
    PATHS="$PATHS $WORK/$f"
done
for f in $QTFILES; do
    fetch "$QTBASE/$f" "$WORK/$f"
    PATHS="$PATHS $WORK/$f"
done
chmod 644 "$WORK"/*

if [ "$KIND" = deb ]; then
    #  Every installed package of the distribution's Lazarus and Free Pascal:
    #  lazarus*, lcl*, fpc*, fp-*. Not the release's own three, which is what
    #  makes running this a second time safe.
    OLD="$(dpkg-query -W -f='${Package} ${Status}\n' 2>/dev/null |
           awk '$4 == "installed" && $1 ~ /^(lazarus|lcl|fpc|fp-)/ &&
                $1 != "lazarus-project" && $1 != "fpc-laz" && $1 != "fpc-src" { print $1 }')"
    if [ -n "$OLD" ]; then
        echo "==> Purging the distribution's Lazarus and Free Pascal"
        # shellcheck disable=SC2086
        $SUDO apt-get purge -y $OLD
    fi
    echo "==> Installing Lazarus $VERSION"
    # shellcheck disable=SC2086
    $SUDO apt-get install -y $PATHS
elif command -v dnf >/dev/null 2>&1; then
    echo "==> Installing Lazarus $VERSION"
    #  --allowerasing lets dnf take the distribution's conflicting packages out.
    # shellcheck disable=SC2086
    $SUDO dnf install -y --allowerasing $PATHS
else
    echo "==> Installing Lazarus $VERSION"
    # shellcheck disable=SC2086
    $SUDO zypper --non-interactive install --allow-unsigned-rpm --force-resolution $PATHS
fi

#  PROVE IT TOOK: the lazbuild now on PATH is the release.
have="$(lazbuild --version 2>/dev/null | grep -E '^[0-9]+(\.[0-9]+)+$' | tail -1 || true)"
case "$have" in
    "$VERSION"|"$VERSION".*)
        echo "==> lazbuild $have: $(command -v lazbuild)"
        ;;
    *)
        echo "Installed Lazarus $VERSION, but lazbuild reports '${have:-nothing}'." >&2
        exit 1
        ;;
esac
