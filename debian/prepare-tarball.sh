#!/usr/bin/env bash

set -o errexit

# PARAMETERS

REPO=https://github.com/mosteo/defol
# Array of files/dirs to remove from the source
CRUFT=(
  ".clinerules"
  ".git"
  ".github"
  "tests"
)

# Pass --local to build the orig tarball from the current working tree
# instead of cloning from GitHub (useful during development before a tag exists).
LOCAL=false
[[ "${1:-}" == "--local" ]] && LOCAL=true

# BEGIN

# Read Debian version from changelog (authoritative for package version).
# Strip the Debian revision suffix (e.g. "1.0.0-1" -> "1.0.0").
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
version=$(dpkg-parsechangelog -l "$SCRIPT_DIR/changelog" -S Version | sed 's/-[^-]*$//')
echo "Preparing source for version $version"

dst=defol-$version
out=../defol_${version}.orig.tar.gz

if $LOCAL; then
    echo "Building orig tarball from local working tree..."
    tmpdir=$(mktemp -d)
    cp -a "$SCRIPT_DIR/.." "$tmpdir/$dst"
    # Remove cruft
    for c in "${CRUFT[@]}"; do rm -rf "$tmpdir/$dst/$c"; done
    rm -rf "$tmpdir/$dst/debian" "$tmpdir/$dst/obj" "$tmpdir/$dst/bin"
    tar -C "$tmpdir" -czf "$out" "$dst"
    rm -rf "$tmpdir"
else
    # Install dependencies for this script, if not already installed
    [ ! -x "$(command -v tomlq)" ] && sudo apt install -y yq

    # Clone the repository at the release tag
    rm -rf "$dst"
    git clone --depth 1 --branch "v$version" "$REPO" "$dst"
    # Remove cruft
    for c in "${CRUFT[@]}"; do rm -rf "$dst/$c"; done
    tar --exclude-vcs -czf "$out" "$dst"
    rm -rf "$dst"
fi

echo "Created: $out"
