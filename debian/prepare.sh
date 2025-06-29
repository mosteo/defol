#!/usr/bin/env bash

set -o errexit

# PARAMETERS (for future reuse?)

REPO=https://github.com/mosteo/defol
# Array of files/dirs to remove from the source

CRUFT=(
  ".clinerules"
  ".git"
  ".github"
  "tests"
)

# BEGIN

# Install dependencies for this script, if not already installed
[ ! -x "$(command -v tomlq)" ] && sudo apt install -y yq

# Extract version from ../alire.toml for source preparation using tomlq
version=$(tomlq -r '.version' ../alire.toml)

echo "Preparing source for version $version"

# Clone the repository
dst=defol-$version
rm -rf "$dst"
git clone --depth 1 --branch v"$version" "$REPO" "$dst"

# Create tarball
tar --exclude-vcs -caf "$dst.tar.gz" "$dst"