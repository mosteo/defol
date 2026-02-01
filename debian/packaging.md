# Debian Packaging Plan for defol

## Overview

Publishing defol to Debian requires vendoring Alire dependencies, creating standard Debian packaging files with Ada-specific requirements per policy, adapting the GPR build system for gprbuild, and following modern debhelper (compat 13) workflows with quilt patch management.

## Steps

### 1. Vendor all Alire dependencies into source tree

Clone den, parse_args, simple_logging, stopwatch, aaa (aaa_base/ subdirectory),
and c_strings into `debian/vendor/` document all vendored packages with
versions, commit hashes, licenses, and upstream URLs in `debian/vendor/README`.

### 2. Adapt GPR configuration for vendored dependencies

Replace Alire-generated `config/defol_config.gpr` with version using relative paths (`with "../debian/vendor/den/den.gpr"`, etc.); set Build_Profile to "release" by default; remove Alire-specific variables; create `debian/patches/01-vendored-deps.patch` for the config changes and any dependency GPR file path adjustments needed.

### 3. Create core Debian packaging files

Write `debian/control` with Build-Depends on debhelper-compat (= 13), gnat (>= 12), gprbuild, and Conflicts with gnat (= 15.1.2) per Ada policy; create `debian/rules` using dh with override_dh_auto_build calling `gprbuild -P defol.gpr -XDEFOL_BUILD_PROFILE=release`; add `debian/changelog` for version 1.0.0-1; write `debian/copyright` in DEP-5 format listing defol and all vendored dependencies with their respective licenses (GPL-3.0-only, MIT, ISC, LGPL-3.0-only, Apache-2.0); set source format to 3.0 (quilt) in `debian/source/format`.

### 4. Create documentation and installation manifests

Write `debian/defol.1` man page from `README.md` and `--help` output documenting all CLI options (-h, -i, -m, -t, -d, -r, --delete-files, --delete-dirs, --dewit) and environment variables (DEFOL_DEBUG, DEFOL_VERBOSE); create `debian/install` to place `bin/defol` in /usr/bin; add `debian/docs` for `README.md`; create `debian/watch` for GitHub releases using version=4; write `debian/README.source` explaining vendoring approach and patch management.

### 5. Test and validate package

Build with `dpkg-buildpackage -us -uc`; run `lintian -i -I --pedantic *.changes` to check policy compliance including Ada-specific rules from `debian/Debian policy for Ada.html`; verify installation with `dpkg -i`, test functionality (file deduplication), check uninstall; ensure reproducible builds with varying `SOURCE_DATE_EPOCH`.

### 6. Prepare upstream release and ITP

Coordinate with upstream to bump version from "1.0-dev" to "1.0.0" in `alire.toml`; request adding LICENSE file to source tree; prepare orig.tar.gz using `debian/prepare.sh` or manually; file ITP (Intent To Package) bug with wnpp@debian.org; set up git-buildpackage repository structure; find Debian sponsor for upload.

## Notes

- Using pre-generated c_strings configuration (committed once) for build reproducibility
- Using 3.0 (quilt) source format as the standard, maintainable approach for packaging external software
- Ada policy requires both `gnat` and specific version dependency, with explicit conflict for known-broken gnat 15.1.2
