[![CI](https://img.shields.io/github/actions/workflow/status/mosteo/defol/selftest.yml?label=CI)](https://github.com/mosteo/defol/actions/workflows/selftest.yml)

## Defol

Defol (deduplicate folders) is a command-line tool that helps you find
duplicate files and folders.

It is heavily inspired by [rdfind](https://github.com/pauldreik/rdfind),
producing similar output, and treating the first folder given in the path as
the reference folder. This simplifies locating duplicates outside that tree.

Main characteristics:

- **Intuitive defaults**: Just run `defol` and optionally pass folder paths to
  compare. By default, it excludes empty files and folders with little overlap
  from the report.
- **Fully parallelized**: it uses all available CPU cores for file comparison.
  - Informal tests show speed-ups of up to 3x compared to `rdfind` and `rmlint`
    (with I/O caching), similar speeds for a "cold" filesystem.
- **Lazy loading**: it only reads the files to be compared when necessary.
- **Smart comparison of files**: it uses a combination of file size, content, and
  hash to determine which files are duplicates.
- **Duplicate folder detection**: it can find folders with a high degree of overlap
  in their contents.
- **Safe deletion**: it can delete duplicate files and folders, with a dry-run mode
  to review what would be deleted before actually performing the deletion.
- **Configurable**: it has several options to customize its behavior, including
  exclusion of empty files, minimum folder overlap, and more (see `defol --help`).

## Reference tree

Although `defol` can be used to find duplicates within a single folder, it is
particularly useful when comparing multiple folder trees. The first folder given
in the command line is considered the *"primary"* or *"reference"* tree. Any
duplicate sets will involve one file from the primary tree and one or more files
from the other trees. In case of enabling deletions, the primary tree is always
preserved, and files in other trees are deleted as needed.

This use case is useful when you have a reference folder (e.g., a master copy of
your photos) and want to find duplicates in other folders. This also speeds up
the matching process, as candidates for matching are greatly reduced.

A folder given more than once will be ignored after the first occurrence. This
simplifies comparing a folder against its siblings:

```sh
# Compare reference_folder against its siblings
defol ./reference_folder ./*
```

If no paths are given, the current working directory is used as the primary (and
only) tree. In this case, duplicates can be found among any two files in this
tree.

## Duplicate File Detection

Duplicates are reported in a single match set, with one of the following
outcomes for each identical file in the set:

- **Starter_In_Primary_Tree**: A file in the primary tree (first path given)
- **Sibling_In_Primary_Tree**: Duplicate in same folder as starter in primary tree
- **Matched_In_Primary_Tree**: Duplicate elsewhere in the primary tree
- **Starter_In_Another_Tree**: A file with duplicates when no copy exists in primary tree
- **Sibling_In_Another_Tree**: Duplicate in same folder as starter outside primary tree
- **Matched_In_Another_Tree**: Duplicate elsewhere outside the primary tree

The primary tree is the first folder given in the command line, which
determines the classification of matches. A starter is the first file in the
match set, but it is not a special file, just the first one found in a set of
duplicates. However, it is guaranteed that if a duplicate exists in the primary
tree, the starter will be in the primary tree. As a consequence, in a match set
there cannot exist a starter in the primary tree and another starter in another
tree.

Match sets are deterministic, with files sorted by their match kind and path.

Results are saved to a `defol_report.txt` file in the current working
directory.

## Duplicate Folder Detection

By default, `defol` will also include folders with more than 50% overlap in its
report.

Note that a best effort is made not to double-count in corner cases (a single
folder containing more than one identical file, or nested duplicate folders),
but some oddities may arise in such cases. For folders with unshared root
(e.g., within different trees given in the command line), accounting should be
correct.

File names are considered part of the folder contents. This means that two
folders with identical file contents, but different file names, will have a
very high overlap, but not 100%.

In consequence, a 100% overlap means two folders contain exactly the same files
with the same names.

The possible outcomes in a match set for folders are:

- **Dir_In_Primary_Tree**: A folder in the primary tree (first path given)
- **Dir_In_Another_Tree**: A folder with some overlap outside the primary tree

## Usage

See `defol --help` for a full list of options and usage information. Note that
`defol` defaults will exclude empty files and folders with little overlap from
the report, which might produce misleading results if unaware. These behaviors
can be changed with command line options.

Debug output can be enabled by defining the `DEFOL_DEBUG` environment variable,
e.g.: `DEFOL_DEBUG= ../bin/defol`. Slightly less verbose output can be enabled
by defining the `DEFOL_VERBOSE` environment variable.

## Deletion of Duplicates

By default, `defol` only reports duplicates without deleting them. The report,
however, contains a column with value 'keep'/'dele' indicating which files
would be deleted by `--delete-files` and `--delete-dirs` options. When one
of those options is used, `defol` will delete all files/folders marked as
'dele' if the safety `--dewit` is used too. Without `--dewit`, `defol` will
just print a summary of what would be deleted.

**Always review the report before using deletion options.** Alternatively, it
is simple to manually delete files by relying on the report contents with a bit
of grepcutting, e.g.:

```sh
# Print all duplicate files outside of the primary tree
cat defol_report.txt | grep MATCHED_IN_ANOTHER | cut -d' ' -f5- | while read file; do ls -l "$file"; done
# Replace ls -l with rm -i to interactively delete files or -f to force deletion
```

## Disclaimer of Warranty

There is no warranty for the program, to the extent permitted by applicable
law. Except when otherwise stated in writing the copyright holders and/or other
parties provide the program "as is" without warranty of any kind, either
expressed or implied, including, but not limited to, the implied warranties of
merchantability and fitness for a particular purpose. The entire risk as to the
quality and performance of the program is with you. Should the program prove
defective, you assume the cost of all necessary servicing, repair or
correction.

## Limitation of Liability

In no event unless required by applicable law or agreed to in writing will any
copyright holder, or any other party who modifies and/or conveys the program as
permitted above, be liable to you for damages, including any general, special,
incidental or consequential damages arising out of the use or inability to use
the program (including but not limited to loss of data or data being rendered
inaccurate or losses sustained by you or third parties or a failure of the
program to operate with any other programs), even if such holder or other party
has been advised of the possibility of such damages.
