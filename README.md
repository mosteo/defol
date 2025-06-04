[![CI](https://img.shields.io/github/actions/workflow/status/mosteo/defol/selftest.yml?label=CI)](https://github.com/mosteo/defol/actions/workflows/selftest.yml)

## Defol

Defol (deduplicate folders) is a command-line tool that helps you find
duplicate files and folders.

Currently it only detects duplicate files. The next big feature on the works is
finding folders with high similarity (not necessarily identical content).

It is heavily inspired by [rdfind](https://github.com/pauldreik/rdfind),
producing similar output, and treating the first folder given in the path as
the reference folder. This simplifies locating duplicates outside that tree.

Main characteristics:

- Fully parallelized: it uses all available CPU cores for file comparison
  (although in general processing is IO-bound).
- Lazy loading: it only reads the files to be compared when necessary.
- "Smart" comparison of files: it uses a combination of file size, content, and
  hash to determine if files are duplicates.

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
cannot be a starter in the primary tree and another starter in another tree.

Match sets are deterministic, with files sorted by their path.

Results are saved to a `defol_report.txt` file in the current working
directory.

At the time, there are no options to change the behavior of the tool. Debug
output can be enabled by defining the `DEFOL_DEBUG` environment variable, e.g.:
`DEFOL_DEBUG= ../bin/defol`

If no paths are given, the current working directory is used as the primary
(and only) tree.
