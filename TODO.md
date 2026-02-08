- [ ] TESTS
- [ ] Path exclusion patterns
- [ ] --max-depth for directory traversal
- [ ] --min-size, --max-size for files to consider
- [ ] Think what to do about soft links
- [ ] Use LML to report matches in any format
- [ ] When linking against defol as a library (e.g. in tests), it hangs as
  there's no main file that triggers it to start and there are tasks waiting.
  Maybe those should be created by some Init procedure.
- [ ] Dir overlaps are not propagated to parents, so dirs containing other
  identical dirs, will only report partial overlaps. This has somehow to be
  taken into account when tracking overlaps.
