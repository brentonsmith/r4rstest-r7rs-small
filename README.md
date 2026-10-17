# r4rstest-r7rs-small

A portable test suite for Scheme implementations targeting the Revised^7 Report on the Algorithmic Language Scheme, Small Edition (R7RS-small).

This project adapts Aubrey Jaffer's IEEE/R4RS conformance suite, **r4rstest.scm**, and adds or revises tests to reflect R7RS-small. Passing the suite is useful conformance evidence, but is not by itself a complete certification.

## Running

Run the suite with any R7RS-small Scheme:

```sh
chibi-scheme r7rstest.scm
gosh -q -r7 r7rstest.scm
sagittarius -r7 r7rstest.scm
guile --r7rs --no-auto-compile r7rstest.scm
csi -s r7rstest.scm
```

The suite prints each assertion as it runs and ends with an
`r7rs-summary:` line reporting total, passed, failed, required-failed
and optional-failed counts. It exits nonzero only on required failures;
optional-library failures (tests needing `(scheme char)`, `(scheme lazy)`,
`(scheme file)`, `(scheme load)`, `(scheme inexact)` or `(scheme complex)`)
are reported but never gate the exit code.

## Harness notes

- Section 6.10.2 re-reads the suite's own source and compares the leading
  datum against a literal copy of the `import` form. Any edit to that form,
  even dropping an unused library, breaks the test on every host; update the
  literal in `r7rs-file-tests` in the same edit.
- The assertion total varies between hosts because a few assertions are
  conditional: R7RS-small 6.2.3 permits implementation restrictions on
  inexact arithmetic, so the `magnitude` and complex-division assertions
  only run where the intermediate results stay usable.

## Source provenance

The baseline is **r4rstest.scm**, copied unchanged from the SCM 5f6 source distribution. It is 40,577 bytes; SHA-256:

```text
c033d57b0942657877ede72ee5e4911d84557c2a2d82cba493b6a6374b317668
```

SCM lists version 5f6 as its current release. The 5f6 archive listing records r4rstest.scm dated 2023-10-15. The source header identifies Aubrey Jaffer as author and carries a GNU GPL version 3 or later notice.

- SCM release information: https://people.csail.mit.edu/jaffer/SCM.html
- SCM 5f6 archive: https://groups.csail.mit.edu/mac/ftpdir/scm/scm-5f6.zip
- SCM 5f6 archive file listing: https://fossies.org/linux/privat/scm-5f6.zip/index_o.html
- Maintained source pointer: http://swiss.csail.mit.edu/ftpdir/scm/r4rstest.scm

## License

This project uses GPL-3.0-or-later. The upstream source's copyright and author notices must be preserved in derivative test files. See LICENSE for the complete GPL version 3 text.
