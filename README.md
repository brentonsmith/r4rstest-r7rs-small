# r4rstest-r7rs-small

A portable test suite for Scheme implementations targeting the Revised^7 Report on the Algorithmic Language Scheme, Small Edition (R7RS-small).

This project adapts Aubrey Jaffer's IEEE/R4RS conformance suite, **r4rstest.scm**, and adds or revises tests to reflect R7RS-small. Passing the suite is useful conformance evidence, but is not by itself a complete certification.

## Status

This repository is a porting scaffold. It contains the unchanged upstream source, its provenance and license, and the coding-agent brief below. The R7RS-small port and its runners have not been implemented, so there are no port test results yet.

## Source provenance

The baseline is **upstream/r4rstest.scm**, copied unchanged from the SCM 5f6 source distribution. It is 40,577 bytes; SHA-256:

```text
c033d57b0942657877ede72ee5e4911d84557c2a2d82cba493b6a6374b317668
```

SCM lists version 5f6 as its current release. The 5f6 archive listing records r4rstest.scm dated 2023-10-15. The source header identifies Aubrey Jaffer as author and carries a GNU GPL version 3 or later notice.

- SCM release information: https://people.csail.mit.edu/jaffer/SCM.html
- SCM 5f6 archive: https://groups.csail.mit.edu/mac/ftpdir/scm/scm-5f6.zip
- SCM 5f6 archive file listing: https://fossies.org/linux/privat/scm-5f6.zip/index_o.html
- Maintained source pointer: http://swiss.csail.mit.edu/ftpdir/scm/r4rstest.scm
- Source copy: upstream/r4rstest.scm

The historical CMU page describes its older 1992 test.scm release as copyright 1989-93 Aubrey Jaffer under GPL v1. That older file is linked for comparison, but is not copied here; this repository uses the maintained SCM source and its GPL-3.0-or-later notice.

- CMU package description: https://www.cs.cmu.edu/afs/cs/project/ai-repository/ai/lang/scheme/doc/standard/tests/0.html
- Historical CMU test file: https://www.cs.cmu.edu/afs/cs/project/ai-repository/ai/lang/scheme/doc/standard/tests/test.scm

## License

This project uses GPL-3.0-or-later. The upstream source's copyright and author notices must be preserved in derivative test files. See LICENSE for the complete GPL version 3 text.

## Local WSL host matrix

The initial targets are Chibi Scheme 0.12.0, Gauche 0.9.15, Cyclone 0.36.0, and Sagittarius 0.9.15. These versions were verified in Ubuntu 24.04.4 LTS under WSL2, x86-64, on 2026-10-03. Availability does not mean this suite has passed on them.

The host environment is configured by C:/GitHub/shen-r7rs/scripts/scheme-env.sh. From Windows PowerShell, confirm the installed executables and versions with:

```powershell
wsl.exe -d Ubuntu -- bash -lc 'source /mnt/c/GitHub/shen-r7rs/scripts/scheme-env.sh && cd /mnt/c/GitHub/r4rstest-r7rs-small && command -v chibi-scheme gosh cyclone sagittarius && chibi-scheme -V && gosh -V && cyclone -v && sagittarius --version'
```

Verified invocation modes, documented in C:/GitHub/shen-r7rs/docs/portability.md:

- Chibi: chibi-scheme -I <repo>/lib <source>
- Gauche: gosh -q -r7 -I <repo>/lib <source>
- Cyclone: compile with cyclone -I <repo>/lib <source>, then execute the generated program. Stage the build in a temporary directory.
- Sagittarius: sagittarius -r7 -L <repo>/lib <source>

These are host invocation patterns. Establish this repository's own repeatable per-host and all-host runner commands. The Shen repo's scripts/test-all runs Shen tests and is not this repo's test runner. If Windows sandboxing blocks a WSL test command, use supported scoped execution approval for that command.

## Porting-agent prompt

Copy the following brief into a coding agent:

### Goal

Create a faithful, portable R7RS-small conformance test suite based on the maintained upstream/r4rstest.scm. Keep the upstream baseline unchanged and put translated code in separate project files. Run the same required test sources on Chibi, Gauche, Cyclone, and Sagittarius.

Some tests may expose actual implementation defects. If an assertion fails on most or all hosts, first investigate the translation, expectation, harness, and R7RS-small interpretation. A common failure calls for investigation; it does not prove that all hosts or the standard are wrong. Do not silently skip or weaken required tests to make the matrix pass.

### Source and license

Use these sources for provenance and comparison:

- Historical CMU description: https://www.cs.cmu.edu/afs/cs/project/ai-repository/ai/lang/scheme/doc/standard/tests/0.html
- Historical CMU file: https://www.cs.cmu.edu/afs/cs/project/ai-repository/ai/lang/scheme/doc/standard/tests/test.scm
- SCM release/source page: https://people.csail.mit.edu/jaffer/SCM.html
- SCM 5f6 archive: https://groups.csail.mit.edu/mac/ftpdir/scm/scm-5f6.zip
- SCM 5f6 archive file listing: https://fossies.org/linux/privat/scm-5f6.zip/index_o.html
- Current source pointer: http://swiss.csail.mit.edu/ftpdir/scm/r4rstest.scm

Preserve the upstream author and copyright notices. The maintained source carries GNU GPL version 3 or later. Keep the repository and derived suite under GPL-3.0-or-later; do not relicense copied material under a permissive license.

### R7RS-small contract and local references

Before coding, read these files from the existing checkout:

- C:/GitHub/shen-r7rs/.agents/skills/r7rs-small/SKILL.md
- C:/GitHub/shen-r7rs/.agents/skills/r7rs-small/references/spec-lookup.md
- C:/GitHub/shen-r7rs/.agents/skills/r7rs-small/references/portability.md
- C:/GitHub/shen-r7rs/docs/r7rs-small/README.md
- C:/GitHub/shen-r7rs/docs/r7rs-small/PROVENANCE.md
- C:/GitHub/shen-r7rs/docs/r7rs-small/source/site/
- C:/GitHub/shen-r7rs/docs/wsl-setup.md
- C:/GitHub/shen-r7rs/docs/portability.md
- C:/GitHub/shen-r7rs/scripts/scheme-env.sh
- C:/GitHub/shen-r7rs/scripts/run-chibi
- C:/GitHub/shen-r7rs/scripts/run-gauche
- C:/GitHub/shen-r7rs/scripts/run-cyclone
- C:/GitHub/shen-r7rs/scripts/run-sagittarius

The skill and surrounding repository contain Shen-specific directions. Apply portable Scheme, report lookup, and test portability guidance; do not apply Shen or KLambda semantics here. R7RS-small is the normative contract. Consult the relevant report sections for each assertion and briefly explain changes to inherited expectations.

Classify behavior carefully:

- Put required R7RS-small behavior in the required suite.
- Report optional standard-library tests separately because implementations may omit those libraries.
- Do not assign one universal expected result to unspecified or implementation-defined behavior.
- Do not treat a result shared by these hosts as a normative guarantee.

Preserve useful test intent while updating R4RS assumptions that do not apply to R7RS-small. Add coverage for relevant R7RS-small syntax and procedures. Keep implementation-specific extensions out of required conformance results.

### WSL hosts and run modes

Verified environment: Ubuntu 24.04.4 LTS under WSL2, x86-64. Pinned host versions:

- Chibi Scheme 0.12.0
- Gauche 0.9.15
- Cyclone 0.36.0
- Sagittarius 0.9.15

From Windows PowerShell, initialize the host environment and check executable discovery:

```powershell
wsl.exe -d Ubuntu -- bash -lc 'source /mnt/c/GitHub/shen-r7rs/scripts/scheme-env.sh && cd /mnt/c/GitHub/r4rstest-r7rs-small && command -v chibi-scheme gosh cyclone sagittarius && chibi-scheme -V && gosh -V && cyclone -v && sagittarius --version'
```

Verified launch patterns:

- Chibi: chibi-scheme -I <repo>/lib <source>
- Gauche: gosh -q -r7 -I <repo>/lib <source>
- Cyclone: cyclone -I <repo>/lib <source>, then run the generated executable. Use a temporary build directory.
- Sagittarius: sagittarius -r7 -L <repo>/lib <source>

Create this project's own repeatable host runners and matrix command. Do not run the Shen repo's scripts/test-all; it tests Shen. If sandboxing blocks WSL execution, request scoped approval for the concrete command.

### Acceptance criteria

1. Keep the upstream baseline unchanged and retain its checksum.
2. Ground every required assertion in R7RS-small and test only behavior the report requires.
3. Separate optional-library tests from required conformance results.
4. Report total, passed, and failed assertions; exit nonzero on failure.
5. Run the same required test sources on all four pinned hosts, including Cyclone's compile-then-execute path.
6. Investigate common failures against the report and harness before attributing them to implementations.
7. Document intentional exclusions, optional tests, and unresolved failures with reasons.
8. Keep README, source, and license documentation accurate. Do not claim complete R7RS-small certification from this suite alone.
9. Report exact commands, host versions, assertion counts, results, and checks not run. Do not push or publish changes beyond the private repository requested by the owner.
