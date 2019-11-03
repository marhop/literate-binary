# Release 1.3.0

2019-11-03

  * Hex syntax extension: The new *string* macro allows arbitrary text content
    inside quotes like `"foo bar"` which produces a UTF-8 encoded byte sequence
    representing the quoted string content.
  * Hex syntax extension: In *repetition* macros, multiplicative suffixes may
    now be added to quantifiers like `(00){2M}`, producing 2 MB of NULL bytes.

# Release 1.2.1

2019-09-29

  * Performance improvements for range macros.

# Release 1.2.0

2019-06-24

  * The hex macro syntax has been extended to make random byte sequences
    possible: An *alternative* macro like `(00|ff|3333)` randomly produces one
    of `00`, `ff` or `3333`. A *range* macro like `(0c-0f)` randomly produces
    one of `0c`, `0d`, `0e` or `0f`. The special range macro `.` produces one
    random byte.

# Release 1.1.0

2018-10-21

  * With the new CLI option `--plain` (or `-p`), hex code without surrounding
    Markdown is accepted as input, allowing quick tests like this:

        $ echo '(00ff){42} 0a' | lb --plain

  * The new CLI option `--version` prints the current version number.

  * It is now possible to create really large files. For example, hex code like
    `(41){5368709000}`, resulting in 5 GB of data, can be compiled regardless of
    the available RAM. (It may take some time though.) Prior to this release,
    this would cause memory errors.

  * Error messages have been improved.

# Release 1.0.0

2018-09-05

  * Initial release.

# Versioning Policy

The `lb` tool uses [semantic versioning][semver] which is summarized as follows:

> Given a version number MAJOR.MINOR.PATCH, increment the:
>
>  1. MAJOR version when you make incompatible API changes,
>  2. MINOR version when you add functionality in a backwards-compatible manner,
>     and
>  3. PATCH version when you make backwards-compatible bug fixes.

Semantic versioning requires software to declare a public API. The public API of
`lb` is defined by two components:

 1. The command line interface syntax (switches, options, arguments).
 2. The hex string language understood by `lb` (hex characters, macros,
    comments).

[semver]: https://semver.org/
