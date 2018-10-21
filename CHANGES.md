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
 2. The hex code "mini language" understood by `lb` (hex characters, macros,
    comments).

[semver]: https://semver.org/

# Release 1.1.0 (2018-10-21)

It is now possible to create really large files. For example, hex code like
`(41){5368709000}`, resulting in 5 GB of data, can be compiled regardless of the
available RAM. (It may take some time though.) Prior to this release, this would
cause memory errors.

With the new CLI option `--plain` (or `-p`), hex code without surrounding
Markdown is accepted as input, allowing quick tests like this:

    $ echo '(00ff){42} 0a' | lb --plain

The new CLI option `--version` prints the current version number.

Error messages have been improved.

# Release 1.0.0 (2018-09-05)

Initial release.
