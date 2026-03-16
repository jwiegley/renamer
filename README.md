# Renamer

I've been managing a large photo library for years, and one thing that's
always bugged me is how cameras name their files. `IMG_4523.CR3` tells
you nothing about when the photo was taken, and when you've got files
from multiple cameras mixed together, it's chaos. Renamer fixes this --
it renames photo files to a consistent `YYMMDD_NNNN.ext` format based on
their EXIF capture timestamps.

The nice thing is how it handles related files. If you shoot RAW+JPEG, or
have XMP sidecar files alongside your photos, renamer groups them together
and gives them the same base name. So `IMG_4523.CR3`, `IMG_4523.JPG`, and
`IMG_4523.xmp` all become `240115_0042.cr3`, `240115_0042.jpg`, and
`240115_0042.xmp`.

It's idempotent too -- running it twice on the same directory is a no-op
the second time around.

## Usage

```bash
# Preview what would be renamed (default, no --execute flag)
renamer rename /photos

# Actually perform the renames
renamer rename --execute /photos

# Import photos from a camera into your library
renamer import --from /media/camera --to /photos /photos

# Recursive directory processing
renamer rename -r --execute /photos

# Verbose or debug output
renamer rename -v /photos
renamer rename -d /photos
```

## Building

```bash
# With Nix (recommended)
nix build

# Enter development shell with all tools
nix develop

# With Cabal (inside nix develop)
cabal build
cabal test
```

## How It Works

The renaming algorithm is a seven-step pipeline:

1. **Analyze** -- Gather EXIF metadata from all files, in parallel
2. **Group** -- Cluster related files by capture time and base name
3. **Name** -- Compute new filenames using date-based counters
4. **Clean** -- Resolve conflicts where multiple files map to the same destination
5. **Plan** -- Handle circular renames by inserting temporary files
6. **Execute** -- Perform the renames with copy-then-delete for safety
7. **Prune** -- Remove empty directories (import mode only)

The business logic is written against abstract type classes (`MonadPhoto`,
`MonadFSRead`, `MonadFSWrite`, etc.), with `IO` instances for production
and a pure `Simulation` monad for testing. This means the test suite
exercises the real algorithm without touching the filesystem.

## Development

After entering the development shell with `nix develop`:

```bash
# Install pre-commit hooks
lefthook install

# Format all Haskell source files
fourmolu --mode inplace $(find src bin tests -name '*.hs')

# Or via nix
nix run .#format

# Lint
hlint src/ bin/ tests/

# Run tests
cabal test

# Run tests with coverage
cabal test --enable-coverage

# Generate profiling data
cabal run renamer -- +RTS -p -hc -RTS rename /some/test/dir

# Run all checks at once
nix flake check
```

## License

BSD-3-Clause. See [LICENSE.md](LICENSE.md) for details.
