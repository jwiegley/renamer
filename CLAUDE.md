# CLAUDE.md - AI Assistant Guide for Renamer Project

> This file provides specific guidance for working with this codebase. For general development practices, see standard Haskell documentation.

## Build and Development Commands

### Building the Project
```bash
# Using Cabal
cabal build                  # Build the project
cabal build --ghc-options=-O2  # Build with optimizations

# Using Nix
nix build                    # Build with Nix flake
nix develop                  # Enter development shell with all dependencies

# Generate cabal file from package.yaml (if modified)
hpack
```

### Running Tests
```bash
# Run all tests
cabal test

# Run tests with detailed output
cabal test --test-show-details=direct

# Run specific test suite
cabal test renamer-tests

# Run tests with coverage
cabal test --enable-coverage

# Run a single test (filter by pattern)
cabal test --test-options="-p 'testSimpleRename'"
```

### Running the Application
```bash
# Using cabal
cabal run renamer -- rename /photos
cabal run renamer -- import --from /media/camera --to /photos /photos

# After building
./_build/install/default/bin/renamer rename /photos

# With profiling (requires rebuild with profiling)
cabal build --enable-profiling
cabal run renamer -- +RTS -p -hc -RTS rename /photos
```

### Development Tools
```bash
# Launch REPL with project loaded
cabal repl

# Load specific module in REPL
:l Renamer

# Type checking and linting
hlint src/                   # Linting suggestions
ghcid                       # Auto-recompile on file changes

# Generate documentation
cabal haddock
```

## High-Level Architecture

**Renamer** renames photo files to `YYMMDD_NNNN.ext` format based on EXIF timestamps, grouping related files (RAW + JPEG + sidecar) together while maintaining idempotency.

### Cross-Module Architecture Pattern

The codebase implements a **dual-monad architecture** that spans three key modules:

1. **Renamer.hs** - Defines abstract type classes (`MonadPhoto`, `MonadFSRead`, etc.) and implements business logic against these abstractions
2. **Fixtures.hs** - Provides pure `Simulation` monad instances for all type classes, enabling deterministic testing
3. **Main.hs** - Provides `IO` monad instances and wires everything together for production

This separation means the same business logic runs in both production (`IO`) and tests (`Simulation`) without modification:
```haskell
-- In Renamer.hs (abstract)
class MonadPhoto m where
  photoCaptureDate :: FilePath -> m (Maybe UTCTime)

-- In production (Main.hs uses this)
instance MonadPhoto IO where
  photoCaptureDate = -- calls exiftool process

-- In tests (Fixtures.hs provides this)
instance MonadPhoto Simulation where
  photoCaptureDate = -- returns mocked data
```

### Algorithm Pipeline (7 Steps)
The renaming algorithm flows through distinct phases:
```
1. Analyze    → Gather FileDetails with EXIF (parallel IO)
2. Group      → Group by time/name (PhotoGroup creation)
3. Name       → Compute renamings (state-based counters)
4. Clean      → Remove conflicts (overlap resolution)
5. Plan       → Handle circular renames (temp file insertion)
6. Execute    → Perform renames (with rollback on error)
7. Prune      → Remove empty directories (import only)
```

## Critical Understanding

### 1. Dual Monad System
```haskell
-- Production: Uses IO
runAppT :: Options -> RenamerState -> AppT IO a -> IO (a, RenamerState)

-- Testing: Uses Simulation
runSimulation :: Simulation a -> (a, Env)
```
- **Never** assume you're in IO - use the `Monad*` type classes
- All file operations must go through `MonadFSRead`/`MonadFSWrite`
- EXIF extraction through `MonadPhoto`

### 2. Lens-Based Data Access
```haskell
-- DON'T: Direct field access
let time = _captureTime details

-- DO: Use lens operators
let time = details ^. captureTime

-- State modification
nameCounter . ix prefix += 1
```
All data types use classy lenses (`makeClassy`) for extensibility.

### 3. Photo Grouping Logic
```haskell
data Reason = ForTime UTCTime | ForBase FilePath
```
- **ForTime**: Files with same capture timestamp (authoritative)
- **ForBase**: Files with same base name but no timestamp (attached to nearest time group)
- Groups MUST have unique normalized extensions or grouping fails

### 4. Circular Rename Handling
Problem: `A → B` then `B → C` fails if B exists
Solution: Insert temporary files
```haskell
A → tmp_PID_0
B → C
tmp_PID_0 → B
```
Uses process PID + unique counter for temp names.

### 5. Idempotency Invariant
**Critical**: Running the renamer twice on the same files must be a no-op.
Tests verify this by running rename operations twice:
```haskell
(scenario1, paths1) <- renamer repos inputs destDir
(scenario2, paths2) <- renamer (reverse paths1) [] Nothing
-- paths2 should equal paths1
```

## Code Navigation

### Core Renaming Logic
- **File grouping**: `Renamer.hs:795-839` (`groupPhotos`)
- **Renaming computation**: `Renamer.hs:841-920` (`computeRenamings`)
- **Overlap resolution**: `Renamer.hs:922-966` (`removeOverlappedRenamings`)
- **Plan building**: `Renamer.hs:1023-1057` (`buildPlan`)
- **Execution**: `Renamer.hs:1134-1200` (`executePlan`)

### File Operations
- **EXIF extraction**: `Renamer.hs:433-465` (`photoCaptureDate`)
- **File details gathering**: `Renamer.hs:552-582` (`gatherDetails`)
- **Safe file moving**: `Renamer.hs:1134-1147` (`safeMoveFile`)

### Testing Infrastructure
- **Simulation monad**: `Fixtures.hs:71-92`
- **Virtual file system**: `Fixtures.hs:46-69`
- **Test DSL**: `Fixtures.hs:227-286`

### CLI Interface
- **Option parsing**: `Main.hs:24-77`
- **Command dispatch**: `Main.hs:79-140`
- **Scenario handling**: `Main.hs:142-214`

## Making Modifications

### Adding a New File Format

1. **Update format detection** (`Renamer.hs:~520`):
```haskell
isImage :: String -> Bool
isImage ext = case strToLower ext of
  -- ... existing formats ...
  ".raf"  -> True  -- ADD: Fuji RAW
  ".orf"  -> True  -- ADD: Olympus RAW
  _       -> False
```

2. **Handle extension normalization** if needed (`Renamer.hs:~530`):
```haskell
normalizeExt :: String -> String
normalizeExt ext = case strToLower ext of
  ".jpeg" -> ".jpg"
  ".tiff" -> ".tif"
  ".rawf" -> ".raf"  -- ADD: Normalize variant
  ext'    -> ext'
```

3. **Test with simulation**:
```haskell
-- tests/Main.hs
testFujiRAW = testCase "fuji raw" $ do
  (_, paths) <- renamer ["test"] [] Nothing $ do
    photo "test/DSCF0001.RAF" "2024-01-01T12:00:00Z"
  paths @?== ["test/240101_0001.raf"]
```

### Modifying the Naming Convention

1. **Update the regex pattern** (`Renamer.hs:~160`):
```haskell
-- Current: YYMMDD_NNNN
nameRe = "^([0-9]{6})_([0-9]{4})$"

-- Example: YYYY-MM-DD_NNNN
nameRe = "^([0-9]{4}-[0-9]{2}-[0-9]{2})_([0-9]{4})$"
```

2. **Update date formatting** (`Renamer.hs:~880`):
```haskell
-- Current
formatTime defaultTimeLocale "%0y%0m%0d" localTime

-- New format
formatTime defaultTimeLocale "%Y-%m-%d" localTime
```

3. **Update reservation parsing** (`Renamer.hs:~710`):
```haskell
maybeWithCounter path f = case takeBaseName path =~ nameRe of
  [(_ : prefix : counter : [])] ->
    -- Update date parsing format
    forM_ (parseTimeM False defaultTimeLocale "%Y-%m-%d" prefix) $
      \_ -> f prefix (read counter)
```

4. **Run full test suite** to verify idempotency:
```bash
cabal test
```

### Adding a CLI Option

1. **Add to Options type** (`Renamer.hs:~85`):
```haskell
data Options = Options
  { -- ... existing fields ...
  , _dryRun :: !Bool  -- ADD: New option
  }
```

2. **Generate lens** (automatic via `makeClassy`)

3. **Add parser** (`Main.hs:~40`):
```haskell
renamerOptions = Options
  <$> -- ... existing parsers ...
  <*> switch (long "dry-run" <> help "Preview changes without executing")
```

4. **Use in logic**:
```haskell
executePlan tz ms = do
  isDryRun <- view dryRun
  if isDryRun
    then logInfo "DRY RUN: Would rename files..." >> pure 0
    else -- existing execution logic
```

### Performance Optimization

1. **For large repositories** (>10,000 files):
```haskell
-- Replace list with Vector
import qualified Data.Vector as V

gatherDetails :: [FilePath] -> m (V.Vector FileDetails)
```

2. **Increase parallelism**:
```bash
renamer rename -j 8  # Use 8 concurrent workers
```

3. **Enable state caching**:
```bash
renamer rename --keep-state  # Cache file details in .file-details.json
```

## Testing and Debugging

### Using the Simulation Monad

Create test scenarios with the DSL:
```haskell
testScenario = do
  -- Create virtual files
  photo "dir/IMG_001.CR3" "2024-01-01T12:00:00Z"  -- With timestamp
  file "dir/IMG_001.xmp"                          -- Without timestamp

  -- Run renamer
  (scenario, paths) <- renamer ["dir"] [] Nothing

  -- Assert results
  paths @?== ["dir/240101_0001.cr3", "dir/240101_0001.xmp"]
```

### Idempotency Testing

Always verify operations are idempotent:
```haskell
renamer repos inputs destDir $ do
  setup
  ((s1, e1), p1) <- renamerNoIdemCheck repos inputs destDir
  ((s2, e2), p2) <- renamerNoIdemCheck (reverse p1) [] Nothing
  -- p2 should equal p1, e2 should be 0
```

### Scenario Debugging

Save complex scenarios for analysis:
```bash
# Save scenario
renamer rename -d --write-scenario debug.json /photos

# Replay scenario
renamer rename --read-scenario debug.json
```

Examine scenario structure:
```haskell
data Scenario = Scenario
  { _scenarioInputs    :: [FilePath]        -- Input files
  , _scenarioDetails   :: [FileDetails]     -- Gathered metadata
  , _scenarioGroups    :: [PhotoGroup]      -- Grouping results
  , _scenarioMappings  :: [Mapping]         -- Computed renames
  , _scenarioCleanMappings :: [Mapping]     -- After cleaning
  , _scenarioPlan      :: [SimpleMapping]   -- Execution plan
  }
```

### Debug Output Levels

```bash
# Quiet (errors only)
renamer rename -q /photos

# Verbose (info + warnings)
renamer rename -v /photos

# Debug (detailed algorithm steps)
renamer rename -d /photos

# Extra debug (includes assertions)
renamer rename -D /photos
```

### Adding Test Cases

1. **Unit test** in `tests/Main.hs`:
```haskell
testNewFeature :: TestTree
testNewFeature = testCase "new feature" $ do
  (_, paths) <- renamer ["test"] [] Nothing $ do
    -- Setup virtual files
    photo "test/example.jpg" "2024-01-01T00:00:00Z"
  -- Assert expected behavior
  paths @?== ["test/240101_0001.jpg"]
```

2. **Property test** (when adding QuickCheck):
```haskell
prop_uniqueDestinations :: [FileDetails] -> Property
prop_uniqueDestinations files =
  let mappings = computeRenamings Nothing (groupPhotos utc Nothing files)
      dests = map (^. renamingTo) mappings
  in dests === nub dests  -- All destinations unique
```

## Common Pitfalls and Gotchas

### 1. Naming Convention Changes
**⚠️ NEVER** change the naming format without updating:
- `nameRe` regex pattern
- Date formatting in `computeRenamings`
- Date parsing in `maybeWithCounter`
- All tests expecting specific formats

### 2. Error Accumulation Pattern
Errors don't abort execution - they accumulate:
```haskell
logErr msg = do
  putStrLn_ Error $ "ERROR: " ++ msg
  errorCount += 1  -- Continues execution!
```
Check `errorCount` after operations to detect failures.

### 3. Photo Group Invariants
Groups MUST have:
- At least one file with capture time (`ForTime`)
- Unique normalized extensions
- Files from the same directory

Violating these breaks grouping logic.

### 4. Case-Insensitive Filesystem Handling
The code handles case-insensitive filesystems - always use `strToLower` for path comparisons when checking for duplicates or collisions.

## Key Improvement Opportunities

### Performance: String → Text Migration
The codebase uses `String` throughout. Migrating to `Text` would provide 2-3x performance improvement for large file lists. Key areas: `strToLower`, file path operations, regex matching.

### Robustness: Property-Based Testing
Add QuickCheck properties to verify:
- Idempotency (running twice produces same result)
- No duplicate destinations
- Photo group invariants hold

### Scalability: Streaming for Large Repositories
For >100,000 files, implement streaming with conduit/pipes to maintain constant memory usage instead of loading all files into memory.

## Quick Reference

### Core Pipeline Functions
```haskell
gatherDetails    :: [FilePath] -> m [FileDetails]           -- Extract metadata
groupPhotos      :: ... -> [Either FileDetails PhotoGroup]  -- Group related files
computeRenamings :: ... -> m [Mapping]                      -- Compute new names
cleanRenamings   :: TimeZone -> [Mapping] -> m [Mapping]    -- Remove conflicts
buildPlan        :: [Mapping] -> m [SimpleMapping]          -- Handle circular renames
executePlan      :: TimeZone -> [SimpleMapping] -> m Integer -- Execute
```

### Key Type Classes (MTL Pattern)
- `MonadPhoto` - EXIF extraction via exiftool
- `MonadFSRead`/`MonadFSWrite` - File operations
- `MonadParallel` - Concurrent processing
- `MonadLog` - Logging with levels
- All have both `IO` and `Simulation` instances for testing