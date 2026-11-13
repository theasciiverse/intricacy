# Changes from upstream

This is the Asciiverse packaging of [Intricacy](https://mbays.sdf.org/intricacy/)
by Martin Bays (GPL-3.0-or-later), built from upstream's `master` at
`212b125` (0.9.1.1, 6 September 2026; upstream tags releases on Hackage, not
in git). Upstream is the `upstream` remote
(https://thegonz.net/~intricacy/intricacy.git); the `asciiverse` branch
carries the changes below.

The catalog builds the curses edition only (`-f curses -f -sdl2 -f -sound
-f -embed`). Online play uses Martin's server at i.thegonz.net, unchanged.

## Ours

- **Data beside the binary.** The tutorial and initiation locks were found
  through cabal's data path only, so a binary unpacked anywhere else could
  not start a lock. `getDataPath` now looks beside the executable first and
  falls back to the cabal path (`Mundanities.hs`).
- **The player's files in the state folder.** Settings, solutions, cached
  locks and the player's own locks went to `~/.intricacy`. A new player's
  now go to `$XDG_STATE_HOME/intricacy`, else `~/.local/state/intricacy`
  (Linux), `~/Library/Application Support/intricacy` (macOS) or
  `%LocalAppData%\intricacy` (Windows). An existing `~/.intricacy` is still
  used, and `--datadir` and `INTRICACY_PATH` still override both
  (`Mundanities.hs`).

## Packaging

- `.github/workflows/asciiverse-release.yml`: builds the curses edition
  for Linux (x86-64 and ARM64) with GHC 9.8.2, smoke-tests it from another
  folder, and attaches archives and SHA256SUMS to the release.
- `asciiverse/smoke.py`: the smoke test.
- `asciiverse/intricacy.cast`: the catalog preview, the first six tutorial
  locks picked one after another.
