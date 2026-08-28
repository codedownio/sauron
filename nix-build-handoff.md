# Sauron nix build — handoff

Context for continuing the `nix build .#grandCombinedGithubArtifacts` work on
another machine. Written 2026-08-28.

All work below is committed as `c2ad2ac` ("noci") on branch
`add-stack-to-dev-shell`, pushed to `codedownio`. The commit before it,
`fbec463`, is the last state without any of these fixes.

Note for anything added later: flakes can't see untracked files, so new files
must be at least `git add`ed before they take effect.

## Goal

Get `nix build .#grandCombinedGithubArtifacts` working. That output is a
`symlinkJoin` over five per-system artifacts:

- `x86_64-linux.githubArtifacts` → `static` (musl cross)
- `aarch64-linux.githubArtifacts` → `dynamic`
- `x86_64-darwin.githubArtifacts` → `darwin-static`
- `aarch64-darwin.githubArtifacts` → `darwin-static`
- `x86_64-linux.windowsGithubArtifacts` → `windows` (mingwW64 cross)

## How this started

`cdb8aa0` ("Add stack to dev shell") bumped the nixpkgs pin *and* changed
`compilerNixVersion` from `9122` to `9124`, but left `stack.yaml` on
`resolver: nightly-2025-09-22`. That snapshot targets **ghc-9.12.2**. The
compiler and the snapshot must agree — otherwise haskell.nix tries to rebuild
the compiler's own boot libraries and every configure fails with
`Encountered missing or private dependencies`.

Verified: `.#dynamic` builds fine at `31112bf` (the commit before the bump).

## Current state of the changes

### `stack.yaml`
`resolver: nightly-2025-09-22` → `nightly-2026-08-07`.

**GHC 9.14 is not reachable via Stackage.** Every nightly checked
(2026-06-15, 07-15, 08-07, 08-14, 08-20, 08-26) pins `compiler: ghc-9.12.4`.
`~/codedown` is not on a 9.14 snapshot either — it forces `ghc9141` onto the
same 9.12.4 snapshot, which is what its `ghc914-bound-optimistic-backport`
patch pays for. Trying 9.14.1 here failed exactly as you'd expect: the
snapshot assumes containers-0.7 (a 9.12 boot lib) while GHC 9.14.1 ships 0.8.

So this is aligned on **9.12.4**, matching `compilerNixVersion = "9124"`.

### `flake.nix`
- `inputs.nixpkgs.follows` → `haskellNix/nixpkgs-2605` (was `nixpkgs-unstable`).
  26.05 is the last release supporting x86_64-darwin, and still has
  `haskellPackages.stack` 3.9.3, `ghc9124` and HLS 2.13.0.0. Staying on
  unstable would mean dropping x86_64-darwin from the release matrix, since
  nixpkgs 26.11 removed the Darwin stdenv for it entirely
  (`config.allowDeprecatedx86_64Darwin = "force"` does not help).
- Applies two patches to haskell.nix itself via `applyPatches`, and one to
  `hackage-for-stackage` (see below).
- `flakeWindows` now includes `fix-ghc-pkgs-module.nix`; it was the only one of
  the four projects missing it.
- `hlsPackages` builds HLS with `doHaddock = false` and
  `enableLibraryProfiling = false` (see "dev shell" below).

### `nix/haskell-nix-patches/ghc-allow-multiple-definition.patch`
Building GHC from source dies linking stage0's haddock:

```
ld.gold: error: .../libHSrts-1.0.2_thr.a(Globals.thr_o):
  multiple definition of 'ghc_unique_counter64'
ld.gold: .../libHSghc-9.12.4-inplace.a(genSym.o): previous definition here
```

Adds `--allow-multiple-definition` to `NIX_LDFLAGS` in
`compiler/ghc/default.nix`. haskell.nix's `useLdGold = false` does **not** help
— the link is done by the *bootstrap* GHC 9.8.4, whose own settings hardcode
ld.gold.

**This has to be a patch, not an overlay.** I first did it as an overlay using
`overrideAttrs`; `overlays/haskell.nix:1305` reaches the compiler through
`.override { ghcEvalPackages = ...; }`, which regenerates the attrset and
discards anything attached with `overrideAttrs` or `//` — including
`cachedDeps`. Losing `cachedDeps` is silent and nasty: `make-config-files.nix`
falls back to `makeCompilerDeps ghc`, which returns the *ghc attrset* rather
than the deps derivation, so `ghcDeps` ends up pointing at the GHC, no
`envDeps/` exist, the ghc-environment is never written, and every package whose
dependencies are all boot libs (microlens, safe-exceptions) fails with
`missing or private dependencies: base`.

### `nix/haskell-nix-patches/windows-crypton-x509-system-patch-path.patch`
Genuine haskell.nix typo. `overlays/windows.nix:95` references
`./patches/crypton-x509-system` but the file is `crypton-x509-system.patch`.
Only bites for crypton-x509-system >= 1.7, which the newer snapshot has.

### `nix/haskell-nix-patches/fix-hgg-3d-flag.patch`
Copied verbatim from `~/codedown/nix/haskell-nix-patches/`. hackage.nix emits a
Cabal flag literally named `3d` unquoted, which is a Nix syntax error. Applied
to the `hackage-for-stackage` input.

### `nix/fix-ghc-pkgs-module.nix`
Was entirely commented out. Now sets `reinstallableLibGhc = false` plus an
explicit `nonReinstallablePkgs`.

Two traps here, both of which I hit:

1. haskell.nix's default list only includes `Cabal`/`containers`/`process`/etc.
   when `reinstallableLibGhc` is false, and it defaults to **true** on Linux.
   Without the flag, haskell.nix rebuilds `process` even though GHC 9.12.4
   ships exactly the version the snapshot wants, and its Setup.hs can't see
   `base`.
2. `nonReinstallablePkgs` **replaces** haskell.nix's version-computed list (the
   option's merge takes the last definition). The old hardcoded list in this
   file was written for an older GHC and omits packages GHC 9.12 ships, which
   leaves the db inconsistent:
   `installed package directory-1.3.10.1 is broken due to missing package file-io-0.1.6-inplace`.

The list is now the union of haskell.nix's computed list and everything
`ghc-pkg list --simple-output` reports for the compiler. It is version
specific — **regenerate it when `compilerNixVersion` changes.**

## Where the build got to

Last run (`/tmp/gc5.log` on the old machine) was at **zero errors**, past all
the failure classes above, building the cross toolchains. It was stopped
because it was bogging the machine down, not because it failed. It had not yet
reached the point of producing any of the five artifacts, so **nothing is
verified end to end**.

Independently verified working on the old machine:
- `nix develop` (ghc 9.12.4, stack 3.9.3, vhs 0.11.0, HLS 2.13.0.0)
- `nix flake show`, `nix flake show --all-systems` (all four systems)
- `nix flake show --option allow-import-from-derivation true`

These were verified *before* the stack.yaml resolver bump, so re-check
`nix develop` on the new machine.

## What to do next

```
nix build .#grandCombinedGithubArtifacts --keep-going
```

Expect a very long first run: no haskell.nix GHC is cached (see below), so it
builds the native, musl, mingw and Darwin compilers from source.

Faster incremental checks, cheapest first:

```
nix build .#dynamic     # native; quickest signal that the plan resolves
nix build .#static      # musl cross
nix build .#windows     # mingw cross
```

## Machine requirements

- **Darwin builder.** `x86_64-darwin` and `aarch64-darwin` artifacts need one.
  The old machine used `/etc/nix/machines`:
  `ssh-ng://tom@mac1.local aarch64-darwin,x86_64-darwin /home/tom/.ssh/id_ed25519 4 1 big-parallel - -`
- **aarch64-linux.** The old machine had `extra-platforms = aarch64-linux`
  (qemu/binfmt). Building GHC under emulation is brutally slow; a native
  aarch64 remote builder would be far better.
- **`cache.iog.io` is NOT configured** on the old machine, and none of these
  GHC paths are in it anyway (checked ghc9122/ghc9124 under both nixpkgs — all
  404, including via haskell.nix's own `legacyPackages`). Worth adding for
  general haskell.nix work, but it will not rescue this build.

## Loose ends / open questions

- `nix flake check` fails on `inherit version;` in `packages` — `version` is a
  string, not a derivation. Pre-existing, unrelated to any of this, confirmed by
  stashing all changes.
- `nix/fix-ghc-pkgs-overlay.nix` defines `hixProject`, which nothing calls. Dead
  code; the overlay is still in the list and harmless.
- `haddock -j$NIX_BUILD_CORES` deadlocks intermittently in the *nixpkgs* Haskell
  builder (seen on the HLS dependency tree: six haddock processes at 0% CPU for
  2.4h on trivial packages). `doHaddock = false` in `hlsPackages` sidesteps it
  for the dev shell. If a nixpkgs-side Haskell build ever stalls, `--cores 4`
  or lower is the escape hatch.
- README still documents an Intel Mac download. Fine as long as nixpkgs stays
  on 26.05; 26.05 is the last release supporting x86_64-darwin, so this needs
  revisiting eventually.
- The GHC 9.14 route remains open if you want it, but it means carrying
  codedown's patch set (`ghc914-bound-optimistic-backport`,
  `rename-mingw-pthreads`) rather than a Stackage-supported pairing.
