# haskell.nix leaves lib:ghc and the libraries shipped alongside it -- Cabal,
# containers, process, text, ... -- out of nonReinstallablePkgs by default on
# Linux, so it rebuilds whichever ones the Stackage snapshot bumps past the
# versions GHC ships. Those rebuilds fail, e.g.
#
#   Setup.hs:1:8: error: [GHC-87110]
#       Could not load module `Prelude'.
#       It is a member of the hidden package `base-4.21.2.0'.
#
# reinstallableLibGhc = false pulls most of them back in, but its list still
# misses several packages GHC 9.12 ships (file-io, Cabal-syntax, ghc-platform,
# ...), which leaves the package db inconsistent:
#
#   installed package directory-1.3.10.1 is broken due to missing package
#   file-io-0.1.6-inplace
#
# So spell out the full set: every library the compiler itself provides. This
# tracks the GHC version -- regenerate it from `ghc-pkg list --simple-output`
# when compilerNixVersion changes.
{
  reinstallableLibGhc = false;

  nonReinstallablePkgs = [
    "array"
    "base"
    "binary"
    "bytestring"
    "Cabal"
    "Cabal-syntax"
    "containers"
    "deepseq"
    "directory"
    "exceptions"
    "file-io"
    "filepath"
    "ghc"
    "ghc-bignum"
    "ghc-boot"
    "ghc-boot-th"
    "ghc-compact"
    "ghc-experimental"
    "ghc-heap"
    "ghci"
    "ghc-internal"
    "ghcjs-prim"
    "ghcjs-th"
    "ghc-platform"
    "ghc-prim"
    "ghc-toolchain"
    "haddock-api"
    "haddock-library"
    "haskeline"
    "hpc"
    "integer-gmp"
    "integer-simple"
    "mtl"
    "os-string"
    "parsec"
    "pretty"
    "process"
    "rts"
    "semaphore-compat"
    "stm"
    "system-cxx-std-lib"
    "template-haskell"
    "terminfo"
    "text"
    "time"
    "transformers"
    "unix"
    "Win32"
    "xhtml"
  ];
}
