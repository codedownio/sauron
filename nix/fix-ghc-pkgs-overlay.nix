final: prev: {
  hixProject = compiler-nix-name: src:
    final.haskell-nix.hix.project {
      inherit src;
      evalSystem = "x86_64-linux";
      inherit compiler-nix-name;
      modules = [
        {
          reinstallableLibGhc = false;
          nonReinstallablePkgs = [
            "rts"
            "ghc-heap"
            "ghc-prim"
            "integer-gmp"
            "integer-simple"
            "base"
            "deepseq"
            "array"
            "ghc-boot-th"
            "pretty"
            "template-haskell"
            "ghcjs-prim"
            "ghcjs-th"
            "ghc-bignum"
            "exceptions"
            "stm"
            "ghc-boot"
            "ghc"
            "Cabal"
            "Win32"
            "array"
            "binary"
            "bytestring"
            "containers"
            "directory"
            "filepath"
            "ghc-boot"
            "ghc-compact"
            "ghc-prim"
            "hpc"
            "mtl"
            "parsec"
            "process"
            "text"
            "time"
            "transformers"
            "unix"
            "xhtml"
            "terminfo"
          ];
        } {
            packages.sauron.components.exes.sauron.configureFlags = [
                ''--ghc-options="-pgml g++"''
            ];
            packages.sauron.components.exes.sauron.build-tools = [prev.gcc];
            packages.sauron.components.exes.sauron.dontStrip = false;
        }
      ];
    };
}
