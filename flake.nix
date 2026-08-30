{
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.gitignore = {
    url = "github:hercules-ci/gitignore.nix";
    inputs.nixpkgs.follows = "nixpkgs";
  };
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  # 26.05 rather than unstable: it's the last release supporting x86_64-darwin, and
  # still has haskellPackages.stack 3.9.3, ghc9124 and HLS 2.13.0.0.
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-2605";

  outputs = { self, flake-utils, gitignore, haskellNix, nixpkgs }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgsNoOverlays = import nixpkgs { inherit system; };

        patchedHaskellNix = pkgsNoOverlays.applyPatches {
          name = "haskell-nix-patched";
          src = haskellNix;
          patches = [
            ./nix/haskell-nix-patches/ghc-allow-multiple-definition.patch
            ./nix/haskell-nix-patches/windows-crypton-x509-system-patch-path.patch
          ];
        };

        # hackage.nix has a package whose Cabal flag is literally named "3d", which
        # it emits unquoted, so importing it is a Nix syntax error.
        haskellNixOverlay = (import (patchedHaskellNix + "/overlays") {
          sources = haskellNix.inputs // {
            hackage-for-stackage = pkgsNoOverlays.applyPatches {
              name = "hackage-for-stackage-hgg-3d-flag";
              src = haskellNix.inputs.hackage-for-stackage;
              patches = [ ./nix/haskell-nix-patches/fix-hgg-3d-flag.patch ];
            };
          };
        }).combined;

        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            haskellNixOverlay
            (import ./nix/fix-ghc-pkgs-overlay.nix)
          ];
          inherit (haskellNix) config;
        };

        src = gitignore.lib.gitignoreSource ./.;

        # Must match the GHC the stack.yaml resolver targets, or haskell.nix tries to
        # rebuild the compiler's own boot libs and configure fails with
        # "Encountered missing or private dependencies".
        compilerNixVersion = "9124";
        compilerNixName = "ghc" + compilerNixVersion;

        flake = (pkgs.haskell-nix.hix.project {
          inherit src;
          evalSystem = "x86_64-linux";
          compiler-nix-name = compilerNixName;
          projectFileName = "stack.yaml";
          modules = [
            (import ./nix/fix-ghc-pkgs-module.nix)
            (import ./nix/os-string-module.nix)
            (import ./nix/module-normal.nix { inherit (pkgs) gcc lib stdenv; })
          ];
        }).flake {};

        # pkgsStatic rather than pkgsCross.musl64: haskell.nix derives the compiler's
        # enableShared from `!isCrossTarget && !targetPlatform.isStatic`, and only
        # pkgsStatic sets isStatic, so only here does it build the static by default
        # GHC that a static executable needs. Under plain musl64 it builds a dynamic
        # GHC, Cabal adds -dynamic-too for TemplateHaskell, and that shared link
        # cannot take the -optl=-static haskell.nix passes for musl executables.
        flakeStatic = (pkgs.pkgsStatic.haskell-nix.hix.project {
          inherit src;
          evalSystem = "x86_64-linux";
          compiler-nix-name = compilerNixName;
          projectFileName = "stack.yaml";
          modules = [
            (import ./nix/fix-ghc-pkgs-module.nix)
            (import ./nix/os-string-module.nix)
            (import ./nix/module-static.nix { inherit (pkgs) pkgsStatic; })
          ];
        }).flake {};

        flakeDarwinStatic = (pkgs.haskell-nix.hix.project {
          inherit src;
          evalSystem = "x86_64-linux";
          compiler-nix-name = compilerNixName;
          projectFileName = "stack.yaml";
          modules = [
            (import ./nix/fix-ghc-pkgs-module.nix)
            (import ./nix/os-string-module.nix)
            (import ./nix/module-darwin-static.nix { inherit (pkgs) clang pkgsStatic; })
          ];
        }).flake {};

        flakeWindows = (pkgs.pkgsCross.mingwW64.haskell-nix.hix.project {
          inherit src;
          compiler-nix-name = compilerNixName;
          projectFileName = "stack.yaml";
          modules = [
            (import ./nix/fix-ghc-pkgs-module.nix)
            (import ./nix/os-string-module.nix)
            (import ./nix/module-windows.nix {})
          ];
        }).flake {};

        version = flake.packages."sauron:exe:sauron".version;

        # We only want the HLS binary, so skip two things that make it painful to
        # build: GHC 9.12.4 panics (lookupIdSubst) on ghcide's profiling objects,
        # and haddock deadlocks under the -j nixpkgs passes it.
        hlsPackages = pkgs.haskell.packages.${compilerNixName}.override {
          overrides = _hfinal: hprev: {
            mkDerivation = args: hprev.mkDerivation (args // {
              enableLibraryProfiling = false;
              doHaddock = false;
            });
          };
        };

        mkGithubArtifacts = binary: system: exeSuffix:
          with pkgs; runCommand "github-artifacts-${system}-${version}" {} ''
          mkdir $out
          BINARY="sauron${exeSuffix}"
          cp "${binary}/bin/$BINARY" "$out/$BINARY"
          tar -czvf "$out/sauron-${system}-${version}.tar.gz" -C "$out" "$BINARY"
          rm "$out/$BINARY"
        '';

      in
        {
          devShells = {
            default = pkgs.mkShell {
              buildInputs = with pkgs; [
                haskellPackages.stack

                gmp
                ncurses
                pcre
                pkg-config
                zlib

                pkgs.haskell.compiler.${compilerNixName}
                hlsPackages.haskell-language-server

                (pkgs.vhs.overrideAttrs (old: {
                  patches = (old.patches or []) ++ [
                    ./nix/vhs-add-home-end-keys.patch
                  ];
                }))
              ];
            };
          };

          packages = rec {
            dynamic = flake.packages."sauron:exe:sauron";
            static = flakeStatic.packages."sauron:exe:sauron";
            darwin-static = flakeDarwinStatic.packages."sauron:exe:sauron";
            windows = flakeWindows.packages."sauron:exe:sauron";

            default = dynamic;

            githubArtifacts = let
              binary = if pkgs.stdenv.hostPlatform.isDarwin then darwin-static
                       else if pkgs.stdenv.hostPlatform.isWindows then windows
                       else if pkgs.stdenv.hostPlatform.isLinux && pkgs.stdenv.hostPlatform.isAarch64 then dynamic
                       else if pkgs.stdenv.hostPlatform.isLinux then static
                       else throw "Unrecognized platform: ${pkgs.stdenv.hostPlatform.system}";
              exeSuffix = if pkgs.stdenv.hostPlatform.isWindows then ".exe" else "";
            in
              mkGithubArtifacts binary pkgs.stdenv.hostPlatform.system exeSuffix;

            windowsGithubArtifacts = mkGithubArtifacts windows "x86_64-windows" ".exe";

            grandCombinedGithubArtifacts = pkgs.symlinkJoin {
              name = "sauron-grand-combined-artifacts";
              paths = [
                self.packages.x86_64-linux.githubArtifacts
                self.packages.aarch64-linux.githubArtifacts

                self.packages.x86_64-darwin.githubArtifacts
                self.packages.aarch64-darwin.githubArtifacts

                self.packages.x86_64-linux.windowsGithubArtifacts
              ];
            };

            inherit version;
          };
        }
    );
}
