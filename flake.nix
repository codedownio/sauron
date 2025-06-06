{
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.gitignore = {
    url = "github:hercules-ci/gitignore.nix";
    inputs.nixpkgs.follows = "nixpkgs";
  };
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";

  outputs = { self, flake-utils, gitignore, haskellNix, nixpkgs }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            haskellNix.overlay
            (import ./nix/fix-ghc-pkgs-overlay.nix)
          ];
          inherit (haskellNix) config;
        };

        src = gitignore.lib.gitignoreSource ./.;

        compilerNixName = "ghc966";

        flake = (pkgs.haskell-nix.hix.project {
          inherit src;
          evalSystem = "x86_64-linux";
          compiler-nix-name = compilerNixName;
          projectFileName = "stack.yaml";
          modules = [
            (import ./nix/fix-ghc-pkgs-module.nix)
            {
              packages.sauron.components.exes.sauron.configureFlags = [
                ''--ghc-options="-pgml g++"''
              ];
              packages.sauron.components.exes.sauron.build-tools = [pkgs.gcc];
              packages.sauron.components.exes.sauron.dontStrip = false;
            }
          ];
        }).flake {};

        flakeStatic = (pkgs.pkgsCross.musl64.haskell-nix.hix.project {
          inherit src;
          evalSystem = "x86_64-linux";
          compiler-nix-name = compilerNixName;
          projectFileName = "stack.yaml";
          modules = [
            (import ./nix/fix-ghc-pkgs-module.nix)
            {
              packages.sauron.components.exes.sauron.configureFlags = [
                ''--ghc-options="-pgml g++"''
              ];
              packages.sauron.components.exes.sauron.build-tools = [pkgs.gcc];
              packages.sauron.components.exes.sauron.dontStrip = false;
            }
          ];
        }).flake {};

        flakeWindows = (pkgs.pkgsCross.mingwW64.haskell-nix.hix.project {
          inherit src;
          compiler-nix-name = compilerNixName;
          projectFileName = "stack.yaml";
          modules = [
            {
              packages.bitvec.components.library.configureFlags = [
                "-f -simd"
              ];
            }
          ];
        }).flake {};

      in
        {
          devShells = {
            default = pkgs.mkShell {
              buildInputs = with pkgs; [
                gmp
                ncurses
                pcre
                pkg-config
                zlib
              ];
            };
          };

          packages = rec {
            inherit pkgs flake flakeStatic;

            inherit (pkgs) cabal2nix;

            normal = flake.packages."sauron:exe:sauron";
            static = flakeStatic.packages."sauron:exe:sauron";
            windows = flakeWindows.packages."sauron:exe:sauron";

            print-nixpkgs = pkgs.writeShellScriptBin "print-nixpkgs.sh" "echo ${pkgs.path}";
          };
        }
    );
}
