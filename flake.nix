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

        compilerNixName = "ghc9122";

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

        flakeStatic = (pkgs.pkgsCross.musl64.haskell-nix.hix.project {
          inherit src;
          evalSystem = "x86_64-linux";
          compiler-nix-name = compilerNixName;
          projectFileName = "stack.yaml";
          modules = [
            (import ./nix/fix-ghc-pkgs-module.nix)
            (import ./nix/os-string-module.nix)
            (import ./nix/module-static.nix { inherit (pkgs) pkgsCross; })
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
            (import ./nix/os-string-module.nix)
            (import ./nix/module-windows.nix {})
          ];
        }).flake {};

        version = flake.packages."sauron:exe:sauron".version;

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
                gmp
                ncurses
                pcre
                pkg-config
                zlib

                pkgs.haskell.compiler.ghc9122
                (pkgs.haskell-language-server.override { supportedGhcVersions = ["9122"]; })
              ];
            };
          };

          packages = rec {
            normal = flake.packages."sauron:exe:sauron";
            static = flakeStatic.packages."sauron:exe:sauron";
            darwin-static = flakeDarwinStatic.packages."sauron:exe:sauron";
            windows = flakeWindows.packages."sauron:exe:sauron";

            default = static;

            githubArtifacts = let
              binary = if pkgs.stdenv.hostPlatform.isDarwin then darwin-static
                       else if pkgs.stdenv.hostPlatform.isWindows then windows
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
                self.packages.x86_64-linux.windowsGithubArtifacts
                self.packages.x86_64-darwin.githubArtifacts
                self.packages.aarch64-darwin.githubArtifacts
              ];
            };
          };
        }
    );
}
